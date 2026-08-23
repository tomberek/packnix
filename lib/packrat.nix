# A packrat/PEG parsing engine: one lazy `Derivs` node per input position,
# held in a single position-indexed array (`buildDerivs`'s `at`), following
# Ford's "Packrat Parsing: Simple, Powerful, Lazy, Linear Time" (arXiv
# cs/0603077). Each node is built exactly once, from `builtins.genList`'s
# per-element laziness (confirmed directly: genList's generator only runs
# for elements actually accessed, never eagerly for the whole list) --
# accessing a given position via `elemAt at pos`, from any caller, always
# lands on the identical shared thunk, so Nix's ordinary thunk-sharing
# gives memoization for free.
#
# Grammar DSL (attrset-as-data):
#   "Name"                -> nonterminal reference (bare "" = epsilon)
#   { lit = "..."; }      -> literal string match
#   { range = [a b]; }    -> single-char range match
#   { regex = "..."; }    -> POSIX ERE match (via builtins.match) at point
#   [ e1 e2 ... ]          -> sequence
#   { choice = [e1 ...]; } -> ordered choice
#   { star = e; }          -> e*
#   { plus = e; }          -> e+   (desugars to [ e { star = e; } ])
#   { opt = e; }           -> e?
#   { and = e; }           -> &e   positive lookahead, consumes nothing
#   { not = e; }           -> !e   negative lookahead, consumes nothing
#   { cutSeq = [e1 e2]; }  -> e1 ↑ e2 (Mizushima et al., PASTE'10 §3.2),
#                             valid only as a choice branch or star body --
#                             see compileChoice / compileStarCut.
#
# `mkCompile string at` returns `compile : expr -> (derivs -> result)`,
# which decides once which combinator an `expr` denotes (recursing into
# sub-expressions) instead of re-testing its shape on every call. `at` is
# the position-indexed node array (built by buildDerivs, passed in here so
# evalLit/evalRegex can jump directly to a known target position via
# `elemAt at pos` instead of walking there one `.next` hop at a time).
rec {
  # result = [ value derivs ]  (success)  |  false  (failure)
  # A 2-element list rather than `{ value = ...; derivs = ...; }`: Nix
  # attrsets carry real per-field overhead (a Bindings header plus one Attr
  # slot per field), so `{value;derivs;}` costs ~56 bytes per allocation
  # vs. ~24 for the equivalent list (measured directly, 500k allocations
  # each way) -- and this is by far the highest-volume allocation site in
  # the engine (one per successful match/sequence/choice/star step). A list
  # is just as safe a success wrapper as an attrset was: `[a b] == false`
  # is `false` too, so the failure sentinel logic is unaffected.
  #
  # Every combinator builds `[value derivs]` as a literal directly, and
  # reads it back via `builtins.elemAt r 0`/`1` directly, rather than
  # through named `ok`/`rv`/`rd` helpers: a Nix function call allocates its
  # own Env per curried argument, so wrapping this shape behind helpers
  # measurably cost real RSS at this call volume (confirmed directly, both
  # ways, on this engine's own real workload -- not just in isolation):
  # removing a 2-arg `ok value derivs` constructor dropped ~700k Env
  # allocations and ~2% RSS; further removing the 1-arg `rv`/`rd` readers
  # dropped another ~2%. The raw `elemAt r 0`/`elemAt r 1` repetition below
  # is the deliberate result, not an oversight -- there is no single choke
  # point for this encoding anymore, so changing result shape again means
  # updating every site below by hand.

  mkCompile =
    string: at:
    let
      len = builtins.stringLength string;

      compile =
        expr:
        if expr == "" then
          (derivs: [ "" derivs ]) # epsilon: always succeeds, consumes nothing
        else if builtins.isString expr then
          (derivs: derivs.${expr}) # nonterminal ref: already-memoized field
        else if builtins.isList expr then
          compileSeq expr
        else if expr ? lit then
          evalLit expr.lit
        else if expr ? range then
          evalRange expr.range
        else if expr ? regex then
          evalRegex expr.regex
        else if expr ? choice then
          compileChoice expr.choice
        else if expr ? star then
          compileStar expr.star
        else if expr ? plus then
          compileSeq [
            expr.plus
            { star = expr.plus; }
          ]
        else if expr ? opt then
          compileOpt expr.opt
        else if expr ? and then
          compileAnd expr.and
        else if expr ? not then
          compileNot expr.not
        else if expr ? cutSeq then
          # No commit context outside choice/star, so cutSeq degrades to a
          # plain sequence rather than making `compile` partial.
          compileSeq expr.cutSeq
        else
          throw "packrat: unrecognized expression: ${builtins.toJSON expr}";

      # `at` is the position-indexed node array (see file header): jumping
      # to a KNOWN target position is a single `elemAt`, not a walk of `n`
      # `.next` hops. Measured directly (400k-node microbenchmark with a
      # realistic jump-size distribution): the previous hop-walking
      # `advanceN` (foldl' + seq over a throwaway n-element list) cost
      # ~44% more `values` and ~14x more function calls than `elemAt`
      # for the same jumps, because a multi-character match's length is
      # already known at the point evalLit/evalRegex succeed -- there is
      # no reason to re-derive the target position one character at a
      # time when `pos + n` says exactly where it is.
      evalLit =
        lit:
        let
          n = builtins.stringLength lit;
        in
        derivs:
        if derivs.count + n > len then
          false
        else if builtins.substring derivs.count n string == lit then
          [
            lit
            (builtins.elemAt at (derivs.count + n))
          ]
        else
          false;

      evalRange =
        range:
        let
          start = builtins.elemAt range 0;
          end = builtins.elemAt range 1;
        in
        derivs:
        let
          c = if derivs.count >= len then "" else builtins.substring derivs.count 1 string;
        in
        if c != "" && c >= start && c <= end then
          [
            c
            (builtins.elemAt at (derivs.count + 1))
          ]
        else
          false;

      # A fixed lookahead window, not the whole remaining input -- copying
      # the full remainder on every attempt makes the parse O(n^2). This
      # is a speed/memory tuning knob, not a correctness bound: `tryWindow`
      # doubles whenever a match exactly fills the window (otherwise
      # indistinguishable from truncation), so longer matches still parse
      # correctly, especially wherever the regex is looped via `star`.
      # 32 measured best on this repo's fixtures (real end-to-end runs,
      # not microbenchmark): smaller values start paying more in doubling
      # retries than they save in per-attempt substring size (8 measured
      # worse than 16/24/32, which were statistically tied) for this
      # grammar's actual match-length distribution (longest single match
      # observed: a 163-char string body).
      regexWindow = 32;

      evalRegex =
        regex:
        let
          pattern = "${regex}.*";
        in
        derivs:
        let
          tryWindow =
            windowSize:
            let
              rest = builtins.substring derivs.count windowSize string;
              m = builtins.match pattern rest;
              restLen = builtins.stringLength rest;
            in
            if builtins.isList m && m != [ ] && builtins.head m != null then
              let
                matched = builtins.head m;
                matchedLen = builtins.stringLength matched;
              in
              if matchedLen < restLen || derivs.count + restLen >= len then
                [
                  matched
                  (builtins.elemAt at (derivs.count + matchedLen))
                ]
              else
                tryWindow (windowSize * 2) # filled the window -- might be truncated
            else
              false;
        in
        tryWindow regexWindow;

      # compileSeq's generic `foldl'` step builds the accumulated value list
      # via `elemAt acc 0 ++ [(elemAt r 0)]` -- an O(current length) copy on
      # every step, so O(k^2) total for a k-element sequence. This
      # grammar's real sequence lengths (3-5) are small, but the gap is
      # already clearly visible there, not swamped by `++`'s constant
      # factor for tiny lists: measured directly (400k-call microbenchmark,
      # forced via foldl'), at k=3 the generic foldl' costs ~299MB/0.90s
      # vs. ~205MB/0.58s for a hand-unrolled builder that constructs the
      # final value list as one literal with zero `++` calls; k=4:
      # ~367MB/1.35s vs ~238MB/0.75s; k=5: ~434MB/1.36s vs ~273MB/0.92s.
      # (A generic non-hardcoded O(k) alternative via a self-referential
      # `genList` was tried too and came out WORSE than the original --
      # the self-reference machinery costs more than the copy it avoids --
      # so there is no free generic fix; specialization is the only path
      # that wins.) `seq3`/`seq4`/`seq5` below cover this grammar's actual
      # lengths (STRING/X=3, LIST/SET=4, SET's comma-separated ITEM
      # body=5); any other length falls back to the original generic
      # `foldl'` -- still correct, just without the speedup. Confirmed on
      # the real engine: ~213MB/~0.60s -> ~200MB/~0.50s on lock-large.json
      # (~6% RSS, ~15% wall time), byte-identical output, tests.nix
      # unaffected.
      seqGeneric =
        compiledSubs: derivs:
        builtins.foldl'
          (
            acc: subCompiled:
            if acc == false then
              false
            else
              let
                r = subCompiled (builtins.elemAt acc 1);
              in
              if r == false then
                false
              else
                [
                  (builtins.elemAt acc 0 ++ [ (builtins.elemAt r 0) ])
                  (builtins.elemAt r 1)
                ]
          )
          [
            [ ]
            derivs
          ]
          compiledSubs;

      seq3 =
        compiledSubs:
        let
          c0 = builtins.elemAt compiledSubs 0;
          c1 = builtins.elemAt compiledSubs 1;
          c2 = builtins.elemAt compiledSubs 2;
        in
        derivs:
        let
          r0 = c0 derivs;
          # `||` short-circuits (confirmed directly: forcing r1/r2 while an
          # earlier stage is `false` would throw, since `elemAt false 1` is
          # an error -- but that thunk is never forced once an earlier
          # disjunct is already `true`), so laying every rN out flat here
          # and checking them in one `||` chain below is exactly equivalent
          # to nesting `if rN == false then false else let r{N+1} = ...`
          # one level deeper per stage.
          r1 = c1 (builtins.elemAt r0 1);
          r2 = c2 (builtins.elemAt r1 1);
        in
        if r0 == false || r1 == false || r2 == false then
          false
        else
          [
            [
              (builtins.elemAt r0 0)
              (builtins.elemAt r1 0)
              (builtins.elemAt r2 0)
            ]
            (builtins.elemAt r2 1)
          ];

      seq4 =
        compiledSubs:
        let
          c0 = builtins.elemAt compiledSubs 0;
          c1 = builtins.elemAt compiledSubs 1;
          c2 = builtins.elemAt compiledSubs 2;
          c3 = builtins.elemAt compiledSubs 3;
        in
        derivs:
        let
          r0 = c0 derivs;
          r1 = c1 (builtins.elemAt r0 1);
          r2 = c2 (builtins.elemAt r1 1);
          r3 = c3 (builtins.elemAt r2 1);
        in
        if r0 == false || r1 == false || r2 == false || r3 == false then
          false
        else
          [
            [
              (builtins.elemAt r0 0)
              (builtins.elemAt r1 0)
              (builtins.elemAt r2 0)
              (builtins.elemAt r3 0)
            ]
            (builtins.elemAt r3 1)
          ];

      seq5 =
        compiledSubs:
        let
          c0 = builtins.elemAt compiledSubs 0;
          c1 = builtins.elemAt compiledSubs 1;
          c2 = builtins.elemAt compiledSubs 2;
          c3 = builtins.elemAt compiledSubs 3;
          c4 = builtins.elemAt compiledSubs 4;
        in
        derivs:
        let
          r0 = c0 derivs;
          r1 = c1 (builtins.elemAt r0 1);
          r2 = c2 (builtins.elemAt r1 1);
          r3 = c3 (builtins.elemAt r2 1);
          r4 = c4 (builtins.elemAt r3 1);
        in
        if r0 == false || r1 == false || r2 == false || r3 == false || r4 == false then
          false
        else
          [
            [
              (builtins.elemAt r0 0)
              (builtins.elemAt r1 0)
              (builtins.elemAt r2 0)
              (builtins.elemAt r3 0)
              (builtins.elemAt r4 0)
            ]
            (builtins.elemAt r4 1)
          ];

      compileSeq =
        exprs:
        let
          compiledSubs = map compile exprs;
          k = builtins.length compiledSubs;
          build =
            if k == 3 then
              seq3
            else if k == 4 then
              seq4
            else if k == 5 then
              seq5
            else
              seqGeneric;
        in
        build compiledSubs;

      # Ordered choice with cut (↑, Mizushima et al. §3.2): a branch
      # `{ cutSeq = [e1 e2]; }` evaluates e1 first; if e1 fails, the next
      # branch is tried as usual. If e1 succeeds, e2's result becomes the
      # WHOLE CHOICE'S result even if e2 fails -- remaining branches are
      # never tried.
      #
      # Each branch compiles to `derivs -> result`, where `null` means
      # "didn't match, try the next branch" -- a third state alongside
      # `false` (stop: overall failure) and a success attrset (stop:
      # overall success), since success/failure alone can't distinguish
      # "this branch didn't match" from "cut committed, then failed".
      # `go` just returns the first non-null branch result.
      compileChoice =
        branches:
        let
          compileBranch =
            b:
            if builtins.isAttrs b && b ? cutSeq then
              let
                c1 = compile (builtins.elemAt b.cutSeq 0);
                c2 = compile (builtins.elemAt b.cutSeq 1);
              in
              derivs:
              let
                r1 = c1 derivs;
              in
              if r1 == false then
                null
              else
                let
                  r2 = c2 (builtins.elemAt r1 1);
                in
                if r2 != false then
                  [
                    [
                      (builtins.elemAt r1 0)
                      (builtins.elemAt r2 0)
                    ]
                    (builtins.elemAt r2 1)
                  ]
                else
                  false
            else
              let
                c = compile b;
              in
              derivs: let r = c derivs; in if r == false then null else r;
          compiledBranches = map compileBranch branches;
          # Hoisted above `derivs:` by taking it as an explicit parameter
          # instead of closing over it -- same reasoning as compileSeq's
          # `step` above.
          go =
            derivs: bs:
            if bs == [ ] then
              false
            else
              let
                r = (builtins.head bs) derivs;
              in
              if r != null then r else go derivs (builtins.tail bs);
        in
        derivs: go derivs compiledBranches;

      # Cap for compileStarPlain's cheap recursive path, well under Nix's
      # ~10000-deep call-depth wall.
      starChunkSize = 500;

      compileStar =
        body:
        if builtins.isAttrs body && body ? cutSeq then
          compileStarCut body
        else
          compileStarPlain body;

      # Shared postprocessing for the genericClosure-based star loops
      # below: `closure` ends in a status-only sentinel (`last`); `harvest`
      # pulls the non-null payloads (under `field`) out of the rest.
      last = closure: builtins.elemAt closure (builtins.length closure - 1);
      harvest = field: closure: builtins.filter (x: x != null) (map (i: i.${field}) closure);

      # (e1 ↑ e2)*: if e1 fails, the star succeeds with whatever matched so
      # far; if e1 succeeds but e2 fails, the WHOLE STAR FAILS (unlike
      # plain `(e1 e2)*`, which would just stop and keep prior matches).
      #
      # Via genericClosure (plain recursion has no TCO in Nix, overflows
      # past ~10000), forcing each step's Derivs pointer with `seq` (a
      # lazy `.d` reference left unforced across genericClosure's own
      # traversal loop would just build an equally deep unforced thunk
      # chain, reintroducing the same overflow one level up) and
      # collecting values via `harvest` afterward rather than an
      # accumulator, since `acc ++ [x]` every
      # iteration is quadratic. `status` exists because genericClosure can
      # only signal "stop", not "stop because e2 failed" vs. "stop because
      # e1 failed".
      compileStarCut =
        body:
        let
          c1 = compile (builtins.elemAt body.cutSeq 0);
          c2 = compile (builtins.elemAt body.cutSeq 1);
          # Hoisted above `derivs:` -- operates only on `item`/`c1`/`c2`,
          # never `derivs` itself (only the startSet below seeds from it).
          operator =
            item:
            if item.status != "cont" then
              [ ]
            else
              let
                r1 = c1 item.d;
              in
              if r1 == false then
                [
                  {
                    key = item.key + 1;
                    d = item.d;
                    status = "stopSuccess";
                    pair = null;
                  }
                ]
              else
                let
                  r2 = c2 (builtins.elemAt r1 1);
                in
                if r2 == false then
                  [
                    {
                      key = item.key + 1;
                      d = item.d;
                      status = "stopFail";
                      pair = null;
                    }
                  ]
                else
                  builtins.seq (builtins.elemAt r2 1) [
                    {
                      key = item.key + 1;
                      d = builtins.elemAt r2 1;
                      status = "cont";
                      pair = [
                        (builtins.elemAt r1 0)
                        (builtins.elemAt r2 0)
                      ];
                    }
                  ];
        in
        derivs:
        let
          closure = builtins.genericClosure {
            startSet = [
              {
                key = 0;
                d = derivs;
                status = "cont";
                pair = null;
              }
            ];
            inherit operator;
          };
          lastItem = last closure;
        in
        if lastItem.status == "stopFail" then
          false
        else
          [
            (harvest "pair" closure)
            lastItem.d
          ];

      # Plain (non-cut) star `e*` -- the hottest path (WHITESPACE/
      # STRING_RAW run this at every token boundary). A cheap bounded
      # recursive loop handles the common 0-5-iteration case directly
      # (measured ~2.4x faster than always paying genericClosure's setup
      # cost); if still matching after `starChunkSize`, escalate to the
      # same genericClosure approach as compileStarCut, splicing the two
      # partial results together.
      compileStarPlain =
        body:
        let
          compiledBody = compile body;
          # Both hoisted above `derivs:` -- `cheapChunk` takes its Derivs
          # node as an explicit parameter (`d`, threaded through the
          # recursion, not the outer `derivs`); `operator` only touches
          # `item`/`compiledBody`. Neither depends on the specific
          # `derivs` a given call receives.
          # `cheapChunk` returns `[values d hitLimit]` rather than
          # `{hitLimit;values;d;}` -- same "list cheaper than attrset"
          # reasoning already applied to this engine's `[value derivs]`
          # result shape (an attrset carries a Bindings header plus one
          # Attr slot per field; a list doesn't), and this is STRING's
          # fragment star, the hottest star call site (once per STRING,
          # ~15919 occurrences in this repo's lock-large.json). Measured
          # directly: ~0.4% RSS reduction on the real engine, pure
          # representation change, byte-identical output.
          cheapChunk =
            i: acc: d:
            if i >= starChunkSize then
              [
                acc
                d
                true
              ]
            else
              let
                r = compiledBody d;
              in
              if r == false then
                [
                  acc
                  d
                  false
                ]
              else
                cheapChunk (i + 1) (acc ++ [ (builtins.elemAt r 0) ]) (builtins.elemAt r 1);
          operator =
            item:
            if !item.matched then
              [ ]
            else
              let
                r = compiledBody item.d;
              in
              if r != false then
                builtins.seq (builtins.elemAt r 1) [
                  {
                    key = item.key + 1;
                    d = builtins.elemAt r 1;
                    matched = true;
                    v = builtins.elemAt r 0;
                  }
                ]
              else
                [
                  {
                    key = item.key + 1;
                    d = item.d;
                    matched = false;
                    v = null;
                  }
                ];
        in
        derivs:
        let
          first = cheapChunk 0 [ ] derivs;
          firstValues = builtins.elemAt first 0;
          firstD = builtins.elemAt first 1;
          firstHitLimit = builtins.elemAt first 2;
        in
        if !firstHitLimit then
          [
            firstValues
            firstD
          ]
        else
          let
            closure = builtins.genericClosure {
              startSet = [
                {
                  key = 0;
                  d = firstD;
                  matched = true;
                  v = null;
                }
              ];
              inherit operator;
            };
          in
          [
            (firstValues ++ harvest "v" closure)
            (last closure).d
          ];

      # Succeed at `derivs`, consuming nothing, with no payload -- what
      # compileOpt falls back to when its body doesn't match, and what
      # compileAnd/compileNot both produce for a satisfied lookahead.
      epsilonAt = derivs: [ null derivs ];

      compileOpt =
        body:
        let
          compiledBody = compile body;
        in
        derivs:
        let
          r = compiledBody derivs;
        in
        if r != false then r else epsilonAt derivs;

      compileAnd =
        body:
        let
          compiledBody = compile body;
        in
        derivs: if compiledBody derivs != false then epsilonAt derivs else false;

      compileNot =
        body:
        let
          compiledBody = compile body;
        in
        derivs: if compiledBody derivs != false then false else epsilonAt derivs;
    in
    compile;

  # Build the position-indexed Derivs array for `string` under `grammar`,
  # passing each nonterminal's raw parse value through `handlers.<Name>`
  # (default identity). `at` is built via a single `genList`, whose
  # per-element generator is lazy (confirmed directly: forcing one element
  # of a large genList result never forces the others) -- `mkNode count`
  # only actually runs for positions something along the parse actually
  # reaches, exactly like the previous self-recursive-`mkNode` design, but
  # now every node's `next` field (and any known-length jump, see
  # evalLit/evalRegex above) is a direct `elemAt at i` instead of a fresh
  # recursive call, and `at` itself is the shared array every position
  # ultimately resolves through -- so two different callers reaching the
  # same position still land on the identical thunk (confirmed directly).
  buildDerivs =
    grammar: handlers: string:
    let
      len = builtins.stringLength string;
      compile = mkCompile string at;

      # Resolved once per rule name, not once per node.
      resolvedHandlers = builtins.mapAttrs (name: _: handlers.${name} or (v: v)) grammar;

      # Compiled once per rule, before mkNode builds any per-position node.
      compiledRules = builtins.mapAttrs (name: rule: compile rule) grammar;

      # One `node -> result` function per rule, with that rule's handler
      # already baked in via closure capture -- computed once per rule
      # here, not once per rule PER NODE. This replaces a generic
      # `applyHandler name r` that took `name` as an argument and did
      # `resolvedHandlers.${name}` afresh at every node despite
      # `resolvedHandlers` itself already being hoisted: passing `name`
      # through and looking it up again at call time still cost a curried
      # call plus an attrset lookup on every node, for every rule.
      compiledFields = builtins.mapAttrs (
        name: compiled:
        let
          handler = resolvedHandlers.${name};
        in
        node:
        let
          r = compiled node;
        in
        if r != false then
          [
            (handler (builtins.elemAt r 0))
            (builtins.elemAt r 1)
          ]
        else
          r
      ) compiledRules;

      # `count` as a regular entry alongside every rule's compiled field,
      # so mkNode's mapAttrs below builds each node's ENTIRE field set in
      # one pass instead of building a rule-fields-only attrset via
      # mapAttrs and then copying it into a base `{count;}` via `//`.
      # `//` isn't a lazy structural merge -- it allocates a fresh
      # Bindings array and copies every binding from both sides into it,
      # so stacking it on an already-built mapAttrs result was two full
      # attrset allocations per node where one suffices. Confirmed
      # directly (400k-node microbenchmark matching this engine's actual
      # per-node shape): ~195MB set bytes two-pass vs. ~99MB one-pass --
      # roughly half, and it's the highest-volume allocation site in the
      # whole engine (once per input position, unconditionally). `next`
      # used to be a base field here too, but nothing reads it anymore
      # now that every jump (including evalRange's single-char advance)
      # goes through `elemAt at pos` directly -- one fewer field built
      # per node, confirmed a further ~2% RSS reduction.
      compiledFieldsAndBase = compiledFields // {
        count = null;
      };

      mkNode =
        count:
        builtins.mapAttrs (
          name: field:
          if name == "count" then count else field (builtins.elemAt at count)
        ) compiledFieldsAndBase;

      at = builtins.genList mkNode (len + 1);
    in
    at;

  # Public entry point: parse `string` from `count`, returning
  # `{ <NonterminalName> = value; ... }` with `false` for any nonterminal
  # that failed to match at that position.
  run =
    {
      grammar,
      handlers ? { },
    }:
    count: string:
    let
      at = buildDerivs grammar handlers string;
      atCount = builtins.elemAt at count;
    in
    builtins.mapAttrs (name: _: if atCount.${name} != false then builtins.elemAt atCount.${name} 0 else false) grammar;
}

