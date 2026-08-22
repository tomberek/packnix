# A packrat/PEG parsing engine: one self-referential lazy `Derivs` node per
# input position (Ford's "Packrat Parsing: Simple, Powerful, Lazy, Linear
# Time", arXiv cs/0603077). `mkNode` builds each node exactly once, from a
# single root (`buildDerivs`); everything else reaches a position via
# already-built `.next` pointers (`advanceN`), so Nix's ordinary
# thunk-sharing gives memoization for free.
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
# `mkCompile string` returns `compile : expr -> (derivs -> result)`, which
# decides once which combinator an `expr` denotes (recursing into
# sub-expressions) instead of re-testing its shape on every call.
rec {
  # Walk `n` `.next` pointers. Plain recursion has no tail-call
  # optimization in Nix (overflows past ~10000 deep); `foldl'` avoids that,
  # but only forces its accumulator to WHNF, not the `.next` field inside
  # it, so `builtins.seq` forces each step -- otherwise the same thunk
  # buildup just reappears one level up.
  advanceN =
    derivs: n:
    builtins.foldl' (acc: _: builtins.seq acc acc.next) derivs (builtins.genList (_: null) n);

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
    string:
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
            (advanceN derivs n)
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
            derivs.next
          ]
        else
          false;

      # A fixed lookahead window, not the whole remaining input -- copying
      # the full remainder on every attempt makes the parse O(n^2). 256 is
      # a speed/memory tuning knob, not a correctness bound: `tryWindow`
      # doubles whenever a match exactly fills the window (otherwise
      # indistinguishable from truncation), so longer matches still parse
      # correctly, especially wherever the regex is looped via `star`.
      regexWindow = 256;

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
                  (advanceN derivs matchedLen)
                ]
              else
                tryWindow (windowSize * 2) # filled the window -- might be truncated
            else
              false;
        in
        tryWindow regexWindow;

      compileSeq =
        exprs:
        let
          compiledSubs = map compile exprs;
          # Hoisted above `derivs:` -- this step function doesn't reference
          # `derivs` (only `acc`/`subCompiled`, both fold-local), so leaving
          # it below `derivs:` would re-close it on every call the returned
          # `derivs -> result` closure receives, for no reason.
          step =
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
                ];
        in
        derivs: builtins.foldl' step [ [ ] derivs ] compiledSubs;

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
      # past ~10000), forcing each step's Derivs pointer with `seq` (same
      # reasoning as `advanceN`) and collecting values via `harvest`
      # afterward rather than an accumulator, since `acc ++ [x]` every
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
          cheapChunk =
            i: acc: d:
            if i >= starChunkSize then
              {
                hitLimit = true;
                values = acc;
                d = d;
              }
            else
              let
                r = compiledBody d;
              in
              if r == false then
                {
                  hitLimit = false;
                  values = acc;
                  d = d;
                }
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
        in
        if !first.hitLimit then
          [
            first.values
            first.d
          ]
        else
          let
            closure = builtins.genericClosure {
              startSet = [
                {
                  key = 0;
                  d = first.d;
                  matched = true;
                  v = null;
                }
              ];
              inherit operator;
            };
          in
          [
            (first.values ++ harvest "v" closure)
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

  # Build the Derivs chain for `string` under `grammar`, passing each
  # nonterminal's raw parse value through `handlers.<Name>` (default
  # identity). `mkNode` runs exactly once per position (via `.next`);
  # calling it again for the same count would break the sharing that
  # makes memoization work.
  buildDerivs =
    grammar: handlers: string:
    let
      compile = mkCompile string;
      len = builtins.stringLength string;

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

      # `count`/`next` as regular entries alongside every rule's compiled
      # field, so mkNode's mapAttrs below builds each node's ENTIRE field
      # set in one pass instead of building a rule-fields-only attrset via
      # mapAttrs and then copying it into a base `{count;next;}` via `//`.
      # `//` isn't a lazy structural merge -- it allocates a fresh
      # Bindings array and copies every binding from both sides into it,
      # so stacking it on an already-built mapAttrs result was two full
      # attrset allocations per node where one suffices. Confirmed
      # directly (400k-node microbenchmark matching this engine's actual
      # per-node shape): ~195MB set bytes two-pass vs. ~99MB one-pass --
      # roughly half, and it's the highest-volume allocation site in the
      # whole engine (once per input position, unconditionally).
      compiledFieldsAndBase = compiledFields // {
        count = null;
        next = null;
      };

      mkNode =
        count:
        let
          node = builtins.mapAttrs (
            name: field:
            if name == "count" then
              count
            else if name == "next" then
              (if count >= len then null else mkNode (count + 1))
            else
              field node
          ) compiledFieldsAndBase;
        in
        node;
    in
    mkNode 0;

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
      root = buildDerivs grammar handlers string;
      at = advanceN root count;
    in
    builtins.mapAttrs (name: _: if at.${name} != false then builtins.elemAt at.${name} 0 else false) grammar;
}
