# A packrat/PEG parsing engine: one lazy `Derivs` node per input position
# (see buildDerivs's `at`), following Ford's "Packrat Parsing: Simple,
# Powerful, Lazy, Linear Time". `genList`'s per-element laziness means a
# node is only built when accessed, and repeated `elemAt at pos` calls land
# on the same shared thunk, giving memoization for free via Nix's ordinary
# thunk-sharing.
#
# Grammar DSL (attrset-as-data):
#   "Name"                -> nonterminal reference (bare "" = epsilon)
#   { lit = "..."; }      -> literal string match
#   { range = [a b]; }    -> single-char range match
#   { regex = "..."; }    -> POSIX ERE match (via builtins.match) at point
#   { regex = "..."; maxLen = N; } -> same, but the caller guarantees the
#                             match can never exceed N characters (e.g. a
#                             `{0,N}`-bounded regex), skipping evalRegex's
#                             window-doubling entirely.
#   [ e1 e2 ... ]          -> sequence
#   { choice = [e1 ...]; } -> ordered choice
#   { star = e; }          -> e*
#   { plus = e; }          -> e+   (desugars to [ e { star = e; } ])
#   { opt = e; }           -> e?
#   { and = e; }           -> &e   positive lookahead, consumes nothing
#   { not = e; }           -> !e   negative lookahead, consumes nothing
#   { eof = { }; }         -> succeeds (consuming nothing) iff no input
#                             remains, fails otherwise -- "assert end of
#                             input". Unlike { not = { regex = "(.)"; }; },
#                             which achieves the same thing via a
#                             single-character lookahead, this is a plain
#                             leaf (one O(1) integer compare against the
#                             input's total length), not a lookahead over
#                             a sub-expression -- so it needs no `compile`
#                             call of its own, and lib/generate.nix can
#                             treat it as an ordinary terminal (generate
#                             "") instead of pattern-matching a specific
#                             not+regex shape.
#   { cutSeq = [e1 e2]; }  -> e1 ↑ e2 (PEG cut): valid only as a choice
#                             branch or star body -- see compileChoice /
#                             compileStarCut.
#   { action = { e; f; }; } -> e, with f applied to its VALUE on success --
#                              lets a value-transform travel with an
#                              inlined sub-expression instead of requiring
#                              a named Derivs-node field. See compileAction.
#   { json = { }; }        -> hand the ENTIRE REMAINING input (position to
#   { toml = { }; }           end of string) to builtins.fromJSON/fromTOML,
#                             rather than parsing it rule-by-rule. COMMIT-ONLY:
#                             unlike every other combinator, this THROWS
#                             instead of returning `false` on malformed input
#                             (builtins.tryEval cannot catch fromJSON/
#                             fromTOML's parse errors), so there is no way
#                             to backtrack past a failed json/toml the way
#                             `choice` backtracks past a failed `lit`/
#                             `regex`. Only place this where the grammar has
#                             ALREADY committed with no other alternative
#                             left to try -- e.g. the last `cutSeq` branch
#                             of a `choice`, or the final element of a
#                             top-level sequence. See evalBuiltinParser.
#
# `mkCompile string at` returns `compile : expr -> (derivs -> result)`,
# deciding once which combinator an `expr` denotes instead of re-testing
# its shape on every call.
rec {
  # result = [ value derivs ]  (success)  |  false  (failure)
  # A list, not an attrset, since this is the highest-volume allocation
  # site in the engine and a list is markedly cheaper per instance. `[a b]
  # == false` is `false` too, so `false` is safe as the failure sentinel.
  # Every combinator reads/builds this via `elemAt r 0`/`1` directly rather
  # than through named helpers, for the same allocation-count reason.

  # `nameToIndex.<Name>` gives the fixed list-slot index of rule `<Name>`
  # within a node (see buildDerivs's `mkNode`); slot 0 is always `count`.
  # A nonterminal reference `expr` is baked to that integer ONCE, at
  # compile time (not looked up by name on every call).
  mkCompile =
    string: at: nameToIndex:
    let
      len = builtins.stringLength string;

      compile =
        expr:
        if expr == "" then
          (derivs: [
            ""
            derivs
          ]) # epsilon: always succeeds, consumes nothing
        else if builtins.isString expr then
          (
            let
              idx = nameToIndex.${expr};
            in
            derivs: builtins.elemAt derivs idx # nonterminal ref: already-memoized list slot
          )
        else if builtins.isList expr then
          compileSeq expr
        else if expr ? lit then
          evalLit expr.lit
        else if expr ? range then
          evalRange expr.range
        else if expr ? regex then
          evalRegex expr.regex (expr.maxLen or null)
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
        else if expr ? eof then
          evalEof
        else if expr ? cutSeq then
          # No commit context outside choice/star, so cutSeq degrades to a
          # plain sequence rather than making `compile` partial.
          compileSeq expr.cutSeq
        else if expr ? action then
          compileAction expr.action.e expr.action.f
        else if expr ? json then
          evalBuiltinParser builtins.fromJSON
        else if expr ? toml then
          evalBuiltinParser builtins.fromTOML
        else
          throw "packrat: unrecognized expression: ${builtins.toJSON expr}";

      # A known-length jump is a single `elemAt`, not a walk of `n` `.next`
      # hops. `count` lives at fixed slot 0 of the node (see mkNode).
      evalLit =
        lit:
        let
          n = builtins.stringLength lit;
        in
        derivs:
        let
          count = builtins.elemAt derivs 0;
        in
        if count + n > len then
          false
        else if builtins.substring count n string == lit then
          [
            lit
            (builtins.elemAt at (count + n))
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
          count = builtins.elemAt derivs 0;
          c = if count >= len then "" else builtins.substring count 1 string;
        in
        if c != "" && c >= start && c <= end then
          [
            c
            (builtins.elemAt at (count + 1))
          ]
        else
          false;

      # `{ eof = {}; }`: succeeds, consuming nothing, iff `count == len`
      # (no characters remain) -- a direct integer comparison against the
      # input's total length, not a lookahead over a sub-expression like
      # `{ not = { regex = "(.)"; }; }` (which achieves the same result by
      # attempting and failing a single-character match).
      evalEof =
        derivs:
        let
          count = builtins.elemAt derivs 0;
        in
        if count == len then epsilonAt derivs else false;

      # A fixed lookahead window, not the whole remaining input -- copying
      # the full remainder on every attempt makes the parse O(n^2). Not a
      # correctness bound: `tryWindow` doubles whenever a match exactly
      # fills the window (otherwise indistinguishable from truncation), so
      # longer matches still parse correctly. 64 was measured to beat 32
      # (fewer doubling retries on longer lines/tokens) with diminishing
      # returns past that.
      regexWindow = 64;

      evalRegex =
        regex: maxLen:
        let
          pattern = "${regex}.*";
        in
        if maxLen != null then
          # Caller guarantees this regex can never match more than
          # `maxLen` characters (e.g. a `{0,N}`-bounded repetition, like
          # grammar/yaml.nix's per-depth indent check), so a single
          # `substring count maxLen` window always contains the whole
          # match -- no truncation possible, no doubling retry needed.
          (
            derivs:
            let
              count = builtins.elemAt derivs 0;
              rest = builtins.substring count maxLen string;
              m = builtins.match pattern rest;
            in
            if builtins.isList m && m != [ ] && builtins.head m != null then
              let
                matched = builtins.head m;
              in
              [
                matched
                (builtins.elemAt at (count + builtins.stringLength matched))
              ]
            else
              false
          )
        else
          (
            derivs:
            let
              count = builtins.elemAt derivs 0;
              tryWindow =
                windowSize:
                let
                  rest = builtins.substring count windowSize string;
                  m = builtins.match pattern rest;
                  restLen = builtins.stringLength rest;
                in
                if builtins.isList m && m != [ ] && builtins.head m != null then
                  let
                    matched = builtins.head m;
                    matchedLen = builtins.stringLength matched;
                  in
                  if matchedLen < restLen || count + restLen >= len then
                    [
                      matched
                      (builtins.elemAt at (count + matchedLen))
                    ]
                  else
                    tryWindow (windowSize * 2) # filled the window -- might be truncated
                else
                  false;
            in
            tryWindow regexWindow
          );

      # `{ json = {}; }`/`{ toml = {}; }`: hands `substring count (len -
      # count) string` (position to end of input, not a bounded window like
      # evalRegex -- fromJSON/fromTOML need the WHOLE value, and both
      # already reject trailing content, so there's no way to ask either
      # for just "the JSON/TOML prefix starting here") to a native builtin
      # parser instead of walking it rule-by-rule.
      #
      # Deliberately NOT wrapped in `builtins.tryEval`: tryEval's catch
      # clause only matches the Nix language's own `throw`/`assert`
      # exception type; fromJSON/fromTOML's parse errors are a different
      # exception type and propagate straight through uncaught. So unlike
      # every other leaf combinator here, this one cannot return `false`
      # on malformed input -- it throws, aborting the whole evaluation,
      # not just this alternative. See this file's header comment for the
      # resulting restriction on where `json`/`toml` may be used.
      #
      # `builtins.seq (parse rest) ...` forces the parse HERE, not lazily.
      # Every other combinator only ever inspects the DERIVS half (`elemAt
      # r 1`) to decide whether to keep going, never the value half -- so
      # an unforced `parse rest` thunk sitting in the value slot would let
      # a malformed json/toml silently ride through `opt`'s "succeeded"
      # path (and any enclosing choice/sequence), with the parse error
      # only surfacing later, far from this call site. Forcing eagerly
      # turns a misuse of this commit-only combinator into an immediate
      # failure at the actual parse site instead of a silently-wrong result.
      evalBuiltinParser =
        parse: derivs:
        let
          count = builtins.elemAt derivs 0;
          rest = builtins.substring count (len - count) string;
          value = parse rest;
        in
        builtins.seq value [
          value
          (builtins.elemAt at len)
        ];

      # compileSeq's generic path builds the result via `foldl'` +
      # `elemAt acc 0 ++ [...]`, an O(current length) copy per step, so
      # O(k^2) for a k-element sequence. `seq2`..`seq7` build the result as
      # one list literal instead, covering every sequence length that
      # appears in this repo's shipped grammars; longer sequences still
      # parse correctly via seqGeneric, just without the speedup.
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

      seq2 =
        compiledSubs:
        let
          c0 = builtins.elemAt compiledSubs 0;
          c1 = builtins.elemAt compiledSubs 1;
        in
        derivs:
        let
          r0 = c0 derivs;
          r1 = c1 (builtins.elemAt r0 1);
        in
        if r0 == false || r1 == false then
          false
        else
          [
            [
              (builtins.elemAt r0 0)
              (builtins.elemAt r1 0)
            ]
            (builtins.elemAt r1 1)
          ];

      seq3 =
        compiledSubs:
        let
          c0 = builtins.elemAt compiledSubs 0;
          c1 = builtins.elemAt compiledSubs 1;
          c2 = builtins.elemAt compiledSubs 2;
        in
        derivs:
        let
          # `||` short-circuits, so laying every rN out flat and checking
          # them in one chain is equivalent to nesting `if rN == false
          # then false else ...` one level deeper per stage.
          r0 = c0 derivs;
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

      seq6 =
        compiledSubs:
        let
          c0 = builtins.elemAt compiledSubs 0;
          c1 = builtins.elemAt compiledSubs 1;
          c2 = builtins.elemAt compiledSubs 2;
          c3 = builtins.elemAt compiledSubs 3;
          c4 = builtins.elemAt compiledSubs 4;
          c5 = builtins.elemAt compiledSubs 5;
        in
        derivs:
        let
          r0 = c0 derivs;
          r1 = c1 (builtins.elemAt r0 1);
          r2 = c2 (builtins.elemAt r1 1);
          r3 = c3 (builtins.elemAt r2 1);
          r4 = c4 (builtins.elemAt r3 1);
          r5 = c5 (builtins.elemAt r4 1);
        in
        if r0 == false || r1 == false || r2 == false || r3 == false || r4 == false || r5 == false then
          false
        else
          [
            [
              (builtins.elemAt r0 0)
              (builtins.elemAt r1 0)
              (builtins.elemAt r2 0)
              (builtins.elemAt r3 0)
              (builtins.elemAt r4 0)
              (builtins.elemAt r5 0)
            ]
            (builtins.elemAt r5 1)
          ];

      seq7 =
        compiledSubs:
        let
          c0 = builtins.elemAt compiledSubs 0;
          c1 = builtins.elemAt compiledSubs 1;
          c2 = builtins.elemAt compiledSubs 2;
          c3 = builtins.elemAt compiledSubs 3;
          c4 = builtins.elemAt compiledSubs 4;
          c5 = builtins.elemAt compiledSubs 5;
          c6 = builtins.elemAt compiledSubs 6;
        in
        derivs:
        let
          r0 = c0 derivs;
          r1 = c1 (builtins.elemAt r0 1);
          r2 = c2 (builtins.elemAt r1 1);
          r3 = c3 (builtins.elemAt r2 1);
          r4 = c4 (builtins.elemAt r3 1);
          r5 = c5 (builtins.elemAt r4 1);
          r6 = c6 (builtins.elemAt r5 1);
        in
        if
          r0 == false
          || r1 == false
          || r2 == false
          || r3 == false
          || r4 == false
          || r5 == false
          || r6 == false
        then
          false
        else
          [
            [
              (builtins.elemAt r0 0)
              (builtins.elemAt r1 0)
              (builtins.elemAt r2 0)
              (builtins.elemAt r3 0)
              (builtins.elemAt r4 0)
              (builtins.elemAt r5 0)
              (builtins.elemAt r6 0)
            ]
            (builtins.elemAt r6 1)
          ];

      compileSeq =
        exprs:
        let
          compiledSubs = map compile exprs;
          k = builtins.length compiledSubs;
          build =
            if k == 2 then
              seq2
            else if k == 3 then
              seq3
            else if k == 4 then
              seq4
            else if k == 5 then
              seq5
            else if k == 6 then
              seq6
            else if k == 7 then
              seq7
            else
              seqGeneric;
        in
        build compiledSubs;

      # Ordered choice with cut (↑): a branch `{ cutSeq = [e1 e2]; }`
      # evaluates e1 first; if e1 fails, the next branch is tried as usual.
      # If e1 succeeds, e2's result becomes the WHOLE CHOICE'S result even
      # if e2 fails -- remaining branches are never tried. Each compiled
      # branch returns `null` for "didn't match, try the next branch", a
      # third state alongside `false` (stop: failure) and success, needed
      # to distinguish "branch didn't match" from "cut committed, then
      # failed". `go` returns the first non-null branch result.
      #
      # `choice2`/`choice3`/`choice6` hand-unroll the same head/tail `go`
      # loop below for the arities this repo's shipped grammars actually
      # invoke at runtime, mirroring seq2-seq7's pattern: hoisting `elemAt
      # compiledBranches i` OUT of the returned closure (computed once per
      # compile-site, not once per parse call) is what makes the win real.
      # Any other arity falls back to `choiceGeneric` (the original `go`),
      # still correct, just without the speedup.
      choice2 =
        compiledBranches:
        let
          b0 = builtins.elemAt compiledBranches 0;
          b1 = builtins.elemAt compiledBranches 1;
        in
        derivs:
        let
          r0 = b0 derivs;
        in
        if r0 != null then
          r0
        else
          let
            r1 = b1 derivs;
          in
          if r1 != null then r1 else false;

      choice3 =
        compiledBranches:
        let
          b0 = builtins.elemAt compiledBranches 0;
          b1 = builtins.elemAt compiledBranches 1;
          b2 = builtins.elemAt compiledBranches 2;
        in
        derivs:
        let
          r0 = b0 derivs;
        in
        if r0 != null then
          r0
        else
          let
            r1 = b1 derivs;
          in
          if r1 != null then
            r1
          else
            let
              r2 = b2 derivs;
            in
            if r2 != null then r2 else false;

      choice6 =
        compiledBranches:
        let
          b0 = builtins.elemAt compiledBranches 0;
          b1 = builtins.elemAt compiledBranches 1;
          b2 = builtins.elemAt compiledBranches 2;
          b3 = builtins.elemAt compiledBranches 3;
          b4 = builtins.elemAt compiledBranches 4;
          b5 = builtins.elemAt compiledBranches 5;
        in
        derivs:
        let
          r0 = b0 derivs;
        in
        if r0 != null then
          r0
        else
          let
            r1 = b1 derivs;
          in
          if r1 != null then
            r1
          else
            let
              r2 = b2 derivs;
            in
            if r2 != null then
              r2
            else
              let
                r3 = b3 derivs;
              in
              if r3 != null then
                r3
              else
                let
                  r4 = b4 derivs;
                in
                if r4 != null then
                  r4
                else
                  let
                    r5 = b5 derivs;
                  in
                  if r5 != null then r5 else false;

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
              derivs:
              let
                r = c derivs;
              in
              if r == false then null else r;
          compiledBranches = map compileBranch branches;
          k = builtins.length compiledBranches;
          # `builtins.tail` is O(remaining length) in Nix (lists are
          # arrays, not linked lists), so repeatedly tail-ing down `bs`
          # makes a k-branch walk O(k^2), not O(k) -- measured: at k=13
          # (this repo's largest un-specialized choice arity), indexing
          # by position instead of `tail`-ing uses ~1/3 the GC bytes over
          # repeated calls; the gap widens with k. `derivs` is an
          # explicit parameter, not closed over, so `go` doesn't get
          # rebuilt every time the outer closure is called.
          choiceGeneric =
            derivs: i:
            if i == k then
              false
            else
              let
                r = (builtins.elemAt compiledBranches i) derivs;
              in
              if r != null then r else choiceGeneric derivs (i + 1);
          build =
            if k == 2 then
              choice2
            else if k == 3 then
              choice3
            else if k == 6 then
              choice6
            else
              (bs: derivs: choiceGeneric derivs 0);
        in
        build compiledBranches;

      # Cap for compileStarPlain's cheap recursive path, well under Nix's
      # ~10000-deep call-depth wall.
      starChunkSize = 500;

      compileStar =
        body: if builtins.isAttrs body && body ? cutSeq then compileStarCut body else compileStarPlain body;

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
      # past ~10000), forcing each step's Derivs pointer with `seq` (an
      # unforced `.d` would just build an equally deep unforced thunk
      # chain, reintroducing the overflow one level up), collecting
      # values via `harvest` afterward rather than an accumulator (`acc ++
      # [x]` every iteration is quadratic). `status` exists because
      # genericClosure can only signal "stop", not why.
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

      # Plain (non-cut) star `e*` -- the hottest path (WHITESPACE/STRING
      # run this at every token boundary). A cheap bounded recursive loop
      # handles the common 0-5-iteration case directly (faster than always
      # paying genericClosure's setup cost); if still matching after
      # `starChunkSize`, escalate to the same genericClosure approach as
      # compileStarCut, splicing the two partial results together.
      compileStarPlain =
        body:
        let
          compiledBody = compile body;
          # `cheapChunk` takes its Derivs node as an explicit parameter
          # (threaded through the recursion, not the outer `derivs`), so
          # it doesn't get rebuilt per call. Returns `[values d hitLimit]`
          # rather than an attrset -- same "list cheaper than attrset"
          # reasoning as the engine's `[value derivs]` result shape.
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
      epsilonAt = derivs: [
        null
        derivs
      ];

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

      # `{ action = { e; f; }; }` applies `f` to `e`'s value on success --
      # lets a value transform (a former named rule's handler) travel with
      # an inlined sub-expression instead of needing a Derivs-node field.
      #
      # Trade-off: a named rule's field is computed once per position and
      # shared by every caller reaching that position; an inlined
      # expression is NOT a field, so it recompiles independently at each
      # call site. Still correct (a PEG's accept/reject behavior doesn't
      # depend on memoization, only its O(n) time bound does), but if two
      # call sites of the SAME inlined expression were ever active at the
      # identical position in one parse, the work would silently
      # duplicate instead of share. Safe when the expression is referenced
      # from exactly one place (structurally impossible to collide with
      # itself); referenced from 2+ places needs an actual
      # position-disjointness check, not just a reference count.
      compileAction =
        e: f:
        let
          compiledE = compile e;
        in
        derivs:
        let
          r = compiledE derivs;
        in
        if r == false then
          false
        else
          [
            (f (builtins.elemAt r 0))
            (builtins.elemAt r 1)
          ];
    in
    compile;

  # Build the position-indexed Derivs array for `string` under `grammar`,
  # passing each nonterminal's raw value through `handlers.<Name>` (default
  # identity). `at` is a single `genList`, built lazily per-element, so
  # `mkNode count` only runs for positions the parse actually reaches.
  #
  # Each node is a LIST, not an attrset: slot 0 is `count`, slots 1..N are
  # the grammar's rules in a fixed order (`names`/`nameToIndex`, baked once
  # here and threaded into `mkCompile` so nonterminal references resolve
  # to a slot index at compile time, not a name lookup at run time). This
  # keeps a single shared spine per position (unlike a struct-of-arrays
  # alternative, which would need N separate full-length arrays) while
  # using the cheaper list representation.
  #
  # `mkNode` hoists `builtins.elemAt at count` ONCE per node (into
  # `derivsNode`) rather than re-deriving it inside a per-slot callback
  # invoked N times.
  buildDerivs =
    grammar: handlers: string:
    let
      len = builtins.stringLength string;
      names = builtins.attrNames grammar;
      numRules = builtins.length names;
      # 1-based: slot 0 is reserved for `count`.
      nameToIndex = builtins.listToAttrs (
        builtins.genList (i: {
          name = builtins.elemAt names i;
          value = i + 1;
        }) numRules
      );
      compile = mkCompile string at nameToIndex;

      # Resolved once per rule name, not once per node.
      resolvedHandlers = builtins.mapAttrs (name: _: handlers.${name} or (v: v)) grammar;

      # Compiled once per rule, before mkNode builds any per-position node.
      compiledRules = builtins.mapAttrs (name: rule: compile rule) grammar;

      # One `node -> result` function per rule, with that rule's handler
      # already captured -- avoids re-looking-up `resolvedHandlers.${name}`
      # by name on every node. Ordered to match `names`/`nameToIndex`, so
      # slot i+1 of a node is `(elemAt compiledFieldsInOrder i) node`.
      compiledFieldsInOrder = builtins.genList (
        i:
        let
          name = builtins.elemAt names i;
          compiled = compiledRules.${name};
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
      ) numRules;

      # One list per position: `[ count field_1 field_2 ... field_numRules ]`.
      # `derivsNode` is `elemAt at count` hoisted to a single `let` binding
      # since every field function is called with the SAME node.
      mkNode =
        count:
        let
          derivsNode = builtins.elemAt at count;
        in
        [ count ] ++ builtins.map (field: field derivsNode) compiledFieldsInOrder;

      at = builtins.genList mkNode (len + 1);
    in
    {
      inherit at nameToIndex;
    };

  # `run`'s own "this rule did not match" sentinel -- NOT `false`. Only
  # `{ json = {}; }`/`{ toml = {}; }` can ever produce a non-string
  # matched VALUE at all (every other combinator's value is always a
  # substring of the input), so a rule using one of those could legitimately
  # match with value `false`, which `false`-as-sentinel would misreport as
  # a non-match.
  #
  # A path sentinel was chosen over wrapping every successful value in a
  # list (mirroring lib/valuewalk.nix's own `null`-collision fix): both
  # require updating every `result.Rule != false` caller, but wrapping
  # also changes every successful value's shape (`result.Rule.someField`
  # becoming `(result.Rule)[0].someField` everywhere), while a path
  # sentinel only changes the failure comparison target.
  NO_MATCH = /var/empty/packrat-no-match-sentinel;

  # Public entry point: parse `string` from `count`, returning
  # `{ <NonterminalName> = value; ... }` with `NO_MATCH` for any
  # nonterminal that failed to match at that position (compare via
  # `result.RuleName != packrat.NO_MATCH`, NOT `!= false` -- a rule's
  # matched value can legitimately BE `false`/`null`/etc. when it uses
  # `{ json = {}; }`/`{ toml = {}; }`, and only that comparison correctly
  # distinguishes the two).
  run =
    {
      grammar,
      handlers ? { },
    }:
    count: string:
    let
      built = buildDerivs grammar handlers string;
      atCount = builtins.elemAt built.at count;
    in
    builtins.mapAttrs (
      name: _:
      let
        r = builtins.elemAt atCount built.nameToIndex.${name};
      in
      if r != false then builtins.elemAt r 0 else NO_MATCH
    ) grammar;
}
