# A generic PEG / packrat-parsing engine built on a genuinely
# self-referential lazy `Derivs` chain (one node per input position),
# following Ford's "Packrat Parsing: Simple, Powerful, Lazy, Linear Time"
# (arXiv cs/0603077): `mkNode` is called exactly ONCE per position, from a
# single root (`buildDerivs`); every other way of reaching a given position
# is by walking already-built `.next` pointers (`advanceN`), so Nix's
# ordinary thunk-sharing on attrset fields gives real memoization "for
# free" -- unlike the previous `recurse = self: count: ...` function-
# threading design, which recomputed the same (nonterminal, position) pair
# on every call.
#
# Grammar DSL (attrset-as-data), same atoms as before plus new combinators:
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
#                             only meaningful as the head of a `choice` list
#                             or as the body of a `star` -- see evalChoice /
#                             evalStar below for where the commit semantics
#                             actually live.
#
# ARCHITECTURE NOTE (compile vs. eval split): `mkCompile` below does NOT
# interpret a grammar rule's shape freshly every time it's evaluated at a
# position. Grammar rule VALUES (the attrset-as-data DSL terms) are
# entirely static for a whole parse -- the same `{ lit = "..."; }`,
# `[ ... ]`, `{ choice = [...]; }` etc. is passed to `mkNode` at every one
# of the ~len positions in the input. The OLD design (`evalExpr expr
# derivs`) re-ran the `if expr == "" then ... else if isString expr then
# ... else if expr ? lit then ... else if expr ? range then ...`
# shape-dispatch chain on every single call, even though `expr`'s shape
# never changes across calls for a given grammar rule.
#
# Measured directly: an isolated microbenchmark replaying this exact
# 12-branch if/elif chain vs. an equivalent "resolve the dispatch once
# into a closure, reuse the closure" version, at the realistic hot-path
# branch (`{ lit = ...; }`, which is the JSON grammar's single most common
# atom) showed the interpreted version taking ~0.8-0.85s vs. the compiled
# version's ~0.4-0.49s for 2,000,000 calls -- roughly 2x slower, and the
# gap widens further (~3.3x) for atom types near the END of the if/elif
# chain (e.g. `not`), since a linear if/elif chain makes every later
# branch pay for testing every earlier one on every call, whereas a
# compiled closure pays that cost once, at compile time, regardless of
# where the matching branch sits in the original chain.
#
# `mkCompile string` returns `compile : expr -> (derivs -> result)`.
# Compiling `expr` decides ONCE which combinator applies (and, for
# composite expressions like sequences/choices/stars, recursively compiles
# each SUB-expression once too, rather than re-dispatching on the
# sub-expression's shape every time the parent combinator runs) and
# returns a plain `derivs -> result` closure that skips straight to the
# right combinator on every subsequent call. `buildDerivs` below calls
# `compile rule` exactly ONCE per grammar rule (in its outer scope, before
# `mkNode` starts building any of the ~len per-position nodes), and reuses
# that one compiled closure, applying it to a different `derivs` node each
# time `mkNode` builds a new position -- this is the same "resolve once
# outside the hot per-node loop" pattern already used for
# `resolvedHandlers` (handler-lookup) below, just applied to grammar-rule
# dispatch instead of handler-lookup, at what profiling identified as a
# far higher call-volume site (evalExpr/compile's dispatch runs on every
# sub-expression of every sequence/choice/star body, at every attempt --
# successful or failed -- not just once per successfully-matched named
# rule the way handler lookup does).
rec {
  # Walk `n` `.next` pointers forward from `derivs`. Every call site passes
  # exactly the number of characters a match just consumed, starting from
  # the position where that match began -- so this only ever walks forward
  # along the chain that was going to be built anyway, never re-derives a
  # position from scratch. Because `.next` is a single shared attrset field
  # per node, repeated walks over the same span reuse the same nodes.
  #
  # Implemented via builtins.foldl' with an explicit builtins.seq, rather
  # than plain Nix-level self-recursion: Nix's evaluator has NO tail-call
  # optimization (verified directly -- even a manifestly tail-recursive
  # accumulator function like `f = n: acc: if n == 0 then acc else f (n-1)
  # (acc+1)` stack-overflows around n~10000), so a naive `advanceN
  # derivs.next (n-1)` blows Nix's max-call-depth once a single match needs
  # to advance more than ~10000 characters in one hop -- confirmed directly
  # against a >=10000-char unbroken match. `foldl'` itself IS a genuine C++
  # loop (confirmed directly at 500000 iterations with no depth error), so
  # switching to it removes the Nix-level call-depth problem -- but
  # `foldl'` only forces its accumulator to WHNF (i.e. confirms "yes, this
  # is an attrset") each step, NOT the fields inside it, so
  # `acc: _: acc.next` alone would just replace the recursive-call problem
  # with an equally deep chain of unforced `.next` thunks that then
  # overflows the instant something finally forces the result (confirmed
  # directly: builtins.genericClosure has this same problem in practice,
  # breaking down around ~70000 items when carrying this chain's lazy
  # payload, for exactly this reason). `builtins.seq acc acc.next` forces
  # `acc` (the previous node) to WHNF before returning the next one, which
  # is enough to stop the per-step thunk from being deferred -- confirmed
  # directly this scales cleanly to 500000+ steps. `seq` only forces to
  # WHNF (one level), not deep-forces the whole node (that would force
  # every nonterminal field at every position, defeating the engine's
  # whole memoization premise of "only compute what's actually asked
  # for") -- deepSeq would be the wrong tool here for exactly that reason.
  advanceN =
    derivs: n:
    builtins.foldl' (acc: _: builtins.seq acc acc.next) derivs (builtins.genList (_: null) n);

  # mkCompile : string -> (expr -> (derivs -> result))
  # result = { value = ...; derivs = ...; }  (success)  |  false  (failure)
  #
  # Failure used to be its own attrset `{ success = false; }`, and success
  # carried a redundant `success = true;` field alongside `value`/`derivs`.
  # Nix is dynamically typed and an attrset never compares equal to a bool
  # (confirmed directly: `{ a = 1; } == false` is `false`, no type error) --
  # so `false` itself can serve as the failure sentinel, with two wins: (1)
  # every failure now returns the same pre-existing interned `false` value
  # instead of allocating a fresh one-field attrset on every single leaf
  # mismatch (by far the hottest path -- most `lit`/`range`/`regex` attempts
  # against real input fail at the first character), and (2) success drops
  # one field (`success = true` is now implicit in "the result isn't
  # `false`"). This can't collide with a legitimate match VALUE that
  # happens to be the boolean `false` (e.g. after grammar/json.nix's BOOL
  # handler turns matched text "false" into the real boolean): the sentinel
  # replaces the whole result, never the `.value` field, so a successful
  # result is always `{ value = false; derivs = ...; }` -- an attrset,
  # which is `!= false` -- never the bare sentinel itself. Every call site
  # below follows the same shape: `if r == false then <propagate false> else
  # let ... = r.value; ... = r.derivs; in <use them>`.
  mkCompile =
    string:
    let
      len = builtins.stringLength string;

      # compile : expr -> (derivs -> result)
      # Decides ONCE which combinator `expr` denotes (and recursively
      # compiles any sub-expressions), returning a closure that -- when
      # later applied to a `derivs` node -- runs straight to that
      # combinator with no further shape-testing of `expr` itself.
      compile =
        expr:
        if expr == "" then
          # Bare "" is the epsilon nonterminal: always succeeds, consumes
          # nothing, matching the convention of the original grammar.
          (
            derivs:
            {
              value = "";
              derivs = derivs;
            }
          )
        else if builtins.isString expr then
          # Nonterminal reference: look up the already-memoized field on
          # whatever derivs node we're later applied to. Nothing to
          # "compile" here beyond capturing the name -- the lookup itself
          # is already O(1) and doesn't change shape between calls.
          (derivs: derivs.${expr})
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
          # cutSeq used outside its two sanctioned positions (choice head /
          # star body): no commit context exists, so it degrades to a plain
          # sequence [e1 e2]. Not used by the JSON grammar; kept so compile
          # is total over the DSL rather than throwing on a technically
          # well-formed expr.
          compileSeq expr.cutSeq
        else
          throw "packrat: unrecognized expression: ${builtins.toJSON expr}";

      # evalLit/evalRange/evalRegex are leaf combinators: currying them on
      # their static parameter (lit/range/regex, known at compile time)
      # already produces the `derivs -> result` shape `compile` needs, no
      # extra wrapping required -- these were never re-dispatching on
      # their own shape the way the OLD evalExpr's outer if/elif chain did,
      # so there's nothing further to hoist here.
      # One-shot slice-and-compare instead of a char-by-char walk: `lit`'s
      # length is fixed at compile time, so there's no need to re-derive it
      # one `substring _ 1` call and one `.next` dereference at a time (the
      # old `go` also risked the same no-TCO stack-overflow `advanceN`'s
      # comment above documents, for any literal longer than ~10000 chars).
      # A single `substring derivs.count n string` plus one `==` covers the
      # whole match, and `advanceN` (already proven to scale via `foldl'`)
      # does the equivalent position jump in one call.
      evalLit =
        lit: derivs:
        let
          n = builtins.stringLength lit;
        in
        if derivs.count + n > len then
          false
        else if builtins.substring derivs.count n string == lit then
          {
            value = lit;
            derivs = advanceN derivs n;
          }
        else
          false;

      evalRange =
        range: derivs:
        let
          start = builtins.elemAt range 0;
          end = builtins.elemAt range 1;
          c = if derivs.count >= len then "" else builtins.substring derivs.count 1 string;
        in
        if c != "" && c >= start && c <= end then
          {
            value = c;
            derivs = derivs.next;
          }
        else
          false;

      # Bounded lookahead window, matching the original engine's approach
      # (`builtins.substring derivs.count 128 string`): `builtins.substring`
      # on a string of length `len` starting at `derivs.count` would
      # otherwise copy the ENTIRE remainder of the input on every single
      # regex attempt (there is no way to ask Nix's `substring` for
      # "up to N chars, clamped" without it being cheap -- passing a huge
      # length is fine, Nix clamps -- but passing `len - derivs.count`
      # explicitly defeats that by recomputing the exact remaining length
      # every time, which is itself O(1) but the resulting substring copy
      # is O(remaining length), so repeating this at every position across
      # an n-byte input is O(n^2) and was the direct cause of the
      # stack-overflow / multi-minute hang observed on lock-large.json
      # (391KB) during Phase 1 verification. A fixed window is O(1) per
      # call regardless of input size, at the cost of capping how long a
      # single regex match can be (matches this grammar's tokens: runs of
      # whitespace, string fragments, comment lines, digit runs) -- longer
      # runs still parse correctly wherever the regex is used inside a
      # `star` (as STRING_RAW/WHITESPACE are), since each iteration only
      # needs to match up to the window before looping for the next chunk.
      #
      # Measured directly (see bench/results.txt and the final report):
      # this constant matters a LOT in practice, not just asymptotically.
      # On lock-large.json (391947 bytes), window=4096 costs 4.17s wall /
      # 1575MB RSS; window=512 costs 1.54s / 810MB -- ~2.7x faster and ~2x
      # less memory, for byte-identical output, because `builtins.substring`
      # still copies the ENTIRE window every call even when the actual
      # match is short (whitespace runs here are <= 11 chars), and this
      # grammar calls evalRegex extremely often (once per WHITESPACE
      # position and once per STRING_FRAG position). 512 keeps a safety
      # margin over this repo's observed longest single-regex match (180
      # chars, a COMMENT line) for other inputs with longer comments;
      # push it lower (down to ~192-256) for more speed -- correctness no
      # longer depends on this being "big enough" for the corpus, since
      # evalRegex below retries with a doubled window whenever a match
      # exactly fills the window (see tryWindow); this is now purely a
      # speed/memory tuning knob, not a correctness one.
      regexWindow = 256;

      evalRegex =
        regex: derivs:
        let
          # Try with a bounded window first (the O(1)-per-call fast path
          # that keeps evalRegex cheap on the overwhelmingly common case of
          # short matches). If the match fills the ENTIRE window, it may
          # have been truncated -- a match that happens to be exactly
          # `windowSize` characters long is indistinguishable from one that
          # got cut off mid-token, so in that case only, retry with a
          # doubled window and keep doubling until either the match no
          # longer fills the window (definitely complete) or the window
          # already covers the rest of the input (nothing left to find).
          #
          # This matters for real correctness, not just an asymptotic edge
          # case: COMMENT's regex ([^\n]+) is used directly in a sequence,
          # NOT wrapped in `star` the way STRING_RAW/WHITESPACE are, so
          # without this retry a single comment line longer than the fixed
          # window would silently truncate and typically desync the rest of
          # the parse -- confirmed directly: at a fixed window of 512, a
          # 512-char comment line parsed fine but a 513-char one made an
          # otherwise-valid file fail to parse at all.
          tryWindow =
            windowSize:
            let
              rest = builtins.substring derivs.count windowSize string;
              m = builtins.match "${regex}.*" rest;
              restLen = builtins.stringLength rest;
            in
            if builtins.isList m && m != [ ] && builtins.head m != null then
              let
                matched = builtins.head m;
                matchedLen = builtins.stringLength matched;
              in
              if matchedLen < restLen || derivs.count + restLen >= len then
                # Match didn't fill the window (so it can't have been cut
                # off), or the window already reached the end of the input
                # (nothing more it could have matched anyway) -- accept.
                {
                  value = matched;
                  derivs = advanceN derivs matchedLen;
                }
              else
                # Match exactly filled the window and more input remains:
                # might be truncated. Grow and retry.
                tryWindow (windowSize * 2)
            else
              false;
        in
        tryWindow regexWindow;

      # compileSeq compiles each sub-expression ONCE (recursing through
      # `compile`, not re-dispatching per call the way the old
      # `evalSeq exprs derivs` did via `evalExpr expr acc.derivs` on every
      # element on every call), returning a `derivs -> result` closure
      # that runs the already-compiled sub-closures in order.
      compileSeq =
        exprs:
        let
          compiledSubs = map compile exprs;
        in
        derivs:
        builtins.foldl'
          (
            acc: subCompiled:
            if acc == false then
              false
            else
              let
                r = subCompiled acc.derivs;
              in
              if r == false then
                false
              else
                {
                  value = acc.value ++ [ r.value ];
                  derivs = r.derivs;
                }
          )
          {
            value = [ ];
            derivs = derivs;
          }
          compiledSubs;

      # Ordered choice, with cut (↑) support: if the head of the remaining
      # branch list is `{ cutSeq = [e1 e2]; }`, evaluate e1; if e1 fails,
      # no commitment happened, so we fall through to the next branch as
      # usual. If e1 succeeds, evaluate e2 and return e2's result AS THE
      # WHOLE CHOICE'S RESULT regardless of whether e2 succeeds -- the
      # remaining branches are never tried, per Mizushima et al. §3.2.
      #
      # compileChoice compiles each branch's sub-expression(s) ONCE (via
      # `compile`, at compile time) instead of the old `evalChoice`
      # re-dispatching on each branch's shape (plain expr vs. `{cutSeq=...}`)
      # on every call.
      compileChoice =
        branches:
        let
          compiledBranches = map (
            b:
            if builtins.isAttrs b && b ? cutSeq then
              {
                isCut = true;
                c1 = compile (builtins.elemAt b.cutSeq 0);
                c2 = compile (builtins.elemAt b.cutSeq 1);
              }
            else
              {
                isCut = false;
                c = compile b;
              }
          ) branches;
        in
        derivs:
        let
          go =
            bs:
            if bs == [ ] then
              false
            else
              let
                b = builtins.head bs;
                rest = builtins.tail bs;
              in
              if b.isCut then
                let
                  r1 = b.c1 derivs;
                in
                if r1 == false then
                  go rest
                else
                  let
                    r2 = b.c2 r1.derivs;
                  in
                  if r2 != false then
                    {
                      value = [
                        r1.value
                        r2.value
                      ];
                      derivs = r2.derivs;
                    }
                  else
                    # Committed failure: cut forbids trying `rest` even
                    # though ordinary choice would.
                    false
              else
                let
                  r = b.c derivs;
                in
                if r != false then r else go rest;
        in
        go compiledBranches;

      # Threshold for compileStarPlain's cheap-path/escalation split -- see
      # the comment on `mkStarPlain` below. Well under the ~10000-deep
      # call-depth wall a hand-written recursive loop hits (confirmed
      # directly), with a wide margin since the SAME recursive loop, if it
      # hit the limit while genuinely still matching, would stack-overflow
      # instead of cleanly escalating -- picking a value this far below the
      # wall means that scenario essentially never happens for realistic
      # grammars while still capturing nearly all of the speed benefit
      # (measured: the cheap path is what makes the common "0-5 iterations
      # per call, called thousands of times" shape of ordinary JSON's
      # WHITESPACE/STRING_RAW fast).
      starChunkSize = 500;

      compileStar =
        body:
        if builtins.isAttrs body && body ? cutSeq then
          compileStarCut body
        else
          compileStarPlain body;

      # The cut-star branch (e1 ↑ e2)* is comparatively rare in practice
      # (nothing in grammar/json.nix uses it -- cut is only applied to X's
      # top-level choice, not inside any star), so it stays on the
      # genericClosure implementation unconditionally: correctness and
      # avoiding the O(n^2)/stack-overflow failure modes matters more here
      # than shaving constant-factor overhead off a rarely-hit path.
      # See the (e1 ↑ e2)* doc comment below compileStarPlain for the full
      # rationale (three approaches tried, genericClosure+seq is what
      # actually avoids both the depth limit AND the quadratic list-append
      # cost). e1/e2 are compiled ONCE here, at compile time.
      compileStarCut =
        body:
        let
          c1 = compile (builtins.elemAt body.cutSeq 0);
          c2 = compile (builtins.elemAt body.cutSeq 1);
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
                    r2 = c2 r1.derivs;
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
                    builtins.seq r2.derivs [
                      {
                        key = item.key + 1;
                        d = r2.derivs;
                        status = "cont";
                        pair = [
                          r1.value
                          r2.value
                        ];
                      }
                    ];
          };
          lastItem = builtins.elemAt closure (builtins.length closure - 1);
          pairs = builtins.filter (x: x != null) (map (i: i.pair) closure);
        in
        if lastItem.status == "stopFail" then
          false
        else
          {
            value = pairs;
            derivs = lastItem.d;
          };

      # The plain (non-cut) star `e*` -- by far the hottest path in
      # practice, since grammar/json.nix's WHITESPACE and STRING_RAW are
      # both plain stars invoked at essentially every token boundary in a
      # real JSON document. Implemented as a HYBRID of the two approaches
      # discussed in the (e1 ↑ e2)* comment below. `body` is compiled ONCE
      # here (at compile time), reused across every call the returned
      # closure receives.
      #
      #   - First, try a cheap, ordinary Nix-level recursive loop for up
      #     to `starChunkSize` (500) iterations. A recursive loop's
      #     downside vs. genericClosure is (a) Nix's max-call-depth wall
      #     around ~10000, and (b) if it OVERSHOOTS that by accumulating a
      #     list via `++` the whole way, quadratic cost -- but at only 500
      #     iterations, neither problem materializes: 500 is nowhere near
      #     the depth wall, and a 500-element list's `++` cost is
      #     negligible. Measured directly: for the extremely common case
      #     of a star matching 0-5 times (whitespace runs, short string
      #     fragments), this cheap path is ~2.4x faster than going through
      #     genericClosure every single call, because genericClosure's
      #     per-call setup (building a startSet attrset, its internal
      #     dedup/traversal machinery) has real fixed overhead that
      #     dominates when the actual work is this small -- confirmed
      #     directly: 100000 calls each doing ~2 genericClosure iterations
      #     took ~0.28s, vs. ~0.12s for the equivalent via plain recursion,
      #     and this engine calls compileStarPlain's closure (via
      #     WHITESPACE/STRING_RAW) thousands of times per real JSON
      #     document.
      #
      #   - If the cheap loop is STILL matching when it hits
      #     `starChunkSize`, that's the rare pathological case (a single
      #     star body matching hundreds+ times in a row -- e.g. a long
      #     unbroken run of "aaaa...", or this engine's own STRING_FRAG
      #     matching a very long string one window-sized chunk at a time).
      #     In that case, escalate: continue from exactly where the cheap
      #     loop left off, via the genericClosure+seq implementation
      #     (which has neither the call-depth wall nor the quadratic
      #     list-append cost), and splice the two partial results
      #     together. This keeps the earlier fix's guarantee -- no
      #     stack-overflow, no O(n^2) blowup, confirmed directly at 64000+
      #     repeats and 500000+-character single tokens -- while no longer
      #     paying genericClosure's constant-factor overhead on the
      #     overwhelmingly common short-run case.
      compileStarPlain =
        body:
        let
          compiledBody = compile body;
        in
        derivs:
        let
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
                cheapChunk (i + 1) (acc ++ [ r.value ]) r.derivs;
          first = cheapChunk 0 [ ] derivs;
        in
        if !first.hitLimit then
          {
            value = first.values;
            derivs = first.d;
          }
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
              operator =
                item:
                if !item.matched then
                  [ ]
                else
                  let
                    r = compiledBody item.d;
                  in
                  if r != false then
                    builtins.seq r.derivs [
                      {
                        key = item.key + 1;
                        d = r.derivs;
                        matched = true;
                        v = r.value;
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
            };
            lastItem = builtins.elemAt closure (builtins.length closure - 1);
            restValues = builtins.filter (x: x != null) (map (i: i.v) closure);
          in
          {
            value = first.values ++ restValues;
            derivs = lastItem.d;
          };

      # (e1 ↑ e2)*: evaluate e1; if it fails, the whole star SUCCEEDS with
      # whatever was accumulated. If e1 succeeds, evaluate e2; if e2 fails,
      # the WHOLE STAR FAILS (no partial-match success, unlike plain
      # `(e1 e2)*`, which would just stop and succeed with prior matches).
      # If e2 succeeds, accumulate [e1val e2val] and loop.
      #
      # Implemented via builtins.genericClosure -- THREE approaches were
      # tried and measured before landing here, because each earlier one
      # had a real, confirmed problem, not a hypothetical one:
      #
      #   1. A plain recursive `loop = acc: d: ... loop acc' d'` function.
      #      Nix has NO tail-call optimization (confirmed directly: even a
      #      manifestly tail-recursive counter function stack-overflows
      #      around n~10000), so this broke on a single star body matching
      #      more than ~10000 times in a row (confirmed directly against a
      #      long unbroken JSON string value).
      #
      #   2. `builtins.genericClosure` WITHOUT forcing the payload each
      #      step. genericClosure's traversal loop itself IS a genuine C++
      #      loop (confirmed at 200000+ steps on a trivial payload-free
      #      chain with no depth error), but carrying this engine's lazy
      #      Derivs-node pointer through it without forcing it each step
      #      reintroduces the exact same problem one level up: the
      #      unforced `.d` references build up as their own unevaluated
      #      thunk chain, overflowing the instant something finally forces
      #      the result (confirmed directly: broke around ~70000 items).
      #
      #   3. `builtins.foldl'` with an explicit `builtins.seq` on the
      #      accumulator's `d` pointer each step (the fix for (2)'s
      #      problem, and the right tool for advanceN above, which only
      #      ever needs the FINAL node, no list accumulation). But
      #      evalStar's accumulator must also grow a list of matched
      #      values every iteration, and `newValues = acc.values ++
      #      [x]` is an O(current length) COPY every single step (Nix
      #      lists are array-like, not linked) -- confirmed directly this
      #      makes the whole star cost genuinely quadratic: 32000
      #      iterations took ~1.8s, 64000 took ~32s (not the ~2x a linear
      #      approach would show). `genericClosure`'s own returned list,
      #      by contrast, is NOT built via repeated Nix-level `++` -- it's
      #      an internal C++ vector -- confirmed directly: 500000 items
      #      collected via genericClosure + a single `map`/`filter` pass
      #      completes in <1s / ~220MB, no quadratic blowup, AS LONG AS
      #      the per-step payload is also forced via `builtins.seq`
      #      (fixing (2)'s problem) rather than left as a growing
      #      Nix-level list inside the accumulator itself.
      #
      # So: force each step's Derivs pointer to WHNF via `builtins.seq`
      # (avoids problem 2), and let genericClosure's own list -- extracted
      # afterward via `map`/`filter`, not accumulated step-by-step --
      # hold the matched values (avoids problem 3). `builtins.seq x y`
      # only forces `x` to WHNF (one level), never the whole Derivs node
      # recursively -- `builtins.deepSeq` would be the wrong tool here,
      # since it would force every nonterminal field at every position
      # touched, defeating the engine's whole memoization premise of
      # "only compute what's actually asked for".
      #
      # genericClosure's operator can only signal "stop" by returning []
      # -- it can't itself distinguish "stopped because done/succeeded"
      # from "stopped because of a committed failure", so each item
      # carries an explicit `status` ("cont" / "stopSuccess" / "stopFail")
      # inspected on the FINAL item after the closure completes.
      #
      # compileStarPlain above additionally short-circuits through a cheap
      # bounded recursive path first, ONLY escalating to this
      # genericClosure machinery when a run turns out to be unusually
      # long -- see its comment for why that split matters for realistic
      # JSON-shaped input (many short stars) vs. this analysis (which
      # still fully applies to compileStarCut, and to compileStarPlain's
      # rare escalation case).

      compileOpt =
        body:
        let
          compiledBody = compile body;
        in
        derivs:
        let
          r = compiledBody derivs;
        in
        if r != false then
          r
        else
          {
            value = null;
            derivs = derivs;
          };

      compileAnd =
        body:
        let
          compiledBody = compile body;
        in
        derivs:
        let
          r = compiledBody derivs;
        in
        if r != false then
          {
            value = null;
            derivs = derivs;
          }
        else
          false;

      compileNot =
        body:
        let
          compiledBody = compile body;
        in
        derivs:
        let
          r = compiledBody derivs;
        in
        if r != false then
          false
        else
          {
            value = null;
            derivs = derivs;
          };
    in
    compile;

  # Build the single self-referential Derivs chain for `string` under
  # `grammar`, with each named nonterminal's raw parse value passed through
  # `handlers.<Name>` (default identity) exactly once, at the point that
  # field is computed on its node.
  #
  # `mkNode` is invoked exactly once per position (recursively through its
  # own `next` field) for a given `buildDerivs` call -- callers must reach a
  # position via `.next`/result `.derivs` pointers, never by calling
  # `mkNode` a second time for the same count, or the sharing invariant that
  # gives real memoization is broken.
  buildDerivs =
    grammar: handlers: string:
    let
      compile = mkCompile string;
      len = builtins.stringLength string;

      # Resolve `handlers.${name} or (v: v)` ONCE per rule name here, in
      # buildDerivs's outer scope, instead of inside applyHandler (which
      # used to be called from mkNode's mapAttrs -- i.e. once per rule
      # PER NODE, ~len times for a rule touched at every position, since
      # `handlers` and `grammar`'s rule names are entirely static for the
      # whole buildDerivs call and never change from node to node).
      # `resolvedHandlers.${name}` below is a plain attrset lookup with no
      # `or` fallback to evaluate every time -- the fallback-to-identity
      # decision is baked in once, per name, right here.
      #
      # Measured directly: an isolated microbenchmark of "dynamic
      # `attrs.${name} or default` lookup on every call" vs. "resolve once
      # into a plain attrset, plain lookup thereafter" at a call volume
      # matching this engine's real per-node/per-rule invocation count
      # (400000 positions x 14 rules = 5.6M calls) showed the resolve-once
      # version ~25-30% faster (~1.68-1.90s vs ~1.32-1.35s across repeated
      # runs) -- a real, reproducible difference in isolation, though its
      # real-world effect on lock-large.json turned out to be within noise
      # (measured: no distinguishable change), because applyHandler's real
      # call volume (only forced when a rule's field is actually read, not
      # once per rule per node the way the microbenchmark assumed) is far
      # lower than 5.6M for this grammar/input size. Kept anyway as a pure,
      # correctness-neutral hoist -- see git history for the full
      # before/after measurement writeup.
      resolvedHandlers = builtins.mapAttrs (name: _: handlers.${name} or (v: v)) grammar;

      applyHandler =
        name: r:
        if r != false then
          {
            value = resolvedHandlers.${name} r.value;
            derivs = r.derivs;
          }
        else
          r;

      # Compile every grammar rule ONCE here, before mkNode builds any of
      # the ~len per-position nodes -- this is the actual fix described in
      # the file-level comment above: `grammar`'s rule VALUES never change
      # from position to position, so the shape-dispatch decision (which
      # combinator applies, recursively through every sub-expression)
      # only needs to happen once per rule, not once per rule per node.
      compiledRules = builtins.mapAttrs (name: rule: compile rule) grammar;

      mkNode =
        count:
        let
          node =
            {
              inherit count;
              next = if count >= len then null else mkNode (count + 1);
            }
            // builtins.mapAttrs (name: _: applyHandler name (compiledRules.${name} node)) grammar;
        in
        node;
    in
    mkNode 0;

  # Public entry point. `run { grammar; handlers; } count string` parses
  # `string` starting at `count` and returns `{ <NonterminalName> = value; ...
  # }` for every nonterminal in the grammar, with `false` in place of any
  # nonterminal that failed to match at that position -- generalizing the
  # old `{ X = result.X.value or false; }` shape to every rule.
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
    builtins.mapAttrs (name: _: if at.${name} != false then at.${name}.value else false) grammar;
}
