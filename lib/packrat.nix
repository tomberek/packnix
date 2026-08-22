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
rec {
  # Walk `n` `.next` pointers forward from `derivs`. Every call site passes
  # exactly the number of characters a match just consumed, starting from
  # the position where that match began -- so this only ever walks forward
  # along the chain that was going to be built anyway, never re-derives a
  # position from scratch. Because `.next` is a single shared attrset field
  # per node, repeated walks over the same span reuse the same nodes.
  advanceN = derivs: n: if n == 0 then derivs else advanceN derivs.next (n - 1);

  # mkEvalExpr : string -> (expr -> derivs -> result)
  # result = { success = true; value = ...; derivs = ...; } | { success = false; }
  mkEvalExpr =
    string:
    let
      len = builtins.stringLength string;

      evalExpr =
        expr: derivs:
        if expr == "" then
          # Bare "" is the epsilon nonterminal: always succeeds, consumes
          # nothing, matching the convention of the original grammar.
          {
            success = true;
            value = "";
            derivs = derivs;
          }
        else if builtins.isString expr then
          # Nonterminal reference: look up the already-memoized field on
          # whatever derivs node we've been threaded to.
          derivs.${expr}
        else if builtins.isList expr then
          evalSeq expr derivs
        else if expr ? lit then
          evalLit expr.lit derivs
        else if expr ? range then
          evalRange expr.range derivs
        else if expr ? regex then
          evalRegex expr.regex derivs
        else if expr ? choice then
          evalChoice expr.choice derivs
        else if expr ? star then
          evalStar expr.star derivs
        else if expr ? plus then
          evalSeq [
            expr.plus
            { star = expr.plus; }
          ] derivs
        else if expr ? opt then
          evalOpt expr.opt derivs
        else if expr ? and then
          evalAnd expr.and derivs
        else if expr ? not then
          evalNot expr.not derivs
        else if expr ? cutSeq then
          # cutSeq used outside its two sanctioned positions (choice head /
          # star body): no commit context exists, so it degrades to a plain
          # sequence [e1 e2]. Not used by the JSON grammar; kept so evalExpr
          # is total over the DSL rather than throwing on a technically
          # well-formed expr.
          evalSeq expr.cutSeq derivs
        else
          throw "packrat: unrecognized expression: ${builtins.toJSON expr}";

      evalLit =
        lit: derivs:
        let
          n = builtins.stringLength lit;
          go =
            i: d:
            if i == n then
              {
                success = true;
                value = lit;
                derivs = d;
              }
            else if d == null then
              { success = false; }
            else if builtins.substring i 1 lit == builtins.substring d.count 1 string then
              go (i + 1) d.next
            else
              { success = false; };
        in
        go 0 derivs;

      evalRange =
        range: derivs:
        let
          start = builtins.elemAt range 0;
          end = builtins.elemAt range 1;
          c = if derivs.count >= len then "" else builtins.substring derivs.count 1 string;
        in
        if c != "" && c >= start && c <= end then
          {
            success = true;
            value = c;
            derivs = derivs.next;
          }
        else
          { success = false; };

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
      # whitespace, string fragments, comment lines, digit runs).
      regexWindow = 4096;

      evalRegex =
        regex: derivs:
        let
          rest = builtins.substring derivs.count regexWindow string;
          m = builtins.match "${regex}.*" rest;
        in
        if builtins.isList m && m != [ ] && builtins.head m != null then
          let
            matched = builtins.head m;
          in
          {
            success = true;
            value = matched;
            derivs = advanceN derivs (builtins.stringLength matched);
          }
        else
          { success = false; };

      evalSeq =
        exprs: derivs:
        builtins.foldl'
          (
            acc: expr:
            if !acc.success then
              acc
            else
              let
                r = evalExpr expr acc.derivs;
              in
              if r.success then
                {
                  success = true;
                  value = acc.value ++ [ r.value ];
                  derivs = r.derivs;
                }
              else
                { success = false; }
          )
          {
            success = true;
            value = [ ];
            derivs = derivs;
          }
          exprs;

      # Ordered choice, with cut (↑) support: if the head of the remaining
      # branch list is `{ cutSeq = [e1 e2]; }`, evaluate e1; if e1 fails,
      # no commitment happened, so we fall through to the next branch as
      # usual. If e1 succeeds, evaluate e2 and return e2's result AS THE
      # WHOLE CHOICE'S RESULT regardless of whether e2 succeeds -- the
      # remaining branches are never tried, per Mizushima et al. §3.2.
      evalChoice =
        branches: derivs:
        let
          go =
            bs:
            if bs == [ ] then
              { success = false; }
            else
              let
                b = builtins.head bs;
                rest = builtins.tail bs;
              in
              if builtins.isAttrs b && b ? cutSeq then
                let
                  e1 = builtins.elemAt b.cutSeq 0;
                  e2 = builtins.elemAt b.cutSeq 1;
                  r1 = evalExpr e1 derivs;
                in
                if !r1.success then
                  go rest
                else
                  let
                    r2 = evalExpr e2 r1.derivs;
                  in
                  if r2.success then
                    {
                      success = true;
                      value = [
                        r1.value
                        r2.value
                      ];
                      derivs = r2.derivs;
                    }
                  else
                    # Committed failure: cut forbids trying `rest` even
                    # though ordinary choice would.
                    { success = false; }
              else
                let
                  r = evalExpr b derivs;
                in
                if r.success then r else go rest;
        in
        go branches;

      # (e1 ↑ e2)*: evaluate e1; if it fails, the whole star SUCCEEDS with
      # whatever was accumulated. If e1 succeeds, evaluate e2; if e2 fails,
      # the WHOLE STAR FAILS (no partial-match success, unlike plain
      # `(e1 e2)*`, which would just stop and succeed with prior matches).
      # If e2 succeeds, accumulate [e1val e2val] and loop.
      evalStar =
        body: derivs:
        if builtins.isAttrs body && body ? cutSeq then
          let
            e1 = builtins.elemAt body.cutSeq 0;
            e2 = builtins.elemAt body.cutSeq 1;
            loop =
              acc: d:
              let
                r1 = evalExpr e1 d;
              in
              if !r1.success then
                {
                  success = true;
                  value = acc;
                  derivs = d;
                }
              else
                let
                  r2 = evalExpr e2 r1.derivs;
                in
                if !r2.success then
                  { success = false; }
                else
                  loop
                    (
                      acc
                      ++ [
                        [
                          r1.value
                          r2.value
                        ]
                      ]
                    )
                    r2.derivs;
          in
          loop [ ] derivs
        else
          let
            loop =
              acc: d:
              let
                r = evalExpr body d;
              in
              if r.success then
                loop (acc ++ [ r.value ]) r.derivs
              else
                {
                  success = true;
                  value = acc;
                  derivs = d;
                };
          in
          loop [ ] derivs;

      evalOpt =
        body: derivs:
        let
          r = evalExpr body derivs;
        in
        if r.success then
          {
            success = true;
            value = r.value;
            derivs = r.derivs;
          }
        else
          {
            success = true;
            value = null;
            derivs = derivs;
          };

      evalAnd =
        body: derivs:
        let
          r = evalExpr body derivs;
        in
        if r.success then
          {
            success = true;
            value = null;
            derivs = derivs;
          }
        else
          { success = false; };

      evalNot =
        body: derivs:
        let
          r = evalExpr body derivs;
        in
        if r.success then
          { success = false; }
        else
          {
            success = true;
            value = null;
            derivs = derivs;
          };
    in
    evalExpr;

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
      evalExpr = mkEvalExpr string;
      len = builtins.stringLength string;

      applyHandler =
        name: r:
        if r.success then
          {
            success = true;
            value = (handlers.${name} or (v: v)) r.value;
            derivs = r.derivs;
          }
        else
          r;

      mkNode =
        count:
        let
          node =
            {
              inherit count;
              next = if count >= len then null else mkNode (count + 1);
            }
            // builtins.mapAttrs (name: rule: applyHandler name (evalExpr rule node)) grammar;
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
    builtins.mapAttrs (name: _: if at.${name}.success then at.${name}.value else false) grammar;
}
