# Static check for lib/packrat.nix's `{ json = {}; }`/`{ toml = {}; }`
# combinator: does any rule place one somewhere a PEG combinator could
# gracefully absorb its failure (`opt`, a non-last `choice` branch,
# `star`'s plain body, `and`, `not`) instead of somewhere failure
# necessarily propagates as the whole rule failing? Runs once per rule,
# over the rule's expression tree, BEFORE any input is parsed -- catches
# a structurally-wrong grammar regardless of what input it's ever fed.
# This matters because `json`/`toml` cannot return `false` at all --
# only succeed or throw (see packrat.nix's evalBuiltinParser) -- so
# placing one where a `false` would normally be gracefully absorbed
# turns backtracking into a hard, uncatchable failure.
#
# Does NOT follow nonterminal references (a bare `"Name"`) into the
# referenced rule; a `json`/`toml` reached only via a chain of
# nonterminal refs from an unsafe position is not caught by this walk.
# Each rule is checked independently, from its own top (always
# `committed = true`: a whole rule reporting `false` to `run`'s caller
# is an ordinary, correctly backtracking outcome, not a swallowed
# failure).
#
# `committed`'s meaning at a given position: true iff a `false` there
# would necessarily make the ENCLOSING RULE's own top-level result
# `false` too -- i.e. no combinator between here and the rule's top
# would instead absorb that `false` into an overall success (`opt`
# producing `null`, `star` just stopping the loop, an earlier `choice`
# branch's failure being retried as the next branch, `and`/`not`
# swallowing it into their own lookahead result). `json`/`toml` is safe
# only where `committed` is true: where it's already true, a hard
# failure was going to happen regardless, so throwing changes nothing
# observable.
rec {
  checkExprSafety =
    path: committed: expr:
    if expr == "" || builtins.isString expr then
      [ ]
    else if builtins.isList expr then
      builtins.concatMap (
        i: checkExprSafety (path ++ [ "seq[${builtins.toString i}]" ]) committed (builtins.elemAt expr i)
      ) (builtins.genList (i: i) (builtins.length expr))
    else if expr ? lit || expr ? range || expr ? regex || expr ? eof then
      [ ]
    else if expr ? choice then
      let
        branches = expr.choice;
        n = builtins.length branches;
        checkBranch =
          i: b:
          let
            isLastBranch = i == n - 1;
          in
          if builtins.isAttrs b && b ? cutSeq then
            # e1: branch not yet committed, failure just tries the next
            # branch -- always ungraceful-absorption-possible, so unsafe.
            # e2: once e1 commits, e2 failing fails the WHOLE CHOICE (see
            # packrat.nix's compileChoice header comment), so e2 inherits
            # the choice's OWN ambient `committed`, same as a plain last
            # branch does.
            checkExprSafety (path ++ [ "choice[${builtins.toString i}].cutSeq[0]" ]) false (
              builtins.elemAt b.cutSeq 0
            )
            ++ checkExprSafety (path ++ [ "choice[${builtins.toString i}].cutSeq[1]" ]) committed (
              builtins.elemAt b.cutSeq 1
            )
          else
            checkExprSafety (path ++ [ "choice[${builtins.toString i}]" ]) (committed && isLastBranch) b;
      in
      builtins.concatMap (i: checkBranch i (builtins.elemAt branches i)) (builtins.genList (i: i) n)
    else if expr ? star then
      let
        body = expr.star;
      in
      if builtins.isAttrs body && body ? cutSeq then
        # Same reasoning as a choice's cutSeq branch: e1's failure just
        # stops the loop gracefully (unsafe); e2's failure fails the WHOLE
        # STAR (see packrat.nix's compileStarCut header comment),
        # inheriting ambient.
        checkExprSafety (path ++ [ "star.cutSeq[0]" ]) false (builtins.elemAt body.cutSeq 0)
        ++ checkExprSafety (path ++ [ "star.cutSeq[1]" ]) committed (builtins.elemAt body.cutSeq 1)
      else
        checkExprSafety (path ++ [ "star" ]) false body
    else if expr ? plus then
      # Desugars to [ body { star = body; } ], same as packrat.nix's compile.
      checkExprSafety path committed [
        expr.plus
        { star = expr.plus; }
      ]
    else if expr ? opt then
      checkExprSafety (path ++ [ "opt" ]) false expr.opt
    else if expr ? and then
      checkExprSafety (path ++ [ "and" ]) false expr.and
    else if expr ? not then
      checkExprSafety (path ++ [ "not" ]) false expr.not
    else if expr ? cutSeq then
      # No commit context outside choice/star: degrades to a plain
      # sequence (same as packrat.nix's compile), both inherit ambient.
      checkExprSafety path committed [
        (builtins.elemAt expr.cutSeq 0)
        (builtins.elemAt expr.cutSeq 1)
      ]
    else if expr ? action then
      # Transparent to failure: fails iff its body fails, same as a
      # passthrough, so it inherits the ambient `committed` unchanged.
      checkExprSafety (path ++ [ "action" ]) committed expr.action.e
    else if expr ? json || expr ? toml then
      if committed then
        [ ]
      else
        [
          "${builtins.concatStringsSep "." path} -- ${
            if expr ? json then "json" else "toml"
          } used where a failure would be gracefully absorbed (inside opt/and/not/star's plain body, or a non-last choice branch); this combinator cannot return `false`, only succeed or throw, so it may only appear where the enclosing rule is already committed to failing hard on this path"
        ]
    else
      throw "packrat: unrecognized expression: ${builtins.toJSON expr}";

  # Runs checkExprSafety over every rule in `grammar`, from that rule's own
  # top (`committed = true`, see checkExprSafety's comment), and throws ONE
  # combined error listing every violation found across the whole grammar
  # -- not just the first -- so a grammar author fixing this sees every bad
  # placement in one pass instead of one throw-fix-rerun cycle per
  # violation. Returns `grammar` unchanged on success, so it composes as
  # `packrat.run { grammar = checkGrammarSafety myGrammar; ... }`.
  checkGrammarSafety =
    grammar:
    let
      violations = builtins.concatMap (
        name: map (v: "${name}: ${v}") (checkExprSafety [ name ] true grammar.${name})
      ) (builtins.attrNames grammar);
    in
    if violations == [ ] then
      grammar
    else
      throw ''
        packrat: unsafe json/toml placement found in ${builtins.toString (builtins.length violations)} spot(s):
        ${builtins.concatStringsSep "\n" (map (v: "  - ${v}") violations)}'';
}
