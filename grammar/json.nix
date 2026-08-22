# The JSON grammar, using the star/opt/cutSeq combinators from
# lib/packrat.nix. Exposes both `grammarNoCut` (top-level `X` is a
# plain ordered choice) and `grammar` (each `X` alternative wrapped in
# cutSeq, since SET/LIST/STRING/NUMBER/BOOL/NULL are first-token-disjoint --
# mirrors the cut paper's AC-FIRST motivating example, PASTE'10 §4.2). Both
# variants are kept for A/B benchmarking (see bench/measure.sh); default.nix
# picks one via `useCut`.
let
  # Rules shared verbatim between the cut and no-cut variants.
  common = {
    # `opt`, not `star`: `[[:space:]]+` already greedily consumes the WHOLE
    # contiguous run in one evalRegex call (confirmed: builtins.match is
    # greedy), so `star`'s iterate-and-recheck loop here only ever runs 0
    # or 1 times -- pure overhead. `opt` gets the same "zero or more"
    # acceptance (a bare `regex` atom requires >=1 match, which would
    # reject e.g. the "[]" in this repo's own lock-large.json) without a
    # star's per-call setup or its list-of-matches result shape.
    WHITESPACE = { opt = { regex = "([[:space:]]+)"; }; };
    NULL = { lit = "null"; };
    # "false" tried first: outnumbers "true" ~14:1 in this repo's fixtures.
    BOOL = {
      choice = [
        { lit = "false"; }
        { lit = "true"; }
      ];
    };
    NUMBER = { regex = "([0-9]+)"; };

    STRING = [
      { lit = "\""; }
      "STRING_RAW"
      { lit = "\""; }
    ];
    # Fragment alternatives inlined rather than a separate STRING_FRAG
    # rule: every nonterminal is a field on every Derivs node, so a
    # single-use wrapper costs real per-node allocation for nothing.
    STRING_RAW = {
      star = {
        choice = [
          { regex = ''([^\\\"]+)''; }
          { lit = ''\"''; }
          { lit = ''\''; }
        ];
      };
    };

    # `opt` lets LIST/SET accept an empty body ("[]"/"{}").
    LIST = [
      { lit = "["; }
      "WHITESPACE"
      { opt = "LIST_ITEMS"; }
      "WHITESPACE"
      { lit = "]"; }
    ];
    # Cut here changes WHY a trailing comma is rejected (the whole star
    # fails outright, vs. plain (e1 e2)* stopping early and letting the
    # outer "]"/"}" reject the leftover ","), but not WHETHER it's
    # rejected: "," is never a valid start of "]"/"}", so both encodings
    # accept/reject identically here (verified directly against
    # trailing-comma, empty, and valid inputs). Also faster/lighter than
    # plain on long runs: measured ~9% less RSS at 50000 items (compileStarCut
    # goes straight to genericClosure once, vs. plain's cheap-path/
    # genericClosure-escalation splicing every 500 items) and statistically
    # tied at this repo's realistic sizes (<=20 items).
    LIST_ITEMS = [
      "X"
      {
        star = {
          cutSeq = [
            { lit = ","; }
            "X"
          ];
        };
      }
    ];

    SET = [
      { lit = "{"; }
      "WHITESPACE"
      { opt = "ITEMS"; }
      "WHITESPACE"
      { lit = "}"; }
    ];
    ITEMS = [
      "ITEM"
      {
        star = {
          cutSeq = [
            { lit = ","; }
            "ITEM"
          ];
        };
      }
    ];
    ITEM = [
      "WHITESPACE"
      "STRING"
      "WHITESPACE"
      { lit = ":"; }
      "X"
    ];
  };

  # Ordered by observed real-world value-type frequency (strings/sets most
  # common, lists rarest) to minimize failed-branch attempts in this
  # non-cut ordered choice -- PEG tries branches left-to-right and stops at
  # the first success.
  xBranches = [
    "STRING"
    "SET"
    "NUMBER"
    "BOOL"
    "LIST"
    "NULL"
  ];

  grammarNoCut = common // {
    X = [
      "WHITESPACE"
      { choice = xBranches; }
      "WHITESPACE"
    ];
  };

  # Each branch becomes `{ cutSeq = [ "<NAME>" ""]; }` (e2 = epsilon, just
  # to give the cut something to commit after). Branches are
  # first-token-disjoint, so committing changes no accept/reject outcome.
  grammar = common // {
    X = [
      "WHITESPACE"
      {
        choice = map (name: { cutSeq = [ name "" ]; }) xBranches;
      }
      "WHITESPACE"
    ];
  };

  # Shared between both variants; only X differs, since the cut variant's
  # inner choice value is wrapped one level deeper ([branchVal ""]) than
  # the plain-choice variant's (branchVal directly).
  handlersCommon = {
    # `opt`'s raw value is the matched string directly, or `null` if there
    # was no whitespace to match (unlike `star`'s list-of-matches shape).
    WHITESPACE = v: if v == null then "" else v;
    STRING_RAW = v: builtins.concatStringsSep "" v;
    # Plain Nix string, not `{ string = ...; }` -- JSON strings decode to
    # (and must re-encode as) plain strings.
    STRING = v: builtins.elemAt v 1;
    ITEM = v: {
      name = builtins.elemAt v 1;
      value = builtins.elemAt v 4;
    };
    NUMBER = builtins.fromJSON;
    # Real Nix true/false/null, not the literally matched text.
    BOOL = v: v == "true";
    NULL = v: null;

    # Unwraps `opt`-wrapped-leading-item-plus-star-of-pairs into a flat
    # list ([] if the opt didn't match).
    LIST =
      v:
      let
        opt = builtins.elemAt v 2;
      in
      if opt == null then [ ] else [ (builtins.elemAt opt 0) ] ++ map (p: builtins.elemAt p 1) (builtins.elemAt opt 1);

    SET =
      v:
      let
        opt = builtins.elemAt v 2;
      in
      builtins.listToAttrs (
        if opt == null then [ ] else [ (builtins.elemAt opt 0) ] ++ map (p: builtins.elemAt p 1) (builtins.elemAt opt 1)
      );
  };

  handlersNoCut = handlersCommon // {
    X = v: builtins.elemAt v 1;
  };

  handlers = handlersCommon // {
    X = v: builtins.elemAt (builtins.elemAt v 1) 0;
  };
in
{
  inherit
    grammarNoCut
    grammar
    handlersNoCut
    handlers
    ;
}
