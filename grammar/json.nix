# The JSON grammar, using the star/opt/cutSeq combinators from
# lib/packrat.nix. Exposes both `grammarNoCut` (top-level `X` is a
# plain ordered choice) and `grammar` (each `X` alternative wrapped in
# cutSeq, since SET/LIST/STRING/NUMBER/BOOL/NULL are first-token-disjoint --
# mirrors the cut paper's AC-FIRST motivating example, PASTE'10 §4.2). Both
# variants are kept for A/B benchmarking (see bench/measure.sh); default.nix
# picks one via `useCut`.
let
  # STRING_RAW was previously a standalone nonterminal referenced only
  # from STRING; inlined into STRING's own sequence body below for the
  # same per-node thunk-allocation reason as STRING_FRAG/COMMENT_CHAR
  # before it -- every nonterminal is a field on every Derivs node
  # regardless of whether it's touched at a given position. Named here
  # (rather than written inline in `common`) purely for readability.
  stringFragment = {
    choice = [
      { regex = ''([^\\\"]+)''; }
      { lit = ''\"''; }
      { lit = ''\''; }
    ];
  };

  # `item ("," item)*`, LIST_ITEMS's/ITEMS's shared shape before they were
  # inlined into LIST's/SET's `opt` (each was a standalone nonterminal
  # referenced only once, same lever as stringFragment above). Kept as a
  # named function rather than duplicating the pattern twice inline --
  # LIST passes the nonterminal reference "X", SET passes setItem's
  # expression directly (see below).
  #
  # Cut on the repetition body: verified this only changes WHY a trailing
  # comma is rejected (whole star fails outright, vs. plain (e1 e2)*
  # stopping early and letting the outer "]"/"}" reject the leftover ","),
  # not WHETHER -- "," is never a valid start of "]"/"}". Also
  # faster/lighter than plain on long runs (compileStarCut goes straight
  # to genericClosure once, vs. plain's cheap-path/genericClosure-
  # escalation splicing every 500 items; measured ~9% less RSS at 50000
  # items) and statistically tied at this repo's realistic sizes
  # (<=20 items).
  commaSeparated = item: [
    item
    {
      star = {
        cutSeq = [
          { lit = ","; }
          item
        ];
      };
    }
  ];

  # `"name": value`, ITEM's body before it was inlined into SET's
  # commaSeparated call below (referenced only from there, same lever as
  # stringFragment/commaSeparated above). Unlike stringFragment, this one
  # ends up duplicated in the compiled grammar tree -- commaSeparated
  # uses `item` twice (the head element and the star body) -- but that's
  # a one-time, position-independent compile cost, not a per-node one:
  # confirmed directly, this measured a further ~3% RSS reduction on
  # lock-large.json from removing this one field, byte-identical output.
  setItem = [
    "WHITESPACE"
    "STRING"
    "WHITESPACE"
    { lit = ":"; }
    "X"
  ];

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

    STRING = [
      { lit = "\""; }
      { star = stringFragment; }
      { lit = "\""; }
    ];
  };

  # NUMBER/BOOL/NULL/LIST/SET were previously named rules in `common`,
  # each referenced from exactly ONE place (xBranches below) -- same
  # single-reference lever as the earlier STRING_RAW/LIST_ITEMS/ITEMS/ITEM
  # inlinings, just extended to rules that carry a real value-transform
  # (handler) rather than pure recognition logic. `{ action = { e; f; }; }`
  # (an EXPERIMENTAL combinator added to lib/packrat.nix for this) lets the
  # transform travel with the inlined expression instead of needing a
  # named Derivs-node field to hang off of.
  #
  # Referenced-from-exactly-one-place is what makes this safe: it's
  # structurally impossible for two call sites of the same inlined
  # expression to collide at the same input position within one parse
  # (there being only one call site), so there's no risk of silently
  # duplicating work that packrat memoization would otherwise have
  # shared. This reasoning does NOT extend to STRING/WHITESPACE/X, which
  # are referenced from 2+ places each -- inlining those would need an
  # actual position-disjointness argument, not just a reference count,
  # so they stay as named rules for now.
  #
  # "false" tried first in BOOL: outnumbers "true" ~14:1 in this repo's
  # fixtures.
  numberBranch = {
    action = {
      e = { regex = "([0-9]+)"; };
      f = builtins.fromJSON;
    };
  };
  boolBranch = {
    action = {
      e = {
        choice = [
          { lit = "false"; }
          { lit = "true"; }
        ];
      };
      f = v: v == "true";
    };
  };
  nullBranch = {
    action = {
      e = { lit = "null"; };
      f = v: null;
    };
  };

  # `opt` lets LIST/SET accept an empty body ("[]"/"{}"). Only ONE
  # WHITESPACE around the body, not two: X already eats its own leading
  # and trailing whitespace (X = [WHITESPACE choice WHITESPACE]), and
  # setItem ends in "X" so it inherits that trailing WHITESPACE too --
  # so a second WHITESPACE right before "]"/"}" would be redundant
  # either way: the last item already ate it if the body is non-empty,
  # or the first WHITESPACE (kept, since there's no item to eat the
  # space in "[ ]"/"{ }") already did if it's empty. Confirmed
  # directly: removing the second WHITESPACE produces byte-identical
  # output on this repo's lock-large.json, and "[]"/"[ ]"/"[1,2,3]"/
  # "[1,2,3 ]"/"[ 1,2,3]" (and the SET/nested equivalents) all still
  # parse correctly.
  #
  # Unwraps `opt`-wrapped-leading-item-plus-star-of-pairs into a flat
  # list ([] if the opt didn't match).
  listBranch = {
    action = {
      e = [
        { lit = "["; }
        "WHITESPACE"
        { opt = commaSeparated "X"; }
        { lit = "]"; }
      ];
      f =
        v:
        let
          opt = builtins.elemAt v 2;
        in
        if opt == null then [ ] else [ (builtins.elemAt opt 0) ] ++ map (p: builtins.elemAt p 1) (builtins.elemAt opt 1);
    };
  };

  # Each raw item is setItem's shape, [WHITESPACE STRING WHITESPACE lit
  # X] -- no separate ITEM handler runs anymore (it's inlined, not a
  # nonterminal), so SET pulls name/value out of the raw sequence itself
  # instead of relying on a pre-transformed {name;value;}.
  setBranch = {
    action = {
      e = [
        { lit = "{"; }
        "WHITESPACE"
        { opt = commaSeparated setItem; }
        { lit = "}"; }
      ];
      f =
        v:
        let
          opt = builtins.elemAt v 2;
          toPair = item: {
            name = builtins.elemAt item 1;
            value = builtins.elemAt item 4;
          };
        in
        builtins.listToAttrs (
          if opt == null then
            [ ]
          else
            [ (toPair (builtins.elemAt opt 0)) ] ++ map (p: toPair (builtins.elemAt p 1)) (builtins.elemAt opt 1)
        );
    };
  };

  # Ordered by observed real-world value-type frequency (strings/sets most
  # common, lists rarest) to minimize failed-branch attempts in this
  # non-cut ordered choice -- PEG tries branches left-to-right and stops at
  # the first success.
  xBranches = [
    "STRING"
    setBranch
    numberBranch
    boolBranch
    listBranch
    nullBranch
  ];

  grammarNoCut = common // {
    X = [
      "WHITESPACE"
      { choice = xBranches; }
      "WHITESPACE"
    ];
  };

  # Each branch becomes `{ cutSeq = [ <branch> ""]; }` (e2 = epsilon, just
  # to give the cut something to commit after). Branches are
  # first-token-disjoint, so committing changes no accept/reject outcome.
  grammar = common // {
    X = [
      "WHITESPACE"
      {
        choice = map (b: { cutSeq = [ b "" ]; }) xBranches;
      }
      "WHITESPACE"
    ];
  };

  # Shared between both variants; only X differs, since the cut variant's
  # inner choice value is wrapped one level deeper ([branchVal ""]) than
  # the plain-choice variant's (branchVal directly). NUMBER/BOOL/NULL/
  # LIST/SET no longer need entries here -- their transforms now travel
  # with the inlined `action` expressions in xBranches above.
  handlersCommon = {
    # `opt`'s raw value is the matched string directly, or `null` if there
    # was no whitespace to match (unlike `star`'s list-of-matches shape).
    WHITESPACE = v: if v == null then "" else v;
    # [lit fragmentList lit]; concatenate the inlined star's fragments
    # directly (no separate STRING_RAW handler now that it's inlined).
    STRING = v: builtins.concatStringsSep "" (builtins.elemAt v 1);
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
