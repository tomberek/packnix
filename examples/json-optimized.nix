# The SAME JSON language as ./json-simple.nix, restructured with the
# `{ action = { e; f; }; }` combinator (lib/packrat.nix) to reduce the
# number of fields on every Derivs node -- this engine builds one node per
# input position, and every named rule in the grammar is a field on EVERY
# one of those nodes regardless of whether a given position ever uses it.
# Fewer rules = smaller nodes = less allocation, at the cost of losing
# per-position memoization SHARING for whatever gets inlined (see the note
# by `action` below). Diff this file against json-simple.nix to see
# exactly what moved and why.
#
# json-simple.nix's grammar has 13 named rules (13 fields per Derivs
# node); this one has 3 (WHITESPACE/STRING/X). Measured directly on the
# repo's lock-large.json fixture (391947 bytes), same input, same
# accept/reject/value output (confirmed byte-identical, both grammars,
# both fixtures) -- json-simple.nix: ~247.5MB RSS; this file: ~191.1MB
# RSS, a ~22.8% reduction from inlining alone (neither example includes
# the further compileSeq/compileStarPlain engine-level optimizations
# grammar/json.nix also uses, so this isolates the inlining technique
# specifically).
#
# Run with:
#   nix eval --impure --expr '
#     let
#       packrat = import ../lib/packrat.nix;
#       g = import ./json-optimized.nix;
#     in packrat.run { grammar = g.grammar; handlers = g.handlers; } 0
#          (builtins.readFile ../lock.json)
#   ' --json
let
  # --- Inlining single-reference rules -----------------------------------
  #
  # A rule referenced from exactly ONE place in the grammar can always be
  # folded into that one call site: there is only one place it's used, so
  # there's no way for two call sites to ever reach the same input
  # position and duplicate work that a shared Derivs-node field would
  # otherwise have memoized. That's a simple, mechanical safety check --
  # count references, and if it's 1, inlining changes nothing about what
  # the grammar accepts or how much work gets shared.
  #
  # In json-simple.nix, STRING_FRAG/STRING_RAW/LIST_ITEMS/ITEMS/ITEM are
  # all single-reference (STRING_FRAG from STRING_RAW's star; STRING_RAW
  # from STRING; LIST_ITEMS from LIST's opt; ITEMS from SET's opt; ITEM
  # from ITEMS, twice -- see stringFragment/commaSeparated/setItem below
  # for how each folds away). None of them carry an independent handler
  # entry in json-simple.nix's `handlers`, so folding them in is pure
  # grammar-structure surgery -- nothing needs to travel with them.
  stringFragment = {
    choice = [
      { regex = ''([^\\\"]+)''; }
      { lit = ''\"''; }
      { lit = ''\''; }
    ];
  };

  # `item ("," item)*` -- LIST_ITEMS's/ITEMS's shared shape, factored into
  # one function instead of writing the same star-of-pairs literal twice.
  # `item` is duplicated in the COMPILED grammar tree (used once as the
  # head, once inside the star), but that's a one-time cost paid when the
  # grammar is compiled, not a per-Derivs-node cost paid at every input
  # position -- worth knowing since it looks like repetition but isn't
  # the same kind of repetition the rest of this file is trying to avoid.
  commaSeparated = item: [
    item
    {
      star = [
        { lit = ","; }
        item
      ];
    }
  ];

  # ITEM's body ("name": value), inlined into commaSeparated's call
  # inside setBranch below instead of staying a named rule.
  setItem = [
    "WHITESPACE"
    "STRING"
    "WHITESPACE"
    { lit = ":"; }
    "X"
  ];

  # --- What's left as a named rule ----------------------------------------
  #
  # WHITESPACE/STRING/X are each referenced from 2+ places (WHITESPACE
  # appears all over; STRING from both X's choice and setItem; X from
  # itself via LIST/setItem and its own top-level use). Inlining a
  # multi-reference rule needs an actual argument that no two of its call
  # sites can be active at the SAME input position in a single parse --
  # not just a reference count -- so these stay as named rules here.
  common = {
    STRING = [
      { lit = "\""; }
      { star = stringFragment; }
      { lit = "\""; }
    ];
  };

  # --- Inlining rules that carry a HANDLER, via `action` -------------------
  #
  # NULL/BOOL/NUMBER/LIST/SET in json-simple.nix aren't just recognition
  # logic -- they each have a `handlers.<Name>` entry that turns their raw
  # match into a real value (a Nix bool, a Nix number, a flat list, an
  # attrset). Folding them into X's choice branches needs somewhere for
  # that transform to live once the rule is no longer a named field with
  # its own handler slot -- that's what `action` is for:
  #
  #   { action = { e; f; }; }
  #
  # evaluates `e` as usual; on success, applies `f` to `e`'s VALUE (not
  # its Derivs pointer) before returning. It's the handler, carried
  # inline with the expression instead of attached to a rule name.
  #
  # SAFETY NOTE: an `action`-wrapped expression is not a named field, so
  # it does NOT get computed once per position and shared the way a named
  # rule does -- it recompiles independently at every call site that
  # embeds it. Still CORRECT either way (a PEG's accept/reject behavior
  # never depends on memoization, only its O(n) time bound does), but if
  # two call sites of the SAME inlined expression were ever active at the
  # identical input position in one parse, the work would silently
  # duplicate instead of being shared. Each branch below is referenced
  # from exactly one place (xBranches), so that can't happen here --
  # exactly the same reference-counting argument as the plain inlinings
  # above, just applied to rules that also carry a value transform.
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
          { lit = "true"; }
          { lit = "false"; }
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
  listBranch = {
    action = {
      e = [
        { lit = "["; }
        "WHITESPACE"
        { opt = commaSeparated "X"; }
        "WHITESPACE"
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
  setBranch = {
    action = {
      e = [
        { lit = "{"; }
        "WHITESPACE"
        { opt = commaSeparated setItem; }
        "WHITESPACE"
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

  grammar = common // {
    WHITESPACE = { star = { regex = "([[:space:]]+)"; }; };
    X = [
      "WHITESPACE"
      {
        choice = [
          "STRING"
          setBranch
          numberBranch
          boolBranch
          listBranch
          nullBranch
        ];
      }
      "WHITESPACE"
    ];
  };

  # Only WHITESPACE/STRING/X need handlers here -- NULL/BOOL/NUMBER/
  # LIST/SET's transforms travel with their `action` expressions above
  # instead of needing an entry in this attrset.
  handlers = {
    WHITESPACE = v: builtins.concatStringsSep "" v;
    STRING = v: builtins.concatStringsSep "" (builtins.elemAt v 1);
    X = v: builtins.elemAt v 1;
  };
in
{
  inherit grammar handlers;
}
