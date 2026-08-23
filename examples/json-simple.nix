# A plain, unoptimized JSON grammar for lib/packrat.nix -- every construct
# gets its own named rule, the way you'd write it reading a grammar spec
# off a page, with no attention to how many Derivs-node fields that
# produces. Contrast with ./json-optimized.nix (same language, fewer
# fields per node).
#
# Run with:
#   nix eval --impure --expr '
#     let
#       packrat = import ../lib/packrat.nix;
#       g = import ./json-simple.nix;
#     in packrat.run { grammar = g.grammar; handlers = g.handlers; } 0
#          (builtins.readFile ../lock.json)
#   ' --json
# (adjust the relative paths to wherever you run this from)
let
  grammar = {
    WHITESPACE = { star = { regex = "([[:space:]]+)"; }; };

    NULL = { lit = "null"; };
    BOOL = {
      choice = [
        { lit = "true"; }
        { lit = "false"; }
      ];
    };
    NUMBER = { regex = "([0-9]+)"; };

    # A string is a quote, some fragments, a quote -- escapes get their
    # own nonterminal.
    STRING = [
      { lit = "\""; }
      "STRING_RAW"
      { lit = "\""; }
    ];
    STRING_FRAG = {
      choice = [
        { regex = ''([^\\\"]+)''; }
        { lit = ''\"''; }
        { lit = ''\''; }
      ];
    };
    STRING_RAW = { star = "STRING_FRAG"; };

    # A list is "[", optional items, "]"; items are one value followed by
    # zero or more ", value" pairs.
    LIST = [
      { lit = "["; }
      "WHITESPACE"
      { opt = "LIST_ITEMS"; }
      "WHITESPACE"
      { lit = "]"; }
    ];
    LIST_ITEMS = [
      "X"
      {
        star = [
          { lit = ","; }
          "X"
        ];
      }
    ];

    # A set (JSON "object") is the same shape as a list, but of "key: value"
    # items instead of bare values.
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
        star = [
          { lit = ","; }
          "ITEM"
        ];
      }
    ];
    ITEM = [
      "WHITESPACE"
      "STRING"
      "WHITESPACE"
      { lit = ":"; }
      "X"
    ];

    # The top-level value: any JSON value, with surrounding whitespace.
    X = [
      "WHITESPACE"
      {
        choice = [
          "STRING"
          "SET"
          "NUMBER"
          "BOOL"
          "LIST"
          "NULL"
        ];
      }
      "WHITESPACE"
    ];
  };

  # One handler per named rule that needs its raw match turned into a real
  # value -- exactly mirroring the grammar above, rule for rule.
  handlers = {
    WHITESPACE = v: builtins.concatStringsSep "" v;
    STRING_RAW = v: builtins.concatStringsSep "" v;
    STRING = v: builtins.elemAt v 1;
    NUMBER = builtins.fromJSON;
    BOOL = v: v == "true";
    NULL = v: null;

    LIST_ITEMS = v: [ (builtins.elemAt v 0) ] ++ map (p: builtins.elemAt p 1) (builtins.elemAt v 1);
    LIST =
      v:
      let
        opt = builtins.elemAt v 2;
      in
      if opt == null then [ ] else opt;

    ITEM = v: {
      name = builtins.elemAt v 1;
      value = builtins.elemAt v 4;
    };
    ITEMS = v: [ (builtins.elemAt v 0) ] ++ map (p: builtins.elemAt p 1) (builtins.elemAt v 1);
    SET =
      v:
      let
        opt = builtins.elemAt v 2;
        items = if opt == null then [ ] else opt;
      in
      builtins.listToAttrs items;

    X = v: builtins.elemAt v 1;
  };
in
{
  inherit grammar handlers;
}
