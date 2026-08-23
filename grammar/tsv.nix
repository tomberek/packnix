# A TSV (tab-separated values) grammar for lib/packrat.nix -- rows of
# fields separated by "\t", each row (including the last) terminated by
# "\n". Fields are any run of characters other than tab or newline
# (including empty, so "a\t\tb" has a middle empty field).
let
  grammar = {
    FIELD = {
      regex = "([^\t\n]*)";
    };
    ROW = [
      "FIELD"
      {
        star = [
          { lit = "\t"; }
          "FIELD"
        ];
      }
    ];
    # `star`, not the "first item, then (sep item)*" shape ROW uses above --
    # "\n" is a terminator here, not a separator, so every iteration is
    # uniform (no leading item to special-case) and always consumes at
    # least the "\n", so an all-empty ROW can't make this loop forever.
    DOCUMENT = {
      star = [
        "ROW"
        { lit = "\n"; }
      ];
    };
  };

  handlers = {
    # ROW's raw shape is "first item, then (sep item)* pairs" -- flatten it
    # into a plain list.
    ROW = v: [ (builtins.elemAt v 0) ] ++ map (p: builtins.elemAt p 1) (builtins.elemAt v 1);
    # DOCUMENT is a list of [row "\n"] pairs -- keep just the rows.
    DOCUMENT = v: map (pair: builtins.elemAt pair 0) v;
  };
in
{
  inherit grammar handlers;
}
