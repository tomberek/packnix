# A TSV (tab-separated values) grammar for lib/packrat.nix -- rows of
# fields separated by "\t", rows separated by "\n", with an optional
# trailing newline. Fields are any run of characters other than tab or
# newline (including empty, so "a\t\tb" has a middle empty field).
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
    DOCUMENT = [
      "ROW"
      {
        # `and = { regex = "(.)"; }` requires a char after the "\n" before
        # committing to another row -- otherwise a single trailing "\n" at
        # EOF would greedily match here as a separator into a phantom
        # empty-field row, instead of being left for the final `opt` below.
        star = [
          { lit = "\n"; }
          {
            and = {
              regex = "(.)";
            };
          }
          "ROW"
        ];
      }
      {
        opt = {
          lit = "\n";
        };
      }
    ];
  };

  # ROW's raw shape is "first item, then (sep item)* pairs" -- flatten it
  # into a plain list.
  flattenPairs =
    itemIndex: v:
    [ (builtins.elemAt v 0) ] ++ map (p: builtins.elemAt p itemIndex) (builtins.elemAt v 1);

  handlers = {
    ROW = flattenPairs 1;
    # DOCUMENT's star body is [lit and ROW] (3 elements, the lookahead
    # sitting between the separator and the row), so ROW is at index 2 in
    # each iteration's raw value, not 1.
    DOCUMENT = flattenPairs 2;
  };
in
{
  inherit grammar handlers;
}
