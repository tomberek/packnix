# Thin wrapper: wires the generic engine (lib/packrat.nix) to the JSON
# grammar (grammar/json.nix). run.sh calls this via
#   nix eval --file default.nix --apply 'x: x ./data/lock.json' --json
# (the exported value is a `__functor` attrset, so `x ./data/lock.json`
# dispatches to `pack`).
#
# `useCut` picks which grammar variant is wired up; flip it to compare, or
# see bench/measure.sh, which imports both directly to run them side by
# side without editing this file.
let
  packrat = import ./lib/packrat.nix;
  jsonGrammar = import ./grammar/json.nix;

  useCut = true;

  grammar = if useCut then jsonGrammar.grammar else jsonGrammar.grammarNoCut;
  handlers = if useCut then jsonGrammar.handlers else jsonGrammar.handlersNoCut;

  myrun = count: string: packrat.run { inherit grammar handlers; } count string;

  # Reads `path` and parses it from position 0, returning the top-level X
  # value (or `packrat.NO_MATCH` on failure -- see lib/packrat.nix's run
  # for why that, not `false`, is the failure sentinel).
  pack =
    path:
    let
      contents = builtins.unsafeDiscardStringContext (builtins.readFile path);
    in
    (myrun 0 contents).X;
in
{
  inherit pack myrun;
  __functor = self: path: self.pack path;
}
