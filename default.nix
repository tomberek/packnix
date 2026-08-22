# Thin wrapper: wires the generic engine (lib/packrat.nix) to the JSON
# grammar (grammar/json.nix) and exposes the same external surface the
# original default.nix had, so run.sh's
#   nix eval --file default.nix --apply 'x: x ./lock.json' --json
# keeps working unmodified (the exported value is a `__functor` attrset,
# so calling it as `x ./lock.json` dispatches to `pack`).
#
# `useCut` picks which JSON grammar variant (Phase 1 no-cut vs Phase 2
# cut-enabled) is wired up; flip it to compare, or see bench/measure.sh
# which imports lib/packrat.nix + grammar/json.nix directly to run both
# variants side by side without editing this file.
let
  packrat = import ./lib/packrat.nix;
  jsonGrammar = import ./grammar/json.nix;

  useCut = true;

  grammar = if useCut then jsonGrammar.grammar else jsonGrammar.grammarNoCut;
  handlers = if useCut then jsonGrammar.handlers else jsonGrammar.handlersNoCut;

  # myrun : count -> string -> { <Nonterminal> = value | false; ... }
  # Same signature/shape as the original file's internal `myrun`.
  myrun = count: string: packrat.run { inherit grammar handlers; } count string;

  # pack : path -> parsed X value (or `false` on failure)
  # Reads the file at `path` and parses it from position 0, returning the
  # top-level X nonterminal's value -- matching what the original file's
  # exported lambda did, except this one actually honors its argument
  # (the original ignored its `file:` parameter and hardcoded
  # `./lock.json`; fixed here as part of the rewrite).
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
