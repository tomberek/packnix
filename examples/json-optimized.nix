# grammar/json.nix itself, re-exported (via the non-cut variant, to keep
# this focused on `action`/inlining rather than the separate cut
# operator) -- a thin passthrough so this file can't drift out of sync
# with the real grammar the way an earlier hand-copied version of it did.
#
# The technique: `{ action = { e; f; }; }` (lib/packrat.nix) lets a
# rule's value-transform travel with an inlined expression instead of
# needing a named field on every Derivs node -- fewer rules means smaller
# nodes means less allocation. Contrast with ./json-simple.nix, the same
# language with every construct as its own named rule (13, vs. this
# grammar's 3: WHITESPACE/STRING/X) -- diff the two to see what moved.
#
# grammar/json.nix layers a few other, unrelated optimizations on top of
# that (opt instead of star for WHITESPACE; one WHITESPACE around LIST/
# SET's body instead of two; cutSeq in commaSeparated; BOOL tried
# false-before-true) -- present here only because this re-exports the
# real file, not part of the action/inlining technique itself.
#
# Measured on a real-world 391947-byte flake.lock-shaped fixture (not
# included in this repo), same accept/reject/value output confirmed
# byte-identical: json-simple.nix ~247.6MB RSS; this file ~170.2MB, a
# ~31.2% reduction (larger than inlining alone would give, ~22.8% in
# isolation -- the rest comes from those other optimizations riding along
# for free).
#
# Run with:
#   nix eval --impure --expr '
#     let
#       packrat = import ../lib/packrat.nix;
#       g = import ./json-optimized.nix;
#     in packrat.run { grammar = g.grammar; handlers = g.handlers; } 0
#          (builtins.readFile ../data/lock.json)
#   ' --json
let
  real = import ../grammar/json.nix;
in
{
  grammar = real.grammarNoCut;
  handlers = real.handlersNoCut;
}
