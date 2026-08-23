# This is grammar/json.nix itself, re-exported (via `handlersNoCut`/
# `grammarNoCut`, the non-cut variant, to keep this example focused on the
# `action`/inlining technique rather than tangled up with the separate cut
# operator) -- kept as a thin passthrough rather than a hand-copied
# lookalike so this file can never silently drift out of sync with the
# real grammar the way an earlier version of this file did.
#
# The technique: `{ action = { e; f; }; }` (lib/packrat.nix) lets a rule's
# value-transform travel with an inlined expression instead of needing a
# named field on every Derivs node. This engine builds one node per input
# position, and every named rule in the grammar is a field on EVERY one
# of those nodes regardless of whether a given position ever uses it --
# fewer rules means smaller nodes means less allocation. Contrast with
# ./json-simple.nix, which is the SAME language with every construct
# written as its own named rule (13 rules) instead of folded down to 3
# (WHITESPACE/STRING/X) -- diff the two to see exactly what moved.
#
# Where grammar/json.nix's actual rules differ from json-simple.nix's
# naive versions (separate, unrelated optimizations layered on top of the
# `action` technique this file is demonstrating):
#   - WHITESPACE uses `opt`, not `star`: `[[:space:]]+` already greedily
#     consumes the whole run in one match, so `star`'s recheck loop would
#     only ever fire 0 or 1 times -- pure overhead `opt` avoids.
#   - LIST/SET have only ONE WHITESPACE around their body, not two: `X`
#     (and setItem, via its own trailing `X`) already eats its own
#     trailing whitespace, so a second WHITESPACE right before the
#     closing bracket would always be redundant.
#   - commaSeparated's repetition uses `cutSeq`, not plain `star`:
#     Mizushima et al.'s commit operator (PASTE'10 §3.2), which measures
#     faster on long comma-separated runs -- a different technique from
#     `action`/inlining, included here only because it's part of the
#     real grammar being re-exported.
#   - BOOL's branch order is "false" before "true": outnumbers it ~14:1
#     in this repo's fixtures, and PEG choice tries branches left-to-right.
#
# json-simple.nix's grammar has 13 named rules (13 fields per Derivs
# node); this one has 3. Measured directly on the repo's lock-large.json
# fixture (391947 bytes), same input, same accept/reject/value output
# (confirmed byte-identical) -- json-simple.nix: ~247.6MB RSS; this file
# (the real grammar/json.nix, WITH its other optimizations, since this
# is a direct re-export rather than a hand-copied approximation): ~170.2MB
# RSS, a ~31.2% reduction. That number is larger than `action`/inlining
# alone would give (a from-scratch inlining-only comparison measured
# ~22.8%) -- the rest comes from the unrelated optimizations listed
# above (opt vs star, single WHITESPACE, cut, branch ordering) riding
# along for free since this file just imports the real grammar.
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
  real = import ../grammar/json.nix;
in
{
  grammar = real.grammarNoCut;
  handlers = real.handlersNoCut;
}
