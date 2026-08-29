# grammar/flakelock.nix itself, re-exported -- a thin passthrough so this
# file can't drift out of sync with the real grammar, same reasoning as
# ./json-optimized.nix.
#
# The technique here is one level up from json-optimized.nix's rule
# inlining: instead of a generic JSON grammar that discovers an object's
# keys at parse time (try each of N possible keys via `choice`, loop
# until "}"), this grammar is written against one *specific* JSON
# schema -- a `nix flake lock` output file -- whose shape is known ahead
# of time from inspecting real files:
#   - every object's keys appear in the file in strict alphabetical
#     order, with no exceptions
#   - every `nodes.*` entry is one of exactly 4 fixed key-sets
#   - every field name maps to exactly one JSON value type, never a union
#   - no string in the file contains a `"` or `\`, so no escape handling
#     is needed
# That lets every object become a fixed, linear sequence of `opt`-wrapped
# fields tried once each in a known order instead of a generic "parse a
# key, dispatch on its name" loop -- no backtracking over key identity or
# order at all. The only genuinely generic parsing left is where key
# names really are arbitrary data (node names in `nodes`, input names in
# `inputs`).
#
# Like json-optimized.nix, every non-recursive sub-expression is inlined
# via `{ action = { e; f; }; }` (lib/packrat.nix) rather than given a
# named Derivs-node field -- here pushed further, to just one named rule
# (`DOCUMENT`) for the whole grammar, since nothing in a flake.lock's
# fixed shape is recursive or revisits a position twice.
#
# Measured on a real-world 391947-byte flake.lock-shaped fixture, full
# parse + materialization, byte-identical output confirmed against
# Python's json.load: grammar/json.nix ~175.5MB RSS / ~0.49s; this
# grammar ~138.1MB / ~0.32s -- roughly -21% RSS, -35% wall time.
#
# Also run against a real, external ~14.2MB flake.lock (23756 nodes):
# grammar/json.nix ~19.4s / 4.71GB RSS; this grammar ~10.6s / 3.4GB RSS
# (-45% time, -28% RSS), output confirmed byte-identical to Python's
# json.load of the same file.
#
# The trade-off for all of this: the grammar only accepts documents
# matching the exact schema above. A flake.lock from a future nix version
# with a new field, or any other JSON document, correctly fails to parse
# rather than silently mis-parsing -- inflexibility is the point, not a
# bug, but it does mean this grammar needs updating if the schema ever
# changes, unlike grammar/json.nix.
#
# Run with:
#   nix eval --impure --expr '
#     let
#       packrat = import ../lib/packrat.nix;
#       g = import ./flakelock-specialized.nix;
#     in packrat.run { grammar = g.grammar; handlers = g.handlers; } 0
#          (builtins.readFile ../bench/fixtures/synth-2000.json)
#   ' --json
let
  real = import ../grammar/flakelock.nix;
in
{
  inherit (real) grammar handlers;
}
