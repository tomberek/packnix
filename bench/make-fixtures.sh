#!/usr/bin/env bash
# Truncated-prefix fixtures of synth-2000.json (636392 bytes) for Phase 3
# memory/time measurement. These are NOT valid JSON (truncated mid-token in
# most cases) -- that's fine and arguably more representative: it forces
# the parser to backtrack through the WHOLE partially-consumed structure
# looking for a way to finish before finally failing, which is exactly the
# kind of pathological-for-backtracking-without-commit case cut is meant to
# help with. Re-run this to regenerate fixtures/ if synth-2000.json changes.
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p fixtures
src=fixtures/synth-2000.json
for n in 50000 150000 300000; do
  head -c "$n" "$src" > "fixtures/fixture-${n}.json"
done
cp "$src" fixtures/fixture-full.json
wc -c fixtures/*.json
