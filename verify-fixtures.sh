#!/usr/bin/env bash
# Correctness gate: every valid JSON fixture in the repo must parse to a
# value byte-identical (structurally equal, via Nix `==`) to
# `builtins.fromJSON`, through BOTH grammar/json.nix (generic) and
# grammar/flakelock.nix (schema-specialized -- only expected to succeed on
# flake.lock-shaped documents, so its failures are reported but don't fail
# the gate unless a fixture is specifically flake.lock-shaped).
#
# This formalizes the "confirmed byte-identical against builtins.fromJSON"
# check that's been done by hand throughout this repo's history (see
# examples/*.nix, bench/*.md) into something CI can run on every push.
#
# Deliberately excludes bench/fixtures/fixture-{50000,150000,300000}.json:
# those are truncated mid-token on purpose (see bench/make-fixtures.sh) to
# exercise backtracking on invalid input, not valid JSON to round-trip.
#
# Usage: ./verify-fixtures.sh
set -uo pipefail
cd "$(dirname "$0")"

fixtures=(
  data/lock.json
  data/lock-small.json
  bench/fixtures/synth-5.json
  bench/fixtures/synth-15.json
  bench/fixtures/synth-30.json
  bench/fixtures/synth-60.json
  bench/fixtures/synth-120.json
  bench/fixtures/synth-250.json
  bench/fixtures/synth-500.json
  bench/fixtures/synth-1000.json
  bench/fixtures/synth-2000.json
)

fail=0

for fx in "${fixtures[@]}"; do
  if [[ ! -f "$fx" ]]; then
    echo "SKIP (missing) $fx"
    continue
  fi

  generic_ok=$(nix eval --impure --expr "
    let
      packrat = import ./lib/packrat.nix;
      j = import ./grammar/json.nix;
      content = builtins.unsafeDiscardStringContext (builtins.readFile ./${fx});
    in (packrat.run { grammar = j.grammar; handlers = j.handlers; } 0 content).X == builtins.fromJSON content
  " 2>&1)

  if [[ "$generic_ok" == "true" ]]; then
    echo "OK   generic  $fx"
  else
    echo "FAIL generic  $fx: $generic_ok"
    fail=1
  fi

  flakelock_result=$(nix eval --impure --expr "
    let
      packrat = import ./lib/packrat.nix;
      g = import ./grammar/flakelock.nix;
      content = builtins.unsafeDiscardStringContext (builtins.readFile ./${fx});
      parsed = (packrat.run { grammar = g.grammar; handlers = g.handlers; } 0 content).DOCUMENT;
    in if parsed == packrat.NO_MATCH then \"REJECTED\" else if parsed == builtins.fromJSON content then \"MATCH\" else \"MISMATCH\"
  " 2>&1)

  case "$flakelock_result" in
    '"MATCH"')
      echo "OK   flakelock $fx"
      ;;
    '"REJECTED"')
      echo "SKIP flakelock $fx (not flake.lock-shaped, correctly rejected)"
      ;;
    *)
      echo "FAIL flakelock $fx: $flakelock_result"
      fail=1
      ;;
  esac
done

if [[ "$fail" -ne 0 ]]; then
  echo
  echo "verify-fixtures: FAILED"
  exit 1
fi

echo
echo "verify-fixtures: all fixtures byte-identical to builtins.fromJSON"
