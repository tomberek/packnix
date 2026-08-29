#!/usr/bin/env bash
# Parity gate: for every flake.lock-shaped fixture, grammar/flakelock.nix
# (string-position packrat grammar) and examples/flakelock-valuewalk.nix
# (lib/valuewalk.nix schema over the ALREADY-PARSED value tree, via
# builtins.fromJSON -- see that file's header comment) must agree on
# every fixture: both accept it and produce byte-identical output, or
# both reject it.
#
# Fixtures list mirrors verify-fixtures.sh's flakelock-shaped subset
# (excludes data/lock.json and data/lock-small.json, which are NOT
# flake.lock-shaped -- verify-fixtures.sh already reports "SKIP
# flakelock ... correctly rejected" for both).
#
# Usage: ./verify-valuewalk-parity.sh
set -uo pipefail
cd "$(dirname "$0")"

fixtures=(
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

  result=$(nix eval --impure --expr "
    let
      packrat = import ./lib/packrat.nix;
      vw = import ./lib/valuewalk.nix;
      g = import ./grammar/flakelock.nix;
      schema = import ./examples/flakelock-valuewalk.nix;
      content = builtins.unsafeDiscardStringContext (builtins.readFile ./${fx});
      stringResult = (packrat.run { grammar = g.grammar; handlers = g.handlers; } 0 content).DOCUMENT;
      valueResult = (vw.run { grammar = schema; } (builtins.fromJSON content)).DOCUMENT;
    in
      if stringResult == packrat.NO_MATCH && valueResult == null then \"BOTH_REJECTED\"
      else if stringResult == packrat.NO_MATCH || valueResult == null then \"DISAGREE_ON_ACCEPT\"
      else if stringResult == valueResult then \"MATCH\"
      else \"MISMATCH\"
  " 2>&1)

  case "$result" in
    '"MATCH"')
      echo "OK   parity   $fx"
      ;;
    '"BOTH_REJECTED"')
      echo "SKIP parity   $fx (both grammars correctly reject: not flake.lock-shaped)"
      ;;
    *)
      echo "FAIL parity   $fx: $result"
      fail=1
      ;;
  esac
done

if [[ "$fail" -ne 0 ]]; then
  echo
  echo "verify-valuewalk-parity: FAILED"
  exit 1
fi

echo
echo "verify-valuewalk-parity: grammar/flakelock.nix and examples/flakelock-valuewalk.nix agree on every fixture"
