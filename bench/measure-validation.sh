#!/usr/bin/env bash
# Benchmark: native fromJSON, native fromJSON + lib.types.json.check
# (structural validation, the nixpkgs equivalent of arcnmx/nixexprs's
# lib/json.nix -- that file turned out to define only lib.types-style
# validators, no parser, so this is what a fair comparison against it
# actually looks like), generic grammar/json.nix, and the schema-specialized
# grammar/flakelock.nix, across synthetic flake.lock-shaped fixtures plus
# the real lock-large.json.
#
# Usage: ./bench/measure-validation.sh  (cd's to repo root itself).
# Requires bench/fixtures/synth-*.json (bench/make-synthetic-fixtures.py)
# and a nixpkgs channel with lib.types.json (added in NixOS 24.05+).
set -uo pipefail
cd "$(dirname "$0")/.."

OUT="bench/validation-results.txt"
: > "$OUT"

fixtures=(
  "5:bench/fixtures/synth-5.json"
  "15:bench/fixtures/synth-15.json"
  "30:bench/fixtures/synth-30.json"
  "60:bench/fixtures/synth-60.json"
  "120:bench/fixtures/synth-120.json"
  "250:bench/fixtures/synth-250.json"
  "500:bench/fixtures/synth-500.json"
  "1000:bench/fixtures/synth-1000.json"
  "2000:bench/fixtures/synth-2000.json"
  "real(lock-large):lock-large.json"
)

variants=(
  "fromJSON"
  "fromJSON+typesCheck"
  "genericGrammar"
  "flakelockGrammar"
)

exprFor() {
  local variant="$1" path="$2"
  case "$variant" in
    fromJSON)
      echo "let content = builtins.unsafeDiscardStringContext (builtins.readFile ./${path}); in builtins.fromJSON content"
      ;;
    fromJSON+typesCheck)
      echo "let pkgs = import <nixpkgs> {}; lib = pkgs.lib; content = builtins.unsafeDiscardStringContext (builtins.readFile ./${path}); v = builtins.fromJSON content; in lib.types.json.check v"
      ;;
    genericGrammar)
      echo "let packrat = import ./lib/packrat.nix; j = import ./grammar/json.nix; content = builtins.unsafeDiscardStringContext (builtins.readFile ./${path}); in (packrat.run { grammar = j.grammar; handlers = j.handlers; } 0 content).X"
      ;;
    flakelockGrammar)
      echo "let packrat = import ./lib/packrat.nix; g = import ./grammar/flakelock.nix; content = builtins.unsafeDiscardStringContext (builtins.readFile ./${path}); in (packrat.run { grammar = g.grammar; handlers = g.handlers; } 0 content).DOCUMENT"
      ;;
  esac
}

printf '%-20s %-20s %10s %12s %6s\n' "fixture" "variant" "wall_s" "max_rss_kb" "exit" | tee -a "$OUT"
printf '%s\n' "--------------------------------------------------------------------------------" | tee -a "$OUT"

for fx in "${fixtures[@]}"; do
  label="${fx%%:*}"
  path="${fx#*:}"
  for vname in "${variants[@]}"; do
    expr="$(exprFor "$vname" "$path")"

    tmp_time=$(mktemp)
    start=$(date +%s.%N)
    /usr/bin/time -v nix eval --impure --expr "$expr" --json \
      > /tmp/bench-out.json 2> "$tmp_time"
    ec=$?
    end=$(date +%s.%N)
    wall=$(echo "$end - $start" | bc)

    rss=$(grep 'Maximum resident set size' "$tmp_time" | awk -F': ' '{print $2}')
    rss=${rss:-NA}

    printf '%-20s %-20s %10s %12s %6s\n' "$label" "$vname" "$wall" "$rss" "$ec" | tee -a "$OUT"

    {
      echo "=== fixture=$label variant=$vname ==="
      cat "$tmp_time"
      echo
    } >> "bench/validation-results-raw.txt"

    rm -f "$tmp_time"
  done
done

echo "Done. See bench/validation-results.txt (summary) and bench/validation-results-raw.txt (full /usr/bin/time -v output)."
