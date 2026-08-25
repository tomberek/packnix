#!/usr/bin/env bash
# Phase 3 measurement harness: real /usr/bin/time -v RSS + wall-clock
# numbers for the no-cut vs cut-enabled JSON grammar, over truncated
# prefixes of synth-2000.json. Does NOT assume Mizushima et al.'s "mostly
# constant space" result transfers to Nix's evaluator (Boehm GC, one-shot
# CLI process, thunk-graph memory model rather than Haskell's) -- it
# actually runs both variants and records what happens.
#
# Usage: ./measure.sh   (from the bench/ directory, or anywhere -- cd's to
# its own directory first). Requires fixtures/ to already be populated by
# make-fixtures.sh. Writes/overwrites results.txt.
set -uo pipefail
cd "$(dirname "$0")/.."

OUT="bench/results.txt"
: > "$OUT"

fixtures=(
  "50000:bench/fixtures/fixture-50000.json"
  "150000:bench/fixtures/fixture-150000.json"
  "300000:bench/fixtures/fixture-300000.json"
  "full(636392):bench/fixtures/fixture-full.json"
)
variants=("noCut:grammarNoCut:handlersNoCut" "cut:grammar:handlers")

printf '%-16s %-6s %10s %10s %14s\n' "fixture" "variant" "wall_s" "max_rss_kb" "exit" | tee -a "$OUT"
printf '%s\n' "----------------------------------------------------------------" | tee -a "$OUT"

for fx in "${fixtures[@]}"; do
  label="${fx%%:*}"
  path="${fx#*:}"
  for v in "${variants[@]}"; do
    vname="${v%%:*}"
    rest="${v#*:}"
    gname="${rest%%:*}"
    hname="${rest#*:}"

    expr="let packrat = import ./lib/packrat.nix; j = import ./grammar/json.nix; content = builtins.unsafeDiscardStringContext (builtins.readFile ./${path}); in (packrat.run { grammar = j.${gname}; handlers = j.${hname}; } 0 content).X"

    tmp_time=$(mktemp)
    start=$(date +%s.%N)
    /usr/bin/time -v nix eval --impure --expr "$expr" --json \
      > /tmp/bench-out.json 2> "$tmp_time"
    ec=$?
    end=$(date +%s.%N)
    wall=$(echo "$end - $start" | bc)

    rss=$(grep 'Maximum resident set size' "$tmp_time" | awk -F': ' '{print $2}')
    rss=${rss:-NA}

    printf '%-16s %-6s %10s %10s %14s\n' "$label" "$vname" "$wall" "$rss" "$ec" | tee -a "$OUT"

    # Keep raw /usr/bin/time output too, for auditability.
    {
      echo "=== fixture=$label variant=$vname ==="
      cat "$tmp_time"
      echo
    } >> "bench/results-raw.txt"

    rm -f "$tmp_time"
  done
done

echo "Done. See bench/results.txt (summary) and bench/results-raw.txt (full /usr/bin/time -v output)."
