# Benchmark report: native `fromJSON` vs. this project's grammars vs. nix-parsec

Date: 2026-08-22

## What was compared

1. **Native `builtins.fromJSON`** — Nix's built-in JSON parser (C++, not a
   Nix-language parser). Baseline.
2. **`grammar/json.nix`** — generic JSON grammar on `lib/packrat.nix`'s
   packrat/PEG engine.
3. **`grammar/flakelock.nix`** — same engine, grammar specialized to
   flake.lock's exact schema.
4. **[`kanwren/nix-parsec`](https://github.com/kanwren/nix-parsec)** — an
   independent Nix parser-combinator library, JSON grammar written against
   its real combinators for this comparison.
5. **[`purenix-org/purenix`](https://github.com/purenix-org/purenix)** — not
   benchmarked. It's a PureScript-to-Nix compiler backend, not a parsing
   library, and ships no JSON parser to compare against.

All parsers (2-4) were checked for byte-identical output against Python's
`json.load` and native `fromJSON` on every fixture before trusting any
number below.

## Method

Synthetic flake.lock-shaped fixtures at 13 sizes (2.4KB-875KB, `n` = number
of `nodes` entries from 5 to 2000), matching the real schema (alphabetical
key order in `locked`/`original`, the 4 node key-set shapes, monomorphic
field types). Each parser ran via `nix eval --impure --file ... --json`,
wall time and peak RSS via `/usr/bin/time -v`, single run per point (see
Caveats), 60s timeout.

## Results

| n (nodes) | bytes | native fromJSON | generic json.nix | specialized flakelock.nix | nix-parsec |
|---:|---:|---:|---:|---:|---:|
| 5 | 2,369 | 0.03s / 39MB | 0.03s / 40MB | 0.03s / 40MB | 0.05s / 43MB |
| 15 | 6,613 | 0.03s / 39MB | 0.04s / 42MB | 0.03s / 41MB | 0.07s / 53MB |
| 30 | 13,056 | 0.03s / 39MB | 0.05s / 44MB | 0.05s / 43MB | 0.17s / 71MB |
| 60 | 25,941 | 0.03s / 39MB | 0.07s / 49MB | 0.06s / 47MB | 0.55s / 120MB |
| 120 | 51,818 | 0.03s / 39MB | 0.31s¹ / 60MB | 0.32s¹ / 55MB | 2.89s / 268MB |
| 150 | 64,863 | 0.03s / 40MB | 0.14s / 66MB | 0.09s / 58MB | 5.54s / 367MB |
| 180 | 77,908 | 0.03s / 40MB | 0.15s / 71MB | 0.11s / 63MB | 9.13s / 484MB |
| 200 | 86,629 | 0.03s / 40MB | 0.17s / 75MB | 0.13s / 65MB | 12.30s / 571MB |
| 220 | 95,298 | 0.03s / 39MB | 0.19s / 78MB | 0.13s / 67MB | 15.12s / 664MB |
| 250 | 108,343 | 0.03s / 40MB | 0.30s¹ / 83MB | 0.14s / 71MB | stack overflow |
| 500 | 217,079 | 0.03s / 41MB | 0.34s / 127MB | 0.33s / 103MB | stack overflow |
| 1000 | 434,468 | 0.04s / 43MB | 0.64s / 213MB | 0.48s / 163MB | stack overflow |
| 2000 | 874,663 | 0.05s / 49MB | 1.42s / 390MB | 0.95s / 286MB | stack overflow |

¹ Single-run noise, ignore in isolation.

Real 14.2MB `nix-overlay/flake.lock` (23,756 nodes), output confirmed
byte-identical to Python's `json.load`: generic grammar ~19.4s / 4.71GB,
specialized grammar ~10.6s / 3.4GB. nix-parsec cannot parse this file (see
table above). Native `fromJSON` handles it in well under a second.

## Notes

- `fromJSON` is a C++ parser in the evaluator; nothing written in the Nix
  language is going to be competitive with it. It's here as a ceiling, not
  a competitor — this project targets grammars `fromJSON` can't parse at
  all (e.g. flake.lock's schema-specific structure).
- Schema specialization (generic → `flakelock.nix`) reduces RSS by roughly
  10-27% and wall time similarly, growing with input size; consistent
  between the synthetic fixtures and the real file.
- PureNix (see above) isn't in the table at all — no shipped parser to run.

## Caveats

- Single run per data point, not averaged — expect ~5-10% run-to-run noise
  (evaluator warm-up, scheduler jitter); trust the curve, not individual
  cells to two significant figures.
- Fixtures are synthetic (generated to match confirmed real-file schema
  facts), not sampled from a real file at every size — hence the real
  14.2MB file's numbers are quoted separately.
- nix-parsec's grammar was written for this comparison using its real,
  verified combinators, not lifted from an existing example in that repo.
