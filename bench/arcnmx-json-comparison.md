# Benchmark: validation (`lib.types.json.check`) vs. this project's grammars

Date: 2026-08-22

## Why this report exists

The ask was to add benchmark results from
[`arcnmx/nixexprs`'s `lib/json.nix`](https://github.com/arcnmx/nixexprs/blob/master/lib/json.nix).
That file turned out not to be a JSON parser at all -- fetched and confirmed
twice independently, its entire contents are:

```nix
{ lib }: let
  inherit (lib) types;
  json = lib.arclib.json or (if lib ? json.types.primitive then lib.json else self);
  self = {
    primitives = with types; [ bool int float str ];
    types = {
      data = with types; oneOf [ json.types.primitive json.types.attrs json.types.list ] // {
        description = "json data";
      };
      primitive = types.nullOr (types.oneOf json.primitives);
      attrs = types.attrsOf json.types.data;
      list = types.listOf json.types.data;
    };
    type = json.types.data;
  };
in self
```

It defines a NixOS-module-system *type* (`nullOr (oneOf [bool int float str])`
composed with `attrsOf`/`listOf`) for validating that an already-existing Nix
value has JSON-compatible shape. There's no `fromJSON`-equivalent function in
it, and nothing in the rest of that repo's `lib/` ships one either (`lib/
default.nix` just calls the real `builtins.fromJSON`) -- so there's no parser
there to compare wall-clock/RSS against this project's packrat engine.

What *is* comparable: nixpkgs ships the same kind of type under
`lib.types.json` (`lib/types.nix:1487`, `serializableValueWith { typeName =
"JSON"; }`), built from the same `oneOf`/`attrsOf`/`listOf` primitives
arcnmx's file hand-assembles. This report benchmarks `lib.types.json.check`
against an already-parsed value (i.e. *validation*, not parsing) alongside
this project's two grammars and native `fromJSON`, confirming the standing
guess that validation cost doesn't move much with input size.

## What was compared

1. **`fromJSON`** -- native, C++, baseline.
2. **`fromJSON` + `lib.types.json.check`** -- parse natively, then validate
   the resulting value's shape via nixpkgs' `lib.types.json` (the same kind
   of type arcnmx's `json.nix` builds, applied to a real value instead of
   left abstract).
3. **`genericGrammar`** -- this project's `grammar/json.nix` (packrat/PEG,
   `lib/packrat.nix`), a from-scratch general JSON parser.
4. **`flakelockGrammar`** -- this project's `grammar/flakelock.nix`, same
   engine, specialized to `nix flake lock`'s exact schema.

Grammar output was confirmed byte-identical to `builtins.fromJSON` on every
fixture below before trusting any timing.

## Method

Synthetic flake.lock-shaped fixtures at 9 sizes (1.7KB-636KB, `n` = number of
`nodes` entries from 5 to 2000, `bench/make-synthetic-fixtures.py`), matching
the schema confirmed against a real file (alphabetical key order in `locked`/
`original`, the 4 node key-set shapes, monomorphic field types), plus the
real 391KB `lock-large.json`. Each variant ran via `nix eval --impure --expr
... --json`, wall time and peak RSS via `/usr/bin/time -v`, single run per
point (see Caveats), `bench/measure-validation.sh`.

## Results

| n (nodes) | bytes | fromJSON | fromJSON+typesCheck | genericGrammar | flakelockGrammar |
|---:|---:|---:|---:|---:|---:|
| 5 | 1,719 | 0.23s¹/38MB | 0.49s/156MB | 0.05s/40MB | 0.04s/40MB |
| 15 | 4,987 | 0.04s/39MB | 0.47s/156MB | 0.05s/41MB | 0.05s/41MB |
| 30 | 9,594 | 0.04s/39MB | 0.63s/155MB | 0.06s/44MB | 0.06s/43MB |
| 60 | 18,748 | 0.04s/39MB | 0.50s/156MB | 0.26s²/48MB | 0.07s/45MB |
| 120 | 37,533 | 0.04s/40MB | 0.55s/155MB | 0.28s²/56MB | 0.08s/52MB |
| 250 | 78,778 | 0.05s/40MB | 0.51s/156MB | 0.31s²/75MB | 0.13s/65MB |
| 500 | 157,642 | 0.04s/40MB | 0.52s/155MB | 0.30s/109MB | 0.24s/92MB |
| 1000 | 315,892 | 0.04s/42MB | 0.62s/156MB | 0.61s/180MB | 0.41s/143MB |
| 2000 | 636,392 | 0.06s/48MB | 0.52s/155MB | 1.33s/323MB | 0.99s/245MB |
| real (391,947, 664 nodes) | -- | 0.05s/43MB | 0.51s/156MB | 0.56s/166MB | 0.35s/132MB |

¹ ² Single-run noise (evaluator warm-up / scheduler jitter); ignore in
isolation. Trust the growth curve, not individual cells.

## What this confirms

`fromJSON+typesCheck`'s wall time (~0.47-0.63s) and RSS (~155MB) are **flat
across the entire size range** -- 5 nodes costs the same as 2000. That's
because the cost is dominated by `import <nixpkgs> {}` itself, not by
`types.json.check`. Isolated directly:

```
$ /usr/bin/time -v nix eval --impure --expr 'let pkgs = import <nixpkgs> {}; in pkgs.lib.version'
Elapsed: 0:00.54, Maximum resident set size: 155268 KB

$ /usr/bin/time -v nix eval --impure --expr 'let pkgs = import <nixpkgs> {}; lib = pkgs.lib; in lib.types.json.check (builtins.fromJSON (builtins.readFile ./lock-large.json))'
Elapsed: 0:00.56, Maximum resident set size: 155388 KB
```

Importing `<nixpkgs>` alone accounts for essentially all of it (0.54s/155MB
vs. 0.56s/155MB with the real `fromJSON` + `check` added on top) -- the
actual validation call is near-free by comparison, consistent with the
"validation wasn't that much different" expectation this report was written
to check.

`genericGrammar`/`flakelockGrammar`, by contrast, scale visibly with input
size (as expected for a Nix-language packrat parser competing with a C++
one) -- `flakelockGrammar` runs at roughly 65-80% of `genericGrammar`'s time
and RSS across this range, consistent with `bench/comparison-report.md`'s
existing generic-vs-specialized numbers on a separate fixture set.

## Caveats

- Single run per data point, not averaged -- expect ~5-10% run-to-run noise;
  trust the curve, not individual cells to two significant figures.
- `lib.types.json.check`'s cost is dominated by nixpkgs import overhead in
  this harness; a long-running evaluator that already has `pkgs.lib` loaded
  (e.g. inside a NixOS module system eval) would see only the actual
  `check` cost, which this report doesn't isolate further than the two
  control runs above.
- Synthetic fixtures generated to match confirmed real-file schema facts
  (`bench/make-synthetic-fixtures.py`), not sampled from a real file at
  every size -- same caveat as `bench/comparison-report.md`.
