# packnix

A packrat/PEG parsing engine written entirely in the Nix expression
language, plus a few grammars built on it — generic JSON, JSON specialized
for `nix flake lock`'s exact output schema, a real (subset of) YAML, TSV,
generic ATerm plus a grammar specialized to Nix's own `.drv` file format,
Python's PEP 508 dependency-specification format and Poetry's version-
constraint syntax, Ruby's Bundler `Gemfile.lock` and (a
group-membership-focused subset of) `Gemfile` formats, and Yarn classic's
`yarn.lock` format.

Built from Ford's ["Packrat Parsing: Simple, Powerful, Lazy, Linear Time"](https://bford.info/pub/lang/packrat-icfp02/)
and Mizushima et al.'s ["Packrat Parsers Can Handle Practical Grammars in
Mostly Constant Space"](https://dl.acm.org/doi/10.1145/1806672.1806679) (the
source of the `cutSeq`/`↑` operator below).

## Quick example

Parsing TSV (tab-separated values) with `grammar/tsv.nix`:

```nix
let
  packrat = import ./lib/packrat.nix;
  tsv = import ./grammar/tsv.nix;
in
(packrat.run { grammar = tsv.grammar; handlers = tsv.handlers; } 0
  (builtins.readFile ./data/example.tsv)).DOCUMENT
```

```console
$ nix eval --impure --expr '
    let packrat = import ./lib/packrat.nix; tsv = import ./grammar/tsv.nix;
    in (packrat.run { grammar = tsv.grammar; handlers = tsv.handlers; } 0
         (builtins.readFile ./data/example.tsv)).DOCUMENT' --json
[["name","type","ref"],["nixpkgs","github","nixpkgs-unstable"],["flake-utils","github","main"]]
```

`DOCUMENT` is a list of rows, each row a list of field strings. See
`grammar/tsv.nix` for the ~30-line grammar itself, and the [Grammar
DSL](#grammar-dsl) and [Usage](#usage) sections below for how to write your
own or use the JSON/flake.lock grammars this project also ships.

## Why

Nix has a native JSON parser (`builtins.fromJSON`, in C++) — this isn't a
faster way to parse JSON. The point is a general parsing *library* for
grammars `fromJSON` can't help with: anything with its own syntax you'd
otherwise hand-roll with `builtins.match`/`substring`/recursion. JSON is the
running example because it's easy to verify and easy to compare against
other Nix parsing libraries.

## Layout

| Path | What |
|---|---|
| `lib/packrat.nix` | The engine: `mkCompile`, `buildDerivs`, `run`. Everything else is built on this. |
| `grammar/json.nix` | A generic, from-scratch JSON grammar (`grammar`/`grammarNoCut` + cut and non-cut variants). |
| `grammar/flakelock.nix` | A grammar specialized to `nix flake lock`'s exact schema — see below. |
| `grammar/tsv.nix` | A small TSV (tab-separated values) grammar — see the quick example above. |
| `grammar/yaml.nix` | A real YAML subset: block mappings/sequences nested by indentation, plain/quoted scalars, flow collections, comments. `mkYamlGrammar { indentStep; maxDepth; }` generates the grammar; see its header for scope limits (no anchors/tags/multi-doc/block-scalars, fixed indent step, bounded depth). |
| `grammar/gemfile-lock.nix` | Ruby Bundler's `Gemfile.lock` format — see below for why this one has a real nixpkgs use case. |
| `grammar/gemfile.nix` | A real (subset of) Ruby Bundler's `Gemfile` format — NOT the lockfile; recovers Bundler *group* membership per gem (`group :x do...end` blocks, inline `group:`/`groups:` kwargs, `if`/`unless`/`else` wrapping), the one fact `Gemfile.lock` never records. See its header for exact scope. |
| `grammar/yarn-lock.nix` | Yarn classic's `yarn.lock` ("yarn lockfile v1") format — see below for why this one has a real nixpkgs use case. Yarn Berry (v2+) lockfiles are a different, YAML-based format and out of scope. |
| `grammar/aterm.nix` | A generic ATerm (Annotated Term) grammar — the format Nix's own `.drv` files are written in, among other uses (ASF+SDF Meta-Environment, Stratego/XT). Covers all six real term kinds (int, real, appl, list, tuple, placeholder) plus annotations; verified against 500 real `.drv` files from a live `/nix/store`. |
| `grammar/drv.nix` | A grammar specialized to Nix's `.drv` file format's exact shape (`Derive(outputs, inputDrvs, inputSrcs, system, builder, args, env)`, always exactly 7 fields) — semantically decodes each field (e.g. a fixed-output derivation's `hashAlgo`'s `"r:"` prefix into a `recursive` flag) rather than returning a generic ATerm tree. |
| `grammar/pep508.nix` | Python's PEP 508 dependency-specification format (`requests (>=2.0,<3.0) ; python_version >= "3.6" and sys_platform == "linux"`) — the same format nixpkgs' `poetry2nix` parses today via ~180 lines of hand-rolled character-walking with no real `and`/`or` precedence. Transcribed directly from PEP 508's own formal grammar (restructured to avoid left recursion); verified against 2126 real, distinct `Requires-Dist` specifiers. |
| `grammar/poetry-semver.nix` | Poetry's version-constraint syntax (`^1.2.3`, `~1.2`, `1.*`, `~2.7 \|\| ^3.5`) — parses AND evaluates (`mkSatisfies packrat version constraint`). Fixes several real, demonstrated bugs found in nixpkgs' `poetry2nix/semver.nix`/`lib.nix` while building this (wrong caret/tilde upper bounds that accept clearly-incompatible major versions, `!=X.Y.*` not actually excluding anything, bare versions/wildcards throwing instead of parsing). Verified against 65 real `python-versions`/`python = "..."` constraint strings from real `poetry.lock`/`pyproject.toml` files. See its header for the specific bugs and how they were confirmed. |
| `examples/json-simple.nix` | A plain, unoptimized JSON grammar — every construct gets its own named rule, no attention paid to allocation. Good starting point for reading/writing your own grammar. |
| `examples/json-optimized.nix` | Re-exports `grammar/json.nix`, annotated with what changed vs. `json-simple.nix` and why (rule inlining via the `action` combinator, fewer redundant whitespace scans, etc). |
| `examples/flakelock-specialized.nix` | Re-exports `grammar/flakelock.nix`, annotated with the schema-specialization technique and measured wins. |
| `examples/gemfile-lock-checksums.nix` | Extracts `{ <gem name> = <sha256>; }` from a `Gemfile.lock`'s `CHECKSUMS` section — the piece a `bundlerEnv` replacement would need. See below. |
| `lib/valuewalk.nix` | A schema-validation engine over an already-parsed value tree (from `fromJSON`/`fromTOML`), not string positions — see [Value-tree validation and generation](#value-tree-validation-and-generation) below. |
| `lib/generate.nix` | Generates a sample value/string that a `lib/valuewalk.nix` schema or `lib/packrat.nix` grammar would accept — the reverse direction of validation, deterministically seeded. |
| `lib/regex-generate.nix` | Inverts a POSIX ERE pattern into a sample string it would accept; backs `generate`'s automatic `pattern`/`regex` synthesis. |
| `lib/roundtrip.nix` | Generates N samples for a grammar/schema and confirms its own parser accepts every one — the fixpoint gate `verify-roundtrip.sh` runs. |
| `examples/flakelock-valuewalk.nix` | The `grammar/flakelock.nix` schema rewritten against `lib/valuewalk.nix`, over `builtins.fromJSON`'s output instead of string positions. |
| `schemas/cargo-lock.nix` | A `lib/valuewalk.nix` schema for Rust's `Cargo.lock` (v3/v4) over `builtins.fromTOML`'s output — no `lib/packrat.nix` grammar counterpart at all, since Cargo.lock is plain TOML with nothing `fromTOML` can't already parse. See below for why this one has a real nixpkgs use case. |
| `examples/cargo-lock-checksums.nix` | Extracts `{ "<crate>-<version>" = <sha256>; ... }` from a `Cargo.lock`'s `checksum` fields — the piece `importCargoLock` needs. See below. |
| `schemas/poetry-lock.nix` | A `lib/valuewalk.nix` schema for Poetry's `poetry.lock`, over `builtins.fromTOML`'s output — same no-grammar reasoning as `cargo-lock.nix`. Handles both real hash-storage layouts confirmed against a real corpus (per-package `files`, and the older top-level `metadata.files`) — see its header for the corpus/source evidence, including why the oldest `metadata.hashes` generation is deliberately out of scope (no real sample exists to verify against). See below for why this one has a real nixpkgs use case. |
| `examples/poetry-lock-checksums.nix` | Extracts `{ "<package>-<version>" = [<sha256> ...]; ... }` from a `poetry.lock`, checking both hash-storage locations. See below. |
| `schemas/package-lock.nix` | A `lib/valuewalk.nix` schema for npm's `package-lock.json` (lockfileVersion 2/3), over `builtins.fromJSON`'s output — no `lib/packrat.nix` grammar counterpart, since `package-lock.json` is plain JSON with nothing bespoke `fromJSON` can't already parse. See below for why this one has a real nixpkgs use case. |
| `examples/package-lock-checksums.nix` | Extracts `{ "<node_modules path>" = { url; hash; }; ... }` from a `package-lock.json`'s `resolved`/`integrity` pairs — the piece `importNpmLock` needs. See below. |
| `schemas/uv-lock.nix` | A `lib/valuewalk.nix` schema for uv's `uv.lock` (schema version 1), over `builtins.fromTOML`'s output — no `lib/packrat.nix` grammar counterpart, same reasoning as `schemas/package-lock.nix`. See below for why this one has a real use case even though nixpkgs itself doesn't consume `uv.lock`. |
| `examples/uv-lock-checksums.nix` | Extracts `{ "<name>-<version>" = [ { url; hash; } ... ]; ... }` from a `uv.lock`'s `sdist`/`wheels` entries — the piece `uv2nix` needs. See below. |
| `default.nix` | Thin wrapper: `pack ./somefile.json` parses a file with the JSON grammar. |
| `tests.nix` | Standalone combinator test suite (cut-operator semantics, star/regex edge cases, etc). |
| `bench/` | Fixture generators, measurement scripts, `comparison-report.md`, `arcnmx-json-comparison.md`. |

## Grammar DSL

```nix
"Name"                 # nonterminal reference (bare "" = epsilon)
{ lit = "...";  }       # literal string match
{ range = [a b]; }      # single-char range match
{ regex = "...";  }     # POSIX ERE match (via builtins.match) at point
[ e1 e2 ... ]           # sequence
{ choice = [e1 ...]; }  # ordered choice
{ star = e; }           # e*
{ plus = e; }           # e+
{ opt = e; }            # e?
{ and = e; }            # &e   positive lookahead, consumes nothing
{ not = e; }            # !e   negative lookahead, consumes nothing
{ eof = { }; }          # succeeds (consuming nothing) iff no input remains
{ cutSeq = [e1 e2]; }   # e1 ↑ e2 -- valid only as a choice branch or star body
{ action = { e; f; }; } # e, with f applied to its matched value on success
```

A grammar is an attrset mapping nonterminal names to expressions; `handlers`
maps the same names to a `rawValue -> value` transform (defaults to
identity). `action` lets a transform travel with an *inlined* sub-expression
instead of requiring a named rule — see `examples/json-optimized.nix` (fewer
fields per `Derivs` node = less allocation). Trade-off: it loses
per-position memoization sharing for the inlined expression, so it's only
safe when that expression can't be reached from two call sites at the same
input position.

## Usage

Parse a file with the built-in JSON grammar:

```console
$ ./run.sh                      # nix eval --file default.nix --apply 'x: x ./data/lock.json' --json | jq
```

Or directly:

```nix
let
  packrat = import ./lib/packrat.nix;
  json = import ./grammar/json.nix;
in
packrat.run { grammar = json.grammar; handlers = json.handlers; } 0
  (builtins.readFile ./data/lock.json)
```

`run` returns `{ <NonterminalName> = value; ... }` for every rule in the
grammar, evaluated from position `count` (usually `0`); a rule that failed
to match at that position is `packrat.NO_MATCH` (a dedicated sentinel, not
`false` — a rule can legitimately match a real `false`/`null` value via
the `{ json = {}; }`/`{ toml = {}; }` combinator, which hands the rest of
the input to `builtins.fromJSON`/`fromTOML` — see `lib/packrat.nix`'s own
header comment — so `false` alone can't distinguish a real match from a
failure).

Using the flake.lock-specialized grammar instead (only accepts documents
matching that exact schema — see next section):

```nix
let
  packrat = import ./lib/packrat.nix;
  g = import ./grammar/flakelock.nix;
in
(packrat.run { grammar = g.grammar; handlers = g.handlers; } 0
  (builtins.readFile ./bench/fixtures/synth-2000.json)).DOCUMENT
```

Writing your own grammar: start from `examples/json-simple.nix` as a
template (every rule named, no optimization), then read
`examples/json-optimized.nix` and `examples/flakelock-specialized.nix` for
what to change once it works and you care about performance.

## The specialized grammar

`grammar/flakelock.nix` isn't a general JSON parser — it accepts *only*
documents matching `nix flake lock`'s exact output shape, confirmed by
inspecting a real 14.2MB flake.lock:

- top-level is always exactly `{"nodes": {...}, "root": "...", "version": N}`, in that order
- every `nodes.*` entry is one of exactly 4 fixed key-sets
- every key within `locked`/`original` appears in strict alphabetical order, with zero exceptions
- every field name maps to exactly one JSON type across the whole file
- no string in the file contains `"` or `\`, so no escape handling is needed

Knowing all of that ahead of time turns every object into a fixed, linear
sequence of `opt`-wrapped fields tried once each in a known order, instead
of a generic "parse a key, dispatch on its name, loop until `}`" parser —
no backtracking over key identity or order. A differently-shaped
flake.lock, or arbitrary JSON, correctly fails to parse rather than
silently mis-parsing; that inflexibility is the trade for the speed.

## Value-tree validation and generation

`lib/packrat.nix`'s `{ json = {}; }`/`{ toml = {}; }` combinator (see that
file's header) hands a substring to the native `fromJSON`/`fromTOML` for
speed, but that alone gives no structural validation — no "does this
object have exactly these fields, in these types" the way a hand-written
packrat grammar gets for free from its rule-by-rule shape. `lib/valuewalk.nix`
is that validation layer, applied to the *already-parsed* value tree
instead of re-deriving it from text: on a 636KB/2000-node synthetic
flake.lock, it's ~8x faster and ~4x less memory than `grammar/flakelock.nix`
on the same fixture, confirmed byte-identical output (see
`examples/flakelock-valuewalk.nix`, the flake.lock schema rewritten
against it). Its schema DSL is deliberately parallel to the [Grammar
DSL](#grammar-dsl) above (`{ string = {}; }`, `{ attrs = { fields;
optional; closed; }; }`, `{ listOf = s; }`, etc., plus shared forms like
`choice`/`action`/`"Name"`-reference) but matches VALUES, not string
positions — see its header comment for the full form list and its
`null`-as-failure-sentinel rationale.

`lib/generate.nix` runs either engine's DSL in reverse: given a
`lib/packrat.nix` grammar or `lib/valuewalk.nix` schema, it produces a
sample string/value that DSL would accept, instead of validating one that
already exists. Nix has no RNG at all (no `builtins.random`, no
`builtins.currentTime`), so generation is deterministically SEEDED
instead of random: every choice derives from `builtins.hashString "sha256"
seed`, and every recursive call derives a fresh child seed, so `generate
schema seed` is a pure function — same schema + same seed always
produces the same value, which makes a failure reproducible instead of
flaky. `{ pattern = "..."; }`/`{ regex = "..."; }` leaves are synthesized
automatically by `lib/regex-generate.nix` (a POSIX ERE parser + AST-
walking generator), with an explicit `patternGenerators` override
available as a fallback; `and`/`not` lookahead has no general generation
strategy and is thrown as an explicit error rather than guessed at — see
`lib/generate.nix`'s header for exactly what's covered and why.

`lib/roundtrip.nix` wires the two together into a fixpoint check: generate
N samples for a grammar/schema, feed each back through that SAME
grammar/schema's own parser, and confirm every one is *accepted*. This is
narrower than "generated value equals the original" — there is no
original here, only "does the parser accept what was generated for it".
`./verify-roundtrip.sh` runs this in CI at N=50 for `grammar/tsv.nix`,
`grammar/json.nix`, `examples/flakelock-valuewalk.nix`, `grammar/aterm.nix`,
`grammar/drv.nix`, `grammar/pep508.nix`, `grammar/poetry-semver.nix`,
`grammar/gemfile-lock.nix`, and `grammar/yarn-lock.nix`. Only
`grammar/gemfile.nix` and `grammar/yaml.nix` remain out of scope for THIS
gate — both use `not`/`and` for real structural disambiguation (excluding
reserved words, asserting a following character) that `lib/generate.nix`
has no general synthesis strategy for. They still get their own hand-
written accept case (real corpus content where a fixture already
existed) plus a reject case exercising a real failure mode specific to
that format in `tests.nix`, so neither ships with zero automated
coverage.

## Gemfile.lock: a real nixpkgs use case

Today, turning a `Gemfile.lock` into the `gemset.nix` that `bundlerEnv`
needs (see `pkgs/development/ruby-modules/bundled-common` in nixpkgs)
requires running [`bundix`](https://github.com/nix-community/bundix), an
external Ruby tool that needs network access (or `nix-prefetch-git`) to
compute each gem's fetch hash. But Bundler ≥2.7 lockfiles have a
`CHECKSUMS` section with a hex sha256 per gem — and that hash is *exactly*
what `bundix` ends up storing, just hex instead of Nix's base32 encoding.
Verified against a real nixpkgs package's paired `Gemfile.lock`/`gemset.nix`:

```console
$ nix hash convert --to base32 --hash-algo sha256 \
    3b9270d8e19f0afb534b11c52f439937dc30028adcbbae2b244f3383ce75de4b
0jyyfp786csg4hmsxfywi8131p1pk51jzi8i9d9zn2lzw7c714iv
```

That's the exact string `gemset.nix` stores for that gem (`actionmailer`).
So for any lockfile with a `CHECKSUMS` section, the dependency graph *and*
every gem's fetch hash are already sitting in the file — `grammar/gemfile-lock.nix`
plus a small base32 re-encode (not reimplemented here — see
`examples/gemfile-lock-checksums.nix`) is enough to skip `bundix` and
network access entirely for those lockfiles.

Correctness: cross-validated against an independent Python reference
parser across 134 real `Gemfile.lock` files pulled from a nixpkgs
checkout — every field (multiple GEM/GIT/PATH blocks, platform-qualified
spec versions, `!`-pinned/multi-constraint dependencies, CHECKSUMS, RUBY
VERSION) byte/value-identical between the two. Deliberately out of
scope: Bundler `PLUGIN SOURCES` (not seen in the corpus at all).

## yarn.lock: a real nixpkgs use case

Today, resolving a `yarn.lock` into Nix-consumable fetch info (as
`yarn2nix`/`mkYarnPackage` need — see
`pkgs/development/tools/yarn2nix-moretea` in nixpkgs) requires running an
external Node-based tool, or a per-package network prefetch, to compute
each package's fetch hash. But a Yarn classic (`yarn lockfile v1`)
lockfile already embeds a `resolved` URL *and* an `integrity` value (SRI
format, `sha512-<base64>` or `sha1-<base64>`) per package inline — the
same "the hash is already sitting in the file" situation as
`Gemfile.lock`'s `CHECKSUMS` section above, just SRI-base64 instead of
hex. `grammar/yarn-lock.nix` reads the whole dependency graph and every
package's fetch info directly out of the lockfile — no `yarn2nix`, no
network, no external Node interpreter needed at eval time. (Converting
the SRI base64 value to the base32 `nix-hash` format a fixed-output
derivation wants is a separate, small step, not reimplemented here — same
category as `examples/gemfile-lock-checksums.nix`'s hex-to-base32
conversion for `Gemfile.lock`.)

Correctness: cross-validated against an independent Python reference
parser across 15 real `yarn.lock` files (2,395 entries total) — scoped
package names, multi-spec lines (both bare and double-quoted forms),
`dependencies:`/`optionalDependencies:` blocks, and every
`version`/`resolved`/`integrity` field byte/value-identical between the
two. A Yarn Berry (v2+) lockfile — a different, YAML-based format
entirely — correctly fails to parse rather than silently mis-parsing.

## Cargo.lock, poetry.lock, package-lock.json, uv.lock: real fetch-hash use cases

Four ecosystems' package managers write a lockfile that already contains
every fetch hash their Nix build tooling would otherwise need to
recompute over the network. Each `schemas/*.nix` here is a
`lib/valuewalk.nix` schema over `fromTOML`/`fromJSON`'s output, not a
`lib/packrat.nix` grammar — none of the four formats has any syntax a
native parser can't already handle, so a from-scratch packrat grammar
would just re-parse text for no benefit (the same reasoning this repo's
own [Why](#why) section gives for not competing with `builtins.fromJSON`
on plain JSON). Each `examples/*-checksums.nix` extracts the hashes; each
schema's own header has the full field-presence breakdown.

| Lockfile | Consumer | Hash field → Nix `hash`/`sha256` | Corpus |
|---|---|---|---|
| `Cargo.lock` | nixpkgs' `importCargoLock` (`pkgs/build-support/rust/import-cargo-lock.nix`) | `checksum`, used as-is (`sha256 = checksum;`, no re-encoding) | 100 real files from a nixpkgs checkout (149B–225KB) |
| `poetry.lock` | poetry2nix's `fetchFromPypi` (`pkgs/development/tools/poetry2nix/poetry2nix/lib.nix`) | `"sha256:<hex>"`, strip the prefix (not a base32 re-encode) | 5 real files (nixpkgs' `rmfuse`/`nixops`, poetry2nix's own vendored copy, two more from live checkouts) |
| `package-lock.json` | nixpkgs' `importNpmLock` (`pkgs/build-support/node/import-npm-lock/default.nix`) | `integrity`, already SRI (`sha512-<base64>`), zero conversion | 43 real files from a nixpkgs checkout |
| `uv.lock` | the external [`uv2nix`](https://github.com/pyproject-nix/uv2nix) project's `lib/build.nix` — nixpkgs itself has no `uv.lock` consumer | `"sha256:<hex>"` on `sdist`/each `wheels[]` entry, zero conversion | uv2nix's own public MIT-licensed test fixtures (`lib/fixtures/*/uv.lock`) |

Format-specific complications each schema/example handles:

- **Cargo.lock**: `checksum` is present *iff* `source` is
  `registry+`/`sparse+` (git-sourced/workspace packages never have one,
  confirmed zero exceptions in the corpus); a `dependencies` entry is a
  bare crate name, or `"name version"` when 2+ versions of that name are
  locked at once (one corpus file locks `bitflags` at both 1.3.2 and
  2.4.1); one corpus file has a `[[patch.unused]]` section (nixpkgs' own
  Rust sysroot lockfile).
- **poetry.lock**: a real hash can live in EITHER of two places
  depending on `lock-version` — a package's own `files` field (current)
  or a top-level `metadata.files.<name>` table (older) — confirmed via
  Poetry's own `locker.py` (`src/poetry/packages/locker.py`), which reads
  both itself and documents a third, filename-less `metadata.hashes`
  layout with no real sample in this corpus to verify against, so it's
  out of scope. `examples/poetry-lock-checksums.nix` checks both,
  preferring `files` when present, matching `locker.py`'s own order.
- **package-lock.json**: `resolved`+`integrity` are NOT a reliable pair
  — a git-sourced package has `resolved` but no `integrity` (nothing for
  npm's registry to hash), and a bundled/workspace package has neither.
  `packages` is keyed by `node_modules/` path, not name — the same
  name+version can legitimately recur at multiple paths (confirmed real:
  `data/example-package-lock.json`'s own `minimist`, locked at the top
  level and nested under `mocha`). Legacy `lockfileVersion` 1 (flat
  `dependencies` tree, no `packages` key) is out of scope.
- **uv.lock**: a package can have BOTH an `sdist` and multiple `wheels`
  (one per platform/Python tag), each independently hashed —
  `examples/uv-lock-checksums.nix` extracts every hash-bearing one. A
  git-sourced, editable, or virtual package has neither, so it's simply
  absent from the result. `source` is a discriminated attrset
  (`registry`/`git`/`editable`/`virtual` confirmed real); `version`
  itself is optional, absent for a build-time-dynamic version (confirmed
  real in the `dynamic-version` fixture) — confirmed against uv2nix's own
  `lib/lock1.nix` (`parseLock`/`parsePackage`) read directly.

## Benchmarks

See [`bench/comparison-report.md`](bench/comparison-report.md) for full
methodology and caveats. Summary, on synthetic flake.lock-shaped fixtures
from 2.4KB to 875KB plus the real 14.2MB `nix-overlay/flake.lock`:

| | 875KB fixture | real 14.2MB file |
|---|---|---|
| native `builtins.fromJSON` | 0.05s / 49MB | <1s |
| generic `grammar/json.nix` | 1.42s / 390MB | 19.4s / 4.71GB |
| specialized `grammar/flakelock.nix` | 0.95s / 286MB | 10.6s / 3.4GB |
| [`kanwren/nix-parsec`](https://github.com/kanwren/nix-parsec) | stack overflow above ~100KB | — |

[`purenix-org/purenix`](https://github.com/purenix-org/purenix) isn't in
the table — it's a PureScript-to-Nix compiler backend, not a parsing
library, and ships no JSON parser to compare against.

[`arcnmx/nixexprs`'s `lib/json.nix`](https://github.com/arcnmx/nixexprs/blob/master/lib/json.nix)
also isn't in the table above — it defines a NixOS-module-system *type* for
validating that an already-parsed value is JSON-shaped, not a parser, so
there's no `fromJSON`-equivalent function there to time against this
project's grammars. See
[`bench/arcnmx-json-comparison.md`](bench/arcnmx-json-comparison.md) for
what *is* comparable: nixpkgs' equivalent type, `lib.types.json.check`,
benchmarked as a post-`fromJSON` validation step. Its cost turns out to be
dominated by `import <nixpkgs> {}` itself (~155MB / ~0.5s flat, regardless
of input size from 1.7KB to 636KB) — the actual `check` call is close to
free by comparison.

Also benchmarked: [`psyclyx/fix`](https://github.com/psyclyx/fix), an
alternative Nix-language evaluator with a `nix`-alike CLI. Running this
project's unmodified `grammar/json.nix` against the same fixtures, `fix`
was consistently slower and used far more memory than `nix`, with the gap
widening as input grew — 14x more RSS at 50KB, ~38x at 384KB — and it hit
`error: OutOfMemory` on a `tests.nix` regression case
(`bigJumpDoesNotOverflow`) that `nix` handles in well under a second. See
the "`fix` vs `nix`" section of
[`bench/comparison-report.md`](bench/comparison-report.md) for full
numbers.

## Tests

```console
$ nix eval --file tests.nix --json
```

Every attribute is a boolean; `allPassed` is true iff every check passed.
Covers cut-operator semantics, star/opt/and/not sanity, and regression
cases for the array-indexed `Derivs` design.

`./verify-fixtures.sh` checks the other direction: every JSON fixture under
`data/` and `bench/fixtures/` parses byte-identical to `builtins.fromJSON`
through both grammars (schema-mismatched fixtures correctly failing
`grammar/flakelock.nix` count as a pass, not an error). `./verify-
valuewalk-parity.sh` checks that `grammar/flakelock.nix` and `examples/
flakelock-valuewalk.nix` agree on every flake.lock-shaped fixture — both
accept with byte-identical output, or both reject. `./verify-roundtrip.sh`
runs the [round-trip](#value-tree-validation-and-generation) fixpoint
check described above. CI (`.github/workflows/ci.yml`) runs all of the
above plus `nixfmt --check` on every push and PR.
