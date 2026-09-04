# packnix

A packrat/PEG parsing engine written entirely in the Nix expression
language, plus grammars for JSON, YAML, TSV, `nix flake.lock`, Nix's own
`.drv` format (via ATerm), Python's PEP 508 and Poetry's version
constraints, Ruby's `Gemfile`/`Gemfile.lock`, Yarn classic's `yarn.lock`,
and Go's `go.sum`.

Built from Ford's ["Packrat Parsing: Simple, Powerful, Lazy, Linear Time"](https://bford.info/pub/lang/packrat-icfp02/)
and Mizushima et al.'s ["Packrat Parsers Can Handle Practical Grammars in
Mostly Constant Space"](https://dl.acm.org/doi/10.1145/1806672.1806679) (the
source of the `cutSeq`/`↑` operator below).

## Quick example

A grammar is just data — an attrset of named rules, each built from a
small set of combinators (see [Grammar DSL](#grammar-dsl)). Here's a
complete one for `NAME=VALUE` lines:

```nix
let
  packrat = import ./lib/packrat.nix;
  grammar = {
    ENTRY = [ "NAME" { lit = "="; } "VALUE" ];
    NAME = { regex = "([A-Za-z_][A-Za-z0-9_]*)"; };
    VALUE = { regex = "([^\n]*)"; };
  };
  handlers.ENTRY = v: {
    name = builtins.elemAt v 0;
    value = builtins.elemAt v 2;
  };
in
(packrat.run { inherit grammar handlers; } 0 "PORT=8080").ENTRY
```

```console
$ nix eval --impure --expr '<the above>' --json
{"name":"PORT","value":"8080"}
```

`ENTRY` references `NAME` and `VALUE` by name; `handlers.ENTRY` turns the
raw `["PORT" "=" "8080"]` match into a clean attrset. That's the whole
pattern this project builds on — every grammar below (`grammar/tsv.nix`
is a good next read, at ~30 lines) is the same combinators composed at
larger scale.

## Why

Nix has a native JSON parser (`builtins.fromJSON`, in C++) — this isn't a
faster way to parse JSON. The point is a general parsing *library* for
grammars `fromJSON` can't help with: anything with its own syntax you'd
otherwise hand-roll with `builtins.match`/`substring`/recursion. JSON is the
running example because it's easy to verify and easy to compare against
other Nix parsing libraries.

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
matching that exact schema — see [The specialized grammar](#the-specialized-grammar)):

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

## Layout

| Path | What |
|---|---|
| `lib/packrat.nix` | The engine: `mkCompile`, `buildDerivs`, `run`. Everything else is built on this. |
| `grammar/json.nix` | A generic, from-scratch JSON grammar (`grammar`/`grammarNoCut` + cut and non-cut variants). |
| `grammar/flakelock.nix` | A grammar specialized to `nix flake lock`'s exact schema — see [The specialized grammar](#the-specialized-grammar). |
| `grammar/tsv.nix` | A small TSV (tab-separated values) grammar. |
| `grammar/yaml.nix` | A real YAML subset: block mappings/sequences nested by indentation, plain/quoted scalars, flow collections, comments. `mkYamlGrammar { indentStep; maxDepth; }` generates the grammar; see its header for scope limits (no anchors/tags/multi-doc/block-scalars, fixed indent step, bounded depth). |
| `grammar/aterm.nix` | A generic ATerm (Annotated Term) grammar — the format Nix's own `.drv` files are written in. Covers all six real term kinds plus annotations; verified against 500 real `.drv` files from a live `/nix/store`. |
| `grammar/drv.nix` | A grammar specialized to Nix's `.drv` file format's exact shape — semantically decodes each field (e.g. `hashAlgo`'s `"r:"` prefix into a `recursive` flag) rather than returning a generic ATerm tree. |
| `grammar/pep508.nix` | Python's PEP 508 dependency-specification format — transcribed from PEP 508's own formal grammar; verified against 2126 real, distinct `Requires-Dist` specifiers. |
| `grammar/poetry-semver.nix` | Poetry's version-constraint syntax (`^1.2.3`, `~1.2`, `1.*`, `~2.7 \|\| ^3.5`) — parses AND evaluates (`mkSatisfies packrat version constraint`). Fixes several real bugs found in nixpkgs' own `poetry2nix/semver.nix` while building this; see its header. |
| `grammar/gemfile-lock.nix`, `grammar/gemfile.nix`, `grammar/yarn-lock.nix`, `grammar/go-sum.nix` | Real dependency-lockfile formats with a real nixpkgs (or nixpkgs-ecosystem) use case — see [Lockfile formats](#lockfile-formats). |
| `schemas/cargo-lock.nix`, `schemas/poetry-lock.nix`, `schemas/package-lock.nix`, `schemas/uv-lock.nix` | `lib/valuewalk.nix` schemas (not `lib/packrat.nix` grammars) for lockfiles that are plain TOML/JSON — see [Lockfile formats](#lockfile-formats). |
| `examples/*-checksums.nix` | Extracts each lockfile's fetch hashes into the shape its real Nix consumer needs — see [Lockfile formats](#lockfile-formats). |
| `examples/json-simple.nix` | A plain, unoptimized JSON grammar — every construct gets its own named rule, no attention paid to allocation. Good starting point for reading/writing your own grammar. |
| `examples/json-optimized.nix` | Re-exports `grammar/json.nix`, annotated with what changed vs. `json-simple.nix` and why (rule inlining via the `action` combinator, fewer redundant whitespace scans, etc). |
| `examples/flakelock-specialized.nix` | Re-exports `grammar/flakelock.nix`, annotated with the schema-specialization technique and measured wins. |
| `lib/valuewalk.nix` | A schema-validation engine over an already-parsed value tree (from `fromJSON`/`fromTOML`), not string positions — see [Value-tree validation and generation](#value-tree-validation-and-generation). |
| `lib/generate.nix` | Generates a sample value/string that a `lib/valuewalk.nix` schema or `lib/packrat.nix` grammar would accept — the reverse direction of validation, deterministically seeded. |
| `lib/regex-generate.nix` | Inverts a POSIX ERE pattern into a sample string it would accept; backs `generate`'s automatic `pattern`/`regex` synthesis. |
| `lib/roundtrip.nix` | Generates N samples for a grammar/schema and confirms its own parser accepts every one — the fixpoint gate `verify-roundtrip.sh` runs. |
| `examples/flakelock-valuewalk.nix` | The `grammar/flakelock.nix` schema rewritten against `lib/valuewalk.nix`, over `builtins.fromJSON`'s output instead of string positions. |
| `default.nix` | Thin wrapper: `pack ./somefile.json` parses a file with the JSON grammar. |
| `tests.nix` | Standalone combinator test suite (cut-operator semantics, star/regex edge cases, etc). |
| `bench/` | Fixture generators, measurement scripts, `comparison-report.md`, `arcnmx-json-comparison.md`. |

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
already exists. Nix has no RNG at all, so generation is deterministically
SEEDED instead of random: every choice derives from `builtins.hashString
"sha256" seed`, and every recursive call derives a fresh child seed, so
`generate schema seed` is a pure function — same schema + same seed always
produces the same value, which makes a failure reproducible instead of
flaky. `{ pattern = "..."; }`/`{ regex = "..."; }` leaves are synthesized
automatically by `lib/regex-generate.nix` (a POSIX ERE parser + AST-
walking generator), with an explicit `patternGenerators` override
available as a fallback. `and`/`not` lookahead is supported when it
appears as a sequence element (directly, or one level indirected through
a named rule) with a genuine sibling following it: the sibling is
generated normally, then verified against the lookahead's body by
reusing `lib/packrat.nix`'s own `run` as the oracle, retrying with a
derived seed on mismatch before throwing — see `lib/generate.nix`'s
header for the exact coverage.

`lib/roundtrip.nix` wires the two together into a fixpoint check: generate
N samples for a grammar/schema, feed each back through that SAME
grammar/schema's own parser, and confirm every one is *accepted*. This is
narrower than "generated value equals the original" — there is no
original here, only "does the parser accept what was generated for it".
`./verify-roundtrip.sh` runs this in CI at N=50 for every grammar/schema
shipped in this repo.

## Lockfile formats

Six dependency-lockfile formats, each with a real (or plausible) Nix
consumer. `grammar/gemfile-lock.nix`/`grammar/gemfile.nix`/
`grammar/yarn-lock.nix`/`grammar/go-sum.nix` are `lib/packrat.nix`
grammars, since those formats have bespoke syntax no native Nix parser
handles. `schemas/cargo-lock.nix`/`poetry-lock.nix`/`package-lock.nix`/
`uv-lock.nix` are `lib/valuewalk.nix` schemas instead — those four formats
are plain TOML/JSON, so a from-scratch grammar would just re-parse text
`fromTOML`/`fromJSON` already handles (see [Why](#why)). Every corpus
claim below is confirmed against real files, not derived from a format's
docs alone — see each grammar/schema file's own header for the full
breakdown and edge cases.

| Format | Consumer | Fetch-hash field | Corpus |
|---|---|---|---|
| `Gemfile.lock` | `bundlerEnv`'s `gemset.nix` (today via external [`bundix`](https://github.com/nix-community/bundix)) | `CHECKSUMS`' hex sha256, base32-re-encode to match `bundix` | 134 real files from a nixpkgs checkout |
| `Gemfile` | *(no lockfile equivalent — see below)* | — | 136-file nixpkgs corpus (6 use `group`) |
| `yarn.lock` | `yarn2nix`/`mkYarnPackage` (today via external Node tooling) | `integrity`, SRI `sha512-`/`sha1-`, base32-re-encode | 15 real files (2,395 entries) |
| `go.sum` | *(not a per-package fetch hash — see below)* | `h1:`-prefixed base64, content hash only | 2 real nixpkgs files (686 lines) |
| `Cargo.lock` | nixpkgs' `importCargoLock` | `checksum`, used as-is | 100 real files (149B–225KB) |
| `poetry.lock` | poetry2nix's `fetchFromPypi` | `"sha256:<hex>"`, strip prefix | 5 real files |
| `package-lock.json` | nixpkgs' `importNpmLock` | `integrity`, already SRI, zero conversion | 43 real files |
| `uv.lock` | external [`uv2nix`](https://github.com/pyproject-nix/uv2nix) (nixpkgs has no consumer) | `"sha256:<hex>"` per `sdist`/`wheels[]` entry | uv2nix's own public fixtures |

Two exceptions worth calling out:

- **`Gemfile`** has no fetch hash of its own — `grammar/gemfile.nix`
  instead recovers Bundler *group* membership per gem (`group :x
  do...end` blocks, inline `group:`/`groups:` kwargs, `if`/`unless`/
  `else` wrapping), the one fact `Gemfile.lock` never records.
- **`go.sum`** doesn't fit the "hash is already in the file" pattern at
  all: nixpkgs' `buildGoModule` computes one *aggregate* `vendorHash` for
  the whole module graph via `go mod download`, never reading `go.sum`'s
  per-module hashes. `examples/go-sum-checksums.nix` uses this format's
  structure for something else instead — cross-referencing the same
  `module@version` across two independent `go.sum` files to flag a hash
  *mismatch*, a real supply-chain integrity signal.

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
