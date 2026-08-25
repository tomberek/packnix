# packnix

A packrat/PEG parsing engine written entirely in the Nix expression
language, plus a few grammars built on it — generic JSON, JSON specialized
for `nix flake lock`'s exact output schema, a real (subset of) YAML, TSV,
generic ATerm plus a grammar specialized to Nix's own `.drv` file format,
Python's PEP 508 dependency-specification format and Poetry's version-
constraint syntax, and Ruby's Bundler `Gemfile.lock` and (a
group-membership-focused subset of) `Gemfile` formats.

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
| `grammar/aterm.nix` | A generic ATerm (Annotated Term) grammar — the format Nix's own `.drv` files are written in, among other uses (ASF+SDF Meta-Environment, Stratego/XT). Covers all six real term kinds (int, real, appl, list, tuple, placeholder) plus annotations; verified against 500 real `.drv` files from a live `/nix/store`. |
| `grammar/drv.nix` | A grammar specialized to Nix's `.drv` file format's exact shape (`Derive(outputs, inputDrvs, inputSrcs, system, builder, args, env)`, always exactly 7 fields) — semantically decodes each field (e.g. a fixed-output derivation's `hashAlgo`'s `"r:"` prefix into a `recursive` flag) rather than returning a generic ATerm tree. See its header for the confirmed field shapes. |
| `grammar/pep508.nix` | Python's PEP 508 dependency-specification format (`requests (>=2.0,<3.0) ; python_version >= "3.6" and sys_platform == "linux"`) — the same format nixpkgs' `poetry2nix` parses today via ~180 lines of hand-rolled character-walking with a known `# TODO: Handle single quoted values` gap and no real `and`/`or` precedence. Transcribed directly from PEP 508's own formal grammar (restructured to avoid left recursion); verified against 2126 real, distinct `Requires-Dist` specifiers extracted from real `*.dist-info/METADATA` files. |
| `grammar/poetry-semver.nix` | Poetry's version-constraint syntax (`^1.2.3`, `~1.2`, `1.*`, `~2.7 \|\| ^3.5`) — parses AND evaluates (`mkSatisfies packrat version constraint`). Fixes several real, demonstrated bugs found in nixpkgs' `poetry2nix/semver.nix`/`lib.nix` while building this (wrong caret/tilde upper bounds that accept clearly-incompatible major versions, `!=X.Y.*` not actually excluding anything, bare versions/wildcards throwing instead of parsing). Verified against 65 real `python-versions`/`python = "..."` constraint strings from real `poetry.lock`/`pyproject.toml` files. See its header for the specific bugs and how they were confirmed. |
| `examples/json-simple.nix` | A plain, unoptimized JSON grammar — every construct gets its own named rule, no attention paid to allocation. Good starting point for reading/writing your own grammar. |
| `examples/json-optimized.nix` | Re-exports `grammar/json.nix`, annotated with what changed vs. `json-simple.nix` and why (rule inlining via the `action` combinator, fewer redundant whitespace scans, etc). |
| `examples/flakelock-specialized.nix` | Re-exports `grammar/flakelock.nix`, annotated with the schema-specialization technique and measured wins. |
| `examples/gemfile-lock-checksums.nix` | Extracts `{ <gem name> = <sha256>; }` from a `Gemfile.lock`'s `CHECKSUMS` section — the piece a `bundlerEnv` replacement would need. See below. |
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
to match at that position is `false`.

Using the flake.lock-specialized grammar instead (only accepts documents
matching that exact schema — see next section):

```nix
let
  packrat = import ./lib/packrat.nix;
  g = import ./grammar/flakelock.nix;
in
(packrat.run { grammar = g.grammar; handlers = g.handlers; } 0
  (builtins.readFile ./data/lock-large.json)).DOCUMENT
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
parser (not derived from this grammar) across 134 real `Gemfile.lock`
files pulled from a nixpkgs checkout — every field (multiple GEM/GIT/PATH
blocks, platform-qualified spec versions, `!`-pinned/multi-constraint
dependencies, CHECKSUMS, RUBY VERSION) byte/value-identical between the
two. Deliberately out of scope: Bundler `PLUGIN SOURCES` (not seen in the
corpus at all).

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
[`bench/comparison-report.md`](bench/comparison-report.md) for the full
numbers and two unrelated CLI-compatibility gaps found along the way.

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
`grammar/flakelock.nix` count as a pass, not an error). CI
(`.github/workflows/ci.yml`) runs both of the above plus `nixfmt --check`
on every push and PR.
