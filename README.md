# packnix

A packrat/PEG parsing engine written entirely in the Nix expression
language, plus a couple of grammars built on it — one for generic JSON, one
specialized for `nix flake lock`'s exact output schema.

Built from Ford's ["Packrat Parsing: Simple, Powerful, Lazy, Linear Time"](https://bford.info/pub/lang/packrat-icfp02/)
and Mizushima et al.'s ["Packrat Parsers Can Handle Practical Grammars in
Mostly Constant Space"](https://dl.acm.org/doi/10.1145/1806672.1806679) (the
source of the `cutSeq`/`↑` operator below).

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
| `examples/json-simple.nix` | A plain, unoptimized JSON grammar — every construct gets its own named rule, no attention paid to allocation. Good starting point for reading/writing your own grammar. |
| `examples/json-optimized.nix` | Re-exports `grammar/json.nix`, annotated with what changed vs. `json-simple.nix` and why (rule inlining via the `action` combinator, fewer redundant whitespace scans, etc). |
| `examples/flakelock-specialized.nix` | Re-exports `grammar/flakelock.nix`, annotated with the schema-specialization technique and measured wins. |
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

## Tests

```console
$ nix eval --file tests.nix --json
```

Every attribute is a boolean; `allPassed` is true iff every check passed.
Covers cut-operator semantics, star/opt/and/not sanity, and regression
cases for the array-indexed `Derivs` design.
