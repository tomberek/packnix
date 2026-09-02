# A real go.sum (Go module checksum database) grammar for lib/packrat.nix.
# Not JSON/YAML/TOML -- a bespoke, line-oriented, fixed-3-field format, one
# of the simplest lockfile-adjacent formats this repo models (no nesting,
# no indentation, no quoting). Schema confirmed against two real go.sum
# files shipped in nixpkgs itself (github.com/NixOS/nixpkgs, the public
# upstream, not an internal fork): `pkgs/by-name/pa/pam_ussh/go.sum` (22
# lines) and `pkgs/by-name/ku/kubemqctl/go.sum` (664 lines) -- 686 lines
# total, every one matching the shape below exactly.
#
# Why this format is DIFFERENT from every other lockfile grammar in this
# repo: Cargo.lock/poetry.lock/package-lock.json/uv.lock/Gemfile.lock/
# yarn.lock all embed a PER-PACKAGE fetch hash that a Nix fetcher can use
# directly (see schemas/cargo-lock.nix's/grammar/gemfile-lock.nix's own
# headers). go.sum has no such use: nixpkgs' `buildGoModule` (pkgs/build-
# support/go/module.nix) runs `go mod download` inside ONE fixed-output
# derivation with `GOSUMDB=off` and gets a single AGGREGATE `vendorHash`
# for the whole module graph -- it never reads go.sum's own per-module
# hashes at all. There is also no URL to fetch from in this format in the
# first place (a go.sum line is `module version hash`, not `module
# version url hash`) -- the module PATH doubles as a resolvable location
# for Go's own tooling, but this grammar doesn't attempt that resolution.
# So this grammar's value is structural (dependency-graph extraction,
# detecting go.mod-only vs actually-built modules, license/SBOM auditing)
# plus a genuinely useful DIFFERENT check: cross-referencing the same
# module@version's hash across two go.sum files to catch a supply-chain
# hash MISMATCH (a sign of a compromised/tampered proxy, a stale vendor
# copy, or a genuine upstream re-tag) -- see
# examples/go-sum-checksums.nix.
#
# Top-level shape (confirmed across the whole corpus, both files):
#   (line)+
# where each line is exactly:
#   <module> <version>[+incompatible][/go.mod] h1:<base64>=\n
#
# Confirmed structural facts driving this grammar's design:
#   - EVERY line has exactly 3 whitespace-separated fields, no exceptions,
#     no blank lines anywhere in either corpus file (unlike every other
#     line-oriented grammar in this repo, which all have some blank-line
#     convention) -- modeled as `plus` on a single fixed-shape LINE rule,
#     no separator handling needed at all.
#   - the module path (field 1) uses both cases (e.g.
#     "github.com/AlecAivazis/survey/v2", "github.com/BurntSushi/toml")
#     plus digits, ".", "/", "_", "-" -- confirmed as the exact charset in
#     use across both corpus files (no "!"-escaped uppercase encoding
#     appears in EITHER file, though Go's own spec allows it for
#     case-sensitive filesystems; not modeled since unobserved).
#   - the version field (field 2) is always "v" + dotted-numeric-or-
#     pseudo-version, OPTIONALLY suffixed with "+incompatible" (a real Go
#     modules marker for a pre-v2-module-system major version, confirmed
#     present in kubemqctl's go.sum, e.g. "v2.22.0+incompatible"),
#     OPTIONALLY further suffixed with "/go.mod" -- the exact same
#     module+version pair appears TWICE in the corpus whenever both a
#     build hash and a go.mod-only hash exist for it (confirmed: 99 of
#     kubemqctl's 664 lines lack a bare hash line, meaning a dependency
#     Go only needed for its go.mod's own requirements, never actually
#     compiled) -- modeled as an independent `opt`-wrapped "/go.mod"
#     suffix on EVERY line, not two separately-shaped line rules, since
#     nothing else about the line's shape depends on which case applies.
#   - the hash field (field 3) is ALWAYS prefixed "h1:" in both corpus
#     files (Go's dirhash package's own "H1" algorithm tag) followed by
#     standard base64 (confirmed: "+/=" appear, no base64url "-_" chars)
#     -- no other hash algorithm tag observed, so only "h1:" is modeled;
#     an unrecognized tag correctly fails to parse rather than silently
#     accepting.
#   - every line ends in an actual "\n", confirmed even for the FILE'S
#     OWN last line in both corpus files (unlike every other line-
#     oriented grammar in this repo, which all tolerate a final line with
#     no trailing newline) -- modeled with a mandatory trailing "\n" on
#     every line, not `lineEnd`'s usual eof-tolerant choice.
#   - lines are NOT required to be sorted (`go mod tidy`'s own output
#     happens to be, but this grammar doesn't check or rely on it -- some
#     hand-edited/vendored go.sum files in the wild aren't, and there's no
#     reason to reject one that isn't).
#
# Deliberately out of scope:
#   - resolving a module path to an actual fetchable URL (Go's own module
#     proxy protocol, not representable as a static field in this format
#     at all -- see this file's header above for why no checksums example
#     here produces a `{url; hash;}` pair the way every OTHER lockfile
#     example in this repo does)
#   - any hash algorithm tag other than "h1:" (none observed in the
#     corpus; Go's dirhash package defines only H1 in practice)
#   - the "!"-prefixed case-encoding Go's own module-path spec allows for
#     mixed-case-insensitive filesystems (unobserved in either corpus
#     file)
# A malformed or differently-structured go.sum correctly fails to parse
# rather than silently mis-parsing, same discipline as every other
# grammar in this repo.
let
  # Confirmed exact charset for a module path across both corpus files:
  # letters (both cases), digits, ".", "/", "_", "-". No escapes exist in
  # this format at all.
  modulePath = {
    regex = "([A-Za-z0-9./_-]+)";
  };

  # "v1.2.3", "v0.0.0-20200313102051-9f266ea9e77c" -- kept as ONE opaque
  # string (not decomposed into semver components), matching this
  # format's own use: a version string is only ever compared for
  # equality against another go.sum line or a go.mod requirement, never
  # range-matched the way pep508/poetry-semver's constraints are.
  version = {
    regex = "(v[0-9][A-Za-z0-9.-]*)";
  };

  # A single go.sum line: "<module> <version>[+incompatible][/go.mod]
  # h1:<base64>=\n". "+incompatible" and "/go.mod" are each modeled as an
  # independent `opt` literal suffix (not folded into `version`'s own
  # regex charset, which would blur an explicit Go-modules marker with an
  # arbitrary version character) -- both are independent of which
  # module/version this is (see file header), so neither needs two
  # separately-shaped line rules.
  LINE = {
    action = {
      e = [
        modulePath
        { lit = " "; }
        version
        {
          opt = {
            lit = "+incompatible";
          };
        }
        {
          opt = {
            lit = "/go.mod";
          };
        }
        { lit = " h1:"; }
        { regex = "([A-Za-z0-9+/]+=*)"; }
        { lit = "\n"; }
      ];
      f = v: {
        module = builtins.elemAt v 0;
        version = builtins.elemAt v 2;
        incompatible = builtins.elemAt v 3 != null;
        isGoModOnly = builtins.elemAt v 4 != null;
        hash = "h1:${builtins.elemAt v 6}";
      };
    };
  };

  DOCUMENT = {
    plus = LINE;
  };

  documentHandler = v: [ (builtins.elemAt v 0) ] ++ builtins.elemAt v 1;

  grammar = {
    inherit DOCUMENT;
  };
  handlers = {
    DOCUMENT = documentHandler;
  };
in
{
  inherit grammar handlers;
}
