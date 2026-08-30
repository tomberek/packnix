# Demonstrates the actual nixpkgs motivation for schemas/poetry-lock.nix:
# extracting the fetch hash needed to build a `builtins.fetchurl { url =
# ...; sha256 = ...; }` (or an `outputHashAlgo = "sha256"; outputHash =
# ...;` fixed-output derivation, the shape poetry2nix's own
# `fetchFromPypi` in pkgs/development/tools/poetry2nix/poetry2nix/lib.nix
# uses -- confirmed directly: `outputHashAlgo = "sha256"; outputHash =
# hash;`) straight out of the lockfile, for every registry-sourced
# package -- no `poetry`, no network.
#
# A poetry.lock hash string is always `"sha256:<hex digest>"` (confirmed
# across every corpus file backing schemas/poetry-lock.nix) -- the
# `sha256:` PREFIX needs stripping (a plain `builtins.substring`, not a
# base32 re-encode) before handing it to `outputHash`/`sha256`, simpler
# than examples/gemfile-lock-checksums.nix's hex-to-base32 step and on
# par with examples/cargo-lock-checksums.nix's no-conversion-at-all case.
#
# UNLIKE Cargo.lock/Gemfile.lock, a real hash can live in EITHER of two
# structurally different places depending on the lockfile's
# `lock-version` (see schemas/poetry-lock.nix's own header for the full
# 3-generation history): a package's own `files` field (current), or
# the top-level `metadata.files.<name>` table (older). This function
# checks BOTH, preferring the package's own `files` when present --
# exactly mirroring Poetry's own `locker.py` reader's own preference
# order.
#
# Run with:
#   nix eval --impure --expr '
#     let
#       vw = import ../lib/valuewalk.nix;
#       g = import ./poetry-lock-checksums.nix;
#     in g.hashesByPackageNameVersion (builtins.readFile ../data/example-poetry.lock)
#   ' --json
let
  vw = import ../lib/valuewalk.nix;
  poetryLock = import ../schemas/poetry-lock.nix;

  parse = toml: (vw.run { grammar = poetryLock; } (builtins.fromTOML toml)).DOCUMENT;

  # "sha256:<hex>" -> "<hex>" -- the only transform needed; `outputHash`
  # accepts the bare hex digest directly when `outputHashAlgo =
  # "sha256";` is set explicitly (confirmed via poetry2nix's own
  # `fetchFromPypi`, which passes the RAW lockfile hash straight through
  # as `outputHash` with no intermediate conversion step at all).
  stripSha256Prefix =
    h:
    if builtins.substring 0 7 h == "sha256:" then
      builtins.substring 7 (builtins.stringLength h) h
    else
      h;

  # A package's own `files`, if present; otherwise fall back to
  # `metadata.files.<name>` (see header) -- `or [ ]` for a package with
  # NEITHER (e.g. a git/directory-sourced package, which poetry.lock
  # never records a fetch hash for at all -- there's nothing to extract).
  filesForPackage = doc: pkg: pkg.files or (doc.metadata.files or { }).${pkg.name} or [ ];

  # { "<name>-<version>" = [ "<hex sha256>" ... ]; ... } -- a LIST, not a
  # single hash, since a real package can (and typically does) have
  # multiple published artifacts (e.g. one wheel per platform, plus an
  # sdist), each with its own hash -- unlike Cargo.lock's/Gemfile.lock's
  # one-hash-per-package shape, there is no single "the" hash here
  # without also knowing which specific file a caller wants to fetch.
  hashesByPackageNameVersion =
    toml:
    let
      doc = parse toml;
    in
    if doc == null then
      throw "not a valid poetry.lock"
    else
      builtins.listToAttrs (
        map (pkg: {
          name = "${pkg.name}-${pkg.version}";
          value = map (f: stripSha256Prefix f.hash) (filesForPackage doc pkg);
        }) doc.package
      );
in
{
  inherit parse hashesByPackageNameVersion;
}
