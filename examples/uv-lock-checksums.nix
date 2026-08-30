# Demonstrates the real-world (though not nixpkgs-vendored -- see
# schemas/uv-lock.nix's own header, and below) motivation for
# schemas/uv-lock.nix: extracting the fetch hash needed to build a
# `builtins.fetchurl { url = ...; hash = ...; }` for every registry-
# fetched package's sdist/wheel(s) in a uv.lock -- no `uv`, no network.
#
# A uv.lock hash string is ALREADY `"sha256:<hex>"`, the exact format
# nixpkgs' `fetchurl` accepts as `hash` with zero conversion -- confirmed
# directly via uv2nix's own `lib/build.nix` (`fetchurl { url =
# package.source.url or package.sdist.url; inherit (package.sdist) hash;
# }` for an sdist, `fetchurl { inherit (wheel) url hash; }` for a wheel),
# the SAME zero-conversion case as schemas/package-lock.nix's SRI
# `integrity`, simpler than poetry.lock's own prefix-strip.
#
# UNLIKE Cargo.lock/poetry.lock/package-lock.json (one fetchable thing
# per package), a uv.lock package can have BOTH an `sdist` (a single
# entry) AND multiple `wheels` (one per platform/Python tag) -- this
# extracts every hash-bearing one, matching poetry-lock-checksums.nix's
# own "a package can have multiple published artifacts" list shape
# rather than package-lock-checksums.nix's/cargo-lock-checksums.nix's
# single-hash shape. A git-sourced, `editable`, or `virtual` package
# (see schemas/uv-lock.nix's own SOURCE header) has neither `sdist` nor
# `wheels` at all -- nothing to extract, so such packages are simply
# absent from the result rather than mapped to `[]`.
#
# Keyed by "<name>-<version>", matching poetry-lock-checksums.nix's own
# convention -- unlike package-lock.json's node_modules-path keying,
# a uv.lock's `package` list is a single dependency RESOLUTION (not a
# tree with the same name re-locked at different paths), so name+version
# is already unique; the one real exception this corpus confirms
# (data/example-uv.lock's own `arpeggio`, locked at both 2.0.0 AND 2.0.1
# for two different `conflicts`-gated extras) is exactly why version is
# part of the key, not just name.
#
# Run with:
#   nix eval --impure --expr '
#     let
#       g = import ./uv-lock-checksums.nix;
#     in g.hashesByPackageNameVersion (builtins.readFile ../data/example-uv.lock)
#   ' --json
let
  vw = import ../lib/valuewalk.nix;
  uvLock = import ../schemas/uv-lock.nix;

  parse = toml: (vw.run { grammar = uvLock; } (builtins.fromTOML toml)).DOCUMENT;

  # A package's sdist (if it has a hash) plus every wheel that has one --
  # `sdist`/a given wheel legitimately lacking `hash` (see
  # schemas/uv-lock.nix's header: lib/build.nix's own hash-less-wheel
  # fallback) is skipped rather than producing a null/broken entry.
  fetchableEntries =
    pkg:
    (if pkg ? sdist && pkg.sdist ? hash then [ pkg.sdist ] else [ ])
    ++ (builtins.filter (w: w ? hash) (pkg.wheels or [ ]));

  # { "<name>-<version>" = [ { url; hash; } ... ]; ... } -- omits
  # packages with neither sdist nor wheels at all (git-sourced,
  # editable, virtual -- see header).
  hashesByPackageNameVersion =
    toml:
    let
      doc = parse toml;
      withEntries = map (pkg: {
        inherit pkg;
        entries = fetchableEntries pkg;
      }) doc.package;
    in
    if doc == null then
      throw "not a valid uv.lock"
    else
      builtins.listToAttrs (
        map (p: {
          name = "${p.pkg.name}-${p.pkg.version or "0.0.0"}";
          value = map (e: {
            inherit (e) url hash;
          }) p.entries;
        }) (builtins.filter (p: p.entries != [ ]) withEntries)
      );
in
{
  inherit parse hashesByPackageNameVersion;
}
