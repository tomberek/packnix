# Demonstrates the actual nixpkgs motivation for schemas/package-lock.nix:
# extracting the fetch hash needed to build a `builtins.fetchurl { url =
# ...; hash = ...; }` for every registry-fetched package in a
# package-lock.json -- no `npm`, no network.
#
# UNLIKE Cargo.lock's hex `checksum` (used as-is) or poetry.lock's
# `"sha256:<hex>"` (prefix stripped), a package-lock.json `integrity`
# string is ALREADY the exact SRI format (`sha512-<base64>`) nixpkgs'
# `fetchurl` accepts as `hash` with zero conversion -- confirmed directly
# via nixpkgs' own `importNpmLock` (pkgs/build-support/node/import-npm-lock/
# default.nix), which does exactly `fetchurl { url = module.resolved; hash
# = module.integrity; }`.
#
# Per schemas/package-lock.nix's own header, `resolved`+`integrity` are
# NOT a reliable pair: a git-sourced package has `resolved` but no
# `integrity` (nothing for npm's registry to have hashed), and a bundled
# or workspace-linked package has neither. This function only extracts
# entries where BOTH are present AND `resolved` is an http(s) URL (the
# only scheme `fetchurl` itself can fetch -- a git-sourced `resolved`
# needs `fetchGit`/`fetchgit` instead, same split `importNpmLock` itself
# makes in its own `fetchModule`).
#
# Keyed by node_modules PATH, not "<name>-<version>", matching
# schemas/package-lock.nix's own `packages` shape: the SAME package name
# (and even version) can legitimately appear at multiple different paths
# in one file (nested node_modules/ for conflicting version
# requirements), so path is the only key guaranteed unique.
#
# Run with:
#   nix eval --impure --expr '
#     let
#       g = import ./package-lock-checksums.nix;
#     in g.hashesByPackagePath (builtins.readFile ../data/example-package-lock.json)
#   ' --json
let
  vw = import ../lib/valuewalk.nix;
  packageLock = import ../schemas/package-lock.nix;

  parse = json: (vw.run { grammar = packageLock; } (builtins.fromJSON json)).DOCUMENT;

  isHttpUrl = url: builtins.match "https?://.*" url != null;

  # { "<node_modules path>" = { url = <resolved>; hash = <integrity>; }; ... }
  # -- omits the root ("") entry, git-sourced packages, and bundled/
  # workspace packages, none of which have a fetchable (resolved, integrity)
  # pair (see header).
  hashesByPackagePath =
    json:
    let
      doc = parse json;
      isFetchable = pkg: pkg ? resolved && pkg ? integrity && isHttpUrl pkg.resolved;
      fetchablePaths = builtins.filter (path: isFetchable doc.packages.${path}) (
        builtins.attrNames doc.packages
      );
    in
    if doc == null then
      throw "not a valid package-lock.json"
    else
      builtins.listToAttrs (
        map (
          path:
          let
            pkg = doc.packages.${path};
          in
          {
            name = path;
            value = {
              url = pkg.resolved;
              hash = pkg.integrity;
            };
          }
        ) fetchablePaths
      );
in
{
  inherit parse hashesByPackagePath;
}
