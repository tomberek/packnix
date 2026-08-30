# Demonstrates the actual nixpkgs motivation for schemas/cargo-lock.nix:
# extracting the fetch hash `importCargoLock` (pkgs/build-support/rust/
# import-cargo-lock.nix) needs for every registry-sourced crate, straight
# out of Cargo.lock's own `checksum` field -- confirmed against nixpkgs'
# own importCargoLock, which passes that field through AS-IS: `checksum =
# pkg.checksum or ...; ...; sha256 = checksum;` (no base32 re-encoding
# step at all, unlike examples/gemfile-lock-checksums.nix's Gemfile.lock
# case -- Cargo.lock's checksum is already the exact hex sha256 a
# fixed-output derivation's `sha256` attribute accepts).
#
# So for any Cargo.lock, the fetch hash importCargoLock exists to obtain
# is ALREADY in the lockfile -- reading it needs no `cargo`, no network,
# just this schema. Git-sourced and local/workspace packages never carry
# a `checksum` (see schemas/cargo-lock.nix's header for why -- they're
# fetched a different way, or not fetched at all) and are correctly
# excluded here, same as this schema's own SOURCE-vs-CHECKSUM invariant.
#
# Run with:
#   nix eval --impure --expr '
#     let
#       vw = import ../lib/valuewalk.nix;
#       g = import ./cargo-lock-checksums.nix;
#     in g.hashesByCrateNameVersion (builtins.readFile ../data/example-Cargo.lock)
#   ' --json
let
  vw = import ../lib/valuewalk.nix;
  cargoLock = import ../schemas/cargo-lock.nix;

  parse = toml: (vw.run { grammar = cargoLock; } (builtins.fromTOML toml)).DOCUMENT;

  # { "<crate name>-<version>" = <hex sha256>; ... } -- exactly the shape
  # a Cargo.lock-vendoring function would need to build each registry
  # crate's `builtins.fetchurl { url = "https://crates.io/api/v1/crates/${name}/${version}/download"; sha256 = ...; }`
  # (or hand straight to `outputHashes` the way importCargoLock's own
  # `${pkg.name}-${pkg.version}` keying does -- matched here for direct
  # drop-in use). Keyed by name+version rather than just name, unlike
  # examples/gemfile-lock-checksums.nix's Gemfile.lock case: Cargo.lock
  # can legitimately lock 2+ versions of the same crate name at once
  # (see schemas/cargo-lock.nix's header), so name alone isn't unique.
  hashesByCrateNameVersion =
    toml:
    let
      doc = parse toml;
    in
    if doc == null then
      throw "not a valid Cargo.lock"
    else
      builtins.listToAttrs (
        map (p: {
          name = "${p.name}-${p.version}";
          value = p.checksum;
        }) (builtins.filter (p: p.checksum or null != null) doc.package)
      );
in
{
  inherit parse hashesByCrateNameVersion;
}
