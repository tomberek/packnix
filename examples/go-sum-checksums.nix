# Demonstrates the actual value grammar/go-sum.nix's structural parsing
# has, GIVEN that (unlike every other lockfile grammar in this repo)
# go.sum's own per-module hashes are never consumed by nixpkgs'
# buildGoModule as a fetch hash -- see grammar/go-sum.nix's own header
# for why: `go mod download` computes ONE aggregate `vendorHash` inside a
# single fixed-output derivation, never reading go.sum module-by-module.
#
# So instead of a `{url; hash;}` extraction (what every OTHER
# examples/*-checksums.nix file in this repo demonstrates), this shows a
# genuinely different, still real use: cross-referencing the SAME
# module@version across two independently-generated go.sum files (e.g.
# two different Go projects, or a vendored copy vs. upstream) and
# flagging any HASH MISMATCH -- go.sum's own `h1:` hash is a content
# hash of that exact module version, so two go.sum files agreeing on a
# shared dependency's version but disagreeing on its hash is a genuine
# supply-chain red flag (a compromised/tampered proxy serving different
# bytes under the same version tag, not merely "different projects
# pinned different versions", which is normal and NOT flagged here).
#
# Run with:
#   nix eval --impure --expr '
#     let
#       packrat = import ../lib/packrat.nix;
#       g = import ./go-sum-checksums.nix;
#     in g.findHashMismatches
#       (builtins.readFile ../data/example-go.sum)
#       (builtins.readFile ../data/example2-go.sum)
#   ' --json
let
  packrat = import ../lib/packrat.nix;
  goSum = import ../grammar/go-sum.nix;

  parse =
    string:
    (packrat.run {
      grammar = goSum.grammar;
      handlers = goSum.handlers;
    } 0 string).DOCUMENT;

  # { "<module>@<version>" = <h1 hash>; ... } -- keyed by module+version,
  # since (unlike e.g. schemas/cargo-lock.nix's crate names) the SAME
  # module@version pair legitimately appears TWICE in one go.sum (a bare
  # build-hash line and a "/go.mod"-suffixed line, see grammar/go-sum.nix's
  # header) -- this keeps only the bare build-hash line's own hash
  # (`isGoModOnly = false`), which is what a real module's CONTENT hash
  # is; the go.mod-only variant hashes a different, smaller artifact (just
  # go.mod itself) and isn't comparable to it.
  hashesByModuleVersion =
    string:
    let
      doc = parse string;
    in
    if doc == packrat.NO_MATCH then
      throw "not a valid go.sum"
    else
      builtins.listToAttrs (
        map (l: {
          name = "${l.module}@${l.version}";
          value = l.hash;
        }) (builtins.filter (l: !l.isGoModOnly) doc)
      );

  # Every module@version present in BOTH go.sum files with a DIFFERING
  # build-hash -- the actual supply-chain-integrity check this format's
  # structure enables (see this file's header for why go.sum has no
  # fetch-hash use the way every other lockfile grammar's checksums
  # example demonstrates instead). A module@version present in only one
  # file, or present in both with the SAME hash, is not reported --
  # neither is a red flag, just an ordinary dependency-set difference.
  findHashMismatches =
    stringA: stringB:
    let
      hashesA = hashesByModuleVersion stringA;
      hashesB = hashesByModuleVersion stringB;
      sharedKeys = builtins.filter (k: hashesB ? ${k}) (builtins.attrNames hashesA);
    in
    builtins.filter (m: m != null) (
      map (
        key:
        if hashesA.${key} != hashesB.${key} then
          {
            moduleVersion = key;
            hashA = hashesA.${key};
            hashB = hashesB.${key};
          }
        else
          null
      ) sharedKeys
    );
in
{
  inherit parse hashesByModuleVersion findHashMismatches;
}
