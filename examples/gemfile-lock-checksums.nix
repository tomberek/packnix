# Demonstrates the actual nixpkgs motivation for grammar/gemfile-lock.nix:
# turning a Gemfile.lock's CHECKSUMS section directly into the same sha256
# hash format bundix computes today via network access / nix-prefetch-git.
#
# `nix hash convert --to base32 --hash-algo sha256 <hex sha256 from
# CHECKSUMS>` produces the EXACT string a bundix-generated gemset.nix
# stores for that same gem (verified against a real nixpkgs package's
# paired Gemfile.lock/gemset.nix). So for any lockfile with a CHECKSUMS
# section (Bundler >=2.7), the gem-fetch hash bundix exists to compute is
# ALREADY in the lockfile -- reading it needs no external tool, no
# network, no Ruby interpreter, just this grammar plus a hex-to-nix32
# base32 re-encode (not covered by native `builtins.fromHexString`/
# `builtins.hashString`; `nix hash convert` or nixpkgs' `lib.strings`
# does that part -- this example only shows the *parsing* half).
#
# Run with:
#   nix eval --impure --expr '
#     let
#       packrat = import ../lib/packrat.nix;
#       g = import ./gemfile-lock-checksums.nix;
#     in g.hashesByGemName (builtins.readFile ../data/example-Gemfile.lock)
#   ' --json
let
  packrat = import ../lib/packrat.nix;
  gemfileLock = import ../grammar/gemfile-lock.nix;

  parse =
    string:
    (packrat.run {
      grammar = gemfileLock.grammar;
      handlers = gemfileLock.handlers;
    } 0 string).DOCUMENT;

  # { <gem name> = <hex sha256 from CHECKSUMS>; ... } -- exactly the shape
  # a `bundlerEnv`-replacement function would need to build each gem's
  # `builtins.fetchurl { url = "https://rubygems.org/gems/${name}-${version}.gem"; sha256 = ...; }`,
  # once the hex string is re-encoded to Nix's base32 (that re-encoding
  # step itself needs `nix hash convert` or nixpkgs' own hash-conversion
  # library code -- deliberately not reimplemented here, since it's a
  # generic base16-to-base32 conversion, not a Gemfile.lock parsing
  # concern).
  hashesByGemName =
    string:
    let
      doc = parse string;
    in
    if doc == packrat.NO_MATCH then
      throw "not a valid Gemfile.lock"
    else if doc.checksums == null then
      throw "this Gemfile.lock has no CHECKSUMS section (needs Bundler >= 2.7)"
    else
      builtins.listToAttrs (
        map (c: {
          name = c.name;
          value = c.sha256;
        }) (builtins.filter (c: c.sha256 != null) doc.checksums)
      );
in
{
  inherit parse hashesByGemName;
}
