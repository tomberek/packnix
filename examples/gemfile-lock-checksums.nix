# Demonstrates the actual nixpkgs motivation for grammar/gemfile-lock.nix:
# turning a Gemfile.lock's CHECKSUMS section directly into the same sha256
# hash format bundix computes today via network access / nix-prefetch-git.
#
# Verified independently (not just asserted here): for a real nixpkgs
# package with both a Gemfile.lock (CHECKSUMS section) and a bundix-
# generated gemset.nix, `nix hash convert --to base32 --hash-algo sha256
# <hex sha256 from CHECKSUMS>` produces the EXACT string gemset.nix stores
# for that same gem -- e.g. actionmailer's CHECKSUMS entry
# "sha256=3b9270d8e19f0afb534b11c52f439937dc30028adcbbae2b244f3383ce75de4b"
# converts to "0jyyfp786csg4hmsxfywi8131p1pk51jzi8i9d9zn2lzw7c714iv", which
# is exactly what that package's gemset.nix has stored for actionmailer.
# So for any lockfile with a CHECKSUMS section (Bundler >=2.7), the
# gem-fetch hash bundix exists to compute is ALREADY in the lockfile --
# reading it needs no external tool, no network, no Ruby interpreter, just
# this grammar plus `builtins.hashFormat`/`nix hash convert`-equivalent
# base32 re-encoding (native `builtins.fromHexString`/`builtins.hashString`
# don't cover this specific hex-to-nix32 re-encode -- `nix hash convert`,
# or nixpkgs' `lib.strings`, needs to do that part; this example just shows
# the *parsing* half, which is this grammar's job).
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
    if doc == false then
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
