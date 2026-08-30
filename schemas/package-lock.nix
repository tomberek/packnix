# npm's `package-lock.json` schema (lockfileVersion 2/3), for
# lib/valuewalk.nix. Same reasoning as schemas/cargo-lock.nix/
# schemas/poetry-lock.nix: package-lock.json is plain JSON with nothing
# bespoke `builtins.fromJSON` can't already parse, so no
# lib/packrat.nix grammar counterpart exists here either.
#
# Confirmed against 43 real package-lock.json files pulled from a
# nixpkgs checkout, screened for private/internal references before
# use (0 found):
#   - `lockfileVersion` distribution: 39 files at 3, 3 at 2, 1 at the
#     legacy v1 (flat `dependencies` tree, no `packages` key at all --
#     npm itself deprecated this format; OUT OF SCOPE, same "confirmed
#     corpus facts, not aspirational spec coverage" convention as every
#     other schema/grammar in this repo. `version`/`lv` are NOT checked
#     by this schema at all -- a caller can read `lockfileVersion`
#     directly off the parsed value if it needs to gate on it).
#   - `packages` (v2/v3's shape) is an attrset keyed by NODE_MODULES
#     PATH, not package name (e.g. `"node_modules/@eslint/core"`, or a
#     workspace member's own relative path like
#     `"packages/eslint-config-eslint"`) -- confirmed: the SAME package
#     name can legitimately appear at multiple different paths in one
#     file (nested `node_modules/` for conflicting version
#     requirements). The bare `""` key is special: the project's own
#     root package (has `dependencies`/`devDependencies`/etc, never
#     `resolved`/`integrity`).
#   - `resolved` (a URL, almost always `https://` -- 19499 of 19503
#     schemed values in this corpus; also confirmed `http://` and
#     `git+ssh://...#<rev>` for git-sourced deps, and a bare relative
#     path for a workspace-linked package) and `integrity` (an SRI
#     string, `sha512-<base64>` confirmed dominant, `sha1-<base64>`
#     also real -- SAME two algorithms `grammar/yarn-lock.nix` already
#     handles for Yarn's own `integrity` field) are BOTH genuinely
#     OPTIONAL, not a reliable pair -- confirmed real, un-paired cases:
#     a git-sourced package has `resolved` but no `integrity` (nothing
#     for npm's registry to have computed a hash against); a BUNDLED
#     dependency (`inBundle = true`, ships inside its parent's own
#     tarball, never independently fetched) or a monorepo WORKSPACE
#     MEMBER (a local package, not a `node_modules/` fetch at all) has
#     NEITHER. So unlike Cargo.lock's checksum<->source invariant, this
#     schema does not assert any presence relationship between the two
#     -- a caller extracting fetch hashes (see
#     examples/package-lock-checksums.nix) must check BOTH are present
#     before treating an entry as "fetchable".
#   - Every OTHER package-level field seen in this corpus (`bin`,
#     `license`, `engines`, `dependencies`, `devDependencies`,
#     `peerDependencies`, `peerDependenciesMeta`,
#     `optionalDependencies`, `bundleDependencies`, `dev`, `optional`,
#     `devOptional`, `peer`, `funding`, `deprecated`,
#     `hasInstallScript`, `cpu`, `os`, `libc`, `workspaces`, `name`,
#     `inBundle`, `link`) is DELIBERATELY NOT modeled -- npm's package
#     metadata surface is far larger than Cargo.lock's/poetry.lock's
#     (362 distinct field-presence combinations found across this
#     43-file corpus alone), none of it needed for the
#     checksum-extraction payoff this schema exists for. `closed =
#     false` passes all of it through unvalidated, same scope call
#     already made twice for Cargo.lock's/poetry.lock's own
#     `dependencies` fields.
#
# FAILURE SENTINEL: lib/valuewalk.nix uses `null`. UNLIKE
# schemas/cargo-lock.nix/schemas/poetry-lock.nix (TOML, no null literal
# at all), JSON DOES have `null` as a real value -- but `name`/
# `version`/`resolved`/`integrity` are never legitimately `null` in any
# real file in this corpus (npm has no reason to ever emit a null
# there), so this is a per-schema-safe choice, not a universal one --
# see lib/valuewalk.nix's own header for what that distinction means.
#
# Run with:
#   nix eval --impure --expr '
#     let
#       vw = import ../lib/valuewalk.nix;
#       g = import ./package-lock.nix;
#     in (vw.run { grammar = g; } (builtins.fromJSON
#          (builtins.readFile ./path/to/package-lock.json))).DOCUMENT
#   ' --json
{
  PACKAGE_ENTRY = {
    attrs = {
      # Not fully closed -- see header. Nothing is REQUIRED either: the
      # root ("") entry, bundled dependencies, and workspace members are
      # all real, valid entries with different (non-overlapping) field
      # subsets, so there is no field every entry shares.
      closed = false;
      optional = {
        version = {
          string = { };
        };
        resolved = {
          string = { };
        };
        integrity = {
          pattern = "((sha1|sha512)-.+)";
        };
      };
    };
  };

  DOCUMENT = {
    attrs = {
      closed = false;
      fields = {
        packages = {
          attrsOf = "PACKAGE_ENTRY";
        };
      };
    };
  };
}
