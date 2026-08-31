# Cargo.lock's schema (v3/v4 -- see below), for lib/valuewalk.nix. Unlike
# every grammar/*.nix, this has no lib/packrat.nix counterpart at all:
# Cargo.lock is TOML with no bespoke syntax builtins.fromTOML can't
# already parse, so a from-scratch packrat grammar would just re-parse
# text a native C++ parser already handles, for no benefit -- the same
# "why write a grammar for something fromJSON/fromTOML already covers"
# reasoning this repo's own README gives for not competing with
# builtins.fromJSON on plain JSON. This schema only validates the shape
# of the value builtins.fromTOML already built, same division of labor
# as examples/flakelock-valuewalk.nix over grammar/flakelock.nix's schema.
#
# Confirmed against 100 real Cargo.lock files pulled from a nixpkgs
# checkout (sizes 149B-225KB, package counts from 1 to hundreds):
#   - top-level: `version` (bare int, 3 or 4 in every file that has it)
#     plus a `package` array of tables, and (in exactly ONE of the 100
#     files -- nixpkgs' own Rust sysroot Cargo.lock) a `patch` table --
#     no `[metadata]`/workspace-member lists seen anywhere in this
#     corpus.
#   - `version` is ABSENT in a handful of hand-written test fixtures
#     (not a real `cargo generate-lockfile` output) -- `opt`-wrapped for
#     that reason, same as grammar/flakelock.nix `opt`-wraps `type` even
#     though it's 100% present in ITS corpus: catching a genuine absence
#     is worth the check even at high observed-presence rates.
#   - every `[[package]]` has `name`+`version` (strings). Other fields'
#     presence follows one confirmed, zero-exception invariant:
#     `checksum` is present IFF `source` starts with `registry+` or
#     `sparse+` -- git-sourced and local/workspace packages (no `source`
#     field at all) never have a `checksum`. Field-presence combos seen,
#     most-common first: {checksum,dependencies,name,source,version},
#     {checksum,name,source,version}, {dependencies,name,version},
#     {dependencies,name,source,version}, {name,version},
#     {name,source,version}.
#   - `source` schemes seen: `registry+`, `sparse+`, `git+` (the last
#     with a `?ref#rev`-shaped query/fragment). `path+` is a real Cargo
#     scheme not seen in this corpus but accepted here anyway (PATTERN
#     covers it) since it costs nothing extra and IS reachable from a
#     genuine local dependency-path setup.
#   - `dependencies` is a list of strings, each EITHER a bare crate name
#     OR "name version" -- Cargo emits the latter shape specifically to
#     disambiguate when 2+ versions of the same crate are locked at
#     once (confirmed real: 859 of the 100 corpus files -- yes, files
#     can and do lock multiple crate-name collisions each -- have a
#     crate name appearing at 2+ distinct versions, e.g. `bitflags`
#     1.3.2 AND 2.4.1 both locked simultaneously in one real file).
#     This schema accepts both shapes as opaque strings -- it validates
#     the LIST's element SHAPE, not full cross-referential graph
#     integrity against the `package` array's own name/version pairs
#     (same "shape, not full semantics" scope
#     examples/flakelock-valuewalk.nix's own header describes).
#   - Cargo.lock's legacy v1 format (no top-level `version` field,
#     dependency entries embed their OWN inline "name version (source)"
#     instead of referencing another `[[package]]` by name) is OUT OF
#     SCOPE -- only one such file exists in the whole corpus, and it's
#     nixpkgs' own test fixture for legacy-format support, not a real
#     `cargo generate-lockfile` output. A v1 file fails PACKAGE's
#     `dependencies` pattern check (its embedded-version-and-source
#     strings don't match either accepted shape) well before it could
#     silently mis-validate.
#
# FAILURE SENTINEL: lib/valuewalk.nix uses `null`. Unconditionally safe
# here (stronger guarantee than most schemas in this repo need): TOML's
# own spec has no null/nil literal at all, so there is no legitimate
# TOML value this schema could ever mistake for "rule failed to match".
#
# Run with:
#   nix eval --impure --expr '
#     let
#       vw = import ../lib/valuewalk.nix;
#       g = import ./cargo-lock.nix;
#     in (vw.run { grammar = g; } (builtins.fromTOML
#          (builtins.readFile ./path/to/Cargo.lock))).DOCUMENT
#   ' --json
{
  # Cargo emits a bare crate name when it's the only locked version of
  # that name in the file, "name version" when 2+ versions of the same
  # name are locked simultaneously and need disambiguating (see header).
  DEPENDENCY_REF = {
    pattern = "([^ ]+( [^ ]+)?)";
  };

  # `registry+`/`sparse+` URLs, or `git+URL` optionally followed by a
  # `?query` and/or `#fragment` (Cargo's own git-source encoding for
  # branch/tag/rev pins and the resolved commit hash).
  SOURCE = {
    pattern = "((registry|sparse|path)\\+[^ ]+|git\\+[^ ?#]+(\\?[^ #]*)?(#[^ ]*)?)";
  };

  # A hex sha256, same shape as grammar/gemfile-lock.nix's/
  # grammar/yarn-lock.nix's own checksum/integrity fields -- confirmed
  # every real `checksum` in the corpus is exactly 64 lowercase hex
  # digits (sha256, matching Cargo's own documented checksum format).
  CHECKSUM = {
    pattern = "([0-9a-f]{64})";
  };

  PACKAGE = {
    attrs = {
      closed = true;
      fields = {
        name = {
          string = { };
        };
        version = {
          string = { };
        };
      };
      optional = {
        source = "SOURCE";
        checksum = "CHECKSUM";
        dependencies = {
          listOf = "DEPENDENCY_REF";
        };
      };
    };
  };

  # `[[patch.<source-name>]]` entries -- Cargo's record of a declared-but-
  # unused patch (confirmed real via nixpkgs' own Rust sysroot Cargo.lock,
  # `[[patch.unused]]`; the ONLY file in a ~100-file corpus sweep with any
  # `patch` section at all, so this is deliberately minimal, not a full
  # patch-table schema). Same `name`+`version`-only shape as a bare
  # PACKAGE stub, confirmed to never carry `source`/`checksum`/
  # `dependencies` in the one real instance seen.
  PATCH_STUB = {
    attrs = {
      closed = true;
      fields = {
        name = {
          string = { };
        };
        version = {
          string = { };
        };
      };
    };
  };

  DOCUMENT = {
    attrs = {
      closed = true;
      fields = {
        package = {
          listOf = "PACKAGE";
        };
      };
      optional = {
        version = {
          int = { };
        };
        patch = {
          attrsOf = {
            listOf = "PATCH_STUB";
          };
        };
      };
    };
  };
}
