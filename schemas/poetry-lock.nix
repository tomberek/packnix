# Poetry's `poetry.lock` schema, for lib/valuewalk.nix. Same reasoning as
# schemas/cargo-lock.nix: `poetry.lock` is plain TOML with nothing
# bespoke `builtins.fromTOML` can't already parse, so no
# lib/packrat.nix grammar counterpart exists here either -- this schema
# only validates the shape of the value `fromTOML` already built.
#
# Confirmed against 5 real poetry.lock files (nixpkgs' rmfuse, nixops,
# and poetry2nix's own vendored copy; two more pulled from live
# /nix/store source checkouts, screened for private/internal references
# before use) spanning `lock-version` 1.1, 2.0, and 2.1, PLUS Poetry's
# own `locker.py` source (github.com/python-poetry/poetry,
# `src/poetry/packages/locker.py`) read directly to confirm which facts
# are stable format guarantees vs artifacts of this small sample:
#
#   - THREE hash-storage generations exist, per `locker.py`'s own
#     `locked_repository` (its comment: "Storing of package files and
#     hashes has been through a few generations in the lockfile, we can
#     read them all"), checked in this priority order:
#       1. per-package `files` (current/preferred, lock-version 2.x) --
#          confirmed real in 2 of the 5 corpus files.
#       2. `metadata.files[name]` (intermediate, lock-version 1.x) --
#          confirmed real in 3 of the 5 corpus files. A dict keyed by
#          package name, each value a list of `{file; hash;}` tables --
#          IDENTICAL shape to a `[[package]]`'s own `files` field, just
#          relocated to a top-level, name-keyed table instead of being
#          inline on the package.
#       3. `metadata.hashes` (oldest, no filenames at all -- just `{name
#          = [hash, ...];}`) -- NOT seen in this corpus (no lock-version
#          old enough was found), but confirmed real via `locker.py`'s
#          own source; not modeled here since no real sample exists to
#          verify the shape against, matching this repo's own
#          "confirmed corpus facts, not aspirational spec coverage"
#          convention -- FILES_LEGACY_HASHES is intentionally absent.
#   - `name`, `version`, `description`, `optional`, `python-versions`
#     are confirmed present on EVERY package in EVERY corpus file, and
#     `locker.py`'s own `_dump_package`/`locked_repository` both
#     read/write them unconditionally (`info["optional"]`, no
#     `.get()`) -- required, not `opt`, per this repo's own convention
#     of matching an unconditional-read guarantee to a required field.
#   - `category` (bare string, e.g. `"main"`) is the lock-version 1.x/
#     2.0 field; `groups` (list of strings, e.g. `["dev","testing"]`)
#     is its lock-version 2.1 replacement -- confirmed both real,
#     mutually exclusive per corpus file (never co-occurring), so
#     modeled as a `choice` on optional presence rather than assuming
#     one or the other.
#   - `dependencies` (a table: dependency name -> constraint) is
#     DELIBERATELY NOT modeled beyond "some attrset" (`closed = false`
#     lets it and `extras`/`markers`/`marker`/`requirements` pass
#     through unvalidated) -- a real dependency VALUE is heterogeneous
#     (a bare version-constraint string, an inline `{version;
#     markers?; extras?; optional?;}` table, OR a list of such tables
#     for multiple marker-gated alternatives -- confirmed real: a
#     corpus file's own `pytest` package locks `more-itertools` via
#     TWO marker-gated version alternatives), and none of that detail
#     is needed for the checksum-extraction payoff this schema exists
#     for. Same scope call schemas/cargo-lock.nix already makes for
#     ITS `dependencies` field.
#   - `source` (git/file/directory/url) is confirmed real (every
#     `[package.source]` seen in this corpus is `type = "git"`, with
#     `reference`/`resolved_reference`/`url`, all git-sourced), and
#     ABSENT for ordinary registry-sourced packages (PyPI/index) --
#     `opt`-wrapped, same as Cargo.lock's own git-vs-registry split.
#     `develop` (bool) only ever accompanies a `directory`/`git`
#     source per `locker.py`'s own write logic.
#
# Because a real hash can live in EITHER of two structurally different
# places depending on `lock-version` (per-package `files`, or a
# top-level `metadata.files` table this schema's caller must cross-
# reference by package NAME -- there is no per-package pointer into
# it), extraction needs BOTH DOCUMENT.package[].files AND
# DOCUMENT.metadata.files to be validated, and a caller (see
# examples/poetry-lock-checksums.nix) merges them: prefer a package's
# own `files` if present, else look up `metadata.files.${name}`.
#
# FAILURE SENTINEL: lib/valuewalk.nix uses `null`, unconditionally safe
# here for the same reason as schemas/cargo-lock.nix: TOML has no
# null/nil literal at all.
#
# Run with:
#   nix eval --impure --expr '
#     let
#       vw = import ../lib/valuewalk.nix;
#       g = import ./poetry-lock.nix;
#     in (vw.run { grammar = g; } (builtins.fromTOML
#          (builtins.readFile ./path/to/poetry.lock))).DOCUMENT
#   ' --json
{
  # `{file; hash;}` -- identical shape whether reached via a package's
  # own `files` field or the top-level `metadata.files.<name>` table.
  FILE_HASH = {
    attrs = {
      closed = true;
      fields = {
        file = {
          string = { };
        };
        hash = {
          string = { };
        };
      };
    };
  };

  SOURCE = {
    attrs = {
      closed = true;
      fields = {
        type = {
          string = { };
        };
        url = {
          string = { };
        };
      };
      optional = {
        reference = {
          string = { };
        };
        resolved_reference = {
          string = { };
        };
        subdirectory = {
          string = { };
        };
      };
    };
  };

  PACKAGE = {
    attrs = {
      # NOT fully closed-validated: `dependencies`/`extras`/`markers`/
      # `marker`/`requirements` are real fields this schema deliberately
      # doesn't model (see header) -- `closed = false` passes them
      # (and anything else unrecognized) through UNCHANGED instead of
      # rejecting the package for having them.
      closed = false;
      fields = {
        name = {
          string = { };
        };
        version = {
          string = { };
        };
        description = {
          string = { };
        };
        optional = {
          bool = { };
        };
        "python-versions" = {
          string = { };
        };
      };
      optional = {
        files = {
          listOf = "FILE_HASH";
        };
        category = {
          string = { };
        };
        groups = {
          listOf = {
            string = { };
          };
        };
        source = "SOURCE";
        develop = {
          bool = { };
        };
      };
    };
  };

  METADATA = {
    attrs = {
      # Not fully closed either: `content-hash` and other bookkeeping
      # fields are real but irrelevant to checksum extraction.
      closed = false;
      fields = {
        "lock-version" = {
          string = { };
        };
      };
      optional = {
        # Lock-version 1.x's hash location -- a table keyed by package
        # NAME (not indexed by the package list itself), each value the
        # same FILE_HASH shape a lock-version 2.x package's own `files`
        # field uses. See header for why a caller must check BOTH this
        # and each package's own `files`.
        files = {
          attrsOf = {
            listOf = "FILE_HASH";
          };
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
        metadata = "METADATA";
      };
      optional = {
        # Project-level extras-group declaration (from `pyproject.toml`,
        # NOT a per-package field -- confirmed real and distinct from
        # PACKAGE's own `extras` pass-through field, e.g. `{ tomli =
        # ["tomli"]; }`), irrelevant to checksum extraction -- accepted
        # as an unconstrained attrset rather than rejecting the whole
        # document for having it.
        extras = {
          attrsOf = {
            listOf = {
              string = { };
            };
          };
        };
      };
    };
  };
}
