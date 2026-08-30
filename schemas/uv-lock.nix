# uv's `uv.lock` schema (schema `version = 1`), for lib/valuewalk.nix. Same
# reasoning as schemas/cargo-lock.nix/schemas/poetry-lock.nix/
# schemas/package-lock.nix: uv.lock is plain TOML with nothing bespoke
# `builtins.fromTOML` can't already parse, so no lib/packrat.nix grammar
# counterpart exists here either -- this schema only validates the shape
# of the value `fromTOML` already built.
#
# UNLIKE Cargo.lock/poetry.lock/package-lock.json, nixpkgs itself has no
# uv.lock consumer -- the real consumer confirmed here is the external
# `uv2nix` project (github.com/pyproject-nix/uv2nix), read directly
# (`lib/lock1.nix`'s `parseLock`/`parsePackage`, `lib/build.nix`'s
# `fetchurl` call) rather than a nixpkgs source file. See below for why
# this one still has a real use case worth building against.
#
# Confirmed against 11 real uv.lock files (uv2nix's own public test
# fixtures -- `lib/fixtures/{conflicts,workspace,dependency-groups,
# git-subdirectory,with-supported-environments,dynamic-version,virtual,
# only-wheels,local-index-sdist}/uv.lock`, small and MIT-licensed, plus
# several real project lockfiles pulled from local checkouts, screened
# for private/internal references before use -- none survived screening
# intact, so uv2nix's own fixtures are the ones this schema and
# data/example-uv.lock are actually built from) PLUS uv2nix's own
# `lib/lock1.nix` source read directly to confirm which facts are
# schema-version guarantees vs artifacts of this small sample:
#   - `version` (bare int, `parseLock`'s own `assert version == 1` --
#     schema versions above 1 are OUT OF SCOPE, same "confirmed corpus
#     facts, not aspirational spec coverage" convention as every other
#     schema in this repo) and `requires-python` are the only two
#     top-level fields `parseLock` reads WITHOUT a `?` default --
#     REQUIRED. Every other top-level field (`manifest`, `package`,
#     `resolution-markers`, `supported-markers`, `required-markers`,
#     `options`, `conflicts`, `revision`) defaults in `parseLock`'s own
#     signature, confirmed optional here too.
#   - `[[package]]` entries: `name` and `source` are the only two
#     fields `parsePackage` reads without a `?` default -- REQUIRED.
#     `version` defaults to `"0.0.0"` in `parsePackage` itself (uv
#     omits it entirely for a package with a build-time-dynamic
#     version -- confirmed real: `lib/fixtures/dynamic-version/uv.lock`
#     has no `version` key on its one package at all), so `version` is
#     modeled `optional` here, matching that default rather than
#     REQUIRED.
#   - `source` is a discriminated attrset (only ONE of `registry` |
#     `git` | `editable` | `virtual` | `directory` | `path` | `url`
#     ever present per `parsePackage`'s own `isLocalPackage`/
#     `getLocalPath` helpers, which check exactly `editable`/
#     `directory`/`virtual`) -- confirmed real in this corpus: registry
#     (bare string, a PyPI/index URL), git (a `git+`-prefixed URL with
#     a `#<rev>` fragment, confirmed via `git-subdirectory`), editable
#     (a relative path, confirmed via `conflicts`/`workspace`), and
#     virtual (a relative path, confirmed via `dynamic-version`/
#     `virtual` -- a project with no build-system, i.e. not even
#     installable, distinct from `editable`). `directory`/`path`/`url`
#     are real per `lib/build.nix`'s own handling but not seen in this
#     corpus -- SOURCE's `closed = false` passes them through
#     unvalidated rather than rejecting a document for having them.
#   - `sdist`/`wheels[]` entries: `hash` is present on every single
#     one in this corpus (confirmed: `grep -c 'hash = "sha256:'` finds
#     one match per `url = ` line, no exceptions), always
#     `"sha256:<hex>"` -- the SAME prefixed-hex shape schemas/
#     poetry-lock.nix's own hash strings use (see below for why this
#     one needs no conversion at all, simpler than poetry.lock's
#     prefix-strip). `lib/build.nix`'s own fallback path for a
#     hash-less wheel (`lib.warn ... builtins.fetchurl` with no `hash`)
#     confirms a real, if unseen-in-corpus, hash-less case exists --
#     `hash` is modeled `optional` on WHEEL_ENTRY/SDIST for that
#     reason, matching `parseWheel`'s own `hash ? null`.
#   - `dependencies`/`optional-dependencies.<name>`/
#     `dev-dependencies.<name>` entries (`parseDependency`) and
#     `[package.metadata]`'s `requires-dist`/`requires-dev.<group>`
#     entries (`parseMetadata`'s own `parseRequires`) both read `name`
#     as the only REQUIRED field, plus a partially-overlapping set of
#     optional ones: `marker` (a PEP 508 marker string, confirmed real
#     on both), `version` (a dependency edge, present only when uv
#     itself judged it AMBIGUOUS) vs `specifier`/`git`/`editable` (a
#     metadata declaration's own fields), and `extra`/`extras` (a list
#     of extra names, confirmed real in 2 corpus files' `apache-
#     airflow`/`fastapi`/`sqlalchemy` dependency entries -- uv spells
#     this singular on a dependency edge, plural on a metadata
#     declaration). Modeled as ONE shared ENTRY covering the union of
#     both field sets rather than two near-duplicate rules: since both
#     are already `closed = false` (see below), a rule recognizing the
#     other call site's fields validates no more, and no less, than
#     two separate rules would.
#   - `conflicts` (a list of lists of `{package; extra;}` OR
#     `{package; group;}` tables, confirmed real via the `conflicts`
#     fixture itself) and `[options]` (`exclude-newer`/
#     `resolution-mode`/`prerelease-mode`, all optional per
#     `parseOptions`'s own defaults) are modeled to the same depth
#     `parseLock`/`parseOptions` themselves read, no deeper.
#
# FAILURE SENTINEL: lib/valuewalk.nix uses `null`, unconditionally safe
# here for the same reason as schemas/cargo-lock.nix/schemas/poetry-lock.nix:
# TOML has no null/nil literal at all.
#
# Run with:
#   nix eval --impure --expr '
#     let
#       vw = import ../lib/valuewalk.nix;
#       g = import ./uv-lock.nix;
#     in (vw.run { grammar = g; } (builtins.fromTOML
#          (builtins.readFile ./path/to/uv.lock))).DOCUMENT
#   ' --json
{
  # Only ONE of these keys is ever present per entry (registry/git/
  # editable/virtual confirmed real in this corpus; directory/path/url
  # real per lib/build.nix but unseen here) -- see header. `closed =
  # false` passes an unmodeled source kind through rather than
  # rejecting the whole document for it.
  SOURCE = {
    attrs = {
      closed = false;
      optional = {
        registry = {
          string = { };
        };
        git = {
          string = { };
        };
        editable = {
          string = { };
        };
        virtual = {
          string = { };
        };
      };
    };
  };

  # Shared by WHEEL_ENTRY/SDIST_ENTRY -- see header for why `hash` is
  # optional despite being present on every corpus file's entries.
  HASH = {
    pattern = "(sha256:.+)";
  };

  WHEEL_ENTRY = {
    attrs = {
      closed = false;
      fields = {
        url = {
          string = { };
        };
      };
      optional = {
        hash = "HASH";
        size = {
          int = { };
        };
      };
    };
  };

  SDIST_ENTRY = {
    attrs = {
      closed = false;
      optional = {
        url = {
          string = { };
        };
        hash = "HASH";
        size = {
          int = { };
        };
      };
    };
  };

  # Shared shape for `dependencies`/`optional-dependencies.<name>`/
  # `dev-dependencies.<name>` list entries AND `[package.metadata]`'s
  # `requires-dist`/`requires-dev.<group>` entries -- see header for why
  # one rule covers both call sites' (partially overlapping) fields.
  ENTRY = {
    attrs = {
      closed = false;
      fields = {
        name = {
          string = { };
        };
      };
      optional = {
        marker = {
          string = { };
        };
        version = {
          string = { };
        };
        source = "SOURCE";
        extra = {
          listOf = {
            string = { };
          };
        };
        specifier = {
          string = { };
        };
        git = {
          string = { };
        };
        editable = {
          string = { };
        };
        extras = {
          listOf = {
            string = { };
          };
        };
      };
    };
  };

  METADATA = {
    attrs = {
      closed = false;
      optional = {
        "requires-dist" = {
          listOf = "ENTRY";
        };
        "requires-dev" = {
          attrsOf = {
            listOf = "ENTRY";
          };
        };
        "provides-extras" = {
          listOf = {
            string = { };
          };
        };
      };
    };
  };

  PACKAGE = {
    attrs = {
      # NOT fully closed -- resolution-markers, and any future
      # top-level package field this schema doesn't model, pass
      # through unvalidated. Same scope call as every other lockfile
      # schema in this repo.
      closed = false;
      fields = {
        name = {
          string = { };
        };
        source = "SOURCE";
      };
      optional = {
        # Absent for a package with a build-time-dynamic version --
        # see header. Callers needing a version must handle this case
        # (uv2nix's own parsePackage defaults to "0.0.0").
        version = {
          string = { };
        };
        sdist = "SDIST_ENTRY";
        wheels = {
          listOf = "WHEEL_ENTRY";
        };
        dependencies = {
          listOf = "ENTRY";
        };
        "optional-dependencies" = {
          attrsOf = {
            listOf = "ENTRY";
          };
        };
        "dev-dependencies" = {
          attrsOf = {
            listOf = "ENTRY";
          };
        };
        metadata = "METADATA";
      };
    };
  };

  DOCUMENT = {
    attrs = {
      closed = false;
      fields = {
        version = {
          int = { };
        };
        "requires-python" = {
          string = { };
        };
      };
      optional = {
        revision = {
          int = { };
        };
        manifest = {
          attrs = {
            closed = false;
            optional = {
              members = {
                listOf = {
                  string = { };
                };
              };
            };
          };
        };
        package = {
          listOf = "PACKAGE";
        };
        options = {
          attrs = {
            closed = false;
            optional = {
              "exclude-newer" = {
                string = { };
              };
              "resolution-mode" = {
                string = { };
              };
              "prerelease-mode" = {
                string = { };
              };
            };
          };
        };
      };
    };
  };
}
