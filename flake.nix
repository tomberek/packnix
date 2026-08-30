{
  description = "A packrat/PEG parsing engine in pure Nix, plus grammars for JSON, YAML, TSV, nix flake.lock, ATerm/Nix .drv, PEP 508, Poetry version constraints, Ruby's Gemfile/Gemfile.lock/yarn.lock, and fromJSON/fromTOML-based schemas for Cargo.lock, poetry.lock, package-lock.json, and uv.lock.";

  # Deliberately no `nixpkgs` input: everything this flake exposes (the
  # library itself, and the one check below) is pure Nix-language
  # evaluation with no need for a compiler/interpreter from nixpkgs. The
  # `checks` output below uses `builtins.derivation` directly instead of
  # `pkgs.runCommand` for the same reason -- see its comment.
  outputs =
    { self }:
    let
      # No `flake-utils`/nixpkgs input to derive this list from, so it's
      # spelled out by hand -- the same four systems most flake-utils-free
      # flakes hardcode. `nix flake check` only builds `checks.<system>.*`
      # for the system it's actually running on, so this only widens which
      # systems CAN evaluate the check, not which one a given CI run does.
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems =
        f:
        builtins.listToAttrs (
          map (system: {
            name = system;
            value = f system;
          }) systems
        );

      # tests.nix is a pure Nix evaluation (see its own header comment): no
      # filesystem/network access beyond what `import` already reads from
      # this very checkout. Forcing `.allPassed` here, at flake-evaluation
      # time, and throwing if it's false, means a broken combinator suite
      # fails `nix flake check` during EVALUATION, before any derivation
      # would even need to build.
      testsResult = import ./tests.nix;
      testsOk =
        if testsResult.allPassed then true else throw "packnix: tests.nix reported allPassed = false";
    in
    {
      lib = {
        packrat = import ./lib/packrat.nix;
        valuewalk = import ./lib/valuewalk.nix;
        generate = import ./lib/generate.nix;
        roundtrip = import ./lib/roundtrip.nix;
        regexGenerate = import ./lib/regex-generate.nix;
        jsonTomlSafety = import ./lib/json-toml-safety.nix;
        grammars = {
          json = import ./grammar/json.nix;
          yaml = import ./grammar/yaml.nix;
          tsv = import ./grammar/tsv.nix;
          flakelock = import ./grammar/flakelock.nix;
          gemfileLock = import ./grammar/gemfile-lock.nix;
          gemfile = import ./grammar/gemfile.nix;
          aterm = import ./grammar/aterm.nix;
          drv = import ./grammar/drv.nix;
          pep508 = import ./grammar/pep508.nix;
          yarnLock = import ./grammar/yarn-lock.nix;
          poetrySemver = import ./grammar/poetry-semver.nix;
        };
        # Schemas for lib/valuewalk.nix with no lib/packrat.nix
        # counterpart -- see schemas/cargo-lock.nix's own header for why.
        schemas = {
          cargoLock = import ./schemas/cargo-lock.nix;
          poetryLock = import ./schemas/poetry-lock.nix;
          packageLock = import ./schemas/package-lock.nix;
          uvLock = import ./schemas/uv-lock.nix;
        };
      };

      # Only `tests.nix`'s pure `allPassed` suite is wired in here, NOT the
      # four verify-*.sh scripts (verify-fixtures.sh,
      # verify-json-toml-commit.sh, verify-valuewalk-parity.sh,
      # verify-roundtrip.sh). Those scripts each shell out to `nix eval`
      # themselves; running THAT inside a `nix flake check` build sandbox
      # would require the sandbox to itself invoke `nix`, which only works
      # if the building Nix daemon has the `recursive-nix` experimental
      # feature enabled -- not a safe assumption for every machine/CI
      # runner that might run `nix flake check` on this flake, and a real
      # deployment cost for a repo whose whole ethos so far has been "pure
      # Nix eval, no extra features required". CI (.github/workflows/ci.yml)
      # already runs all four scripts directly on every push/PR, so their
      # coverage isn't lost -- it's just not duplicated (and made fragile)
      # inside `nix flake check`.
      #
      # The check itself does no I/O beyond evaluating `tests.nix` (already
      # forced above, at flake-evaluation time, before any system-specific
      # derivation exists) and writing one constant string to `$out` --
      # `builtins.derivation` directly, rather than `pkgs.runCommand`,
      # because pulling in a `nixpkgs` input (a many-hundred-MB fetch) just
      # for `runCommand`'s ergonomics would cost far more than it saves for
      # a single one-line builder command.
      checks = forAllSystems (system: {
        tests = builtins.derivation {
          name = "packnix-tests-check";
          inherit system;
          builder = "/bin/sh";
          args = [
            "-c"
            "echo ${if testsOk then "ok" else "unreachable"} > $out"
          ];
        };
      });
    };
}
