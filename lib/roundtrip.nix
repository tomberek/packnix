# A general round-trip property checker: for a given grammar/schema,
# generate N samples (lib/generate.nix) across N distinct seeds, feed
# each back through the SAME grammar/schema's parser (lib/packrat.nix's
# `run` or lib/valuewalk.nix's `run`/`compile`), and confirm the parser
# ACCEPTS what the generator produced FOR it -- the fixpoint property this
# whole generate/parse pair exists to have. This is deliberately a
# THIN wrapper, not a new engine: it just wires generate.nix's output
# into packrat.nix's/valuewalk.nix's own `run`, the same way a human
# would by hand (as done throughout this conversation before this file
# existed) -- formalized here so it can run at N=50-style scale in CI
# instead of N=5 ad hoc spot checks.
#
# Deliberately does NOT check "generated value equals original value"
# (there is no "original" here -- generation has no input to compare
# against, only "does the parser accept what was generated FOR it").
# Contrast with lib/valuewalk.nix's own README-style checks elsewhere in
# this repo (verify-fixtures.sh, verify-valuewalk-parity.sh), which DO
# have a real document to compare byte-identical output against; this
# file's property is narrower and more basic: soundness of the
# generator against its own grammar, not equivalence between two
# different grammars.
let
  packrat = import ./packrat.nix;
  valuewalk = import ./valuewalk.nix;
  generate = import ./generate.nix;
in
rec {
  # For a lib/packrat.nix STRING grammar. `ruleName`: which rule to
  # generate for and check (usually the grammar's top-level document
  # rule). Returns `{ allPassed; results = [ { seed; generated;
  # accepted; } ... ]; }` -- every individual result is kept (not just a
  # pass/fail count) so a failure can be reproduced from the report
  # alone, without re-running anything.
  checkPackratGrammar =
    {
      grammar,
      handlers ? { },
      ruleName,
      seedPrefix,
      numSamples ? 20,
      patternGenerators ? { },
      builtinParserGenerators ? { },
      maxDepth ? 4,
    }:
    let
      results = builtins.genList (
        i:
        let
          seed = "${seedPrefix}-${builtins.toString i}";
          generated = generate.generate {
            inherit
              grammar
              ruleName
              seed
              patternGenerators
              builtinParserGenerators
              maxDepth
              ;
          };
          parsed = (packrat.run { inherit grammar handlers; } 0 generated).${ruleName};
        in
        {
          inherit seed generated;
          accepted = parsed != packrat.NO_MATCH;
        }
      ) numSamples;
    in
    {
      allPassed = builtins.all (r: r.accepted) results;
      inherit results;
    };

  # For a lib/valuewalk.nix VALUE schema/grammar. Same shape as
  # checkPackratGrammar, but `accepted` checks against `null` (valuewalk's
  # failure sentinel, see that file's header) rather than packrat's
  # `NO_MATCH`.
  checkValuewalkGrammar =
    {
      grammar,
      ruleName,
      seedPrefix,
      numSamples ? 20,
      patternGenerators ? { },
      maxDepth ? 4,
    }:
    let
      results = builtins.genList (
        i:
        let
          seed = "${seedPrefix}-${builtins.toString i}";
          generated = generate.generate {
            inherit
              grammar
              ruleName
              seed
              patternGenerators
              maxDepth
              ;
          };
          parsed = (valuewalk.run { inherit grammar; } generated).${ruleName};
        in
        {
          inherit seed generated;
          accepted = parsed != null;
        }
      ) numSamples;
    in
    {
      allPassed = builtins.all (r: r.accepted) results;
      inherit results;
    };

  # Same as checkValuewalkGrammar, but for a single, UNNAMED schema (no
  # grammar attrset) -- mirrors lib/valuewalk.nix's own `compile` vs.
  # `run` split.
  checkValuewalkSchema =
    {
      schema,
      seedPrefix,
      numSamples ? 20,
      patternGenerators ? { },
      maxDepth ? 4,
    }:
    let
      matcher = valuewalk.compile schema;
      results = builtins.genList (
        i:
        let
          seed = "${seedPrefix}-${builtins.toString i}";
          generated = generate.generateFromSchema {
            inherit
              schema
              seed
              patternGenerators
              maxDepth
              ;
          };
        in
        {
          inherit seed generated;
          accepted = matcher generated != null;
        }
      ) numSamples;
    in
    {
      allPassed = builtins.all (r: r.accepted) results;
      inherit results;
    };
}
