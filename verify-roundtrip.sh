#!/usr/bin/env bash
# Round-trip gate: for each grammar/schema listed below, generate N
# samples (lib/generate.nix) and confirm the SAME grammar/schema's own
# parser (lib/packrat.nix's run, or lib/valuewalk.nix's run/compile)
# accepts every one -- the fixpoint property lib/roundtrip.nix formalizes
# (see that file's header for exactly what is/isn't checked: acceptance,
# not equality against some "original").
#
# Scope: every grammar whose ONLY blocker was lack of wiring, not a real
# generation gap, is covered here. Two grammars remain genuinely out of
# reach: grammar/gemfile.nix and grammar/yaml.nix use `not`/`and` for
# real structural disambiguation (excluding reserved words, asserting a
# following character) that lib/generate.nix has no general synthesis
# strategy for. grammar/gemfile-lock.nix and grammar/yarn-lock.nix are
# ALSO currently excluded -- not because of and/not, but because both
# use `{ eof = {}; }` inside a non-terminal `choice` (their shared
# `lineEnd` idiom), which lib/generate.nix's `eof` case doesn't yet
# generate correctly outside the single "trailing element of the whole
# document" position it was written for (see that file's own header).
# All 6 excluded grammars still get their own hand-written accept/reject
# fixture in tests.nix instead (see that file's "grammar/aterm.nix"
# section onward), since round-trip generation isn't the only way to
# get coverage. tests.nix's `generate_*` checks cover additional
# toy/synthetic schemas at smaller scale (N=5) as part of the main
# combinator test suite.
#
# Usage: ./verify-roundtrip.sh
set -uo pipefail
cd "$(dirname "$0")"

fail=0

tsv_passed=$(nix eval --impure --expr '
  let
    rt = import ./lib/roundtrip.nix;
    g = import ./grammar/tsv.nix;
    result = rt.checkPackratGrammar {
      grammar = g.grammar;
      handlers = g.handlers;
      ruleName = "DOCUMENT";
      seedPrefix = "verify-roundtrip-tsv";
      numSamples = 50;
    };
  in result.allPassed
' 2>&1)

if [[ "$tsv_passed" == "true" ]]; then
  echo "OK   roundtrip grammar/tsv.nix (50 samples)"
else
  echo "FAIL roundtrip grammar/tsv.nix: $tsv_passed"
  fail=1
fi

json_passed=$(nix eval --impure --expr '
  let
    rt = import ./lib/roundtrip.nix;
    g = import ./grammar/json.nix;
    result = rt.checkPackratGrammar {
      grammar = g.grammar;
      handlers = g.handlers;
      ruleName = "X";
      seedPrefix = "verify-roundtrip-json";
      numSamples = 50;
      maxDepth = 3;
    };
  in result.allPassed
' 2>&1)

if [[ "$json_passed" == "true" ]]; then
  echo "OK   roundtrip grammar/json.nix (50 samples)"
else
  echo "FAIL roundtrip grammar/json.nix: $json_passed"
  fail=1
fi

flakelock_passed=$(nix eval --impure --expr '
  let
    rt = import ./lib/roundtrip.nix;
    g = import ./examples/flakelock-valuewalk.nix;
    result = rt.checkValuewalkGrammar {
      grammar = g;
      ruleName = "DOCUMENT";
      seedPrefix = "verify-roundtrip-flakelock";
      numSamples = 50;
    };
  in result.allPassed
' 2>&1)

if [[ "$flakelock_passed" == "true" ]]; then
  echo "OK   roundtrip examples/flakelock-valuewalk.nix (50 samples)"
else
  echo "FAIL roundtrip examples/flakelock-valuewalk.nix: $flakelock_passed"
  fail=1
fi

aterm_passed=$(nix eval --impure --expr '
  let
    rt = import ./lib/roundtrip.nix;
    g = import ./grammar/aterm.nix;
    result = rt.checkPackratGrammar {
      grammar = g.grammar;
      handlers = g.handlers;
      ruleName = "DOCUMENT";
      seedPrefix = "verify-roundtrip-aterm";
      numSamples = 50;
    };
  in result.allPassed
' 2>&1)

if [[ "$aterm_passed" == "true" ]]; then
  echo "OK   roundtrip grammar/aterm.nix (50 samples)"
else
  echo "FAIL roundtrip grammar/aterm.nix: $aterm_passed"
  fail=1
fi

drv_passed=$(nix eval --impure --expr '
  let
    rt = import ./lib/roundtrip.nix;
    g = import ./grammar/drv.nix;
    result = rt.checkPackratGrammar {
      grammar = g.grammar;
      handlers = g.handlers;
      ruleName = "DOCUMENT";
      seedPrefix = "verify-roundtrip-drv";
      numSamples = 50;
    };
  in result.allPassed
' 2>&1)

if [[ "$drv_passed" == "true" ]]; then
  echo "OK   roundtrip grammar/drv.nix (50 samples)"
else
  echo "FAIL roundtrip grammar/drv.nix: $drv_passed"
  fail=1
fi

pep508_passed=$(nix eval --impure --expr '
  let
    rt = import ./lib/roundtrip.nix;
    g = import ./grammar/pep508.nix;
    result = rt.checkPackratGrammar {
      grammar = g.grammar;
      handlers = g.handlers;
      ruleName = "SPECIFICATION";
      seedPrefix = "verify-roundtrip-pep508";
      numSamples = 50;
    };
  in result.allPassed
' 2>&1)

if [[ "$pep508_passed" == "true" ]]; then
  echo "OK   roundtrip grammar/pep508.nix (50 samples)"
else
  echo "FAIL roundtrip grammar/pep508.nix: $pep508_passed"
  fail=1
fi

poetry_semver_passed=$(nix eval --impure --expr '
  let
    rt = import ./lib/roundtrip.nix;
    g = import ./grammar/poetry-semver.nix;
    result = rt.checkPackratGrammar {
      grammar = g.grammar;
      handlers = g.handlers;
      ruleName = "CONSTRAINT";
      seedPrefix = "verify-roundtrip-poetry-semver";
      numSamples = 50;
    };
  in result.allPassed
' 2>&1)

if [[ "$poetry_semver_passed" == "true" ]]; then
  echo "OK   roundtrip grammar/poetry-semver.nix (50 samples)"
else
  echo "FAIL roundtrip grammar/poetry-semver.nix: $poetry_semver_passed"
  fail=1
fi

if [[ "$fail" -ne 0 ]]; then
  echo
  echo "verify-roundtrip: FAILED"
  exit 1
fi

echo
echo "verify-roundtrip: generated samples accepted by their own grammar/schema"
