#!/usr/bin/env bash
# Round-trip gate: for each grammar/schema listed below, generate N
# samples (lib/generate.nix) and confirm the SAME grammar/schema's own
# parser (lib/packrat.nix's run, or lib/valuewalk.nix's run/compile)
# accepts every one -- the fixpoint property lib/roundtrip.nix formalizes
# (see that file's header for exactly what is/isn't checked: acceptance,
# not equality against some "original").
#
# Scope deliberately limited to grammars that don't use `and`/`not`
# (lookahead has no general generation strategy -- see lib/generate.nix's
# header). grammar/{drv,gemfile-lock,aterm,yarn-lock,pep508,yaml,
# gemfile}.nix all use and/not somewhere, so they remain out of reach for
# THIS gate -- but each has its own hand-written accept/reject fixture in
# tests.nix instead (see that file's "grammar/aterm.nix" section onward),
# since round-trip generation isn't the only way to get coverage.
# grammar/tsv.nix, grammar/json.nix, and examples/flakelock-valuewalk.nix
# are the three real, non-toy cases covered by THIS gate. tests.nix's
# `generate_*` checks cover additional toy/synthetic schemas at smaller
# scale (N=5) as part of the main combinator test suite.
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

if [[ "$fail" -ne 0 ]]; then
  echo
  echo "verify-roundtrip: FAILED"
  exit 1
fi

echo
echo "verify-roundtrip: generated samples accepted by their own grammar/schema"
