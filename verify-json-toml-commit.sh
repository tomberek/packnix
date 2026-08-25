#!/usr/bin/env bash
# Regression guard for lib/packrat.nix's evalBuiltinParser (the { json =
# {}; } / { toml = {}; } combinator): confirms it throws EAGERLY on
# malformed input reached from inside a backtrackable position (`opt`),
# rather than silently letting `opt` report a bogus successful parse with
# the actual error hidden in an unforced thunk that only surfaces later, if
# ever. This can't be a tests.nix `checks` entry -- a genuinely-throwing
# expression there would abort that whole file's evaluation (and
# builtins.tryEval can't catch this: it's a JSON-library parse-error
# exception, not the Nix language's own AssertionError). So this is a
# black-box check on the actual `nix eval` process instead: expect it to
# fail, and to fail with the specific parse-error message, not some other
# unrelated failure.
#
# Usage: ./verify-json-toml-commit.sh
set -uo pipefail
cd "$(dirname "$0")"

fail=0

output=$(nix eval --impure --expr '
  let
    packrat = import ./lib/packrat.nix;
    grammar.DOC = [ { opt = { json = {}; }; } { lit = "trailing"; } ];
  in (packrat.run { inherit grammar; } 0 "not json at alltrailing").DOC
' 2>&1)
status=$?

if [[ "$status" -eq 0 ]]; then
  echo "FAIL: expected malformed json inside opt to throw, but nix eval exited 0"
  echo "$output"
  fail=1
elif ! grep -q "json.exception.parse_error" <<<"$output"; then
  echo "FAIL: nix eval failed as expected, but not with the expected JSON parse error"
  echo "$output"
  fail=1
else
  echo "OK   json inside opt throws eagerly on malformed input (not silently swallowed)"
fi

if [[ "$fail" -ne 0 ]]; then
  echo
  echo "verify-json-toml-commit: FAILED"
  exit 1
fi

echo
echo "verify-json-toml-commit: evalBuiltinParser's commit-only throw behaves as documented"
