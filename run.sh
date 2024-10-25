#!/usr/bin/env bash
nix eval --file default.nix --apply 'x: x ./lock.json' --json | jq
