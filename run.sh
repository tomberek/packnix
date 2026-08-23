#!/usr/bin/env bash
nix eval --file default.nix --apply 'x: x ./data/lock.json' --json | jq
