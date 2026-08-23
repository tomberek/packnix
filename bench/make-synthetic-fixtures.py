#!/usr/bin/env python3
"""Generate synthetic flake.lock-shaped fixtures at several sizes (by node
count), matching the schema confirmed against the real lock-large.json:
top-level {nodes, root, version} in that order; each node is one of the 4
observed key-sets ({flake,locked,original}, {inputs,locked,original},
{locked,original}, {inputs}); locked/original keys always alphabetical.
Used to benchmark native fromJSON+lib.types.json.check validation against
the packrat parser/recognizer (generic grammar/json.nix and the
schema-specialized grammar/flakelock.nix) across input sizes.
"""
import json
import sys
import os

def locked(i):
    return {
        "lastModified": 1700000000 + i,
        "narHash": f"sha256-{'A' * 40}{i:04d}",
        "ref": f"refs/tags/{i}.0.0",
        "rev": f"{i:040x}",
        "revCount": i * 3,
        "type": "git",
        "url": f"ssh://git@example.com/org/repo{i}",
    }

def original(i):
    return {
        "ref": f"refs/tags/{i}.0.0",
        "type": "git",
        "url": f"ssh://git@example.com/org/repo{i}",
    }

def node(i):
    shape = i % 4
    if shape == 0:
        return {"flake": False, "locked": locked(i), "original": original(i)}
    elif shape == 1:
        inputs = {f"dep{j}": f"node{(i + j) % max(i, 1) or 1}" for j in range(min(3, i))}
        return {"inputs": inputs, "locked": locked(i), "original": original(i)}
    elif shape == 2:
        return {"locked": locked(i), "original": original(i)}
    else:
        inputs = {f"dep{j}": f"node{(i + j) % max(i, 1) or 1}" for j in range(min(2, i))}
        return {"inputs": inputs}

def make(n):
    nodes = {f"node{i}": node(i) for i in range(n)}
    nodes["root"] = {"inputs": {f"dep{i}": f"node{i}" for i in range(min(n, 5))}}
    return {"nodes": nodes, "root": "root", "version": 7}

if __name__ == "__main__":
    outdir = os.path.join(os.path.dirname(__file__), "fixtures")
    os.makedirs(outdir, exist_ok=True)
    sizes = [5, 15, 30, 60, 120, 250, 500, 1000, 2000]
    for n in sizes:
        doc = make(n)
        path = os.path.join(outdir, f"synth-{n}.json")
        with open(path, "w") as f:
            json.dump(doc, f)
        print(f"{path}: {os.path.getsize(path)} bytes, n={n}")
