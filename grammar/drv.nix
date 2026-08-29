# A grammar specialized to Nix's `.drv` file format -- not generic
# ATerm (see grammar/aterm.nix for that), but the one exact shape every
# real `.drv` on disk has. Confirmed by directly inspecting 500 real
# `.drv` files from a live /nix/store (not derived from Nix's source or
# documentation alone): every single one is
#
#   Derive(outputs, inputDrvs, inputSrcs, system, builder, args, env)
#
# -- always exactly 7 arguments, in this fixed order, to a bare
# `Derive` application (never quoted, never any other constructor
# name). This means, like grammar/flakelock.nix's approach to
# flake.lock:
#   - no generic "parse an ATerm, then inspect what came out" step --
#     each of the 7 positions is parsed by its own specialized rule,
#     not a shared generic "value" parser.
#   - no backtracking over the top-level shape: `Derive(` is a literal
#     tried once, and the 7 fields are read in their fixed order.
#
# Field shapes (confirmed across the whole sample, not assumed from
# a schema doc):
#   - outputs:   [ (outputName, path, hashAlgo, hash) ]  -- ALWAYS
#     4-tuples. `hashAlgo`/`hash` are both "" for a normal
#     (non-fixed-output) derivation; for a fixed-output derivation,
#     `hashAlgo` is one of "sha1"/"sha256"/"sha512", OPTIONALLY
#     prefixed with "r:" (meaning Nix's "recursive"/NAR hash mode,
#     vs. the default "flat" file-content hash) -- confirmed real
#     values in the sample: "", "sha1", "sha256", "sha512", "r:sha256".
#   - inputDrvs: [ (drvPath, [outputNames]) ]  -- ALWAYS 2-tuples; the
#     inner list is the specific outputs of that input drv this
#     derivation depends on (usually just ["out"], but not always).
#   - inputSrcs: [ path, ... ]  -- a flat list of store paths (plain
#     source files copied in, as opposed to other derivations' outputs).
#   - system:  a single string (e.g. "x86_64-linux").
#   - builder: a single string (a store path to the build program).
#   - args:    [ string, ... ]  -- builder's argv, flat list of strings.
#   - env:     [ (name, value) ]  -- ALWAYS 2-tuples, a flat
#     name->value string map. `__json` is a real, commonly-present key
#     (Nix's "structured attrs" feature) whose value is itself a
#     JSON-encoded string -- this grammar does NOT parse that JSON,
#     same "one format at a time" boundary grammar/gemfile-lock.nix
#     draws around Ruby version-constraint strings it also leaves raw.
#
# String syntax/escapes are identical to grammar/aterm.nix's (this is
# still real ATerm syntax underneath, just for a known-shaped document)
# -- see that file's header for the confirmed `\"`,`\\`,`\n`,`\r`,`\t`
# escape set.
let
  # Real `.drv` strings can and do contain any of the confirmed escapes
  # (unlike grammar/flakelock.nix's flake.lock corpus, which happened to
  # have zero escaped characters at all) -- store paths/env values are
  # arbitrary, so this needs the full fragment/escape choice, not a
  # single greedy `[^"]+` regex.
  stringFragment = {
    choice = [
      { regex = "([^\"\\\\]+)"; }
      {
        action = {
          e = [
            { lit = "\\"; }
            {
              choice = [
                { lit = "\""; }
                { lit = "\\"; }
                { lit = "n"; }
                { lit = "r"; }
                { lit = "t"; }
              ];
            }
          ];
          f =
            v:
            let
              c = builtins.elemAt v 1;
            in
            if c == "n" then
              "\n"
            else if c == "r" then
              "\r"
            else if c == "t" then
              "\t"
            else
              c;
        };
      }
    ];
  };
  drvString = {
    action = {
      e = [
        { lit = "\""; }
        { star = stringFragment; }
        { lit = "\""; }
      ];
      f = v: builtins.concatStringsSep "" (builtins.elemAt v 1);
    };
  };

  commaSeparated = item: [
    item
    {
      star = {
        cutSeq = [
          { lit = ","; }
          item
        ];
      };
    }
  ];

  # `[ item (, item)* ]?` -- the one list SHAPE every one of outputs/
  # inputDrvs/inputSrcs/args/env uses; only what `item` parses each
  # element AS differs between the 5 call sites below. Unwraps
  # `commaSeparated`'s leading-item-plus-star-of-cutSeq-pairs shape into
  # a flat list (same pattern grammar/json.nix's listBranch uses: each
  # star element is a `["," item]` pair from the cutSeq, so `elemAt p 1`
  # pulls just the item back out).
  drvList = item: {
    action = {
      e = [
        { lit = "["; }
        {
          opt = commaSeparated item;
        }
        { lit = "]"; }
      ];
      f =
        v:
        let
          opt = builtins.elemAt v 1;
        in
        if opt == null then
          [ ]
        else
          [ (builtins.elemAt opt 0) ] ++ map (p: builtins.elemAt p 1) (builtins.elemAt opt 1);
    };
  };

  # `hashAlgo`'s optional "r:" prefix (Nix's recursive/NAR hash mode,
  # vs. the default flat file-content hash) is decoded here rather than
  # left as a raw string with the prefix still attached -- confirmed
  # real values: "", "sha1", "sha256", "sha512", "r:sha256" (the exact
  # combination of algorithm and recursive-or-not seen varies per file,
  # but "r:" is always a prefix on one of the 3 real algorithm names,
  # never its own value).
  outputTuple = {
    action = {
      e = [
        { lit = "("; }
        drvString
        { lit = ","; }
        drvString
        { lit = ","; }
        drvString
        { lit = ","; }
        drvString
        { lit = ")"; }
      ];
      f =
        v:
        let
          hashAlgoRaw = builtins.elemAt v 5;
          isRecursive = builtins.match "r:.*" hashAlgoRaw != null;
        in
        {
          outputName = builtins.elemAt v 1;
          path = builtins.elemAt v 3;
          hashAlgo =
            if hashAlgoRaw == "" then
              null
            else
              (
                if isRecursive then
                  builtins.substring 2 (builtins.stringLength hashAlgoRaw - 2) hashAlgoRaw
                else
                  hashAlgoRaw
              );
          recursive = if hashAlgoRaw == "" then null else isRecursive;
          hash = if builtins.elemAt v 7 == "" then null else builtins.elemAt v 7;
        };
    };
  };

  inputDrvTuple = {
    action = {
      e = [
        { lit = "("; }
        drvString
        { lit = ","; }
        (drvList drvString)
        { lit = ")"; }
      ];
      f = v: {
        drvPath = builtins.elemAt v 1;
        outputNames = builtins.elemAt v 3;
      };
    };
  };

  envTuple = {
    action = {
      e = [
        { lit = "("; }
        drvString
        { lit = ","; }
        drvString
        { lit = ")"; }
      ];
      f = v: {
        name = builtins.elemAt v 1;
        value = builtins.elemAt v 3;
      };
    };
  };
in
{
  grammar = {
    DOCUMENT = {
      action = {
        e = [
          { lit = "Derive("; }
          (drvList outputTuple)
          { lit = ","; }
          (drvList inputDrvTuple)
          { lit = ","; }
          (drvList drvString)
          { lit = ","; }
          drvString
          { lit = ","; }
          drvString
          { lit = ","; }
          (drvList drvString)
          { lit = ","; }
          (drvList envTuple)
          { lit = ")"; }
          # End-of-input guard: succeeds, consuming nothing, only when no
          # input remains -- rejects trailing garbage after the closing
          # paren. `f` never reads this element.
          { eof = { }; }
        ];
        f = v: {
          outputs = builtins.elemAt v 1;
          inputDrvs = builtins.elemAt v 3;
          inputSrcs = builtins.elemAt v 5;
          system = builtins.elemAt v 7;
          builder = builtins.elemAt v 9;
          args = builtins.elemAt v 11;
          env = builtins.elemAt v 13;
        };
      };
    };
  };

  handlers = {
    DOCUMENT = v: v;
  };
}
