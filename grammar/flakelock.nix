# A grammar specialized for flake.lock's exact schema, not generic JSON.
# Built from directly inspecting a real 14.2MB flake.lock's structure: key
# ORDER within every object is always alphabetical (nix flake lock's
# canonical output), so every field-set is a fixed, known sequence of
# optional fields tried once each, in order -- never a generic "parse
# arbitrary key, then dispatch on its name" choice. This means:
#   - no backtracking over key order/identity at all (every key is a
#     literal `lit` tried at a fixed position, not matched against an
#     unbounded alternative set)
#   - no escape handling in strings (confirmed: zero `"`/`\` characters in
#     any string value in the real file) -- a bare JSON string body is a
#     single greedy `[^"]+`, no star/choice over fragment-vs-escape
#     alternatives at all
#   - no polymorphic fields (every field name maps to exactly one JSON
#     type across the whole file) -- one parser per field name, not a
#     generic value-type dispatch
#
# This is the "we know the shape, so skip discovering it" complement to
# grammar/json.nix's fully generic approach. It accepts ONLY documents
# matching this exact schema; anything else (a differently-shaped
# flake.lock, or arbitrary JSON) correctly fails to parse -- that
# inflexibility is the whole point, not a bug.
#
# Every non-leaf sub-expression below is single-reference (used from
# exactly one place in the grammar, or from multiple mutually-exclusive
# `choice` branches that can never both be live at the same input
# position within one parse), so each is inlined via `{ action = {e;f;};
# }` (lib/packrat.nix) instead of being a named Derivs-node field: only
# `DOCUMENT` is a named rule at all. This sacrifices per-position
# memoization sharing for all of it, same trade-off grammar/json.nix's
# NUMBER/BOOL/NULL/LIST/SET inlining makes -- fine here since nothing in
# this grammar is recursive or revisits the same position twice.
let
  ws = { opt = { regex = "([[:space:]]+)"; }; };

  # A bare JSON string body has no escapes in this file (confirmed: 0
  # occurrences of `"` or `\` inside any string value across the whole
  # 14.2MB corpus), so this is a single regex atom, not a star-of-fragments
  # loop -- there is no escape case to chunk against. `"` isn't a regex
  # metacharacter, so no escaping is needed in the character class.
  jsonString = {
    action = {
      e = [
        { lit = "\""; }
        { opt = { regex = "([^\"]+)"; }; }
        { lit = "\""; }
      ];
      f = v: (if builtins.elemAt v 1 == null then "" else builtins.elemAt v 1);
    };
  };

  jsonNumber = {
    action = {
      e = { regex = "([0-9]+)"; };
      f = builtins.fromJSON;
    };
  };
  jsonBool = {
    action = {
      e = {
        choice = [
          { lit = "false"; }
          { lit = "true"; }
        ];
      };
      f = v: v == "true";
    };
  };

  # `"key":` at a fixed position. Since which of the 12 known fields are
  # actually present varies per object (a `locked`/`original` object's key
  # set is any alphabetical subsequence, not always the same subset), the
  # field that happens to be first among those PRESENT is not knowable
  # ahead of parsing -- so the leading "," can't be tied to "is this the
  # first field tried" (a hardcoded "no comma before the first-tried
  # field" would leave every object where that field is absent missing a
  # comma). Instead the comma itself is `opt`: for the one field that
  # happens to be actually-first, no "," precedes it and the opt simply
  # matches zero times; for every other present field, the "," left by the
  # previous present field is there and the opt matches it.
  field = key: valueExpr: {
    opt = [
      { opt = { lit = ","; }; }
      ws
      { lit = ''"${key}":''; }
      ws
      valueExpr
      ws
    ];
  };

  # Every `locked`/`original` field observed in the real file, alphabetical
  # (confirmed universal: every object's actual key set is exactly a
  # subsequence of this order, with zero exceptions across ~23756 nodes).
  # `type` is confirmed present in 100% of locked/original objects, but is
  # still expressed as `opt` like the rest (getting a real "missing type"
  # parse failure instead of silently accepting a schema violation costs
  # nothing here); the whole point is one linear scan per object -- no
  # combinatorial choice over which of these 12 fields are present.
  lockedOriginalFields = [
    {
      name = "dir";
      value = jsonString;
    }
    {
      name = "lastModified";
      value = jsonNumber;
    }
    {
      name = "narHash";
      value = jsonString;
    }
    {
      name = "owner";
      value = jsonString;
    }
    {
      name = "ref";
      value = jsonString;
    }
    {
      name = "repo";
      value = jsonString;
    }
    {
      name = "rev";
      value = jsonString;
    }
    {
      name = "revCount";
      value = jsonNumber;
    }
    {
      name = "shallow";
      value = jsonBool;
    }
    {
      name = "submodules";
      value = jsonBool;
    }
    {
      name = "type";
      value = jsonString;
    }
    {
      name = "url";
      value = jsonString;
    }
  ];
  lockedOriginalNames = map (f: f.name) lockedOriginalFields;

  # Extracts the non-null [key value] pairs the field-sequence above
  # actually matched out of `v` (the whole `[ "{" ws field0 .. fieldN "}"
  # ]` sequence value), dropping the `null`s left by fields that weren't
  # present (each `field` is `opt`, so an absent field's raw value is
  # `null`, not a [comma? ws "key": ws VALUE ws] tuple). `offset` is where
  # field0 lands in `v` (2: past the leading "{" and ws).
  collectFields =
    fieldNames: offset: v:
    builtins.listToAttrs (
      builtins.filter (x: x != null) (
        builtins.genList (
          i:
          let
            raw = builtins.elemAt v (offset + i);
          in
          if raw == null then
            null
          else
            {
              name = builtins.elemAt fieldNames i;
              value = builtins.elemAt raw 4; # [comma? ws "key": ws VALUE ws] -> VALUE at index 4
            }
        ) (builtins.length fieldNames)
      )
    );

  # `"locked": {...}` / `"original": {...}`'s body: the field-scan above,
  # wrapped in the object braces -- never a generic "parse a JSON object"
  # (no key-count/order discovery at all).
  lockedOrOriginalObject = {
    action = {
      e = [
        { lit = "{"; }
        ws
      ]
      ++ (map (f: field f.name f.value) lockedOriginalFields)
      ++ [ { lit = "}"; } ];
      f = collectFields lockedOriginalNames 2;
    };
  };

  # `inputs`' values are either a plain input-name string (referencing
  # another top-level node by name) or a path (list of such names) --
  # confirmed: every list element observed is a string, list lengths 1-10.
  jsonListOfStrings = {
    action = {
      e = [
        { lit = "["; }
        ws
        {
          opt = [
            jsonString
            {
              star = [
                ws
                { lit = ","; }
                ws
                jsonString
              ];
            }
          ];
        }
        ws
        { lit = "]"; }
      ];
      f =
        v:
        let
          opt = builtins.elemAt v 2;
        in
        if opt == null then
          [ ]
        else
          [ (builtins.elemAt opt 0) ] ++ map (p: builtins.elemAt p 3) (builtins.elemAt opt 1);
    };
  };
  inputValue = {
    choice = [
      jsonListOfStrings
      jsonString
    ];
  };

  # `"name": <inputValue>` pairs, comma-separated, inside `inputs`' braces.
  # Unlike locked/original's fixed field set, input names are arbitrary
  # (they're the OTHER nodes' names) -- this is the one place genuinely
  # needing a generic "parse a key, then its value" shape.
  inputsItem = {
    action = {
      e = [
        ws
        jsonString
        ws
        { lit = ":"; }
        ws
        inputValue
      ];
      f = v: {
        name = builtins.elemAt v 1;
        value = builtins.elemAt v 5;
      };
    };
  };
  inputsObject = {
    action = {
      e = [
        { lit = "{"; }
        ws
        {
          opt = [
            inputsItem
            {
              star = [
                ws
                { lit = ","; }
                inputsItem
              ];
            }
          ];
        }
        ws
        { lit = "}"; }
      ];
      f =
        v:
        let
          opt = builtins.elemAt v 2;
        in
        builtins.listToAttrs (
          if opt == null then
            [ ]
          else
            [ (builtins.elemAt opt 0) ] ++ map (p: builtins.elemAt p 2) (builtins.elemAt opt 1)
        );
    };
  };

  # A single node: exactly one of the 4 shapes confirmed present in the
  # real file (counts from the actual corpus, most-common first so the
  # ordered choice tries the likeliest shape first):
  #   ~21150x {flake, locked, original}   (a regular flake input)
  #   ~1654x  {inputs, locked, original}  (an input that itself has inputs)
  #   ~951x   {locked, original}          (a leaf input, no sub-inputs)
  #   1x      {inputs}                   (the flake's own root node)
  # Order within each shape is always alphabetical, confirmed universal.
  nodeFlakeLockedOriginal = {
    action = {
      e = [
        { lit = "{"; }
        ws
        { lit = ''"flake":''; }
        ws
        jsonBool
        ws
        { lit = ","; }
        ws
        { lit = ''"locked":''; }
        ws
        lockedOrOriginalObject
        ws
        { lit = ","; }
        ws
        { lit = ''"original":''; }
        ws
        lockedOrOriginalObject
        ws
        { lit = "}"; }
      ];
      f = v: {
        flake = builtins.elemAt v 4;
        locked = builtins.elemAt v 10;
        original = builtins.elemAt v 16;
      };
    };
  };

  nodeInputsLockedOriginal = {
    action = {
      e = [
        { lit = "{"; }
        ws
        { lit = ''"inputs":''; }
        ws
        inputsObject
        ws
        { lit = ","; }
        ws
        { lit = ''"locked":''; }
        ws
        lockedOrOriginalObject
        ws
        { lit = ","; }
        ws
        { lit = ''"original":''; }
        ws
        lockedOrOriginalObject
        ws
        { lit = "}"; }
      ];
      f = v: {
        inputs = builtins.elemAt v 4;
        locked = builtins.elemAt v 10;
        original = builtins.elemAt v 16;
      };
    };
  };

  nodeLockedOriginal = {
    action = {
      e = [
        { lit = "{"; }
        ws
        { lit = ''"locked":''; }
        ws
        lockedOrOriginalObject
        ws
        { lit = ","; }
        ws
        { lit = ''"original":''; }
        ws
        lockedOrOriginalObject
        ws
        { lit = "}"; }
      ];
      f = v: {
        locked = builtins.elemAt v 4;
        original = builtins.elemAt v 10;
      };
    };
  };

  nodeInputsOnly = {
    action = {
      e = [
        { lit = "{"; }
        ws
        { lit = ''"inputs":''; }
        ws
        inputsObject
        ws
        { lit = "}"; }
      ];
      f = v: {
        inputs = builtins.elemAt v 4;
      };
    };
  };

  # Each branch above is itself `action`-wrapped, so `choice` here returns
  # whichever branch matched, already fully transformed -- no extra
  # transform needed at the `node` level itself.
  node = {
    choice = [
      nodeFlakeLockedOriginal
      nodeInputsLockedOriginal
      nodeLockedOriginal
      nodeInputsOnly
    ];
  };

  # `"name": <node>` pairs, comma-separated, inside `nodes`' braces -- node
  # names are arbitrary (same reasoning as inputsItem).
  nodesEntry = {
    action = {
      e = [
        ws
        jsonString
        ws
        { lit = ":"; }
        ws
        node
      ];
      f = v: {
        name = builtins.elemAt v 1;
        value = builtins.elemAt v 5;
      };
    };
  };
  nodesObject = {
    action = {
      e = [
        { lit = "{"; }
        ws
        {
          opt = [
            nodesEntry
            {
              star = [
                ws
                { lit = ","; }
                nodesEntry
              ];
            }
          ];
        }
        ws
        { lit = "}"; }
      ];
      f =
        v:
        let
          opt = builtins.elemAt v 2;
        in
        builtins.listToAttrs (
          if opt == null then
            [ ]
          else
            [ (builtins.elemAt opt 0) ] ++ map (p: builtins.elemAt p 2) (builtins.elemAt opt 1)
        );
    };
  };

  # The whole file: `{"nodes": {...}, "root": "...", "version": N}`,
  # always in exactly this key order (confirmed: nix flake lock's own
  # output format, and the only ordering observed in the real file). The
  # one named rule in this grammar -- everything else above is inlined.
  document = [
    { lit = "{"; }
    ws
    { lit = ''"nodes":''; }
    ws
    nodesObject
    ws
    { lit = ","; }
    ws
    { lit = ''"root":''; }
    ws
    jsonString
    ws
    { lit = ","; }
    ws
    { lit = ''"version":''; }
    ws
    jsonNumber
    ws
    { lit = "}"; }
  ];
  documentHandler = v: {
    nodes = builtins.elemAt v 4;
    root = builtins.elemAt v 10;
    version = builtins.elemAt v 16;
  };

  grammar = {
    DOCUMENT = document;
  };
  handlers = {
    DOCUMENT = documentHandler;
  };
in
{
  inherit grammar handlers;
}
