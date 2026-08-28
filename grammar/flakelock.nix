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
  ws = {
    opt = {
      regex = "([[:space:]]+)";
    };
  };

  # A bare JSON string body has no escapes in this file (confirmed: 0
  # occurrences of `"` or `\` inside any string value across the whole
  # 14.2MB corpus), so this is a single regex atom, not a star-of-fragments
  # loop -- there is no escape case to chunk against. `"` isn't a regex
  # metacharacter, so no escaping is needed in the character class.
  jsonString = {
    action = {
      e = [
        { lit = "\""; }
        {
          opt = {
            regex = "([^\"]+)";
          };
        }
        { lit = "\""; }
      ];
      f = v: (if builtins.elemAt v 1 == null then "" else builtins.elemAt v 1);
    };
  };

  jsonNumber = {
    action = {
      e = {
        regex = "([0-9]+)";
      };
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

  # `"key":` at a fixed position, producing `{ name; value; }` directly
  # (not a raw sequence tuple -- see fieldWithLeadingComma/lockedOrOriginalObject
  # below for why). No comma handling here at all: this is the CORE
  # match, reused for both "the first present field" (no leading comma)
  # and "a later present field" (mandatory leading comma) below.
  fieldCore = key: valueExpr: {
    action = {
      e = [
        { lit = ''"${key}":''; }
        ws
        valueExpr
        ws
      ];
      f = v: {
        name = key;
        value = builtins.elemAt v 2;
      };
    };
  };

  # A field that, IF PRESENT, must have a mandatory leading "," (used for
  # every field except whichever one a lockedOrOriginalObject `choice`
  # branch below has committed to as "the first present field" --
  # BUGFIX: an earlier version made this comma `opt` unconditionally for
  # EVERY field, which meant nothing in the grammar actually required a
  # "," between two present fields at all -- confirmed independently via
  # lib/generate.nix's round-trip testing, which generated
  # `{"dir":"x""narHash":"y"}` and found this grammar accepted it as
  # valid; that string is not valid JSON, and correctly returning `false`
  # for it is exactly what the choice-over-first-present-field structure
  # below now guarantees, since a field appearing anywhere other than
  # immediately after a mandatory "," or as the branch's own committed
  # first field simply has no matching position to land in).
  fieldWithLeadingComma = key: valueExpr: {
    opt = [
      { lit = ","; }
      ws
    ]
    ++ [ (fieldCore key valueExpr) ];
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

  # A single `choice` branch committing to "field index k is the FIRST
  # PRESENT field in this object" -- fields 0..k-1 are simply never
  # tried (correct: if k is genuinely first, none of them can be
  # present), field k itself is matched via fieldCore (no leading comma:
  # it's the object's first field, nothing precedes it), and every field
  # after k uses fieldWithLeadingComma (present ⟺ preceded by a
  # mandatory ",", fixing the missing-comma bug -- see
  # fieldWithLeadingComma's comment). Result: a flat list of the
  # `{name;value;}`s that fields k..N-1 actually matched (fields after k
  # that were absent contribute `null`, filtered out below).
  branchForFirstFieldIndex =
    k:
    let
      n = builtins.length lockedOriginalFields;
      fieldAt = i: builtins.elemAt lockedOriginalFields i;
    in
    [ (fieldCore (fieldAt k).name (fieldAt k).value) ]
    ++ (map (i: fieldWithLeadingComma (fieldAt i).name (fieldAt i).value) (
      builtins.genList (i: k + 1 + i) (n - k - 1)
    ));

  # One choice branch per possible "first present field" index, plus one
  # more for "no fields present at all" (an empty `locked`/`original`
  # object -- not confirmed in the corpus but not excludable either,
  # same discipline as every `opt`-wrapped field already applies).
  lockedOrOriginalFieldsChoice = {
    choice =
      (map branchForFirstFieldIndex (builtins.genList (i: i) (builtins.length lockedOriginalFields)))
      ++ [ [ ] ];
  };

  # `"locked": {...}` / `"original": {...}`'s body: the choice-over-
  # first-present-field structure above, wrapped in the object braces --
  # never a generic "parse a JSON object" (no key-count/order discovery
  # at all: which alphabetical SUBSEQUENCE of the 12 fixed fields is
  # present is discovered by the choice's branch selection, but their
  # relative ORDER is still fixed and enforced, same as before this fix).
  lockedOrOriginalObject = {
    action = {
      e = [
        { lit = "{"; }
        ws
        lockedOrOriginalFieldsChoice
        ws
        { lit = "}"; }
      ];
      f =
        v:
        builtins.listToAttrs (
          builtins.filter (x: x != null) (
            map (
              raw:
              if raw == null then
                null
              else if builtins.isAttrs raw && raw ? name then
                raw
              else
                # fieldWithLeadingComma's `opt` wraps a MATCHED field in
                # its own 3-element sequence `[ {lit=",";} ws
                # fieldCoreResult ]` (`opt`'s success value is exactly
                # its body's value, unwrapped no further -- confirmed
                # against lib/packrat.nix's compileOpt) -- fieldCoreResult
                # is at index 2, not 1 (an earlier version of this
                # comment/index was WRONG, describing a shape this file
                # never actually produced; caught by verify-fixtures.sh
                # failing on every real fixture with "expected a set but
                # found a string" once this fix was first written).
                builtins.elemAt raw 2
            ) (builtins.elemAt v 2)
          )
        );
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
