# Standalone test suite for lib/packrat.nix's combinators, with emphasis on
# the cutSeq (↑) operator's semantics from Mizushima et al., "Packrat
# Parsers Can Handle Practical Grammars in Mostly Constant Space"
# (PASTE'10), §3.2. Run with:
#
#   nix eval --file tests.nix --json
#
# Every attribute is a boolean; `allPassed` is true iff every check passed.
let
  packrat = import ./lib/packrat.nix;
  jsonTomlSafety = import ./lib/json-toml-safety.nix;
  valuewalk = import ./lib/valuewalk.nix;
  generate = import ./lib/generate.nix;
  regexGenerate = import ./lib/regex-generate.nix;

  run =
    grammar: count: string:
    packrat.run { inherit grammar; } count string;

  # --- Paper test case 1 (§3.2 main example) ---------------------------
  # M <- E ";" ;
  # E <- P "+" cut E / P ;      i.e.  cutSeq = [ [P "+"] E ]
  # P <- "a" / "b" ;
  cutMainGrammar = {
    M = [
      "E"
      { lit = ";"; }
    ];
    E = {
      choice = [
        {
          cutSeq = [
            [
              "P"
              { lit = "+"; }
            ]
            "E"
          ];
        }
        "P"
      ];
    };
    P = {
      choice = [
        { lit = "a"; }
        { lit = "b"; }
      ];
    };
  };
  cutMainResult = run cutMainGrammar 0 "a+b+a;";

  # --- Paper test case 2 (§3.2 misplaced-cut cautionary example) -------
  # P <- cut "a" / "b" ;
  # The cut has an empty e1 (nothing precedes the "^" in the paper's
  # shorthand before the first token), i.e. cutSeq = [ "" {lit="a";} ]:
  # e1 = epsilon always succeeds, so the choice commits UNCONDITIONALLY
  # before even trying "a"; only then is e2 = {lit="a";} evaluated. If the
  # input isn't "a", e2 fails, and because we already committed, the whole
  # choice fails -- "b" is never reached, even when the input is "b".
  cutMisplacedGrammar = {
    P = {
      choice = [
        {
          cutSeq = [
            ""
            { lit = "a"; }
          ];
        }
        { lit = "b"; }
      ];
    };
  };
  cutMisplacedA = run cutMisplacedGrammar 0 "a";
  cutMisplacedB = run cutMisplacedGrammar 0 "b";

  # --- Star-with-cut behavioral difference ------------------------------
  # (e1 ^ e2)*  where e2 can fail after e1 succeeds must FAIL THE WHOLE
  # STAR (no partial-match success), unlike plain (e1 e2)* which would just
  # stop iterating and succeed with what it had. Grammar: pairs of
  # "a" followed by a digit, cut between them -- "a5a6ax" should fail
  # entirely (the trailing "a" has no digit after it), whereas the
  # no-cut analogue (star of a plain sequence) would succeed having
  # consumed "a5a6" and left "ax" unconsumed.
  starCutGrammar = {
    S = {
      star = {
        cutSeq = [
          { lit = "a"; }
          {
            range = [
              "0"
              "9"
            ];
          }
        ];
      };
    };
  };
  starPlainGrammar = {
    S = {
      star = [
        { lit = "a"; }
        {
          range = [
            "0"
            "9"
          ];
        }
      ];
    };
  };
  starCutOnBadInput = run starCutGrammar 0 "a5a6ax";
  starPlainOnBadInput = run starPlainGrammar 0 "a5a6ax";
  starCutOnGoodInput = run starCutGrammar 0 "a5a6a7";

  # --- Basic combinator sanity -----------------------------------------
  basicGrammar = {
    OPT_PRESENT = [
      {
        opt = {
          lit = "x";
        };
      }
      { lit = "y"; }
    ];
    OPT_ABSENT = [
      {
        opt = {
          lit = "x";
        };
      }
      { lit = "y"; }
    ];
    PLUS_OK = {
      plus = {
        range = [
          "0"
          "9"
        ];
      };
    };
    PLUS_FAIL = {
      plus = {
        range = [
          "0"
          "9"
        ];
      };
    };
    AND_LOOKAHEAD = [
      {
        and = {
          lit = "ab";
        };
      }
      { lit = "a"; }
    ];
    # !e should fail here because e ("x") DOES match at this position.
    NOT_LOOKAHEAD_REJECTS = [
      {
        not = {
          lit = "a";
        };
      }
      { lit = "a"; }
    ];
    # !e should succeed here (consuming nothing) because e ("x") does NOT
    # match "a...", then the following lit "a" matches normally.
    NOT_LOOKAHEAD_PASSES = [
      {
        not = {
          lit = "x";
        };
      }
      { lit = "a"; }
    ];
  };
  rOptPresent = run basicGrammar 0 "xy";
  rOptAbsent = run basicGrammar 0 "y";
  rPlusOk = run basicGrammar 0 "123";
  rPlusFail = run basicGrammar 0 "abc";
  rAnd = run basicGrammar 0 "ab";
  rNotRejects = run basicGrammar 0 "ab";
  rNotPasses = run basicGrammar 0 "ab";

  # --- Regression: evalRegex's bounded lookahead window must not silently
  # truncate a match longer than the window (a plain, non-`star`-wrapped
  # regex atom is not otherwise self-chunking). evalRegex retries with a
  # doubled window whenever a match exactly fills the current one, so this
  # is a pure speed/memory tuning knob, not a correctness bound.
  longMatchGrammar = {
    LONG = {
      regex = "([a-z]+)";
    };
  };
  longInput = builtins.concatStringsSep "" (builtins.genList (_: "x") 2000);
  rLongMatch = run longMatchGrammar 0 longInput;

  # --- Regression: a `star` whose body matches MANY times in a row must
  # not stack-overflow or be quadratic-time (see lib/packrat.nix's
  # compileStarPlain/compileStarCut for the fix).
  manyRepeatsGrammar = {
    MANY = {
      star = {
        lit = "a";
      };
    };
  };
  manyRepeatsInput = builtins.concatStringsSep "" (builtins.genList (_: "a") 64000);
  rManyRepeats = run manyRepeatsGrammar 0 manyRepeatsInput;

  # --- Regression: jumping far ahead in the position-indexed Derivs array
  # (one match consuming many characters at once) must not stack-overflow.
  bigJumpGrammar = {
    A = {
      regex = "([a-z]+)";
    };
    B = [
      "A"
      { lit = "!"; }
    ];
  };
  bigJumpInput = builtins.concatStringsSep "" (builtins.genList (_: "a") 90000) + "!";
  rBigJump = run bigJumpGrammar 0 bigJumpInput;

  # --- json/toml combinators: hand the rest of the input to a native
  # builtins.fromJSON/fromTOML instead of parsing rule-by-rule (see
  # lib/packrat.nix's evalBuiltinParser). Confirms both the success path
  # (correct value, full input consumed) and a literal-prefix sequence
  # committing before handing off the remainder.
  jsonGrammar = {
    DOC = {
      json = { };
    };
  };
  rJson = run jsonGrammar 0 ''{"a":1,"b":[1,2,3]}'';

  tomlGrammar = {
    DOC = {
      toml = { };
    };
  };
  rToml = run tomlGrammar 0 "a = 1\nb = [1, 2, 3]\n";

  prefixedJsonGrammar = {
    DOC = [
      { lit = "PAYLOAD="; }
      { json = { }; }
    ];
  };
  rPrefixedJson = run prefixedJsonGrammar 0 ''PAYLOAD={"x":true}'';

  # NOT a `checks` entry -- this demonstrates json/toml's commit-only
  # restriction (see lib/packrat.nix's evalBuiltinParser) by actually
  # throwing, which builtins.tryEval cannot catch (a JSON-library
  # parse-error exception, not the Nix language's own AssertionError).
  # Including a genuinely-throwing expression as a `checks` value would
  # abort this whole file's evaluation, so this is a comment-documented
  # reproducer instead:
  #
  #   nix eval --impure --expr '
  #     let
  #       packrat = import ./lib/packrat.nix;
  #       grammar.DOC = [ { opt = { json = {}; }; } { lit = "trailing"; } ];
  #     in (packrat.run { inherit grammar; } 0 "not json at alltrailing").DOC
  #   '
  #
  # throws immediately (eager `builtins.seq` in evalBuiltinParser) instead
  # of `opt` silently swallowing the malformed JSON and reporting a bogus
  # successful parse.

  # --- lib/json-toml-safety.nix: static (no input needed) check that no
  # rule places json/toml somewhere a `false` would be gracefully absorbed
  # instead of propagating as the whole rule failing. Unlike
  # evalBuiltinParser's runtime throw (above), checkGrammarSafety's error
  # IS a plain Nix `throw` (an AssertionError), so builtins.tryEval CAN
  # catch it here -- these can be ordinary `checks` entries.
  safeJsonPlacementGrammar = {
    DOC = {
      json = { };
    };
  };
  safeJsonInLastChoiceBranch = {
    DOC = {
      choice = [
        { lit = "x"; }
        { json = { }; }
      ];
    };
  };
  unsafeJsonInOpt = {
    DOC = {
      opt = {
        json = { };
      };
    };
  };
  unsafeJsonInNonLastChoiceBranch = {
    DOC = {
      choice = [
        { json = { }; }
        { lit = "x"; }
      ];
    };
  };
  unsafeJsonInStarBody = {
    DOC = {
      star = {
        json = { };
      };
    };
  };
  unsafeJsonInCutSeqE1 = {
    DOC = {
      choice = [
        { lit = "x"; }
        {
          cutSeq = [
            { json = { }; }
            { lit = "y"; }
          ];
        }
      ];
    };
  };
  safeJsonInCutSeqE2OfLastBranch = {
    DOC = {
      choice = [
        { lit = "x"; }
        {
          cutSeq = [
            { lit = "y"; }
            { json = { }; }
          ];
        }
      ];
    };
  };

  # --- lib/valuewalk.nix: named-grammar API (run/compileGrammar),
  # mirroring lib/packrat.nix's grammar shape and bare-string
  # nonterminal-reference syntax over an already-parsed value tree
  # instead of string positions. Confirms rule cross-reference by name
  # ("LOCKED"/"NODE"), recursive self-reference via plain `rec`, and that
  # a real, valid `false`/legitimately-shaped value doesn't collide with
  # the `null` failure sentinel (see lib/valuewalk.nix's header for why
  # that's a per-schema fact, confirmed here for THIS schema).
  namedGrammar = {
    LOCKED = {
      attrs = {
        closed = true;
        optional = {
          narHash = {
            string = { };
          };
        };
      };
    };
    NODE = {
      choice = [
        {
          attrs = {
            closed = true;
            fields = {
              flake = {
                bool = { };
              };
              locked = "LOCKED";
              original = "LOCKED";
            };
          };
        }
        {
          attrs = {
            closed = true;
            fields = {
              locked = "LOCKED";
              original = "LOCKED";
            };
          };
        }
      ];
    };
    DOCUMENT = {
      attrs = {
        closed = true;
        fields = {
          nodes = {
            attrsOf = "NODE";
          };
          root = {
            string = { };
          };
          version = {
            int = { };
          };
        };
      };
    };
  };
  namedGrammarValidDoc = {
    nodes = {
      # A real, legitimate `false` value on a field whose own type is
      # bool -- must NOT be confused with valuewalk's `null` failure
      # sentinel (it isn't `null` at all, so there's nothing to confuse
      # here, but this is the same field flake.lock's real corpus has
      # that first surfaced the false-vs-failure question this file's
      # header comment discusses).
      a = {
        flake = false;
        locked = {
          narHash = "x";
        };
        original = {
          narHash = "y";
        };
      };
    };
    root = "a";
    version = 7;
  };
  rNamedGrammar = valuewalk.run { grammar = namedGrammar; } namedGrammarValidDoc;

  namedGrammarInvalidDoc = namedGrammarValidDoc // {
    version = "not an int";
  };
  rNamedGrammarInvalid = valuewalk.run { grammar = namedGrammar; } namedGrammarInvalidDoc;

  # Recursive schema via plain `rec` self-reference (no named-grammar
  # indirection at all) -- confirms lib/valuewalk.nix's `compile` (the
  # single-schema entry point, refs = {}) handles a self-referential
  # generic-JSON-value schema correctly: string/int/bool leaves, lists
  # and attrsets of the SAME schema, recursively.
  jsonValueSchema = rec {
    choice = [
      { string = { }; }
      { int = { }; }
      { bool = { }; }
      { listOf = jsonValueSchema; }
      { attrsOf = jsonValueSchema; }
    ];
  };
  jsonValueMatcher = valuewalk.compile jsonValueSchema;
  nestedValue = {
    a = 1;
    b = {
      c = [
        "x"
        "y"
        [
          true
          [ "deep" ]
        ]
      ];
    };
  };
  rNestedValue = jsonValueMatcher nestedValue;
  rWrongTypeValue = jsonValueMatcher (x: x);

  # --- lib/generate.nix: generates a sample string/value that a
  # lib/packrat.nix grammar or lib/valuewalk.nix schema would ACCEPT --
  # the reverse direction of parsing. Every check here round-trips: the
  # SAME grammar/schema generates a sample, then validates it via the
  # SAME grammar/schema, confirming the two directions actually agree
  # (not just that generation runs without throwing).
  genFlakelockSchema = import ./examples/flakelock-valuewalk.nix;
  genFlakelockSamples = builtins.genList (
    i:
    generate.generate {
      grammar = genFlakelockSchema;
      ruleName = "DOCUMENT";
      seed = "gen-flakelock-${builtins.toString i}";
      maxDepth = 4;
    }
  ) 5;
  genFlakelockValidated = map (
    sample: (valuewalk.run { grammar = genFlakelockSchema; } sample).DOCUMENT == sample
  ) genFlakelockSamples;

  genRecursiveJsonValueSamples = builtins.genList (
    i:
    generate.generateFromSchema {
      schema = jsonValueSchema;
      seed = "gen-rec-${builtins.toString i}";
      maxDepth = 4;
    }
  ) 5;
  genRecursiveJsonValueValidated = map (
    sample: jsonValueMatcher sample == sample
  ) genRecursiveJsonValueSamples;

  genLitGrammar = {
    DOC = {
      lit = "hello";
    };
  };
  genLitSample = generate.generate {
    grammar = genLitGrammar;
    ruleName = "DOC";
    seed = "gen-lit";
  };

  genRangeGrammar = {
    DOC = {
      range = [
        "a"
        "z"
      ];
    };
  };
  genRangeSamples = builtins.genList (
    i:
    generate.generate {
      grammar = genRangeGrammar;
      ruleName = "DOC";
      seed = "gen-range-${builtins.toString i}";
    }
  ) 10;

  genPlusGrammar = {
    DOC = {
      plus = {
        range = [
          "0"
          "9"
        ];
      };
    };
  };
  genPlusSamples = builtins.genList (
    i:
    generate.generate {
      grammar = genPlusGrammar;
      ruleName = "DOC";
      seed = "gen-plus-${builtins.toString i}";
    }
  ) 10;

  genEpsilonSample = generate.generate {
    grammar = {
      DOC = "";
    };
    ruleName = "DOC";
    seed = "gen-eps";
  };

  # `action`'s `f` must be IGNORED for generation (see lib/generate.nix's
  # header comment) -- confirms a schema/grammar using `action` still
  # generates successfully rather than throwing, and the generated value
  # matches what `e` alone (without `f`) would accept.
  genActionGrammar = {
    DOC = {
      action = {
        e = {
          lit = "raw";
        };
        f = v: "TRANSFORMED:${v}";
      };
    };
  };
  genActionSample = generate.generate {
    grammar = genActionGrammar;
    ruleName = "DOC";
    seed = "gen-action";
  };

  # Pattern/regex generation: an explicit patternGenerators override
  # takes precedence when supplied; with none, lib/regex-generate.nix's
  # automatic POSIX-ERE synthesis is the fallback (confirms both paths).
  # The "unparseable pattern still throws" case is tested directly
  # against lib/regex-generate.nix's own generateForRegex below, NOT
  # through generate.nix's pattern/regex wrapper: an unparseable pattern
  # is, in every case checked, ALSO invalid ERE syntax that
  # builtins.match itself rejects with a DIFFERENT, tryEval-uncatchable
  # exception (same class as fromJSON/fromTOML's parse errors -- see
  # lib/packrat.nix's evalBuiltinParser). generate.nix's wrapper always
  # re-verifies via builtins.match, so testing through it would hit
  # that uncatchable path instead of regex-generate.nix's own catchable
  # one.
  genPatternSchema = {
    pattern = "([0-9]+)";
  };
  genPatternSample = generate.generateFromSchema {
    schema = genPatternSchema;
    seed = "gen-pattern";
    patternGenerators = {
      "([0-9]+)" = seed: "42";
    };
  };
  genPatternAutoSample = generate.generateFromSchema {
    schema = genPatternSchema;
    seed = "gen-pattern-auto";
  };
  # An unrecognized POSIX class name: valid enough syntax to reach
  # regex-generate.nix's OWN throw (not builtins.match's uncatchable
  # one), confirmed directly against regexGenerate.generateForRegex,
  # bypassing generate.nix's wrapper entirely for this specific check.
  regexGenerateUnsupportedResult = builtins.tryEval (
    regexGenerate.generateForRegex "([[:bogus:]])" "gen-pattern-unsupported"
  );

  # Regression: every static (non-Nix-interpolated) regex pattern
  # actually used in grammar/*.nix, cross-checked when this file was
  # integrated (re-grep `grep -oh 'regex = "[^"]*"' grammar/*.nix | sort
  # -u` to keep this list current if a new grammar adds a pattern).
  # Confirms regex-generate.nix's synthesis+verification loop
  # (generateForRegexChecked) holds for the actual corpus, not just
  # hand-picked examples.
  regexCorpusPatterns = [
    "(.)"
    "([^']+)"
    "([^)]*)"
    "(-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?)"
    "(-?[0-9]+)"
    "([0-9]+)"
    "(-?[0-9]+\\.[0-9]+([eE][-+]?[0-9]+)?)"
    "([0-9a-f]+)"
    "([A-Za-z_]+)"
    "([A-Za-z0-9_.*+!-]+)"
    "([A-Za-z0-9_.-]+)"
    "([A-Za-z0-9]([A-Za-z0-9._-]*[A-Za-z0-9])?)"
    "(:[A-Za-z_][A-Za-z0-9_]*)"
    "([A-Za-z_][A-Za-z0-9_]*)"
    "([^ \r\n]+)"
    "([^'\r\n]*)"
    "([^:\r\n#]+)"
    "([^\r\n#]+)"
    "([^\r\n]*)"
    "([^\r\n]+)"
    "([^]},:\r\n#]+)"
    "(\r?\n)"
    "([[:space:]]+)"
    "([ \t])"
    "([ \t]*)"
    "([ \t]+)"
    "([^ \t;]+)"
    "([^\t\n]*)"
    "([ \t\r\n]+)"
  ];
  regexCorpusAllMatch = builtins.all (
    pattern:
    builtins.all (
      i:
      builtins.match pattern (regexGenerate.generateForRegex pattern "corpus-${builtins.toString i}")
      != null
    ) (builtins.genList (i: i) 5)
  ) regexCorpusPatterns;

  # Regression for the specific bug caught and fixed while integrating
  # this generator: [:punct:] originally included digits (0-9 sits well
  # before 'A' in ASCII, so a range-position filter never excluded them)
  # and excluded some real punctuation ([ \ ] ^ _ ` -- the symbols
  # between 'Z' and 'a') -- both from checking ASCII-range POSITION
  # instead of excluding letters/digits/space explicitly.
  regexPunctClass = regexGenerate.posixClassChars "punct";

  # `and`/`not` have no general generation strategy -- confirms this
  # throws rather than silently producing a wrong value.
  genNotSchema = {
    not = {
      string = { };
    };
  };
  genNotResult = builtins.tryEval (
    generate.generateFromSchema {
      schema = genNotSchema;
      seed = "gen-not";
    }
  );

  # Determinism: same schema + same seed must always produce the same
  # value (see lib/generate.nix's header comment -- this is a design
  # property, not an accident of implementation).
  genDeterminismSchema = {
    attrs = {
      closed = true;
      fields = {
        a = {
          string = { };
        };
        b = {
          int = { };
        };
      };
      optional = {
        c = {
          bool = { };
        };
      };
    };
  };
  genDeterminismRun1 = generate.generateFromSchema {
    schema = genDeterminismSchema;
    seed = "gen-determinism";
  };
  genDeterminismRun2 = generate.generateFromSchema {
    schema = genDeterminismSchema;
    seed = "gen-determinism";
  };

  # { json = {}; }/{ toml = {}; } generation (see lib/packrat.nix's
  # evalBuiltinParser): json needs no override (builtins.toJSON exists
  # and is exact); toml has no builtins.toTOML, so requires an explicit
  # builtinParserGenerators.toml override. Round-trips each generated
  # sample through packrat.run to confirm the SAME grammar accepts what
  # generate produced for it.
  genJsonGrammar = {
    DOC = {
      json = { };
    };
  };
  genJsonSamples = builtins.genList (
    i:
    generate.generate {
      grammar = genJsonGrammar;
      ruleName = "DOC";
      seed = "gen-json-${builtins.toString i}";
    }
  ) 5;
  genJsonValidated = map (
    sample:
    let
      parsed = (run genJsonGrammar 0 sample).DOC;
    in
    # Comparing directly against `builtins.fromJSON sample` (the ground
    # truth) rather than checking `parsed != packrat.NO_MATCH` first --
    # both are correct since lib/packrat.nix's run() uses a dedicated
    # NO_MATCH sentinel instead of `false` (see that file's comment for
    # why: `false` alone can't distinguish a rule that failed from a
    # rule whose matched VALUE legitimately IS `false`, only reachable
    # via json/toml). Comparing against fromJSON directly is simpler
    # here and needs no sentinel check at all.
    parsed == builtins.fromJSON sample
  ) genJsonSamples;

  genTomlGrammar = {
    DOC = {
      toml = { };
    };
  };
  genTomlMissingOverrideResult = builtins.tryEval (
    generate.generate {
      grammar = genTomlGrammar;
      ruleName = "DOC";
      seed = "gen-toml-missing";
    }
  );
  genTomlSample = generate.generate {
    grammar = genTomlGrammar;
    ruleName = "DOC";
    seed = "gen-toml";
    builtinParserGenerators = {
      toml = seed: "a = 1\nb = true\n";
    };
  };
  genTomlValidated = (run genTomlGrammar 0 genTomlSample).DOC != packrat.NO_MATCH;

  # --- Regression: grammar/flakelock.nix must reject a JSON object
  # missing the "," between two present fields (see that file's
  # fieldWithLeadingComma comment).
  flakelockGrammarModule = import ./grammar/flakelock.nix;
  flakelockMissingCommaInput = ''{"nodes": {"a": {"locked":{"dir": "x" "narHash":"y"},"original":{}}},"root": "a" ,"version":1 }'';
  rFlakelockMissingComma = packrat.run {
    grammar = flakelockGrammarModule.grammar;
    handlers = flakelockGrammarModule.handlers;
  } 0 flakelockMissingCommaInput;

  # Confirm a VALID document (same shape, comma correctly present) still
  # parses -- the fix must not have made the grammar reject legitimate
  # input while fixing the missing-comma leniency.
  flakelockValidCommaInput = ''{"nodes": {"a": {"locked":{"dir": "x", "narHash":"y"},"original":{}}},"root": "a" ,"version":1 }'';
  rFlakelockValidComma = packrat.run {
    grammar = flakelockGrammarModule.grammar;
    handlers = flakelockGrammarModule.handlers;
  } 0 flakelockValidCommaInput;

  # --- Regression: grammar/json.nix's STRING handler must correctly
  # unescape `\"`/`\\`/etc. rather than leaving raw matched text
  # unescaped, and must reject a lone backslash with no escape partner
  # (never valid JSON) -- see that file's stringFragment comment.
  jsonGrammarModule = import ./grammar/json.nix;
  jsonParseString =
    s:
    (packrat.run {
      grammar = jsonGrammarModule.grammar;
      handlers = jsonGrammarModule.handlers;
    } 0 s).STRING;
  # Every escape JSON defines except \uXXXX (out of scope -- see
  # grammar/json.nix's header comment: needs Unicode codepoint-to-UTF-8
  # encoding from scratch, no builtin exists), plus a lone backslash
  # (never valid) and a mixed multi-escape string.
  jsonEscapeCases = [
    {
      input = ''"\""'';
      expected = builtins.fromJSON ''"\""'';
    }
    {
      input = ''"\\"'';
      expected = builtins.fromJSON ''"\\"'';
    }
    {
      input = ''"\/"'';
      expected = builtins.fromJSON ''"\/"'';
    }
    {
      input = ''"\b"'';
      expected = builtins.fromJSON ''"\b"'';
    }
    {
      input = ''"\f"'';
      expected = builtins.fromJSON ''"\f"'';
    }
    {
      input = ''"\n"'';
      expected = builtins.fromJSON ''"\n"'';
    }
    {
      input = ''"\r"'';
      expected = builtins.fromJSON ''"\r"'';
    }
    {
      input = ''"\t"'';
      expected = builtins.fromJSON ''"\t"'';
    }
    {
      input = ''"x\"y\\z\/w\n\t"'';
      expected = builtins.fromJSON ''"x\"y\\z\/w\n\t"'';
    }
  ];
  jsonEscapeResults = map (c: (jsonParseString c.input) == c.expected) jsonEscapeCases;

  # A lone backslash (no escape partner) is never valid JSON -- confirms
  # this is correctly REJECTED, not silently accepted.
  jsonLoneBackslashInput = ''"a\ b"'';
  rJsonLoneBackslash = jsonParseString jsonLoneBackslashInput;

  # --- Regression: lib/generate.nix's isRecursiveExpr must treat the
  # epsilon marker `""` (packrat.nix's cutSeq uses `{ cutSeq = [ b ""];
  # }`) as the special epsilon case, not an unresolvable RULE REFERENCE
  # -- otherwise every choice branch wrapped in a cutSeq registers as
  # unconditionally recursive, starving `choice` of any terminal branch
  # to bottom out at maxDepth even when most branches are genuinely
  # non-recursive (e.g. grammar/json.nix's cut variant, where
  # STRING/NUMBER/BOOL/NULL are all non-recursive but every X branch is
  # cutSeq-wrapped).
  generateModule = import ./lib/generate.nix;
  jsonCutChoiceBranches = (builtins.elemAt jsonGrammarModule.grammar.X 1).choice;
  jsonCutChoiceTerminality = map (
    b: generateModule.isTerminal jsonGrammarModule.grammar b
  ) jsonCutChoiceBranches;

  checks = {
    cutMain_parsesFullString = cutMainResult.M != packrat.NO_MATCH;
    cutMain_correctValue =
      cutMainResult.M == [
        [
          [
            "a"
            "+"
          ]
          [
            [
              "b"
              "+"
            ]
            "a"
          ]
        ]
        ";"
      ];

    cutMisplaced_acceptsA = cutMisplacedA.P != packrat.NO_MATCH;
    cutMisplaced_rejectsB = cutMisplacedB.P == packrat.NO_MATCH;

    starCut_wholeStarFailsOnTrailingUnmatchedA = starCutOnBadInput.S == packrat.NO_MATCH;
    starPlain_stopsAndSucceedsOnSameInput = starPlainOnBadInput.S != packrat.NO_MATCH;
    starCut_succeedsWhenEveryPairComplete = starCutOnGoodInput.S != packrat.NO_MATCH;

    opt_matchesWhenPresent = rOptPresent.OPT_PRESENT != packrat.NO_MATCH;
    opt_matchesWhenAbsent = rOptAbsent.OPT_ABSENT != packrat.NO_MATCH;
    plus_matchesOneOrMore = rPlusOk.PLUS_OK != packrat.NO_MATCH;
    plus_failsOnZero = rPlusFail.PLUS_FAIL == packrat.NO_MATCH;
    and_lookaheadDoesNotConsume = rAnd.AND_LOOKAHEAD != packrat.NO_MATCH;
    not_lookaheadRejectsWhenPresent = rNotRejects.NOT_LOOKAHEAD_REJECTS == packrat.NO_MATCH;
    not_lookaheadPassesWhenAbsent = rNotPasses.NOT_LOOKAHEAD_PASSES != packrat.NO_MATCH;

    regex_matchLongerThanWindowIsNotTruncated =
      rLongMatch.LONG != packrat.NO_MATCH && builtins.stringLength rLongMatch.LONG == 2000;

    star_manyRepeatsDoesNotOverflowOrHang =
      rManyRepeats.MANY != packrat.NO_MATCH && builtins.length rManyRepeats.MANY == 64000;

    bigJumpDoesNotOverflow = rBigJump.B != packrat.NO_MATCH;

    json_parsesRemainderOfInput =
      rJson.DOC == {
        a = 1;
        b = [
          1
          2
          3
        ];
      };
    toml_parsesRemainderOfInput =
      rToml.DOC == {
        a = 1;
        b = [
          1
          2
          3
        ];
      };
    json_commitsAfterLiteralPrefix =
      rPrefixedJson.DOC == [
        "PAYLOAD="
        { x = true; }
      ];

    jsonSafety_acceptsSoleJson =
      (jsonTomlSafety.checkGrammarSafety safeJsonPlacementGrammar) == safeJsonPlacementGrammar;
    jsonSafety_acceptsLastChoiceBranch =
      (builtins.tryEval (jsonTomlSafety.checkGrammarSafety safeJsonInLastChoiceBranch)).success;
    jsonSafety_rejectsOpt =
      !(builtins.tryEval (jsonTomlSafety.checkGrammarSafety unsafeJsonInOpt)).success;
    jsonSafety_rejectsNonLastChoiceBranch =
      !(builtins.tryEval (jsonTomlSafety.checkGrammarSafety unsafeJsonInNonLastChoiceBranch)).success;
    jsonSafety_rejectsStarBody =
      !(builtins.tryEval (jsonTomlSafety.checkGrammarSafety unsafeJsonInStarBody)).success;
    jsonSafety_rejectsCutSeqE1 =
      !(builtins.tryEval (jsonTomlSafety.checkGrammarSafety unsafeJsonInCutSeqE1)).success;
    jsonSafety_acceptsCutSeqE2OfCommittedLastBranch =
      (builtins.tryEval (jsonTomlSafety.checkGrammarSafety safeJsonInCutSeqE2OfLastBranch)).success;

    valuewalk_namedGrammarMatchesAndPreservesRealFalse = rNamedGrammar.DOCUMENT == namedGrammarValidDoc;
    valuewalk_namedGrammarRejectsWrongType = rNamedGrammarInvalid.DOCUMENT == null;
    valuewalk_recursiveSchemaMatchesNestedValue = rNestedValue == nestedValue;
    valuewalk_recursiveSchemaRejectsWrongType = rWrongTypeValue == null;

    generate_flakelockSamplesAllValidate = builtins.all (x: x) genFlakelockValidated;
    generate_recursiveJsonValueSamplesAllValidate = builtins.all (x: x) genRecursiveJsonValueValidated;
    generate_litIsTheLiteralItself = genLitSample == "hello";
    generate_rangeSamplesAllInBounds = builtins.all (
      s: builtins.stringLength s == 1 && s >= "a" && s <= "z"
    ) genRangeSamples;
    generate_plusSamplesNeverEmpty = builtins.all (s: builtins.stringLength s >= 1) genPlusSamples;
    generate_epsilonIsEmptyString = genEpsilonSample == "";
    generate_actionIgnoresFAndGeneratesForE = genActionSample == "raw";
    generate_patternOverrideUsedAndVerified = genPatternSample == "42";
    generate_patternAutoSynthesisMatches =
      builtins.match genPatternSchema.pattern genPatternAutoSample != null;
    regexGenerate_unsupportedPosixClassThrows = !regexGenerateUnsupportedResult.success;
    regexGenerate_corpusPatternsAllMatch = regexCorpusAllMatch;
    regexGenerate_punctExcludesDigitsAndSpace =
      !(builtins.any (c: builtins.elem c regexPunctClass) [
        "0"
        "9"
        " "
        "a"
        "Z"
      ]);
    regexGenerate_punctIncludesBracketSymbols = builtins.all (c: builtins.elem c regexPunctClass) [
      "["
      "\\"
      "]"
      "^"
      "_"
      "`"
    ];
    generate_notHasNoStrategyAndThrows = !genNotResult.success;
    generate_isDeterministic = genDeterminismRun1 == genDeterminismRun2;
    generate_jsonSamplesAllValidate = builtins.all (x: x) genJsonValidated;
    generate_tomlMissingOverrideThrows = !genTomlMissingOverrideResult.success;
    generate_tomlWithOverrideValidates = genTomlValidated;

    flakelock_rejectsMissingCommaBetweenFields = rFlakelockMissingComma.DOCUMENT == packrat.NO_MATCH;
    flakelock_stillAcceptsValidCommaPlacement = rFlakelockValidComma.DOCUMENT != packrat.NO_MATCH;

    json_allEscapeSequencesDecodeCorrectly = builtins.all (x: x) jsonEscapeResults;
    json_rejectsLoneBackslash = rJsonLoneBackslash == packrat.NO_MATCH;
    json_cutChoiceBranchesHaveTerminalOptions = builtins.any (x: x) jsonCutChoiceTerminality;
  };

  allPassed = builtins.all (x: x) (builtins.attrValues checks);
in
checks // { inherit allPassed; }
