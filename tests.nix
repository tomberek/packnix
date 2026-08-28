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
  # throwing, and builtins.tryEval cannot catch that throw (confirmed: it's
  # a JSON-library parse-error exception, not the Nix language's own
  # AssertionError, which is all tryEval catches -- see evalBuiltinParser's
  # comment). Including a genuinely-throwing expression as a `checks`
  # value would abort this whole file's evaluation, not just fail one
  # check, so this is a comment-documented reproducer instead, same spirit
  # as a grammar file's header "Run with:" example. Confirmed manually:
  #
  #   nix eval --impure --expr '
  #     let
  #       packrat = import ./lib/packrat.nix;
  #       grammar.DOC = [ { opt = { json = {}; }; } { lit = "trailing"; } ];
  #     in (packrat.run { inherit grammar; } 0 "not json at alltrailing").DOC
  #   '
  #
  # throws immediately (eager `builtins.seq` in evalBuiltinParser) instead
  # of the `opt` silently swallowing the malformed JSON and reporting a
  # bogus successful parse -- which is what happened before that `seq` was
  # added: the error sat in an unforced thunk and only surfaced (if at
  # all) whenever something later happened to read the value, arbitrarily
  # far from the actual parse site.

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

  # Pattern/regex generation requires an explicit override -- confirms
  # both the success path (override used, verified via builtins.match)
  # and that a missing override throws (checked via tryEval, since this
  # IS a plain Nix throw, unlike fromJSON/fromTOML's uncatchable errors).
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
  genPatternMissingOverrideResult = builtins.tryEval (
    generate.generateFromSchema {
      schema = genPatternSchema;
      seed = "gen-pattern-missing";
    }
  );

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
    # both are correct now that lib/packrat.nix's run() uses a dedicated
    # NO_MATCH sentinel instead of `false` (see that file's own comment
    # for the bug this fixed: `false` alone couldn't distinguish a rule
    # that failed from a rule whose matched VALUE legitimately IS
    # `false`, only reachable via json/toml). Comparing against
    # fromJSON directly is simpler here and needs no sentinel check at
    # all.
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

  # --- Regression: grammar/flakelock.nix used to accept a JSON object
  # missing the "," between two present fields (each field's leading
  # comma was independently `opt`, with nothing actually requiring one
  # between two REAL fields -- see grammar/flakelock.nix's
  # fieldWithLeadingComma comment for the fix). Found independently via
  # lib/generate.nix's round-trip testing, not by hand.
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
    generate_patternMissingOverrideThrows = !genPatternMissingOverrideResult.success;
    generate_notHasNoStrategyAndThrows = !genNotResult.success;
    generate_isDeterministic = genDeterminismRun1 == genDeterminismRun2;
    generate_jsonSamplesAllValidate = builtins.all (x: x) genJsonValidated;
    generate_tomlMissingOverrideThrows = !genTomlMissingOverrideResult.success;
    generate_tomlWithOverrideValidates = genTomlValidated;

    flakelock_rejectsMissingCommaBetweenFields = rFlakelockMissingComma.DOCUMENT == packrat.NO_MATCH;
    flakelock_stillAcceptsValidCommaPlacement = rFlakelockValidComma.DOCUMENT != packrat.NO_MATCH;
  };

  allPassed = builtins.all (x: x) (builtins.attrValues checks);
in
checks // { inherit allPassed; }
