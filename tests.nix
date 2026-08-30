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
    # { eof = {}; }: succeeds, consuming nothing, only when no input
    # remains -- the plain-leaf alternative to { not = { regex = "(.)"; }; }.
    EOF_AT_END = [
      { lit = "ab"; }
      { eof = { }; }
    ];
    EOF_REJECTS_TRAILING = [
      { lit = "a"; }
      { eof = { }; }
    ];
  };
  rOptPresent = run basicGrammar 0 "xy";
  rOptAbsent = run basicGrammar 0 "y";
  rPlusOk = run basicGrammar 0 "123";
  rPlusFail = run basicGrammar 0 "abc";
  rAnd = run basicGrammar 0 "ab";
  rNotRejects = run basicGrammar 0 "ab";
  rNotPasses = run basicGrammar 0 "ab";
  rEofAtEnd = run basicGrammar 0 "ab";
  rEofRejectsTrailing = run basicGrammar 0 "ab";

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

  # --- schemas/package-lock.nix: same "no packrat grammar" reasoning as
  # schemas/cargo-lock.nix/schemas/poetry-lock.nix, but over
  # builtins.fromJSON's output instead of fromTOML's. Checked against
  # data/example-package-lock.json, a real npm-generated lockfile
  # (git-run 0.5.5) covering the root ("") entry, a plain registry
  # dependency with a nested dependency of its own, a git-sourced
  # dependency (resolved but no integrity), and two independently
  # locked node_modules/ paths for the SAME package name+version
  # (minimist, at the top level and nested under mocha).
  packageLockSchema = import ./schemas/package-lock.nix;
  packageLockDoc = valuewalk.run {
    grammar = packageLockSchema;
  } (builtins.fromJSON (builtins.readFile ./data/example-package-lock.json));
  packageLockChecksums = (import ./examples/package-lock-checksums.nix).hashesByPackagePath (
    builtins.readFile ./data/example-package-lock.json
  );
  packageLockInvalidDoc = valuewalk.run {
    grammar = packageLockSchema;
  } {
    packages = {
      "node_modules/foo" = {
        version = "1.0.0";
        resolved = "https://registry.npmjs.org/foo/-/foo-1.0.0.tgz";
        # Neither sha1- nor sha512-, so PACKAGE_ENTRY.integrity's pattern
        # must reject this -- confirms `optional` fields are still
        # type-checked when present, not just skipped when absent.
        integrity = "md5-deadbeef";
      };
    };
  };

  # --- schemas/uv-lock.nix: same "no packrat grammar" reasoning as
  # schemas/cargo-lock.nix/schemas/poetry-lock.nix/schemas/package-lock.nix,
  # over builtins.fromTOML's output. Checked against data/example-uv.lock
  # (uv2nix's own public "conflicts" test fixture, plus a git-sourced
  # "hatchling" package spliced in from uv2nix's "git-subdirectory"
  # fixture) covering an editable root package, two independently
  # locked versions of the SAME registry package ("arpeggio", gated by
  # `conflicts`), optional-dependencies/dev-dependencies, and a
  # git-sourced package with no sdist/wheels at all.
  uvLockSchema = import ./schemas/uv-lock.nix;
  uvLockDoc = valuewalk.run {
    grammar = uvLockSchema;
  } (builtins.fromTOML (builtins.readFile ./data/example-uv.lock));
  uvLockChecksums = (import ./examples/uv-lock-checksums.nix).hashesByPackageNameVersion (
    builtins.readFile ./data/example-uv.lock
  );
  uvLockInvalidDoc = valuewalk.run {
    grammar = uvLockSchema;
  } {
    version = 1;
    "requires-python" = ">=3.12";
    package = [
      {
        name = "foo";
        source = {
          registry = "https://pypi.org/simple";
        };
        # Not "sha256:..." at all, so SDIST_ENTRY.hash's pattern must
        # reject this -- same "optional fields are still type-checked
        # when present" confirmation as packageLockInvalidDoc above.
        sdist = {
          url = "https://files.pythonhosted.org/packages/foo/foo-1.0.0.tar.gz";
          hash = "md5:deadbeef";
        };
      }
    ];
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
    eof_succeedsAtTrueEndOfInput = rEofAtEnd.EOF_AT_END != packrat.NO_MATCH;
    eof_rejectsWhenInputRemains = rEofRejectsTrailing.EOF_REJECTS_TRAILING == packrat.NO_MATCH;

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

    packageLock_acceptsRealNpmLockfile = packageLockDoc.DOCUMENT != null;
    packageLock_extractsChecksumsForFetchableRegistryDeps =
      packageLockChecksums == {
        "node_modules/async" = {
          hash = "sha512-mzo5dfJYwAn29PeiJ0zvwTo04zj8HDJj0Mn8TD7sno7q12prdbnasKJHhkm2c1LgrhlJ0teaea8860oxi51mGA==";
          url = "https://registry.npmjs.org/async/-/async-2.6.4.tgz";
        };
        "node_modules/lodash" = {
          hash = "sha512-v2kDEe57lecTulaDIuNTPy3Ry4-tp8IN4WGE22mVFhKUxvqDzeQhZFDGeQ7RQoWJq9nBSRSl2iJm5ffTgLdrbg==";
          url = "https://registry.npmjs.org/lodash/-/lodash-4.17.21.tgz";
        };
        "node_modules/minimist" = {
          hash = "sha512-2yyAR8qBkN3YuheJanUpWC5U3bb5osDywNB8RzDVlDwDHbocAJveqqj1u8+SVD7jkWT4yvsHCpWqqWqAxb0zCA==";
          url = "https://registry.npmjs.org/minimist/-/minimist-1.2.8.tgz";
        };
        "node_modules/mocha/node_modules/minimist" = {
          hash = "sha512-miQKw5Hv4NS1Psg2517mV4e4dYNaO3++hjAvLOAzKqZ61rH8NS1SK+vbfBWZ5PY/Me/bEWhUwqMghEW5Fb9T7Q==";
          url = "https://registry.npmjs.org/minimist/-/minimist-0.0.8.tgz";
        };
      };
    packageLock_rejectsMalformedIntegrityPattern = packageLockInvalidDoc.DOCUMENT == null;

    uvLock_acceptsRealUvLockfile = uvLockDoc.DOCUMENT != null;
    uvLock_extractsChecksumsForFetchableRegistryDeps =
      uvLockChecksums == {
        "arpeggio-2.0.0" = [
          {
            hash = "sha256:d6b03839019bb8a68785f9292ee6a36b1954eb84b925b84a6b8a5e1e26d3ed3d";
            url = "https://files.pythonhosted.org/packages/3d/ed/53c315e680fdf58818c0938f6c132df4342c95fc68977001244403fee476/Arpeggio-2.0.0.tar.gz";
          }
          {
            hash = "sha256:448e332deb0e9ccd04046f1c6c14529d197f41bc2fdb3931e43fc209042fbdd3";
            url = "https://files.pythonhosted.org/packages/7a/b7/62898ef180bbfea60d28678040ddbb50e36c180d5c56e9cc62b7944c4623/Arpeggio-2.0.0-py2.py3-none-any.whl";
          }
        ];
        "arpeggio-2.0.1" = [
          {
            hash = "sha256:8dfee59d546e0192e3c47f630f08f12ba7cf542caf157c58d516a193e3bfb854";
            url = "https://files.pythonhosted.org/packages/66/a5/4e39a94abf59bff8c9dde4880039172e0efe874453443e1e13651b6bd149/Arpeggio-2.0.1.tar.gz";
          }
          {
            hash = "sha256:5372cf9daee84bd695e99f17371c844504ead3b1d96c70b95dfc54f957fe69de";
            url = "https://files.pythonhosted.org/packages/02/1f/01b7e8d3dec71b52a149ac04f48fcc8e559bda065bcb1b39d32a4f1da474/Arpeggio-2.0.1-py2.py3-none-any.whl";
          }
        ];
      };
    uvLock_rejectsMalformedHashPattern = uvLockInvalidDoc.DOCUMENT == null;

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
