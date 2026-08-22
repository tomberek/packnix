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

  run = grammar: count: string: packrat.run { inherit grammar; } count string;

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
          { range = [ "0" "9" ]; }
        ];
      };
    };
  };
  starPlainGrammar = {
    S = {
      star = [
        { lit = "a"; }
        { range = [ "0" "9" ]; }
      ];
    };
  };
  starCutOnBadInput = run starCutGrammar 0 "a5a6ax";
  starPlainOnBadInput = run starPlainGrammar 0 "a5a6ax";
  starCutOnGoodInput = run starCutGrammar 0 "a5a6a7";

  # --- Basic combinator sanity -----------------------------------------
  basicGrammar = {
    OPT_PRESENT = [
      { opt = { lit = "x"; }; }
      { lit = "y"; }
    ];
    OPT_ABSENT = [
      { opt = { lit = "x"; }; }
      { lit = "y"; }
    ];
    PLUS_OK = { plus = { range = [ "0" "9" ]; }; };
    PLUS_FAIL = { plus = { range = [ "0" "9" ]; }; };
    AND_LOOKAHEAD = [
      { and = { lit = "ab"; }; }
      { lit = "a"; }
    ];
    # !e should fail here because e ("x") DOES match at this position.
    NOT_LOOKAHEAD_REJECTS = [
      { not = { lit = "a"; }; }
      { lit = "a"; }
    ];
    # !e should succeed here (consuming nothing) because e ("x") does NOT
    # match "a...", then the following lit "a" matches normally.
    NOT_LOOKAHEAD_PASSES = [
      { not = { lit = "x"; }; }
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
  # truncate a match longer than the window. A regex used directly in a
  # sequence (NOT wrapped in `star`, which self-chunks and so was already
  # safe at any length) previously broke on real input once the match
  # exceeded the fixed window -- confirmed directly against
  # grammar/json.nix's COMMENT rule: at a fixed window of 512, a 512-char
  # comment line parsed fine but a 513-char one made an otherwise-valid
  # file fail to parse entirely. evalRegex now retries with a doubled
  # window whenever a match exactly fills the current window, so this
  # grammar's regexWindow is a pure speed/memory tuning knob now, not a
  # correctness bound. Test directly against a plain (non-star) regex atom
  # with a match longer than any realistic window default.
  longMatchGrammar = {
    LONG = { regex = "([a-z]+)"; };
  };
  longInput = builtins.concatStringsSep "" (builtins.genList (_: "x") 2000);
  rLongMatch = run longMatchGrammar 0 longInput;

  checks = {
    cutMain_parsesFullString = cutMainResult.M != false;
    cutMain_correctValue =
      cutMainResult.M
      == [
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

    cutMisplaced_acceptsA = cutMisplacedA.P != false;
    cutMisplaced_rejectsB = cutMisplacedB.P == false;

    starCut_wholeStarFailsOnTrailingUnmatchedA = starCutOnBadInput.S == false;
    starPlain_stopsAndSucceedsOnSameInput = starPlainOnBadInput.S != false;
    starCut_succeedsWhenEveryPairComplete = starCutOnGoodInput.S != false;

    opt_matchesWhenPresent = rOptPresent.OPT_PRESENT != false;
    opt_matchesWhenAbsent = rOptAbsent.OPT_ABSENT != false;
    plus_matchesOneOrMore = rPlusOk.PLUS_OK != false;
    plus_failsOnZero = rPlusFail.PLUS_FAIL == false;
    and_lookaheadDoesNotConsume = rAnd.AND_LOOKAHEAD != false;
    not_lookaheadRejectsWhenPresent = rNotRejects.NOT_LOOKAHEAD_REJECTS == false;
    not_lookaheadPassesWhenAbsent = rNotPasses.NOT_LOOKAHEAD_PASSES != false;

    regex_matchLongerThanWindowIsNotTruncated =
      rLongMatch.LONG != false && builtins.stringLength rLongMatch.LONG == 2000;
  };

  allPassed = builtins.all (x: x) (builtins.attrValues checks);
in
checks // { inherit allPassed; }
