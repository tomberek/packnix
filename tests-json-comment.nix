# Regression test for grammar/json.nix's COMMENT rule: a long single-line
# comment must not break parsing of an otherwise-valid file. Kept separate
# from tests.nix (which is engine-level/combinator-general) since this
# needs the actual JSON grammar and ITEM/COMMENT specifically.
#
# Background: COMMENT's regex used to be a single greedy `([^\n]+)` match
# over the WHOLE comment line, with no `star`-based self-chunking safety
# net (unlike STRING_RAW/WHITESPACE, which already looped a single-char/
# single-fragment regex via `star` and so were already robust to any
# match length regardless of the engine's bounded lookahead window). That
# made COMMENT the one rule in this grammar whose correctness depended on
# evalRegex's fixed window being "big enough" -- confirmed a comment line
# longer than the window would silently truncate and break the parse.
# lib/packrat.nix's evalRegex now retries with a doubled window whenever a
# match exactly fills the current one (an engine-level fix protecting any
# rule, not just this one), and separately COMMENT itself was rewritten to
# use `star` over single-character matches (COMMENT_CHAR/COMMENT_BODY),
# removing the one place in this grammar that needed the engine-level
# retry at all. Both fixes are kept (defense in depth): the grammar fix
# means COMMENT specifically no longer relies on the window-retry
# mechanism; the engine fix still protects any other/future rule that
# writes a plain non-star regex atom.
#
# Run with: nix eval --file tests-json-comment.nix --json
let
  packrat = import ./lib/packrat.nix;
  j = import ./grammar/json.nix;

  mkDoc =
    commentLen:
    "{#"
    + builtins.concatStringsSep "" (builtins.genList (_: "x") commentLen)
    + "\n\"k\": 1}";

  parse = doc: (packrat.run { grammar = j.grammarNoCut; handlers = j.handlersNoCut; } 0 doc).X;

  # 9900 chars comfortably clears any realistic comment line while staying
  # well under the ~9900-10000 char ceiling documented in lib/packrat.nix
  # where Nix's own max-call-depth becomes the limiting factor for a
  # SINGLE unbroken token of that length, independent of this grammar or
  # window mechanism -- that ceiling is a known, separately-documented
  # structural limit of the current engine (advanceN / evalStar's loop are
  # not tail-call-depth-bounded), not something this test is trying to
  # push past.
  shortComment = parse (mkDoc 10);
  windowSizedComment = parse (mkDoc 512); # exactly the old fixed window
  longerThanOldWindowComment = parse (mkDoc 513); # previously broke
  veryLongComment = parse (mkDoc 9000);

  checks = {
    shortComment_parses = shortComment == { k = 1; };
    windowSizedComment_parses = windowSizedComment == { k = 1; };
    longerThanOldWindowComment_parses = longerThanOldWindowComment == { k = 1; };
    veryLongComment_parses = veryLongComment == { k = 1; };
  };
  allPassed = builtins.all (x: x) (builtins.attrValues checks);
in
checks // { inherit allPassed; }
