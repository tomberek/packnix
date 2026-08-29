# Generates a sample value that EITHER lib/valuewalk.nix's schema DSL or
# lib/packrat.nix's string-grammar DSL would ACCEPT -- the reverse
# direction of vw.compile/vw.run (or packrat.run): instead of walking an
# already-parsed value (or a string) against a schema/grammar to validate
# it, this walks the SAME schema/grammar to PRODUCE a value/string
# satisfying it. Motivation: a future round-trip test (generate a sample,
# feed it back through the parser, assert it's accepted / equals the
# original) needs a generator that uses the EXACT SAME schema/grammar
# value the parser already consumes -- no separate generator-schema file,
# no annotations on the schema itself.
#
# Both DSLs' forms are handled by the SAME `generateWith`, dispatching on
# which key an expr attrset has -- the two vocabularies' keys are
# disjoint (`string`/`int`/`bool`/`listOf`/`attrsOf`/`attrs` only ever
# appear in a lib/valuewalk.nix schema; `lit`/`range`/`regex`/`star`/
# `plus`/`opt`/`cutSeq` only ever appear in a lib/packrat.nix grammar), so
# there is no ambiguity in practice, and `choice`/`action`/`and`/`not`/
# `"Name"`/epsilon (`""`) are genuinely SHARED forms with the same
# semantics in both domains (see lib/packrat.nix's and lib/valuewalk.nix's
# own header comments for each domain's exact meaning of these).
#
# DETERMINISTIC, SEEDED generation, not "random": Nix has no RNG at all
# (confirmed: no builtins.random, no builtins.currentTime) -- the only
# source of pseudo-randomness available is `builtins.hashString "sha256"
# seed`, so every generation step derives its choices from a hash of a
# `seed` STRING, and every recursive/repeated call derives a FRESH CHILD
# seed (`seed + "/branch0"`, `seed + "/elem3"`, etc.) so two different
# positions in one generated value don't make the same "random" choice by
# accident. This makes `generate schema seed` a pure, fully reproducible
# function -- same schema + same seed always produces the same value,
# which is a FEATURE for a test generator (reproducible failures), not a
# limitation to work around.
#
# lib/valuewalk.nix schema DSL coverage (see that file's header for the
# full semantics of each form -- this generates whatever `compile` would
# ACCEPT, so the two files should be read side by side):
#   { string = {}; }        -> a seed-derived string (see genString)
#   { int = {}; }           -> a seed-derived int
#   { bool = {}; }          -> a seed-derived bool
#   { pattern = "..."; }    -> NOT synthesized from the pattern text (see
#                              PATTERN/REGEX GENERATION below) -- requires
#                              an explicit override.
#   { listOf = s; }         -> a seed-derived length (0 once at
#                              maxDepth), each element from a derived seed
#   { attrsOf = s; }        -> like listOf, but also needs KEY strings --
#                              generated the same way a `{ string = {};
#                              }` value would be (arbitrary, since the
#                              schema itself declares no constraint on
#                              keys for `attrsOf`)
#   { attrs = { fields; optional; closed; }; }
#                            -> every `fields` entry always generated;
#                              every `optional` entry included via a
#                              seed-derived coin flip
#
# lib/packrat.nix grammar DSL coverage (see that file's header for full
# semantics):
#   ""                      -> the empty string (epsilon) -- checked
#                              BEFORE the general nonterminal-reference
#                              case below, same dispatch order as
#                              packrat.nix's own `compile` (a bare `""` is
#                              NOT a rule reference, even though both are
#                              plain Nix strings)
#   { lit = "..."; }        -> the literal string itself; no synthesis
#                              needed at all
#   { range = [a b]; }      -> a seed-derived single character in [a, b]
#                              (see PRINTABLE-ASCII TABLE below for how,
#                              given Nix has no chr/ord)
#   { regex = "..."; }      -> same treatment as `pattern` above: NOT
#                              synthesized from the pattern text, requires
#                              an explicit override (the SAME
#                              `patternGenerators` table covers both --
#                              see PATTERN/REGEX GENERATION below)
#   [ e1 e2 ... ]           -> a sequence: generate each in order, in the
#                              STRING domain this means concatenation
#                              (builtins.concatStringsSep ""), in the
#                              VALUE domain... there is no sequence form
#                              in lib/valuewalk.nix's DSL at all (a bare
#                              Nix list as an expr is packrat-only,
#                              disambiguated by `builtins.isList expr`,
#                              which no valuewalk form ever is)
#   { star = e; }           -> a seed-derived repeat count (0 once at
#                              maxDepth), each repetition from a derived
#                              seed, concatenated
#   { plus = e; }           -> same as star but with a MINIMUM of 1
#                              repetition (desugars to `[ e { star = e;
#                              }; ]`, same as packrat.nix's own compile)
#   { opt = e; }            -> a seed-derived coin flip: generate `e`, or
#                              the empty string
#   { cutSeq = [e1 e2]; }   -> degrades to a plain sequence `[e1 e2]` for
#                              GENERATION purposes -- the cut operator's
#                              whole point (committing to e2 once e1
#                              matches, so a LATER e2 failure doesn't
#                              backtrack past e1) only has meaning for
#                              PARSING an existing string; generating a
#                              string that e1-then-e2 would accept doesn't
#                              need to know about commit/backtrack at all,
#                              since there's no failure to recover from --
#                              e1 and e2 are simply both generated and
#                              concatenated, same as `[e1 e2]`.
#
# SHARED forms (same semantics in both domains):
#   { choice = [...]; }     -> picks one branch (seed-derived index,
#                              restricted to a TERMINAL branch once
#                              `depth >= maxDepth` -- see DEPTH CONTROL)
#   { action = { e; f; }; } -> generates for `e`, IGNORING `f` entirely.
#                              `f` transforms E's MATCHED VALUE on
#                              success -- it operates on what parsing
#                              produced, not on the input being consumed.
#                              Generation produces the input directly
#                              (never parses anything, so `f` never
#                              runs), so `f` is simply irrelevant here,
#                              not a gap to work around: whatever `e`
#                              itself would accept is exactly what this
#                              generates. Confirmed by re-reading real
#                              `action` usages in grammar/flakelock.nix
#                              (e.g. jsonString's `f` just extracts the
#                              inner string from an already-matched
#                              `[lit opt lit]` triple -- generating a
#                              string that SEQUENCE would accept needs no
#                              knowledge of `f` at all).
#   { and = e; } / { not = e; } -> see NOT SUPPORTED below
#   "Name"                  -> resolved by name against `grammar` (see
#                              generateGrammar/run below), recursing with
#                              depth+1
#
# PRINTABLE-ASCII TABLE: Nix has no `chr`/`ord` (confirmed earlier in
# this conversation) -- `{ range = [a b]; }` generation instead indexes
# into a single 95-character string literal covering every printable
# ASCII codepoint (32 space through 126 `~`) via `builtins.substring`,
# the same "index into a fixed alphabet string" trick genString already
# uses for `{ string = {}; }`. Bounded to printable ASCII deliberately:
# every `range` actually used in this repo's shipped grammars (surveyed
# via grep) is a plain a-z/0-9-style range, and Nix strings are
# byte-oriented (confirmed: `builtins.readFile` rejects embedded NUL, see
# this repo's own binary-format investigation), so full Unicode codepoint
# generation was never in scope for `range` regardless.
#
# NOT SUPPORTED (thrown as a clear error, not silently wrong output):
#   { and = e; } / { not = e; } -> lookahead, asserting something about
#                              the input WITHOUT consuming it. Especially
#                              `not`: synthesizing a value that satisfies
#                              "schema `e` does NOT match" is a
#                              negation-synthesis problem, genuinely
#                              harder than every other form here -- not
#                              attempted.
#
# PATTERN/REGEX GENERATION: both `{ pattern = "..."; }` (lib/valuewalk.nix)
# and `{ regex = "..."; }` (lib/packrat.nix, its `maxLen` option is
# irrelevant here -- that's a PARSE-time performance hint, not something
# generation needs) are synthesized AUTOMATICALLY via lib/regex-generate.nix
# (a POSIX ERE parser + AST-walking generator), unless an explicit
# override is supplied via `patternGenerators`: `{ "<the exact pattern
# string>" = seed: <a string that matches it>; }` -- override takes
# precedence when present (one table covers both DSLs, since a POSIX ERE
# string is a POSIX ERE string regardless of which combinator name wraps
# it), automatic synthesis is the fallback otherwise. `generate` verifies
# EITHER path's produced string actually matches the pattern via
# `builtins.match` before using it (never trusts either blindly).
#
# This started as two options explored in parallel: an explicit-
# override-only design (what shipped first) and this automatic-synthesis
# approach (built in worktree-agent-a3cf5185d19937ae1, independently
# re-verified -- not just trusting its own self-report -- against every
# static pattern in grammar/*.nix plus nested-alternation/bracket/bound
# edge cases, and one real bug fixed during that review: `[:punct:]`
# wrongly included digits and excluded some symbols, from a flawed
# ASCII-range-position filter instead of an explicit letters+digits+
# space exclusion). Merged as the default with override-as-escape-hatch,
# since automatic synthesis covers the actual corpus without per-pattern
# maintenance, while `patternGenerators` remains available for any
# pattern regex-generate.nix can't parse (its own header comment lists
# what's out of scope) or where a caller wants specific, non-arbitrary
# sample values.
#
# DEPTH CONTROL: `choice`/`listOf`/`attrsOf`/`star`/`plus`/rule-references
# all consume one unit of `maxDepth`. A schema's recursion structure is analyzed
# STATICALLY (no schema annotation needed -- see `collectRefs`/
# `isTerminal` below, same "walk the schema-as-data" technique
# lib/json-toml-safety.nix already uses for a different property): once
# `depth >= maxDepth`, `choice` is restricted to branches PROVEN not to
# recurse (via collectRefs), and `listOf`/`attrsOf` are forced to length
# 0. This is correct and TERMINATES BY CONSTRUCTION, not by convention
# (e.g. "branch 0 happens to be the simplest") -- confirmed against
# lib/valuewalk.nix's own recursive `jsonValueSchema` test case in
# tests.nix.
let
  regexGenerate = import ./regex-generate.nix;
in
rec {
  # --- Seeded pseudo-randomness -----------------------------------------
  # The ONLY source of pseudo-randomness in pure Nix (confirmed: no
  # builtins.random). `hashString` is a one-way function of the seed
  # STRING -- deterministic, and a different seed (even a 1-character
  # difference, e.g. appending "/branch0" vs "/branch1") produces an
  # unrelated-looking hash, which is all "pseudo-random" needs to mean
  # here (this is a test-data generator, not cryptography).
  hashOf = seed: builtins.hashString "sha256" seed;

  hexDigitTable = {
    "0" = 0;
    "1" = 1;
    "2" = 2;
    "3" = 3;
    "4" = 4;
    "5" = 5;
    "6" = 6;
    "7" = 7;
    "8" = 8;
    "9" = 9;
    "a" = 10;
    "b" = 11;
    "c" = 12;
    "d" = 13;
    "e" = 14;
    "f" = 15;
  };

  # First `n` hex digits of `hashOf seed`, folded into a single
  # non-negative int (max value 16^n - 1). `n` is capped at 13 (16^13 <
  # 2^63, comfortably inside Nix's 64-bit int range) -- no shipped use
  # here needs more entropy than that per draw.
  seedToInt =
    n: seed:
    let
      hash = hashOf seed;
      digits = builtins.genList (i: builtins.substring i 1 hash) n;
    in
    builtins.foldl' (acc: d: acc * 16 + hexDigitTable.${d}) 0 digits;

  # A bool from one hex digit's low bit -- cheap, doesn't need seedToInt's
  # full fold for something this small.
  seedToBool = seed: hexDigitTable.${builtins.substring 0 1 (hashOf seed)} >= 8;

  # Picks an index in [0, n) from a seed. Modulo, not clamping (confirmed
  # elemAt throws on an out-of-range index rather than clamping, so an
  # explicit bound is required either way).
  # Nix has no `mod`/`%` operator, only `builtins.div` (integer
  # division) -- `a - (a/n)*n` is the standard manual modulo from that.
  seedToIndex =
    n: seed:
    let
      a = seedToInt 8 seed;
    in
    a - (builtins.div a n) * n;

  # --- String/int generation for plain leaf schemas ---------------------
  # An arbitrary alphanumeric string (used for `{ string = {}; }` and for
  # `attrsOf`'s generated key names, which the schema itself declares no
  # constraint on). Length 1-8 chars, seed-derived, from a fixed alphabet
  # -- deliberately boring: this is a placeholder value satisfying "is a
  # string", not an attempt to look like realistic data (a schema
  # generating REALISTIC strings needs a `pattern` override, same as any
  # other constrained string).
  stringAlphabet = builtins.stringLength "abcdefghijklmnopqrstuvwxyz0123456789";
  stringAlphabetChars = builtins.genList (
    i: builtins.substring i 1 "abcdefghijklmnopqrstuvwxyz0123456789"
  ) 36;
  genString =
    seed:
    let
      len = 1 + seedToIndex 8 (seed + "/len");
      chars = builtins.genList (
        i: builtins.elemAt stringAlphabetChars (seedToIndex 36 (seed + "/c${builtins.toString i}"))
      ) len;
    in
    builtins.concatStringsSep "" chars;

  # --- Printable ASCII table, for { range = [a b]; } generation ---------
  # Nix has no chr/ord (confirmed earlier in this conversation) -- this
  # 95-character string literal covers every printable ASCII codepoint
  # (32 space through 126 `~`); index i of it IS codepoint 32+i, so
  # substring-based indexing stands in for chr, and a linear scan for the
  # (rare, only-at-range-endpoints) reverse direction stands in for ord.
  printableAsciiTable = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
  charAtPrintableAsciiIndex = i: builtins.substring i 1 printableAsciiTable;
  indexOfPrintableAsciiChar =
    c:
    let
      len = builtins.stringLength printableAsciiTable;
      go =
        i:
        if i >= len then
          throw "generate: range endpoint is not printable ASCII: ${c}"
        else if charAtPrintableAsciiIndex i == c then
          i
        else
          go (i + 1);
    in
    go 0;

  # A small signed int, seed-derived (range deliberately modest: this is
  # sample test data, not exercising Nix's int range limits).
  genInt = seed: (seedToInt 4 (seed + "/int")) - 32768;

  genBool = seed: seedToBool (seed + "/bool");

  # --- Static schema analysis: which rule/branch is a base case? --------
  # Walks an expr tree collecting every bare-string rule reference it
  # contains -- pure structural analysis over the schema-as-data, no
  # evaluation of leaf generation logic, same technique
  # lib/json-toml-safety.nix uses for a different property. A `choice`
  # branch (or a whole rule) with an empty `collectRefs` result can never
  # recurse, so it's always safe to pick once depth is exhausted.
  #
  # NOTE: an earlier version of this walk (collectRefs, name-collecting
  # only) infinite-looped on a FLAT self-referential schema (a `rec {
  # choice = [...]; }` binding that nests the SAME Nix object inside its
  # own `listOf`/`attrsOf`, with no bare-string rule name anywhere to
  # stop at -- confirmed: `builtins.length`/`attrNames` on such a value
  # terminate fine, since Nix's laziness makes it a perfectly good FINITE
  # thunk graph, but a naive recursive WALK of it that doesn't track
  # already-visited nodes has nothing to stop it revisiting the same
  # object forever). Fixed by threading `seen` (a list of expr objects
  # already visited on the current path) and checking `builtins.elem expr
  # seen` -- Nix's `==`/`elem` on attrsets is structural, but a node that
  # is LITERALLY the same thunk as an ancestor (true self-reference, not
  # just "shaped the same") compares elem-true trivially, which is
  # exactly the cycle this needs to catch.
  #
  # A bare-string reference is resolved BY NAME into `grammar` (the empty
  # attrset `{}` for the schema-only entry point, where every string is
  # therefore unresolvable and conservatively treated as recursive --
  # correct, if pessimistic: a plain schema with no grammar has no named
  # rules at all, so a string appearing in it can only be a mistake or an
  # unsupported reference, either way "assume recursive" never produces a
  # WRONG generated value, only a possibly-more-conservative maxDepth
  # cutoff). Resolving through the actual grammar is what makes a
  # genuinely non-recursive named rule (e.g. flake.lock's LOCKED, which
  # never references anything else) correctly NOT count as recursive,
  # confirmed against examples/flakelock-valuewalk.nix's NODE/LOCKED
  # rules before this was wired into `generate` itself.
  isRecursiveExpr =
    grammar: seen: expr:
    if builtins.elem expr seen then
      true
    else if builtins.isString expr then
      if !(grammar ? ${expr}) then true else isRecursiveExpr grammar (seen ++ [ expr ]) grammar.${expr}
    else if builtins.isList expr then
      builtins.any (isRecursiveExpr grammar (seen ++ [ expr ])) expr
    else if expr ? choice then
      builtins.any (isRecursiveExpr grammar (seen ++ [ expr ])) expr.choice
    else if expr ? listOf then
      isRecursiveExpr grammar (seen ++ [ expr ]) expr.listOf
    else if expr ? attrsOf then
      isRecursiveExpr grammar (seen ++ [ expr ]) expr.attrsOf
    else if expr ? attrs then
      builtins.any (isRecursiveExpr grammar (seen ++ [ expr ])) (
        builtins.attrValues (expr.attrs.fields or { })
      )
      || builtins.any (isRecursiveExpr grammar (seen ++ [ expr ])) (
        builtins.attrValues (expr.attrs.optional or { })
      )
    else if expr ? star then
      isRecursiveExpr grammar (seen ++ [ expr ]) expr.star
    else if expr ? plus then
      isRecursiveExpr grammar (seen ++ [ expr ]) expr.plus
    else if expr ? opt then
      isRecursiveExpr grammar (seen ++ [ expr ]) expr.opt
    else if expr ? cutSeq then
      builtins.any (isRecursiveExpr grammar (seen ++ [ expr ])) expr.cutSeq
    else if expr ? action then
      isRecursiveExpr grammar (seen ++ [ expr ]) expr.action.e
    else
      false;

  isTerminal = grammar: expr: !(isRecursiveExpr grammar [ ] expr);

  # A generic "any JSON value" schema, in lib/valuewalk.nix's OWN DSL
  # (string/int/bool/choice/listOf/attrsOf) -- reused to generate for
  # `{ json = {}; }` (see generateWith's `expr ? json` case below) by
  # generating a Nix value with THIS schema, then `builtins.toJSON`-
  # serializing it, rather than a separate ad-hoc value-construction path.
  # Same shape as tests.nix's `jsonValueSchema` test case (confirmed there
  # that a `rec`-based self-reference like this one generates and
  # terminates correctly via isRecursiveExpr's object-identity cycle
  # detection).
  anyJsonValueSchema = rec {
    choice = [
      { string = { }; }
      { int = { }; }
      { bool = { }; }
      {
        listOf = anyJsonValueSchema;
      }
      {
        attrsOf = anyJsonValueSchema;
      }
    ];
  };
  generateAnyJsonValue =
    seed: depth: generateWith { } { } { } { } 3 anyJsonValueSchema (seed + "/json-value") depth;

  # --- Generation ---------------------------------------------------------
  # `refs`: lazily self-referential attrset of `{ <RuleName> = seed:
  # depth: value; ... }`, mirroring lib/valuewalk.nix's `compileWith`
  # (same "no position to index by, so plain name lookup via Nix's own
  # laziness is sufficient" reasoning -- see that file's header comment).
  # `patternGenerators`: `{ "<pattern>" = seed: string; }` -- see
  # PATTERN/REGEX GENERATION above. `builtinParserGenerators`: `{ toml =
  # seed: string; }` -- see the `expr ? toml` case below for why `json`
  # needs no equivalent override. `maxDepth`: hard backstop even with
  # terminal-branch detection, since `choice` can pick a non-terminal
  # branch repeatedly before exhausting depth even when a terminal IS
  # reachable.
  generateWith =
    grammar: refs: patternGenerators: builtinParserGenerators: maxDepth: expr: seed: depth:
    let
      generate = generateWith grammar refs patternGenerators builtinParserGenerators maxDepth;
    in
    if expr == "" then
      # Epsilon: the empty string, ALWAYS checked before the general
      # isString/rule-reference case below -- same dispatch order as
      # packrat.nix's own `compile` (a bare "" is not a rule reference,
      # even though both are plain Nix strings).
      ""
    else if builtins.isString expr then
      if !(refs ? ${expr}) then
        throw "generate: no such rule \"${expr}\" in grammar"
      else
        refs.${expr} seed (depth + 1)
    else if expr ? string then
      genString seed
    else if expr ? int then
      genInt seed
    else if expr ? bool then
      genBool seed
    else if expr ? lit then
      expr.lit
    else if expr ? range then
      let
        start = builtins.elemAt expr.range 0;
        end = builtins.elemAt expr.range 1;
        startIdx = indexOfPrintableAsciiChar start;
        endIdx = indexOfPrintableAsciiChar end;
        span = endIdx - startIdx + 1;
      in
      charAtPrintableAsciiIndex (startIdx + seedToIndex span (seed + "/range"))
    else if expr ? pattern || expr ? regex then
      let
        pat = if expr ? pattern then expr.pattern else expr.regex;
        candidate =
          if patternGenerators ? ${pat} then
            patternGenerators.${pat} seed
          else
            # No explicit override -- fall back to automatic POSIX-ERE
            # synthesis (lib/regex-generate.nix) rather than throwing
            # outright. That file parses the pattern into an AST and
            # walks it seed-deterministically; verified independently
            # (not just trusting its own self-test) against every static
            # pattern actually used in grammar/*.nix plus nested-
            # alternation/bracket/bound edge cases before being wired in
            # here. Still throws its OWN clear error for a pattern it
            # can't parse (backreferences don't exist in POSIX ERE so
            # that's not a real gap; unbalanced parens/malformed bounds/
            # etc. do throw) -- callers needing to cover such a pattern
            # still use `patternGenerators` to override it explicitly,
            # same mechanism as before, just no longer MANDATORY for
            # every pattern.
            regexGenerate.generateForRegex pat seed;
      in
      if builtins.match pat candidate == null then
        throw "generate: ${
          if patternGenerators ? ${pat} then "patternGenerators override" else "automatic regex synthesis"
        } for \"${pat}\" produced a non-matching string: ${candidate}"
      else
        candidate
    else if expr ? choice then
      let
        branches = expr.choice;
        n = builtins.length branches;
        eligible =
          if depth < maxDepth then
            builtins.genList (i: i) n
          else
            builtins.filter (i: isTerminal grammar (builtins.elemAt branches i)) (builtins.genList (i: i) n);
      in
      if eligible == [ ] then
        throw "generate: choice has no terminal branch to bottom out at maxDepth (schema is unconditionally recursive)"
      else
        let
          chosen = builtins.elemAt eligible (seedToIndex (builtins.length eligible) (seed + "/choice"));
        in
        generate (builtins.elemAt branches chosen) (seed + "/branch${builtins.toString chosen}") depth
    else if expr ? listOf then
      let
        len = if depth < maxDepth then seedToIndex 5 (seed + "/len") else 0;
      in
      builtins.genList (i: generate expr.listOf (seed + "/elem${builtins.toString i}") (depth + 1)) len
    else if expr ? attrsOf then
      let
        len = if depth < maxDepth then seedToIndex 5 (seed + "/len") else 0;
        keys = builtins.genList (i: genString (seed + "/key${builtins.toString i}")) len;
        # A generated key colliding with an earlier one would silently
        # shrink the result below `len` (listToAttrs/mapAttrs dedup on
        # name) -- disambiguate defensively by appending the index, since
        # genString's alphabet/length are both small enough for
        # collisions to be a real, not just theoretical, risk.
        uniqueKeys = builtins.genList (i: "${builtins.elemAt keys i}${builtins.toString i}") len;
      in
      builtins.listToAttrs (
        map (i: {
          name = builtins.elemAt uniqueKeys i;
          value = generate expr.attrsOf (seed + "/val${builtins.toString i}") (depth + 1);
        }) (builtins.genList (i: i) len)
      )
    else if expr ? attrs then
      let
        required = expr.attrs.fields or { };
        optional = expr.attrs.optional or { };
        reqNames = builtins.attrNames required;
        optNames = builtins.attrNames optional;
        reqResult = builtins.listToAttrs (
          map (name: {
            inherit name;
            value = generate required.${name} (seed + "/f-${name}") depth;
          }) reqNames
        );
        includedOptNames = builtins.filter (
          name: depth < maxDepth && seedToBool (seed + "/inc-${name}")
        ) optNames;
        optResult = builtins.listToAttrs (
          map (name: {
            inherit name;
            value = generate optional.${name} (seed + "/f-${name}") depth;
          }) includedOptNames
        );
      in
      reqResult // optResult
    else if builtins.isList expr then
      # Sequence: generate each sub-expr in order and concatenate as
      # strings -- packrat.nix-only (no valuewalk.nix form is ever a bare
      # Nix list), matching that a sequence's whole point is consuming/
      # emitting a stream, not a value tree.
      builtins.concatStringsSep "" (
        map (i: generate (builtins.elemAt expr i) (seed + "/seq${builtins.toString i}") depth) (
          builtins.genList (i: i) (builtins.length expr)
        )
      )
    else if expr ? star then
      let
        count = if depth < maxDepth then seedToIndex 5 (seed + "/count") else 0;
      in
      builtins.concatStringsSep "" (
        builtins.genList (i: generate expr.star (seed + "/rep${builtins.toString i}") (depth + 1)) count
      )
    else if expr ? plus then
      # Desugars to [ e { star = e; } ] -- same as packrat.nix's own
      # compile -- guaranteeing at least 1 repetition even at maxDepth
      # (star alone would emit 0 at maxDepth, but plus's minimum-1
      # semantics must hold regardless of depth, so this is NOT
      # depth-limited the way a bare star's repeat count is).
      generate [
        expr.plus
        { star = expr.plus; }
      ] seed depth
    else if expr ? opt then
      if depth < maxDepth && seedToBool (seed + "/opt") then
        generate expr.opt (seed + "/opt-body") depth
      else
        ""
    else if expr ? cutSeq then
      # Degrades to a plain sequence for GENERATION -- see this file's
      # header comment for why cut/commit semantics don't apply when
      # producing a string rather than parsing one.
      generate expr.cutSeq seed depth
    else if expr ? action then
      # `f` transforms E's MATCHED VALUE on success -- it operates on
      # what parsing produced, not on the input being consumed. Since
      # generation produces the input directly (never parses anything,
      # so `f` never runs), `f` is simply irrelevant here: generate
      # whatever `e` itself would accept and ignore `f` entirely. This
      # is NOT a gap the way `and`/`not` genuinely are below -- confirmed
      # by re-reading real `action` usages in grammar/flakelock.nix (e.g.
      # jsonString: `f` just extracts the inner string from an already-
      # matched `[lit opt lit]` triple; generating a string that
      # SEQUENCE would accept needs no knowledge of `f` at all).
      generate expr.action.e seed depth
    else if expr ? json then
      # `{ json = {}; }` accepts ANY JSON-shaped string at this position
      # (see lib/packrat.nix's evalBuiltinParser) -- generate an
      # arbitrary Nix value via a small self-referential valuewalk-domain
      # schema (reusing the SAME string/int/bool/choice/listOf/attrsOf
      # cases already above, not a separate code path), then serialize
      # with builtins.toJSON, which exists and is exact (confirmed:
      # `fromJSON (toJSON v) == v` for every value shape this schema can
      # produce). No override needed, unlike `pattern`/`regex`/`toml`
      # below -- toJSON's existence makes this the one builtin-parser
      # escape hatch generation can synthesize for automatically.
      builtins.toJSON (generateAnyJsonValue seed depth)
    else if expr ? toml then
      # Unlike `json` above, there is NO `builtins.toTOML` (confirmed) --
      # no automatic synthesis is possible, so this requires an explicit
      # override via `builtinParserGenerators.toml`, same override
      # pattern as `patternGenerators` for `pattern`/`regex`.
      #
      # Still verified via `builtins.fromTOML candidate` (forced eagerly
      # via `builtins.seq`, same reasoning as lib/packrat.nix's
      # evalBuiltinParser: an unforced thunk here would let a bad
      # override silently ride through until something ELSE happens to
      # read the value) -- just not wrapped in `builtins.tryEval` to
      # produce a custom message, unlike `pattern`/`regex`'s
      # `builtins.match`-based check above: confirmed earlier (see
      # evalBuiltinParser's own comment) that tryEval cannot catch
      # fromJSON/fromTOML's parse errors at all (they are not the Nix
      # language's own AssertionError, which is all tryEval catches). A
      # bad override's error surfaces as fromTOML's own raw parse-error
      # message instead -- not silently swallowed, just not re-wrapped.
      if !(builtinParserGenerators ? toml) then
        throw "generate: no generator provided for { toml = {}; } -- pass builtinParserGenerators.toml (no builtins.toTOML exists to synthesize automatically, unlike json)"
      else
        let
          candidate = builtinParserGenerators.toml seed;
          verified = builtins.fromTOML candidate;
        in
        builtins.seq verified candidate
    else if expr ? and || expr ? not then
      throw "generate: { and = ...; }/{ not = ...; } (lookahead) has no general generation strategy -- especially `not`, which would require negation-synthesis"
    else
      throw "generate: unrecognized expression: ${builtins.toJSON expr}";

  # Compiles every rule's generator ONCE via a self-referential attrset,
  # mirroring lib/valuewalk.nix's compileGrammar.
  generateGrammar =
    patternGenerators: builtinParserGenerators: maxDepth: grammar:
    let
      compiled = builtins.mapAttrs (
        _: expr: seed: depth:
        generateWith grammar compiled patternGenerators builtinParserGenerators maxDepth expr seed depth
      ) grammar;
    in
    compiled;

  # Public entry point for a NAMED grammar, mirroring lib/valuewalk.nix's
  # `run`/lib/packrat.nix's `run` shape: `generate { grammar; ruleName;
  # seed; patternGenerators ? {}; builtinParserGenerators ? {}; maxDepth
  # ? 5; }` returns a single generated value for `ruleName`.
  generate =
    {
      grammar,
      ruleName,
      seed,
      patternGenerators ? { },
      builtinParserGenerators ? { },
      maxDepth ? 5,
    }:
    let
      compiled = generateGrammar patternGenerators builtinParserGenerators maxDepth grammar;
    in
    if !(compiled ? ${ruleName}) then
      throw "generate: no such rule \"${ruleName}\" in grammar"
    else
      compiled.${ruleName} seed 0;

  # Convenience entry point for a single, UNNAMED schema (no grammar
  # attrset, no rule references possible) -- mirrors lib/valuewalk.nix's
  # single-argument `compile`.
  generateFromSchema =
    {
      schema,
      seed,
      patternGenerators ? { },
      builtinParserGenerators ? { },
      maxDepth ? 5,
    }:
    generateWith { } { } patternGenerators builtinParserGenerators maxDepth schema seed 0;
}
