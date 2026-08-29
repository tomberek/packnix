# Generates a sample value that EITHER lib/valuewalk.nix's schema DSL or
# lib/packrat.nix's string-grammar DSL would ACCEPT -- the reverse
# direction of vw.compile/vw.run (or packrat.run): instead of walking an
# already-parsed value (or string) against a schema/grammar to validate
# it, this walks the SAME schema/grammar to PRODUCE a value/string
# satisfying it, for round-trip testing (generate a sample, feed it back
# through the parser, assert it's accepted) without a separate
# generator-schema file.
#
# Both DSLs' forms are handled by the SAME `generateWith`, dispatching on
# which key an expr attrset has -- the two vocabularies' keys are
# disjoint (`string`/`int`/`bool`/`listOf`/`attrsOf`/`attrs` only ever
# appear in a lib/valuewalk.nix schema; `lit`/`range`/`regex`/`star`/
# `plus`/`opt`/`cutSeq` only ever appear in a lib/packrat.nix grammar), so
# there is no ambiguity, and `choice`/`action`/`and`/`not`/`"Name"`/
# epsilon (`""`) are SHARED forms with the same semantics in both domains
# (see lib/packrat.nix's and lib/valuewalk.nix's own header comments).
#
# DETERMINISTIC, SEEDED generation, not "random": Nix has no RNG at all,
# so every generation step derives its choices from `builtins.hashString
# "sha256" seed`, and every recursive/repeated call derives a FRESH CHILD
# seed (`seed + "/branch0"`, `seed + "/elem3"`, etc.) so two different
# positions in one generated value don't make the same "random" choice.
# `generate schema seed` is therefore pure and fully reproducible -- a
# feature for a test generator (reproducible failures).
#
# lib/valuewalk.nix schema DSL coverage (see that file's header for full
# semantics -- this generates whatever `compile` would ACCEPT):
#   { string = {}; }        -> a seed-derived string (see genString)
#   { int = {}; }           -> a seed-derived int
#   { bool = {}; }          -> a seed-derived bool
#   { pattern = "..."; }    -> synthesized via lib/regex-generate.nix
#                              unless an explicit override is given (see
#                              PATTERN/REGEX GENERATION below)
#   { listOf = s; }         -> a seed-derived length (0 once at
#                              maxDepth), each element from a derived seed
#   { attrsOf = s; }        -> like listOf, but also needs KEY strings --
#                              generated the same way a `{ string = {};
#                              }` value would be (the schema declares no
#                              key constraint for `attrsOf`)
#   { attrs = { fields; optional; closed; }; }
#                            -> every `fields` entry always generated;
#                              every `optional` entry included via a
#                              seed-derived coin flip
#
# lib/packrat.nix grammar DSL coverage (see that file's header for full
# semantics):
#   ""                      -> the empty string (epsilon) -- checked
#                              BEFORE the general nonterminal-reference
#                              case below, matching packrat.nix's own
#                              `compile` dispatch order (a bare `""` is
#                              NOT a rule reference)
#   { lit = "..."; }        -> the literal string itself; no synthesis
#                              needed
#   { range = [a b]; }      -> a seed-derived single character in [a, b]
#                              (see PRINTABLE-ASCII TABLE below, since Nix
#                              has no chr/ord)
#   { regex = "..."; }      -> same treatment as `pattern` above (the SAME
#                              `patternGenerators` table covers both)
#   [ e1 e2 ... ]           -> a sequence: generate each in order and
#                              concatenate (there is no sequence form in
#                              lib/valuewalk.nix's DSL, disambiguated by
#                              `builtins.isList expr`)
#   { star = e; }           -> a seed-derived repeat count (0 once at
#                              maxDepth), each repetition from a derived
#                              seed, concatenated
#   { plus = e; }           -> same as star but with a MINIMUM of 1
#                              repetition (desugars to `[ e { star = e;
#                              }; ]`, same as packrat.nix's own compile)
#   { opt = e; }            -> a seed-derived coin flip: generate `e`, or
#                              the empty string
#   { cutSeq = [e1 e2]; }   -> degrades to a plain sequence `[e1 e2]` --
#                              the cut operator's commit/backtrack
#                              semantics only matter when PARSING an
#                              existing string, not when generating one
#                              that e1-then-e2 would accept.
#
# SHARED forms (same semantics in both domains):
#   { choice = [...]; }     -> picks one branch (seed-derived index,
#                              restricted to a TERMINAL branch once
#                              `depth >= maxDepth` -- see DEPTH CONTROL)
#   { action = { e; f; }; } -> generates for `e`, IGNORING `f` entirely:
#                              `f` transforms E's MATCHED VALUE on
#                              success, operating on what parsing
#                              produced, not on the input being consumed.
#                              Generation never parses, so `f` never runs
#                              and is simply irrelevant here.
#   { and = e; } / { not = e; } -> see NOT SUPPORTED below
#   "Name"                  -> resolved by name against `grammar` (see
#                              generateGrammar/run below), recursing with
#                              depth+1
#
# PRINTABLE-ASCII TABLE: Nix has no `chr`/`ord` -- `{ range = [a b]; }`
# generation instead indexes into a 95-character string literal covering
# every printable ASCII codepoint (32 space through 126 `~`), the same
# "index into a fixed alphabet string" trick genString uses for
# `{ string = {}; }`. Bounded to printable ASCII deliberately: every
# `range` used in this repo's shipped grammars is a plain a-z/0-9-style
# range, and Nix strings are byte-oriented, so full Unicode codepoint
# generation was never in scope.
#
# NOT SUPPORTED (thrown as a clear error, not silently wrong output):
#   { and = e; } / { not = e; } -> lookahead, asserting something about
#                              the input WITHOUT consuming it. `not` in
#                              particular would require synthesizing a
#                              value that satisfies "schema `e` does NOT
#                              match" -- genuinely harder, not attempted.
#                              ONE exception: `{ not = { regex = "(.)"; }; }`
#                              (the "assert end of input" idiom, e.g.
#                              grammar/aterm.nix's DOCUMENT) generates as
#                              "" -- see the `generateWith` case below.
#
# PATTERN/REGEX GENERATION: both `{ pattern = "..."; }` (lib/valuewalk.nix)
# and `{ regex = "..."; }` (lib/packrat.nix; its `maxLen` option is a
# parse-time performance hint, irrelevant here) are synthesized
# AUTOMATICALLY via lib/regex-generate.nix (a POSIX ERE parser + AST-
# walking generator), unless an explicit override is supplied via
# `patternGenerators`: `{ "<the exact pattern string>" = seed: <a string
# that matches it>; }` -- override takes precedence when present (one
# table covers both DSLs). `generate` verifies EITHER path's produced
# string actually matches the pattern via `builtins.match` before using
# it.
#
# DEPTH CONTROL: `choice`/`listOf`/`attrsOf`/`star`/`plus`/rule-references
# all consume one unit of `maxDepth`. A schema's recursion structure is
# analyzed STATICALLY (no schema annotation needed -- see
# `collectRefs`/`isTerminal` below, same "walk the schema-as-data"
# technique lib/json-toml-safety.nix uses for a different property): once
# `depth >= maxDepth`, `choice` is restricted to branches PROVEN not to
# recurse, and `listOf`/`attrsOf` are forced to length 0. This terminates
# by construction, not by convention (e.g. "branch 0 happens to be the
# simplest").
let
  regexGenerate = import ./regex-generate.nix;
in
rec {
  # --- Seeded pseudo-randomness -----------------------------------------
  # The ONLY source of pseudo-randomness in pure Nix. `hashString` is a
  # one-way, deterministic function of the seed STRING, and a different
  # seed produces an unrelated-looking hash -- all "pseudo-random" needs
  # to mean here (test-data generation, not cryptography).
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
  # non-negative int. `n` is capped at 13 (16^13 < 2^63, inside Nix's
  # 64-bit int range).
  seedToInt =
    n: seed:
    let
      hash = hashOf seed;
      digits = builtins.genList (i: builtins.substring i 1 hash) n;
    in
    builtins.foldl' (acc: d: acc * 16 + hexDigitTable.${d}) 0 digits;

  # A bool from one hex digit's low bit -- cheap, avoids seedToInt's fold.
  seedToBool = seed: hexDigitTable.${builtins.substring 0 1 (hashOf seed)} >= 8;

  # Picks an index in [0, n) from a seed via manual modulo (Nix has no
  # `mod`/`%` operator, only `builtins.div`).
  seedToIndex =
    n: seed:
    let
      a = seedToInt 8 seed;
    in
    a - (builtins.div a n) * n;

  # --- String/int generation for plain leaf schemas ---------------------
  # An arbitrary alphanumeric string (used for `{ string = {}; }` and for
  # `attrsOf`'s generated key names). Length 1-8 chars, seed-derived --
  # deliberately boring placeholder data; a schema wanting REALISTIC
  # strings needs a `pattern` override instead.
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
  # Nix has no chr/ord -- this 95-character literal covers every
  # printable ASCII codepoint (32 space through 126 `~`); index i of it
  # IS codepoint 32+i, so substring indexing stands in for chr, and a
  # linear scan stands in for ord (only needed at range endpoints).
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
  # contains -- pure structural analysis over the schema-as-data, same
  # technique lib/json-toml-safety.nix uses for a different property. A
  # `choice` branch (or a whole rule) with an empty `collectRefs` result
  # can never recurse, so it's always safe to pick once depth is
  # exhausted.
  #
  # `seen` (a list of expr objects visited on the current path, checked
  # via `builtins.elem`) guards against a FLAT self-referential schema
  # (a `rec { ... }` binding that nests the SAME Nix object inside its
  # own `listOf`/`attrsOf`, with no bare-string rule name to stop at) --
  # without it, a naive recursive walk has nothing to stop it revisiting
  # the same object forever, even though Nix's laziness makes the value
  # itself a perfectly good finite thunk graph.
  #
  # A bare-string reference is resolved BY NAME into `grammar` (the empty
  # attrset `{}` for the schema-only entry point, where every string is
  # therefore unresolvable and conservatively treated as recursive --
  # pessimistic but never wrong: "assume recursive" only produces a
  # more-conservative maxDepth cutoff, never an incorrect value).
  # Resolving through the actual grammar is what makes a genuinely
  # non-recursive named rule correctly NOT count as recursive.
  isRecursiveExpr =
    grammar: seen: expr:
    if builtins.elem expr seen then
      true
    else if expr == "" then
      # Epsilon -- NOT a rule reference despite also being a plain Nix
      # string, so this MUST be checked before the general isString case
      # below. Without it, `!(grammar ? "")` is true for any real
      # grammar, so epsilon gets treated as an unresolvable reference and
      # marked recursive -- which then makes EVERY choice branch
      # containing a cutSeq (e.g. grammar/json.nix's `{ cutSeq = [ b
      # ""]; }` pattern) recursive too, leaving `choice` with zero
      # terminal branches to bottom out at maxDepth even though the
      # underlying alternatives are genuinely non-recursive.
      false
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

  # A generic "any JSON value" schema, in lib/valuewalk.nix's OWN DSL --
  # reused to generate for `{ json = {}; }` (see generateWith's `expr ?
  # json` case below) by generating a Nix value with THIS schema, then
  # `builtins.toJSON`-serializing it.
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
            # synthesis (lib/regex-generate.nix), which parses the
            # pattern into an AST and walks it seed-deterministically.
            # Still throws its OWN error for a pattern it can't parse;
            # callers can cover those via `patternGenerators` instead.
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
        # shrink the result below `len` (listToAttrs dedups on name) --
        # disambiguate by appending the index.
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
      # Sequence: generate each sub-expr in order and concatenate --
      # packrat.nix-only (no valuewalk.nix form is ever a bare Nix list).
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
      # Desugars to [ e { star = e; } ], same as packrat.nix's own
      # compile -- guarantees at least 1 repetition even at maxDepth,
      # unlike a bare star which emits 0 there.
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
      # header comment for why cut/commit semantics don't apply here.
      generate expr.cutSeq seed depth
    else if expr ? action then
      # `f` transforms E's matched value on success, operating on what
      # parsing produced. Generation never parses, so `f` never runs --
      # generate whatever `e` itself would accept and ignore `f`.
      generate expr.action.e seed depth
    else if expr ? json then
      # `{ json = {}; }` accepts ANY JSON-shaped string here -- generate
      # an arbitrary Nix value via a small self-referential valuewalk
      # schema (reusing the cases already above), then serialize with
      # `builtins.toJSON`. No override needed, unlike `pattern`/`regex`/
      # `toml` below, since toJSON always produces valid JSON.
      builtins.toJSON (generateAnyJsonValue seed depth)
    else if expr ? toml then
      # Unlike `json` above, there is no `builtins.toTOML` -- no
      # automatic synthesis is possible, so this requires an explicit
      # override via `builtinParserGenerators.toml`.
      #
      # Verified via `builtins.fromTOML candidate`, forced eagerly via
      # `builtins.seq` (same reasoning as lib/packrat.nix's
      # evalBuiltinParser: an unforced thunk would let a bad override
      # silently ride through). Not wrapped in `builtins.tryEval` since
      # tryEval cannot catch fromTOML's parse errors -- a bad override's
      # error surfaces as fromTOML's own raw message instead.
      if !(builtinParserGenerators ? toml) then
        throw "generate: no generator provided for { toml = {}; } -- pass builtinParserGenerators.toml (no builtins.toTOML exists to synthesize automatically, unlike json)"
      else
        let
          candidate = builtinParserGenerators.toml seed;
          verified = builtins.fromTOML candidate;
        in
        builtins.seq verified candidate
    else if expr ? not && (expr.not ? regex) && expr.not.regex == "(.)" then
      # Special case: `{ not = { regex = "(.)"; }; }` is the "assert end
      # of input" idiom (see grammar/aterm.nix's DOCUMENT for the
      # canonical example) -- a single-char negated-regex lookahead used
      # as the trailing element of a top-level sequence, with nothing
      # generated after it. This is provably sound to generate as "",
      # NOT a heuristic: since nothing is emitted following this point,
      # whatever string the enclosing sequence produces IS, by
      # construction, the entire generated document -- feeding it back
      # into the real, unmodified lib/packrat.nix, the parse position at
      # this point necessarily equals the string's length, so the actual
      # `not = { regex = "(.)"; };` combinator will find no character to
      # match and succeed for real. No negation-synthesis is needed
      # because the assertion becomes trivially true by the very fact
      # that generation stops here. This does NOT generalize to `not`
      # over an arbitrary pattern, or to `and` at all -- see the `throw`
      # below for those.
      ""
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
