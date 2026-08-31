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
#   { eof = {}; }           -> "" when this occurrence is PROVABLY the
#                              last thing generated in the whole
#                              document (see ISLAND/ISLAST below);
#                              otherwise excluded as an ineligible
#                              `choice` branch (see DEPTH CONTROL below
#                              for the analogous maxDepth exclusion) --
#                              NEVER generated as "" at a non-last
#                              position, which would be unsound (the
#                              real `eof` combinator would then fail to
#                              accept the round-tripped result).
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
#   { and = e; } / { not = e; } -> see NOT/AND LOOKAHEAD SYNTHESIS below
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
# NOT/AND LOOKAHEAD SYNTHESIS: `{ and = e; }`/`{ not = e; }` assert
# something about the input WITHOUT consuming it. Supported ONLY as a
# sequence element with a genuine sibling immediately following it in the
# SAME list -- either directly (`[ ... { not = e; } sibling ... ]`) or
# ONE level indirected through a named rule whose own body IS (or is a
# sequence ENDING in) a bare not/and (grammar/yaml.nix's NOT_SEQ_MARKER/
# COLON_SEP take exactly this shape: `[ ... "NOT_SEQ_MARKER" sibling ... ]`
# where NOT_SEQ_MARKER's whole body is `{ not = ...; }`) -- see
# `resolveLookahead`. In either shape, the sibling is generated and then
# VERIFIED against the lookahead's body via `lookaheadHolds`, which reuses
# lib/packrat.nix's OWN `run` as the oracle (a one-off `{ CHECK = body; }`
# grammar) rather than re-deriving match semantics for `lit`/`regex`/
# `choice`/sequence a second time in this file -- any divergence between
# two independent interpretations of the same DSL forms would otherwise
# risk unsound "verification" (this repo has hit exactly that failure mode
# before, see grammar/aterm.nix's/examples/json-simple.nix's fixed bugs).
# If the sibling's generated text doesn't satisfy the assertion, it is
# regenerated from a seed-derived child seed (`seed + "/lookahead-retryN"`)
# up to `lookaheadMaxRetries` times before throwing -- mirroring the
# existing "generate, verify via match, retry-or-throw" discipline
# `pattern`/`regex` generation already uses. `not`/`and` are symmetric
# under this mechanism: same generate-then-verify step, inverted match
# condition (`not` wants the body to NOT hold; `and` wants it to hold).
#
# The lookahead's BODY is validated by `checkLookaheadBodySupported`:
# only `lit`/`range`/`regex`/`eof`, sequences, and `choice` thereof are
# supported -- a rule reference, a NESTED not/and, or an unbounded `star`/
# `plus`/`opt` inside the body throws immediately with the offending
# sub-expression named, rather than silently miscompiling. `eof` inside
# the body is additionally PRUNED before verification (`pruneEofBranches`):
# since this whole mechanism only ever fires when a sibling
# unconditionally follows, `eof` can never legitimately hold there, so
# treating it as reachable would be unsound; if pruning would leave the
# body with nothing checkable at all, that's an unconditionally
# unsatisfiable assertion and throws immediately rather than retrying.
#
# NOT SUPPORTED (thrown as a clear error, not silently wrong output):
#   { and = e; } / { not = e; } -> as a sequence element with NO sibling
#                              following it (nothing to constrain), or
#                              where the body fails `checkLookaheadBodySupported`
#                              above, or where the lookahead appears
#                              somewhere other than a plain sequence
#                              element / one-level-indirected named-rule
#                              reference (e.g. nested inside a `choice` or
#                              `opt`). ONE exception predates all of this:
#                              `{ not = { regex = "(.)"; }; }`, a
#                              "(.)"-negated-lookahead spelling of "assert
#                              end of input" that predates `{ eof = {}; }`
#                              (no grammar in this repo still uses it --
#                              all migrated, see lib/packrat.nix's own
#                              `eof` primitive -- kept only for external
#                              grammars built against the same DSL). Same
#                              provably-last-in-document gating as `eof`
#                              itself -- see the `generateWith` case below.
#
# ISLAST (a per-occurrence, NOT per-expr, positional fact): `{ eof = {}; }`
# and its `not`-regex predecessor above are only sound to generate as ""
# when the occurrence is the actual last thing emitted in the WHOLE
# document, not merely "last element of its own immediate enclosing
# sequence" -- a `choice` (e.g. grammar/gemfile-lock.nix's/grammar/yarn-
# lock.nix's shared `lineEnd = { choice = [ regex; eof; ]; }`) can itself
# be a non-terminal element of some enclosing sequence, with real content
# generated after it. `generateWith`'s `isLast` parameter tracks this,
# threaded the same way `depth` already is; `choice`'s own branch-
# eligibility filter (mirroring its existing `depth`/`maxDepth` filter)
# excludes an eof-like branch whenever `isLast` is false, so an unsound ""
# is never actually generated -- the sound alternative branch is picked
# instead.
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
  packrat = import ./packrat.nix;
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
  stringAlphabet = "abcdefghijklmnopqrstuvwxyz0123456789";
  stringAlphabetLength = builtins.stringLength stringAlphabet;
  stringAlphabetChars = builtins.genList (
    i: builtins.substring i 1 stringAlphabet
  ) stringAlphabetLength;
  genString =
    seed:
    let
      len = 1 + seedToIndex 8 (seed + "/len");
      chars = builtins.genList (
        i:
        builtins.elemAt stringAlphabetChars (
          seedToIndex stringAlphabetLength (seed + "/c${builtins.toString i}")
        )
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

  # Identifies exprs `generateWith`'s `eof`/`not`-regex cases treat as
  # eof-like -- see this file's header (ISLAST) for why this matters:
  # `choice`'s own branch-eligibility filter uses this to exclude an
  # eof-like branch whenever it's not provably in trailing position.
  isEofLike = expr: (expr ? eof) || (expr ? not && (expr.not ? regex) && expr.not.regex == "(.)");

  # --- not/and lookahead synthesis ---------------------------------------
  # See this file's header (NOT/AND LOOKAHEAD SYNTHESIS) for the overall
  # design. Resolves `expr` to a not/and LOOKAHEAD element if it either
  # directly IS one, or -- the shape grammar/yaml.nix's NOT_SEQ_MARKER/
  # COLON_SEP take -- is a bare-string reference to a named rule whose own
  # body IS (or is a sequence ENDING in) a bare not/and. Anything deeper (a
  # rule referencing a rule referencing not/and, or not/and nested inside a
  # choice/opt) is NOT resolved here -- returns `null`, so the caller (the
  # `isList expr` case below) falls through to ordinary generation, which
  # throws at the point the not/and is actually reached (see the `expr ?
  # and || expr ? not` case at the bottom of `generateWith`) rather than
  # mishandling it silently.
  #
  # Returns `null` when `expr` is not a lookahead at all, otherwise
  # `{ prefix; kind; body; }`: `prefix` is the list of sub-exprs (possibly
  # `[]`) that must still be generated normally BEFORE the lookahead's own
  # zero-width contribution (COLON_SEP's own leading `{lit=":";}`, for
  # instance); `kind` is `"and"` or `"not"`; `body` is the lookahead's
  # sub-expression to verify a sibling against.
  resolveLookahead =
    grammar: expr:
    if expr ? and then
      {
        prefix = [ ];
        kind = "and";
        body = expr.and;
      }
    else if expr ? not then
      {
        prefix = [ ];
        kind = "not";
        body = expr.not;
      }
    else if builtins.isString expr && expr != "" && grammar ? ${expr} then
      let
        resolved = grammar.${expr};
      in
      if resolved ? and then
        {
          prefix = [ ];
          kind = "and";
          body = resolved.and;
        }
      else if resolved ? not then
        {
          prefix = [ ];
          kind = "not";
          body = resolved.not;
        }
      else if builtins.isList resolved && resolved != [ ] then
        let
          n = builtins.length resolved;
          lastElem = builtins.elemAt resolved (n - 1);
          # No `builtins.sublist` in Nix -- `genList` + `elemAt` is this
          # repo's own convention for a list slice (see e.g.
          # lib/json-toml-safety.nix).
          prefix = builtins.genList (j: builtins.elemAt resolved j) (n - 1);
        in
        if lastElem ? and then
          {
            inherit prefix;
            kind = "and";
            body = lastElem.and;
          }
        else if lastElem ? not then
          {
            inherit prefix;
            kind = "not";
            body = lastElem.not;
          }
        else
          null
      else
        null
    else
      null;

  # Validates a not/and lookahead BODY is one this file knows how to
  # verify: only lit/range/regex/eof, sequences, and choice thereof --
  # anything else (a rule reference, nested not/and, or unbounded star/
  # plus/opt) would need machinery this file doesn't implement, and throws
  # with a specific message naming the culprit rather than silently
  # miscompiling into a wrong or infinitely-looping verification.
  checkLookaheadBodySupported =
    expr:
    if expr ? lit || expr ? range || expr ? regex || expr ? eof then
      true
    else if builtins.isList expr then
      builtins.all checkLookaheadBodySupported expr
    else if expr ? choice then
      builtins.all checkLookaheadBodySupported expr.choice
    else
      throw "generate: { and = ...; }/{ not = ...; } lookahead body contains an unsupported form for synthesis: ${builtins.toJSON expr} -- only lit/range/regex/eof, sequences, and choice thereof are supported (no rule references, nested not/and, or star/plus/opt)";

  # Removes an eof-like alternative from a not/and lookahead BODY -- only
  # ever called in a context where the lookahead is statically known to
  # have a sibling generated right after it (see resolveLookahead's use in
  # the `isList expr` case below), meaning it can never be the last thing
  # in the document, meaning `{ eof = {}; }` inside the body can never
  # actually hold. Including an unreachable eof-like alternative in the
  # one-off verification grammar below would wrongly treat it as
  # reachable. Throws if pruning would leave a `choice` with no
  # alternatives, or if the WHOLE body is a bare eof-like leaf -- both mean
  # the assertion is unconditionally impossible to satisfy, not something
  # retrying with a different seed could ever fix.
  pruneEofBranches =
    expr:
    if isEofLike expr then
      throw "generate: not/and lookahead body is (or reduces to) a bare eof-like leaf, but this lookahead always has a sibling following it -- eof can never hold here, making the assertion unconditionally impossible to satisfy"
    else if builtins.isList expr then
      map pruneEofBranches expr
    else if expr ? choice then
      let
        survivors = builtins.filter (b: !(isEofLike b)) expr.choice;
      in
      if survivors == [ ] then
        throw "generate: not/and lookahead body's every choice alternative is eof-like, but this lookahead always has a sibling following it -- the assertion is unconditionally impossible to satisfy"
      else
        { choice = map pruneEofBranches survivors; }
    else
      expr;

  # Whether lookahead BODY matches at the very start of `candidate` --
  # reuses lib/packrat.nix's OWN `run` as the oracle (a one-off, single-
  # rule grammar `{ CHECK = body; }`), rather than re-deriving `lit`/
  # `regex`/`choice`/sequence match semantics a second time in this file:
  # any future divergence between the two engines' interpretation of the
  # SAME DSL forms (this repo has hit that failure mode more than once --
  # see grammar/aterm.nix's/examples/json-simple.nix's fixed bugs) would
  # otherwise silently make this verification unsound. A PREFIX match (not
  # requiring `body` to consume the whole of `candidate`) is correct here,
  # matching lib/packrat.nix's own `not`/`and` semantics: `candidate` is
  # only the sibling's own generated text, not the entire rest of the real
  # document, but real `not`/`and` themselves only ever look at a prefix
  # of whatever remains too.
  lookaheadHolds =
    body: candidate:
    (packrat.run {
      grammar = {
        CHECK = body;
      };
    } 0 candidate).CHECK != packrat.NO_MATCH;

  # Bounded retry count for lookahead-constrained sibling generation (see
  # the `isList expr` case below) -- a schema making the assertion
  # unconditionally true/false regardless of seed should throw, not loop
  # forever trying seeds that can never work.
  lookaheadMaxRetries = 10;

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
    seed: depth:
    # `isLast = false`: anyJsonValueSchema is a lib/valuewalk.nix-DSL
    # schema with no `eof`/`not`-regex forms at all (JSON's own grammar
    # has no such combinator), so `isLast` can never actually matter
    # here -- passed as a fixed `false` only because generateWith's
    # signature now requires SOME value.
    generateWith { } { } { } { } 3 anyJsonValueSchema (seed + "/json-value") depth false;

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
  # reachable. `isLast`: true iff nothing will be generated after THIS
  # expr in the whole document -- true at the top-level entry point
  # (`generate`/`generateFromSchema` below), and propagated through
  # exactly those cases where "the last element I generate is the last
  # thing in the document" holds (a sequence's own final element, a
  # `star`/`plus`'s own final repetition once no more are generated,
  # `choice`'s chosen branch, `opt`'s body when chosen, `action`'s `e`,
  # `cutSeq` degrading to a sequence) -- false everywhere something is
  # generated afterward (every non-final sequence element, every
  # non-final repetition, `listOf`/`attrsOf`/`attrs`' own elements, since
  # each one has siblings or a closing structure after it). See
  # `isEofLike` above for what this actually gates.
  generateWith =
    grammar: refs: patternGenerators: builtinParserGenerators: maxDepth: expr: seed: depth: isLast:
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
        refs.${expr} seed (depth + 1) isLast
    else if expr ? string then
      genString seed
    else if expr ? int then
      genInt seed
    else if expr ? bool then
      genBool seed
    else if expr ? lit then
      expr.lit
    else if expr ? eof then
      # { eof = {}; } (lib/packrat.nix): a plain leaf, not a lookahead --
      # sound to generate as "" ONLY when `isLast` holds (see this
      # file's header and `isEofLike` above for why): whatever the rest
      # of the document produced up to this point IS, by construction,
      # the entire generated document, so the real `eof` combinator
      # will find no input remaining when the parser reaches this
      # point. When `isLast` is false, something real is generated
      # after this occurrence, so "" would be UNSOUND -- `choice`'s own
      # branch-eligibility filter below is what keeps a non-last `eof`
      # branch from ever being chosen in the first place, so reaching
      # this case with `isLast = false` would be this file's own bug,
      # not a schema issue -- hence a throw, not a silent "".
      if isLast then
        ""
      else
        throw "generate: internal error -- { eof = {}; } reached with isLast = false (should have been filtered by choice's eligibility check)"
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
        # An eof-like branch is only a legitimate choice when THIS choice
        # itself is in trailing position -- excluding it otherwise (rather
        # than picking it and generating an unsound "") is what actually
        # fixes generation for grammar/gemfile-lock.nix's/grammar/yarn-
        # lock.nix's shared `lineEnd` idiom: their `eof` branch is never
        # reachable there (a real trailing `{ eof = {}; }` already exists
        # separately, at the true end of DOCUMENT), so excluding it here
        # simply leaves the sound `regex` branch as the only real option.
        depthEligible =
          if depth < maxDepth then
            builtins.genList (i: i) n
          else
            builtins.filter (i: isTerminal grammar (builtins.elemAt branches i)) (builtins.genList (i: i) n);
        eligible =
          if isLast then
            depthEligible
          else
            builtins.filter (i: !(isEofLike (builtins.elemAt branches i))) depthEligible;
      in
      if eligible == [ ] then
        throw "generate: choice has no terminal branch to bottom out at maxDepth (schema is unconditionally recursive), or every non-maxDepth-excluded branch is eof-like at a non-trailing position"
      else
        let
          chosen = builtins.elemAt eligible (seedToIndex (builtins.length eligible) (seed + "/choice"));
        in
        generate (builtins.elemAt branches chosen) (
          seed + "/branch${builtins.toString chosen}"
        ) depth isLast
    else if expr ? listOf then
      let
        len = if depth < maxDepth then seedToIndex 5 (seed + "/len") else 0;
      in
      builtins.genList (
        i: generate expr.listOf (seed + "/elem${builtins.toString i}") (depth + 1) false
      ) len
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
          value = generate expr.attrsOf (seed + "/val${builtins.toString i}") (depth + 1) false;
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
            value = generate required.${name} (seed + "/f-${name}") depth false;
          }) reqNames
        );
        includedOptNames = builtins.filter (
          name: depth < maxDepth && seedToBool (seed + "/inc-${name}")
        ) optNames;
        optResult = builtins.listToAttrs (
          map (name: {
            inherit name;
            value = generate optional.${name} (seed + "/f-${name}") depth false;
          }) includedOptNames
        );
      in
      reqResult // optResult
    else if builtins.isList expr then
      # Sequence: generate each sub-expr in order and concatenate --
      # packrat.nix-only (no valuewalk.nix form is ever a bare Nix list).
      # `isLast` only ever propagates to the FINAL element -- every
      # earlier element has real content generated after it in this
      # same sequence, so it can never be the document's own last thing.
      #
      # BEFORE generating element `i` plain, check whether it's a not/and
      # LOOKAHEAD (see NOT/AND LOOKAHEAD SYNTHESIS in this file's header
      # and `resolveLookahead` above) with a genuine sibling at `i+1` in
      # THIS SAME list -- if so, elements `i` and `i+1` are handled
      # together as one unit (see `genLookaheadUnit` below) instead of
      # independently, and generation continues at `i+2`. Every other
      # element (including a lookahead with no sibling, which is
      # deliberately left to throw via the ordinary `and`/`not` case
      # below) is generated exactly as before.
      let
        len = builtins.length expr;
        genPlain =
          i:
          generate (builtins.elemAt expr i) (seed + "/seq${builtins.toString i}") depth (
            isLast && i == len - 1
          );
        # Handles one (lookahead, sibling) pair: generate `look.prefix`
        # normally, then generate the sibling and verify -- retrying with
        # a derived seed on mismatch, bounded by `lookaheadMaxRetries` --
        # that it does/doesn't match `look.body` per `look.kind`. `eof`
        # inside `look.body` is pruned first (see `pruneEofBranches`):
        # since a sibling unconditionally follows here, `eof` can never
        # legitimately hold, and MUST be excluded from the check rather
        # than evaluated against the sibling's own (necessarily partial,
        # not whole-document) generated text.
        genLookaheadUnit =
          i: look:
          let
            sibling = builtins.elemAt expr (i + 1);
            prefixLen = builtins.length look.prefix;
            prefixText = builtins.concatStringsSep "" (
              builtins.genList (
                j:
                generate (builtins.elemAt look.prefix j) (
                  seed + "/seq${builtins.toString i}-pre${builtins.toString j}"
                ) depth false
              ) prefixLen
            );
            prunedBody = builtins.seq (checkLookaheadBodySupported look.body) (pruneEofBranches look.body);
            siblingIsLast = isLast && i + 1 == len - 1;
            trySibling =
              n:
              let
                siblingSeed =
                  seed
                  + "/seq${builtins.toString (i + 1)}"
                  + (if n == 0 then "" else "/lookahead-retry${builtins.toString n}");
                candidate = generate sibling siblingSeed depth siblingIsLast;
                matches = lookaheadHolds prunedBody candidate;
                ok = if look.kind == "not" then !matches else matches;
              in
              if ok then
                candidate
              else if n >= lookaheadMaxRetries then
                throw "generate: could not satisfy { ${look.kind} = ...; } lookahead after ${builtins.toString lookaheadMaxRetries} retries -- schema may make this assertion unconditionally ${
                  if look.kind == "not" then "true" else "false"
                } regardless of seed"
              else
                trySibling (n + 1);
          in
          prefixText + trySibling 0;
        genFrom =
          i:
          if i >= len then
            [ ]
          else
            let
              look = resolveLookahead grammar (builtins.elemAt expr i);
            in
            if look != null && i + 1 < len then
              [ (genLookaheadUnit i look) ] ++ genFrom (i + 2)
            else
              [ (genPlain i) ] ++ genFrom (i + 1);
      in
      builtins.concatStringsSep "" (genFrom 0)
    else if expr ? star then
      let
        count = if depth < maxDepth then seedToIndex 5 (seed + "/count") else 0;
      in
      builtins.concatStringsSep "" (
        builtins.genList (
          i: generate expr.star (seed + "/rep${builtins.toString i}") (depth + 1) (isLast && i == count - 1)
        ) count
      )
    else if expr ? plus then
      # Desugars to [ e { star = e; } ], same as packrat.nix's own
      # compile -- guarantees at least 1 repetition even at maxDepth,
      # unlike a bare star which emits 0 there.
      generate [
        expr.plus
        { star = expr.plus; }
      ] seed depth isLast
    else if expr ? opt then
      if depth < maxDepth && seedToBool (seed + "/opt") then
        generate expr.opt (seed + "/opt-body") depth isLast
      else
        ""
    else if expr ? cutSeq then
      # Degrades to a plain sequence for GENERATION -- see this file's
      # header comment for why cut/commit semantics don't apply here.
      generate expr.cutSeq seed depth isLast
    else if expr ? action then
      # `f` transforms E's matched value on success, operating on what
      # parsing produced. Generation never parses, so `f` never runs --
      # generate whatever `e` itself would accept and ignore `f`.
      generate expr.action.e seed depth isLast
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
      # of input" idiom -- no grammar currently shipped in this repo
      # still uses it (all migrated to `{ eof = {}; }`, see
      # lib/packrat.nix's own `eof` primitive), but the case is kept for
      # any external grammar built against the same DSL. Same `isLast`
      # gating as the `eof` case above, for the identical reason: sound
      # only when nothing is generated after this point.
      if isLast then
        ""
      else
        throw "generate: internal error -- { not = { regex = \"(.)\"; }; } reached with isLast = false (should have been filtered by choice's eligibility check)"
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
        _: expr: seed: depth: isLast:
        generateWith grammar compiled patternGenerators builtinParserGenerators maxDepth expr seed depth
          isLast
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
      # isLast = true: `ruleName` is the top-level entry point -- by
      # definition, nothing else is generated after it.
      compiled.${ruleName} seed 0 true;

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
    # isLast = true: `schema` is the top-level entry point.
    generateWith { } { } patternGenerators builtinParserGenerators maxDepth schema seed 0 true;
}
