# Generates a sample string that a POSIX ERE pattern (the kind used in
# this repo's `{ regex = "..."; }` grammar leaves, matched via
# `builtins.match`) would accept. Companion to lib/generate.nix, which
# handles every other grammar/schema form but delegates regex leaves
# here since a regex doesn't invert as trivially as `lit`/`range`/
# `choice`/`star` do.
#
# Approach: parse the ERE into a small AST (parseERE), then walk the AST
# producing a string, drawing every pseudo-random decision (which
# alternative, how many repetitions, which class member) from
# `builtins.hashString "sha256" seed`, deriving a fresh child seed for
# every recursive call -- a pure, reproducible function of (pattern,
# seed). Nix has no RNG and no char<->codepoint builtins, so both
# randomness and "pick the Nth character of a range" are faked with
# `hashString` and a precomputed character table, respectively.
#
# Supported ERE constructs (sufficient for every pattern used in
# grammar/*.nix):
#   - literals, including backslash-escaped metacharacters (\. \* \( etc.)
#     and the \r \n \t shorthands
#   - `.` (any character)
#   - bracket expressions `[...]` / `[^...]`, including ranges (`a-z`),
#     POSIX classes (`[:space:]`, `[:alpha:]`, `[:digit:]`, `[:alnum:]`,
#     `[:upper:]`, `[:lower:]`, `[:punct:]`), and the POSIX leading-`]`-is-
#     literal / trailing-`-`-is-literal rules
#   - grouping `(...)`
#   - alternation `a|b|c` (at top level or inside a group)
#   - quantifiers `*`, `+`, `?`, `{m}`, `{m,}`, `{m,n}`
#   - `^` / `$` treated as zero-width no-ops (this repo's grammars only
#     ever hand a pattern to `builtins.match` as a whole-string match, so
#     anchors carry no extra constraint -- see lib/packrat.nix's evalRegex)
#
# Explicitly NOT supported (throws a clear "regex-generate: ..." error
# rather than guessing):
#   - backreferences (don't exist in POSIX ERE, so no loss)
#   - anything that doesn't parse per the grammar below (unbalanced
#     parens, dangling backslash, malformed `{m,n}`, unterminated bracket
#     expressions, etc.)
#   - POSIX collating symbols / equivalence classes (`[.foo.]`, `[=a=]`)
#     are not specially parsed here, matching that `builtins.match`
#     doesn't implement them either (`[.foo.]` behaves identically to the
#     literal-char class `[.fo]` under `builtins.match`)
#
# Every generated string is meant to be checked by the CALLER via
# `builtins.match pattern generated != null` (see `generateForRegexChecked`
# below for a convenience wrapper that does this and throws loudly if
# generation ever produces something the pattern rejects).
rec {
  ################################################################
  # Small pure-Nix utilities: none of these exist as builtins.
  ################################################################

  # Ordered table of every character generation ever needs to reach into a
  # `[a-z]`-style range by "index arithmetic" -- Nix has no
  # builtins.chr/ord, so a range like `A-Z` is resolved by finding "A" and
  # "Z"'s positions in this literal, hand-written table and slicing it,
  # not by computing codepoints.
  asciiOrder = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";

  asciiChars = builtins.genList (i: builtins.substring i 1 asciiOrder) (
    builtins.stringLength asciiOrder
  );

  # char -> index in asciiChars, so expandRange can slice by position.
  asciiIndex = builtins.listToAttrs (
    builtins.genList (i: {
      name = builtins.elemAt asciiChars i;
      value = i;
    }) (builtins.length asciiChars)
  );

  expandRange =
    a: b:
    let
      ia =
        asciiIndex.${a} or (throw "regex-generate: range endpoint '${a}' outside supported ASCII table");
      ib =
        asciiIndex.${b} or (throw "regex-generate: range endpoint '${b}' outside supported ASCII table");
    in
    if ib < ia then
      throw "regex-generate: malformed range [${a}-${b}] (end before start)"
    else
      builtins.genList (i: builtins.elemAt asciiChars (ia + i)) (ib - ia + 1);

  # Decimal-digit lookup, used to turn the digit *characters* consumed by
  # the `{m,n}` brace-quantifier parser into an actual int (no
  # builtins.ord to subtract "0" from).
  decVal = {
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
  };

  isDigitChar = c: c != "" && decVal ? ${c};

  hexVal = decVal // {
    a = 10;
    b = 11;
    c = 12;
    d = 13;
    e = 14;
    f = 15;
  };

  ################################################################
  # Deterministic pseudo-randomness: every draw is `hashString sha256
  # seed`-derived, every recursive call forks the seed (seed + "/tag") so
  # sibling draws never collide and the whole walk is reproducible from
  # (pattern, seed) alone.
  ################################################################

  mkSeed = seed: tag: seed + "/" + tag;

  # First 8 hex digits (32 bits) of the seed's hash, as a plain int --
  # comfortably inside Nix's 64-bit signed int range, so no overflow care
  # needed turning it into a mod-able quantity.
  hashToInt =
    seed:
    let
      hex = builtins.substring 0 8 (builtins.hashString "sha256" seed);
      digits = builtins.genList (i: builtins.substring i 1 hex) (builtins.stringLength hex);
    in
    builtins.foldl' (acc: c: acc * 16 + hexVal.${c}) 0 digits;

  # Uniform-ish int in [0, max). `max <= 1` always yields 0 (avoids a
  # division by zero for single-option draws).
  randInt =
    seed: max:
    if max <= 1 then
      0
    else
      let
        h = hashToInt seed;
      in
      h - max * (h / max);

  ################################################################
  # POSIX class membership, used both for `[:name:]` bracket items and
  # nowhere else (dot/negated classes draw from asciiChars directly, not
  # through these predicates) -- deliberately only the classes actually
  # spellable in POSIX ERE bracket expressions.
  ################################################################

  posixClassChars =
    name:
    if name == "space" then
      [
        " "
        "\t"
        "\r"
        "\n"
      ]
    else if name == "digit" then
      builtins.genList (i: builtins.substring i 1 "0123456789") 10
    else if name == "alpha" then
      expandRange "A" "Z" ++ expandRange "a" "z"
    else if name == "alnum" then
      expandRange "A" "Z" ++ expandRange "a" "z" ++ expandRange "0" "9"
    else if name == "upper" then
      expandRange "A" "Z"
    else if name == "lower" then
      expandRange "a" "z"
    else if name == "punct" then
      # Excludes letters/digits/space explicitly rather than filtering by
      # ASCII-range position: a range-position filter would wrongly sweep
      # in the symbols between 'Z' and 'a' ([ \ ] ^ _ `) as excluded.
      let
        excluded = expandRange "A" "Z" ++ expandRange "a" "z" ++ expandRange "0" "9" ++ [ " " ];
      in
      builtins.filter (c: !(builtins.elem c excluded)) (expandRange "!" "~")
    else
      throw "regex-generate: unsupported POSIX class [:${name}:]";

  ################################################################
  # Parser: pattern (string) -> AST. Hand-written recursive descent,
  # threading position explicitly (Nix has no mutable state) --
  # `{ node; pos; }` is the universal return shape of every parse* helper
  # below, mirroring lib/packrat.nix's own "thread position by hand" style
  # but over a regex string instead of Derivs nodes.
  #
  # AST node shapes:
  #   { lit = "c"; }            one literal character
  #   { any = true; }           `.`
  #   { class = { negate; items = [ {single=c;} {range=[a b];}
  #               {posixClass=name;} ... ]; }; }
  #   { group = node; }         `(...)`
  #   { alt = [ node ... ]; }   `a|b|c`
  #   { concat = [ node ... ]; }
  #   { star = node; } / { plus = node; } / { opt = node; }
  #   { repeat = node; min = m; max = n-or-null; }   `{m,n}` / `{m,}` / `{m}`
  #   { zeroWidth = true; }     `^` / `$`
  ################################################################

  strAt = s: pos: if pos < builtins.stringLength s then builtins.substring pos 1 s else "";

  parseDigits =
    s: pos:
    let
      go =
        p: acc:
        let
          c = strAt s p;
        in
        if isDigitChar c then
          go (p + 1) (acc * 10 + decVal.${c})
        else
          {
            num = acc;
            pos = p;
          };
      start = go pos 0;
    in
    # Distinguish "no digits consumed" (num should read as absent) from
    # "consumed the digit 0" by comparing positions, not by testing the
    # value.
    if start.pos == pos then
      {
        num = null;
        pos = pos;
      }
    else
      start;

  parseEscape =
    e:
    if e == "" then
      throw "regex-generate: dangling backslash at end of pattern"
    else if e == "n" then
      "\n"
    else if e == "r" then
      "\r"
    else if e == "t" then
      "\t"
    else
      e; # \. \* \( \) \| \^ \$ \{ \} \[ \] \\ etc. -> literal char

  # Parses the inside of `[...]` / `[^...]`, starting just after the `[`.
  # Returns { node = { class = {...}; }; pos; } with `pos` just after the
  # matching `]`.
  parseBracket =
    s: pos0:
    let
      len = builtins.stringLength s;
      negate = strAt s pos0 == "^";
      pos1 = if negate then pos0 + 1 else pos0;

      go =
        pos: acc: first:
        let
          c = strAt s pos;
        in
        if c == "" then
          throw "regex-generate: unterminated bracket expression in pattern"
        else if c == "]" && !first then
          {
            items = acc;
            pos = pos + 1;
          }
        else if c == "[" && strAt s (pos + 1) == ":" then
          let
            closeRel = findPosixClassClose s (pos + 2);
            className = builtins.substring (pos + 2) (closeRel - (pos + 2)) s;
          in
          go (closeRel + 2) (acc ++ [ { posixClass = className; } ]) false
        else if strAt s (pos + 1) == "-" && strAt s (pos + 2) != "]" && strAt s (pos + 2) != "" then
          go (pos + 3) (
            acc
            ++ [
              {
                range = [
                  c
                  (strAt s (pos + 2))
                ];
              }
            ]
          ) false
        else
          go (pos + 1) (acc ++ [ { single = c; } ]) false;

      r = go pos1 [ ] true;
    in
    {
      node = {
        class = {
          inherit negate;
          items = r.items;
        };
      };
      pos = r.pos;
    };

  # Scans forward from `pos` (just after "[:") for the closing ":]",
  # returning the position of the ':' in ":]". Only used by parseBracket.
  findPosixClassClose =
    s: pos:
    let
      len = builtins.stringLength s;
      go =
        p:
        if p + 1 >= len then
          throw "regex-generate: unterminated POSIX class [:...:] in pattern"
        else if builtins.substring p 2 s == ":]" then
          p
        else
          go (p + 1);
    in
    go pos;

  parseAtom =
    s: pos:
    let
      c = strAt s pos;
    in
    if c == "(" then
      let
        inner = parseAlt s (pos + 1);
      in
      if strAt s inner.pos != ")" then
        throw "regex-generate: unbalanced '(' in pattern: ${s}"
      else
        {
          node = {
            group = inner.node;
          };
          pos = inner.pos + 1;
        }
    else if c == "[" then
      parseBracket s (pos + 1)
    else if c == "." then
      {
        node = {
          any = true;
        };
        pos = pos + 1;
      }
    else if c == "^" || c == "$" then
      {
        node = {
          zeroWidth = true;
        };
        pos = pos + 1;
      }
    else if c == "\\" then
      {
        node = {
          lit = parseEscape (strAt s (pos + 1));
        };
        pos = pos + 2;
      }
    else if c == "" then
      throw "regex-generate: unexpected end of pattern while expecting an atom"
    else if c == "*" || c == "+" || c == "?" then
      throw "regex-generate: quantifier '${c}' with no preceding atom in pattern: ${s}"
    else
      {
        node = {
          lit = c;
        };
        pos = pos + 1;
      };

  # Parses an optional quantifier suffix onto an already-parsed atom.
  parsePiece =
    s: pos:
    let
      atom = parseAtom s pos;
      c = strAt s atom.pos;
    in
    if c == "*" then
      {
        node = {
          star = atom.node;
        };
        pos = atom.pos + 1;
      }
    else if c == "+" then
      {
        node = {
          plus = atom.node;
        };
        pos = atom.pos + 1;
      }
    else if c == "?" then
      {
        node = {
          opt = atom.node;
        };
        pos = atom.pos + 1;
      }
    else if c == "{" then
      parseBound s (atom.pos + 1) atom.node
    else
      atom;

  # Parses `m}` / `m,}` / `m,n}`, starting just after the `{`.
  parseBound =
    s: pos: innerNode:
    let
      d1 = parseDigits s pos;
    in
    if d1.num == null then
      throw "regex-generate: malformed bound (expected digits after '{') in pattern: ${s}"
    else if strAt s d1.pos == "}" then
      {
        node = {
          repeat = innerNode;
          min = d1.num;
          max = d1.num;
        };
        pos = d1.pos + 1;
      }
    else if strAt s d1.pos == "," then
      let
        d2 = parseDigits s (d1.pos + 1);
      in
      if strAt s d2.pos != "}" then
        throw "regex-generate: malformed bound (expected '}') in pattern: ${s}"
      else
        {
          node = {
            repeat = innerNode;
            min = d1.num;
            max = d2.num; # null means unbounded ("{m,}")
          };
          pos = d2.pos + 1;
        }
    else
      throw "regex-generate: malformed bound (expected ',' or '}') in pattern: ${s}";

  parseConcat =
    s: pos:
    let
      go =
        p: acc:
        let
          c = strAt s p;
        in
        if c == "" || c == ")" || c == "|" then
          {
            node = {
              concat = acc;
            };
            pos = p;
          }
        else
          let
            piece = parsePiece s p;
          in
          go piece.pos (acc ++ [ piece.node ]);
    in
    go pos [ ];

  parseAlt =
    s: pos:
    let
      go =
        p: acc:
        let
          branch = parseConcat s p;
        in
        if strAt s branch.pos == "|" then
          go (branch.pos + 1) (acc ++ [ branch.node ])
        else
          {
            node = if acc == [ ] then branch.node else { alt = acc ++ [ branch.node ]; };
            pos = branch.pos;
          };
    in
    go pos [ ];

  # Top-level entry point: parses the WHOLE pattern as one ERE, throwing
  # if anything is left over (e.g. a stray unmatched ')').
  parseERE =
    pattern:
    let
      r = parseAlt pattern 0;
    in
    if r.pos != builtins.stringLength pattern then
      throw "regex-generate: unparsed trailing input in pattern (stray ')'?): ${pattern}"
    else
      r.node;

  ################################################################
  # Generator: AST -> seed -> string.
  ################################################################

  # How many repetitions to synthesize for an unbounded upper end
  # (`*`, `+`, `{m,}`) -- an arbitrary, documented small constant. `*`/`?`
  # get min=0, `+` gets min=1, `{m,}` keeps whatever `m` the pattern gave;
  # in every case the generated count is min + a small random extra, never
  # exhaustive over "unbounded".
  unboundedSlack = 3;

  buildClassPool =
    class: seed:
    let
      itemChars =
        item:
        if item ? single then
          [ item.single ]
        else if item ? range then
          expandRange (builtins.elemAt item.range 0) (builtins.elemAt item.range 1)
        else if item ? posixClass then
          posixClassChars item.posixClass
        else
          throw "regex-generate: unrecognized bracket item: ${builtins.toJSON item}";
      included = builtins.concatMap itemChars class.items;
    in
    if !class.negate then
      included
    else
      # Negated class: there's no way to enumerate "everything except
      # these characters" without a finite universe to draw from, so
      # draw from the same asciiChars table used for ranges and filter
      # out whatever the class excludes.
      let
        pool = builtins.filter (c: !(builtins.elem c included)) asciiChars;
      in
      if pool == [ ] then
        throw "regex-generate: negated class excludes the entire fallback character pool: ${builtins.toJSON class}"
      else
        pool;

  generate =
    node: seed:
    if node ? lit then
      node.lit
    else if node ? zeroWidth then
      ""
    else if node ? any then
      let
        pool = asciiChars ++ [
          "\n"
          "\r"
          "\t"
        ];
      in
      builtins.elemAt pool (randInt (mkSeed seed "any") (builtins.length pool))
    else if node ? class then
      let
        pool = buildClassPool node.class seed;
      in
      builtins.elemAt pool (randInt (mkSeed seed "cls") (builtins.length pool))
    else if node ? group then
      generate node.group seed
    else if node ? concat then
      builtins.concatStringsSep "" (
        builtins.genList (
          i: generate (builtins.elemAt node.concat i) (mkSeed seed "c${builtins.toString i}")
        ) (builtins.length node.concat)
      )
    else if node ? alt then
      let
        idx = randInt (mkSeed seed "alt") (builtins.length node.alt);
      in
      generate (builtins.elemAt node.alt idx) (mkSeed seed "alt${builtins.toString idx}")
    else if node ? star then
      generateRepeat node.star 0 null seed
    else if node ? plus then
      generateRepeat node.plus 1 null seed
    else if node ? opt then
      generateRepeat node.opt 0 1 seed
    else if node ? repeat then
      generateRepeat node.repeat node.min node.max seed
    else
      throw "regex-generate: unhandled AST node (regex-generate bug, or a construct claimed as supported but not implemented): ${builtins.toJSON node}";

  generateRepeat =
    inner: min: max: seed:
    let
      cap = if max == null then min + unboundedSlack else max;
      n = if cap <= min then min else min + randInt (mkSeed seed "n") (cap - min + 1);
    in
    builtins.concatStringsSep "" (
      builtins.genList (i: generate inner (mkSeed seed "r${builtins.toString i}")) n
    );

  ################################################################
  # Public entry points.
  ################################################################

  # pattern: string -> seed: string -> string
  # Throws "regex-generate: ..." for any pattern this file can't parse or
  # synthesize for (see the module header for exactly what's excluded).
  generateForRegex = pattern: seed: generate (parseERE pattern) seed;

  # Same, but self-verifying: throws loudly (rather than returning a
  # silently-wrong string) if the string it produced doesn't actually
  # match `pattern` under `builtins.match`. Useful for callers who want
  # the ground-truth check inline rather than doing it themselves; the
  # bare `generateForRegex` above skips this check to stay cheap for
  # bulk/exploratory use.
  generateForRegexChecked =
    pattern: seed:
    let
      s = generateForRegex pattern seed;
      ok = builtins.match pattern s != null;
    in
    if ok then
      s
    else
      throw "regex-generate: generated string failed builtins.match verification -- pattern: ${builtins.toJSON pattern}, generated: ${builtins.toJSON s}";
}
