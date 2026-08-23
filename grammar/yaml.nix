# A real (subset of) YAML grammar for lib/packrat.nix -- block mappings and
# sequences nested by indentation, plain/quoted scalars, flow collections,
# comments. Contrast with grammar/flakelock.nix (single fixed schema, no
# generic recursion) and grammar/json.nix (generic, but no indentation
# sensitivity at all, since JSON's nesting is punctuation-delimited) -- YAML
# is the first grammar in this repo where nesting depth is driven by
# something the *input* decides (indentation), not by matching brackets.
#
# The engine (lib/packrat.nix) has no notion of "current indent column" --
# every nonterminal is a fixed rule, compiled once, independent of how deep
# in the document it's invoked. So indentation-sensitivity here is faked at
# the GRAMMAR level: `mkYamlGrammar` generates one full set of block rules
# per nesting depth (0..maxDepth), each depth's rules requiring exactly
# `indentStep * depth` literal leading spaces. A block value that continues
# more deeply just means "depth d's rules reference depth (d+1)'s rules" --
# ordinary PEG nonterminal reference, no new engine primitive. This is the
# same "generate repeated grammar structure via a Nix-level function" idea
# `grammar/flakelock.nix` uses for its field sequences, applied to depth
# instead of field name.
#
# Deliberate scope, to keep this a practical, correctness-checked subset
# rather than a full YAML 1.2 implementation:
#   - indentation must be exact multiples of `indentStep` spaces (no mixed
#     or arbitrary-width indents, no tabs for indentation)
#   - a document deeper than `maxDepth` levels fails to parse
#   - no anchors/aliases (&foo/*foo), no tags (!!str), no multi-document
#     ---/... separators, no directives
#   - explicit indentation indicators (`|2`, `>3-`, etc.) aren't supported,
#     AND a block scalar's base indent is always exactly the current
#     depth's indent plus one `indentStep` -- real YAML instead
#     auto-detects the base indent from the block's first content line
#     (so e.g. a block scalar indented 3 extra spaces under a 2-space-step
#     document still works in real YAML, treating all 3 as the base).
#     This subset requires exactly `indentStep` extra spaces; anything
#     beyond that is kept as literal leading whitespace in the content
#     instead of being absorbed into the base indent (confirmed
#     divergence from PyYAML on over-indented block scalars specifically;
#     exactly-`indentStep`-indented block scalars, the common case, are
#     unaffected and match PyYAML exactly)
#   - flow-collection scalars (inside [...]/{...}) can't contain a literal
#     `,` `]` `}` `:` unquoted -- quote such values, same restriction real
#     YAML has for flow-context plain scalars
#   - a `#` anywhere ends a scalar (even mid-word, e.g. inside a URL
#     fragment) -- real YAML only treats `#` as a comment when preceded by
#     whitespace; this subset always does
# A differently-indented or otherwise malformed document correctly fails to
# parse rather than silently mis-parsing.
#
# Every rule below is written as a named grammar/Derivs-node field (not
# `action`-inlined) even where inlining would be safe -- unlike
# grammar/flakelock.nix, this is written for debuggability first (each
# piece individually testable via `packrat.run`), matching how
# examples/json-simple.nix precedes examples/json-optimized.nix. Inlining
# is a plausible follow-up once this is verified correct.
let
  spaces = n: builtins.concatStringsSep "" (builtins.genList (_: " ") n);

  # Strips trailing spaces/tabs (not applied to leading whitespace, which
  # the grammar itself never captures into a scalar in the first place).
  # `match` requires >=1 non-space/tab character to anchor the capture, so
  # an all-whitespace or empty input has no match -- correctly trims to "".
  trimTrailing =
    s:
    let
      m = builtins.match "(.*[^ \t])[ \t]*" s;
    in
    if m == null then "" else builtins.head m;

  # Implements block scalar chomping + (for folded style) line-folding, per
  # the YAML spec's §8.1 rules -- verified empirically against PyYAML's
  # `safe_load` across dozens of hand-constructed cases (blank-line runs of
  # every length at every transition, leading/trailing blanks, chomp
  # indicators, more-indented lines interacting with folding) rather than
  # derived from the spec text, since the interaction between folding and
  # "more-indented" lines is easy to get subtly wrong -- e.g. it is NOT
  # simply "N blank lines -> N newlines except 0 -> fold": a transition
  # touching a "more-indented" line always uses N+1 newlines even at N=0,
  # while a plain-to-plain transition uses exactly N newlines at N>=1 (not
  # N+1) and folds to a single space only at N=0.
  #
  # `lines` is a list of `{kind; text;}` per source line, in order, where
  # `kind` is "blank" (a line that's empty once its whitespace is
  # discarded), "plain" (content with no extra indentation beyond the
  # block's base), or "more" (content with extra leading whitespace,
  # preserved verbatim in `text`) -- tagged by the grammar itself (see
  # BLOCK_LINE_BLANK/BLOCK_LINE_CONTENT below), not inferred from the
  # dedented string after the fact: a "more" line's text can itself be
  # pure whitespace (e.g. two literal spaces of content), which is
  # genuinely ambiguous to tell apart from a "blank" line by string
  # content alone -- confirmed via PyYAML that these two cases behave
  # differently (verified: a line with MORE leading whitespace than the
  # block's base indent keeps its excess as literal content even if nothing
  # follows it, while a line with the same-or-fewer whitespace is a true
  # blank whose content is discarded). `folded` is true for `>` style,
  # false for `|` (literal never folds, regardless of indentation);
  # `chomp` is "clip" (default), "strip" (`-`), or "keep" (`+`).
  foldBlockScalar =
    {
      lines,
      folded,
      chomp,
    }:
    let
      n = builtins.length lines;
      kindAt = i: (builtins.elemAt lines i).kind;
      textAt = i: (builtins.elemAt lines i).text;

      firstContentIdx =
        let
          go = i: if i >= n || kindAt i != "blank" then i else go (i + 1);
        in
        go 0;
      lastContentIdx =
        let
          go = i: if i < 0 || kindAt i != "blank" then i else go (i - 1);
        in
        go (n - 1);

      leadingBlankCount = firstContentIdx;
      trailingBlankCount =
        if firstContentIdx > lastContentIdx then n else n - 1 - lastContentIdx;

      # Walks lines[firstContentIdx..lastContentIdx] (guaranteed to start
      # and end on real content whenever this range is non-empty),
      # inserting the correct separator between each pair of content
      # lines, threading the interior blank-run length via `gap` (reset to
      # 0 whenever a content line is emitted).
      step =
        acc: i: gap: prevMore:
        if i > lastContentIdx then
          acc
        else if kindAt i == "blank" then
          step acc (i + 1) (gap + 1) prevMore
        else
          let
            more = kindAt i == "more";
            sep =
              if acc == "" then
                "" # first content line emitted: nothing precedes it
              else if !folded || prevMore || more then
                builtins.concatStringsSep "" (builtins.genList (_: "\n") (gap + 1))
              else if gap == 0 then
                " "
              else
                builtins.concatStringsSep "" (builtins.genList (_: "\n") gap);
          in
          step (acc + sep + textAt i) (i + 1) 0 more;

      hasContent = firstContentIdx <= lastContentIdx;
      body =
        if !hasContent then
          ""
        else
          builtins.concatStringsSep "" (builtins.genList (_: "\n") leadingBlankCount)
          + step "" firstContentIdx 0 false;
    in
    if n == 0 then
      ""
    else if chomp == "strip" then
      body
    else if chomp == "keep" then
      body
      + builtins.concatStringsSep "" (builtins.genList (_: "\n") (trailingBlankCount + (if body == "" then 0 else 1)))
    else
      # "clip": exactly one trailing newline, UNLESS the body is entirely
      # empty (all-blank content -- verified: "a: |\n\n\nb: 2\n" -> "").
      if body == "" then "" else body + "\n";

  # --- shared (depth-independent) rules ---------------------------------

  # Block-context whitespace: spaces/tabs only, NEVER newlines -- a
  # newline is structurally significant (ends a line/entry), so it must
  # never be silently skipped the way grammar/json.nix's WHITESPACE skips
  # everything. Flow collections use FLOW_WS instead, which does skip
  # newlines (flow content is indentation-independent in real YAML too).
  sharedGrammar = {
    WS = { opt = { regex = "([ \t]+)"; }; };
    FLOW_WS = { opt = { regex = "([ \t\r\n]+)"; }; };

    # Rejects when the upcoming line is actually a sequence marker ("-"
    # followed by whitespace or end-of-line). Without this, MAPPING_ENTRY
    # would happily match a "- key: value" sequence-item line as a
    # mapping entry with key "- key" -- PLAIN_KEY's regex has no reason to
    # stop at a leading "-", it only stops at ":"/"#"/EOL. Every depth's
    # MAPPING_ENTRY starts with this lookahead so the enclosing `choice`
    # correctly falls through to BLOCK_SEQUENCE instead.
    NOT_SEQ_MARKER = {
      not = [
        { lit = "-"; }
        {
          choice = [
            { regex = "([ \t])"; }
            { not = { regex = "(.)"; }; }
          ];
        }
      ];
    };

    # A mapping key/value separator ":" -- but only when followed by
    # whitespace, newline, or end-of-input, never consumed as part of a
    # bare colon-containing scalar. Without this check, a plain scalar
    # like "http://example.com" (used as e.g. a sequence item) would get
    # misparsed as a one-entry mapping {name="http"; value="//example.com";}
    # -- PLAIN_KEY's regex has no reason to stop before a colon that isn't
    # actually a separator. Real YAML has the same "colon needs following
    # whitespace to be a separator" rule. `{and=...;}` is a zero-width
    # lookahead: it doesn't consume the whitespace/newline itself, it's
    # left for MAPPING_VALUE's own WS/EOL to consume normally.
    COLON_SEP = [
      { lit = ":"; }
      {
        and = {
          choice = [
            { regex = "([ \t])"; }
            { regex = "(\r?\n)"; }
            { not = { regex = "(.)"; }; }
          ];
        };
      }
    ];

    COMMENT = {
      opt = [
        { lit = "#"; }
        { opt = { regex = "([^\r\n]*)"; }; }
      ];
    };

    # "end of line": an actual newline, OR end of input (the last line of
    # a file need not have a trailing newline). Consumes nothing in the
    # EOF case -- never `star`/`plus`-repeated directly (see BLANK_LINE
    # below for why that would be unsafe). `{regex="(.)";}` matches any
    # single character and fails only at end-of-input, so `!(.)` is a
    # correct EOF test (confirmed: builtins.match "(.)" "" == null).
    LINE_END = {
      choice = [
        { regex = "(\r?\n)"; }
        { not = { regex = "(.)"; }; }
      ];
    };

    # Trailing whitespace, optional comment, then LINE_END. Used once at
    # the end of every value-bearing line (mapping/sequence entries,
    # top-level scalar document).
    EOL = [
      "WS"
      "COMMENT"
      "LINE_END"
    ];

    # Like EOL, but requires an ACTUAL newline, not the EOF alternative --
    # `star`-repeated to skip blank/comment-only lines (leading/trailing).
    # If this allowed the EOF branch, {star = "BLANK_LINE";} could match
    # zero-width forever at end-of-input and never terminate the loop.
    BLANK_LINE = [
      "WS"
      "COMMENT"
      { regex = "(\r?\n)"; }
    ];

    QUOTED_SINGLE = {
      action = {
        e = [
          { lit = "'"; }
          {
            star = {
              choice = [
                { lit = "''"; }
                { regex = "([^']+)"; }
              ];
            };
          }
          { lit = "'"; }
        ];
        f = v: builtins.concatStringsSep "" (map (frag: if frag == "''" then "'" else frag) (builtins.elemAt v 1));
      };
    };

    QUOTED_DOUBLE = {
      action = {
        e = [
          { lit = "\""; }
          {
            star = {
              choice = [
                { regex = "([^\\\\\"]+)"; }
                { lit = "\\\""; }
                { lit = "\\\\"; }
                { lit = "\\n"; }
                { lit = "\\t"; }
                { lit = "\\r"; }
              ];
            };
          }
          { lit = "\""; }
        ];
        f =
          v:
          let
            mapFrag =
              frag:
              if frag == "\\\"" then
                "\""
              else if frag == "\\\\" then
                "\\"
              else if frag == "\\n" then
                "\n"
              else if frag == "\\t" then
                "\t"
              else if frag == "\\r" then
                "\r"
              else
                frag;
          in
          builtins.concatStringsSep "" (map mapFrag (builtins.elemAt v 1));
      };
    };

    NUMBER = {
      action = {
        e = { regex = "(-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?)"; };
        f = builtins.fromJSON;
      };
    };
    BOOL = {
      action = {
        e = {
          choice = [
            { lit = "true"; }
            { lit = "false"; }
          ];
        };
        f = v: v == "true";
      };
    };
    NULL = {
      action = {
        e = {
          choice = [
            { lit = "null"; }
            { lit = "~"; }
          ];
        };
        f = v: null;
      };
    };

    # A plain (unquoted) key: anything up to the separating ":", "#", or
    # end of line. Trimmed of trailing whitespace picked up by the greedy
    # match (e.g. "key : value" leaves a trailing space in the capture).
    # Must NOT start with a space/tab: without that check, a mis-indented
    # line (indentation that isn't an exact multiple of indentStep, so
    # indentLit consumes too little or too much) would silently absorb the
    # leftover/missing spaces into the key name instead of failing to
    # parse -- e.g. "a:\n   b: 1" (3-space indent under a 2-space-step
    # grammar) would parse as key " b" rather than rejecting the bad
    # indentation.
    PLAIN_KEY = {
      action = {
        e = [
          { not = { regex = "([ \t])"; }; }
          { regex = "([^:\r\n#]+)"; }
        ];
        f = v: trimTrailing (builtins.elemAt v 1);
      };
    };
    KEY = {
      choice = [
        "QUOTED_DOUBLE"
        "QUOTED_SINGLE"
        "PLAIN_KEY"
      ];
    };

    # Unquoted scalar filling the rest of a block-context line. Stops at
    # "#" unconditionally (see file header: real YAML requires preceding
    # whitespace for "#" to start a comment; this subset doesn't check).
    # Guarded by NOT_SEQ_MARKER: without it, "- 1" right after another "- "
    # (compact nested-sequence notation, e.g. "- - 1") would be silently
    # swallowed whole as the scalar string "- 1" instead of failing to
    # parse. Compact nested sequences aren't supported by this grammar
    # (see file header) -- rejecting cleanly here is the point, not a
    # workaround to make them work.
    PLAIN_SCALAR_LINE = {
      action = {
        e = [
          "NOT_SEQ_MARKER"
          { regex = "([^\r\n#]+)"; }
        ];
        f = v: trimTrailing (builtins.elemAt v 1);
      };
    };
    # Unquoted scalar inside a flow collection: additionally stops at the
    # flow delimiters "," "]" "}" ":" (a literal colon/comma/bracket in a
    # flow scalar needs quoting -- see file header).
    PLAIN_SCALAR_FLOW = {
      action = {
        e = { regex = "([^]},:\r\n#]+)"; };
        f = trimTrailing;
      };
    };

    FLOW_VALUE = {
      choice = [
        "QUOTED_DOUBLE"
        "QUOTED_SINGLE"
        "FLOW_LIST"
        "FLOW_MAP"
        "NULL"
        "BOOL"
        "NUMBER"
        "PLAIN_SCALAR_FLOW"
      ];
    };
    INLINE_VALUE = {
      choice = [
        "QUOTED_DOUBLE"
        "QUOTED_SINGLE"
        "FLOW_LIST"
        "FLOW_MAP"
        "NULL"
        "BOOL"
        "NUMBER"
        "PLAIN_SCALAR_LINE"
      ];
    };

    # `|` (literal) or `>` (folded), optionally followed by a chomping
    # indicator (`-` strip, `+` keep, absent = clip), then trailing
    # whitespace/comment/EOL -- the header line a block scalar starts
    # with. Explicit indentation-indicator digits (e.g. `|2`) aren't
    # supported (see file header) -- only the bare style+chomp form.
    # `{regex="([|>])";}`'s single capture group is all evalRegex ever
    # returns (see lib/packrat.nix's evalRegex -- only builtins.head of
    # the match is used), so the style and chomp characters are captured
    # as two SEPARATE regex atoms in sequence, not two groups of one
    # match.
    BLOCK_HEADER = {
      action = {
        e = [
          {
            choice = [
              { lit = "|"; }
              { lit = ">"; }
            ];
          }
          {
            opt = {
              choice = [
                { lit = "-"; }
                { lit = "+"; }
              ];
            };
          }
          "EOL"
        ];
        f = v: {
          folded = builtins.elemAt v 0 == ">";
          chomp =
            if builtins.elemAt v 1 == "-" then
              "strip"
            else if builtins.elemAt v 1 == "+" then
              "keep"
            else
              "clip";
        };
      };
    };

    FLOW_LIST = {
      action = {
        e = [
          { lit = "["; }
          "FLOW_WS"
          {
            opt = [
              "FLOW_VALUE"
              {
                star = [
                  "FLOW_WS"
                  { lit = ","; }
                  "FLOW_WS"
                  "FLOW_VALUE"
                ];
              }
            ];
          }
          "FLOW_WS"
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

    FLOW_PAIR = {
      action = {
        e = [
          "KEY"
          "FLOW_WS"
          { lit = ":"; }
          "FLOW_WS"
          "FLOW_VALUE"
        ];
        f = v: {
          name = builtins.elemAt v 0;
          value = builtins.elemAt v 4;
        };
      };
    };
    FLOW_MAP = {
      action = {
        e = [
          { lit = "{"; }
          "FLOW_WS"
          {
            opt = [
              "FLOW_PAIR"
              {
                star = [
                  "FLOW_WS"
                  { lit = ","; }
                  "FLOW_WS"
                  "FLOW_PAIR"
                ];
              }
            ];
          }
          "FLOW_WS"
          { lit = "}"; }
        ];
        f =
          v:
          let
            opt = builtins.elemAt v 2;
          in
          builtins.listToAttrs (
            if opt == null then [ ] else [ (builtins.elemAt opt 0) ] ++ map (p: builtins.elemAt p 3) (builtins.elemAt opt 1)
          );
      };
    };

    # "- key: value" shorthand: a sequence item that's itself a one-entry
    # inline mapping. The ":" must be followed by at least one space/tab
    # to count as a separator here -- without that check, a plain scalar
    # like "http://x.com" (colon with no following space) would be
    # misparsed as {name="http"; value="//x.com";}. Real YAML has the same
    # "colon needs trailing whitespace to be a separator" rule.
    INLINE_MAPPING_SHORTHAND = {
      action = {
        e = [
          "KEY"
          { lit = ":"; }
          { regex = "([ \t]+)"; }
          "INLINE_VALUE"
        ];
        f = v: builtins.listToAttrs [
          {
            name = builtins.elemAt v 0;
            value = builtins.elemAt v 3;
          }
        ];
      };
    };
    INLINE_ENTRY = {
      choice = [
        "INLINE_MAPPING_SHORTHAND"
        "INLINE_VALUE"
      ];
    };

    SCALAR_LINE = {
      action = {
        e = [
          "WS"
          "INLINE_VALUE"
          "EOL"
        ];
        f = v: builtins.elemAt v 1;
      };
    };
    BODY = {
      choice = [
        "BLOCK_MAPPING_0"
        "BLOCK_SEQUENCE_0"
        "SCALAR_LINE"
      ];
    };
    # Requires consuming the ENTIRE input, not just a prefix of it --
    # without the trailing EOF check, a document this grammar can't fully
    # parse (e.g. compact nested sequences, or any other unsupported
    # construct) would silently succeed on whatever prefix DID parse and
    # drop the rest, rather than failing outright. Same reasoning as
    # grammar/flakelock.nix's exact-schema-or-fail design.
    DOCUMENT = {
      action = {
        e = [
          { star = "BLANK_LINE"; }
          "BODY"
          { star = "BLANK_LINE"; }
          { not = { regex = "(.)"; }; }
        ];
        f = v: builtins.elemAt v 1;
      };
    };
  };
  sharedHandlers = { };

  # --- per-depth rules ---------------------------------------------------

  mkYamlGrammar =
    {
      indentStep ? 2,
      maxDepth ? 10,
    }:
    let
      depths = builtins.genList (d: d) (maxDepth + 1);

      rulesForDepth =
        d:
        let
          n = suffix: "${suffix}_${toString d}";
          nAt = suffix: dd: "${suffix}_${toString dd}";
          indentLit = { lit = spaces (indentStep * d); };
          hasNested = d < maxDepth;

          # A block scalar's content lines are indented at least one level
          # deeper than the key/dash introducing it -- exactly the depth
          # (d+1) rules already used for nested blocks, so no new
          # indent-arithmetic is needed. A "blank" line is a line with
          # AT MOST that indent worth of pure whitespace before the
          # newline (the indent itself is consumed, discarded, and the
          # line tagged "blank"); a "content" line has strictly more --
          # its exact leading whitespace is `indentLit_(d+1)` (discarded)
          # plus zero or more EXTRA spaces (kept, tagged "more" if
          # nonzero, "plain" if zero) followed by the rest of the line
          # (kept verbatim, since a block scalar's content is never
          # further interpreted -- no escapes, no plain/quoted-scalar
          # rules apply inside it).
          blockIndentNext = { lit = spaces (indentStep * (d + 1)); };
          blockLineBlank = {
            action = {
              e = [
                { regex = "([ \t]{0,${toString (indentStep * (d + 1))}})"; }
                { regex = "(\r?\n)"; }
              ];
              f = v: {
                kind = "blank";
                text = "";
              };
            };
          };
          blockLineContent = {
            action = {
              e = [
                blockIndentNext
                { opt = { regex = "([ \t]+)"; }; } # extra indent beyond the base, if any
                { opt = { regex = "([^\r\n]+)"; }; } # rest of the line (can be empty: e.g. "  \n" is extra-indent-only)
                {
                  choice = [
                    { regex = "(\r?\n)"; }
                    { not = { regex = "(.)"; }; } # EOF: last line, no trailing newline
                  ];
                }
              ];
              f =
                v:
                let
                  extra = if builtins.elemAt v 1 == null then "" else builtins.elemAt v 1;
                  rest = if builtins.elemAt v 2 == null then "" else builtins.elemAt v 2;
                in
                {
                  kind = if extra == "" then "plain" else "more";
                  text = extra + rest;
                };
            };
          };
          # Ordered so a genuinely blank line (no non-whitespace content)
          # is never misread as "more" with empty rest-of-line -- tried
          # BEFORE blockLineContent since blockLineContent's own indent
          # literal would otherwise happily consume a blank line's
          # whitespace as "extra indent" with an empty "rest", producing
          # kind "more" instead of "blank" for e.g. a line with exactly
          # `indentStep*(d+1)` spaces and nothing else (both branches can
          # match that specific case; blank must win -- confirmed via
          # PyYAML that a line with AT MOST the base indent's worth of
          # whitespace is always blank, never "more").
          blockLine = {
            choice = [
              blockLineBlank
              blockLineContent
            ];
          };
          blockScalarBody = { star = blockLine; };

          mappingValueBranchA = [
            "WS"
            "INLINE_VALUE"
            "EOL"
          ];
          mappingValueBranchB = [
            "EOL"
            {
              choice = [
                (nAt "BLOCK_MAPPING" (d + 1))
                (nAt "BLOCK_SEQUENCE" (d + 1))
              ];
            }
          ];
          # "key: |" / "key: >" -- see BLOCK_HEADER; must be tried before
          # branchB/C since its header line always ends in EOL too, but
          # the leading "|"/">" makes it unambiguous with a single
          # first-token check, same PEG-ordering reasoning as every other
          # disjoint-first-token choice in this grammar.
          mappingValueBranchBlockScalar = {
            action = {
              e = [
                "WS"
                "BLOCK_HEADER"
                blockScalarBody
              ];
              f =
                v:
                let
                  header = builtins.elemAt v 1;
                in
                foldBlockScalar {
                  lines = builtins.elemAt v 2;
                  inherit (header) folded chomp;
                };
            };
          };
          # "key:" with nothing after it at all -- no inline value, no
          # nested block on a following line -- is implicit null in real
          # YAML (confirmed via PyYAML: "on:\n  pull_request:\n" ->
          # {pull_request: None}), and shows up often in practice (e.g. a
          # GitHub Actions trigger key with no filters). Tried LAST: it's
          # just "EOL" with no value at all, a prefix of branchB's "EOL
          # <nested block>" shape, so it must only match once a nested
          # block genuinely isn't there (ordered choice handles this
          # naturally -- branchB is tried first and wins whenever a nested
          # block IS present).
          mappingValueBranchC = { action = { e = "EOL"; f = v: null; }; };
          mappingValueExpr =
            if hasNested then
              {
                choice = [
                  mappingValueBranchBlockScalar
                  mappingValueBranchA
                  mappingValueBranchB
                  mappingValueBranchC
                ];
              }
            else
              {
                choice = [
                  mappingValueBranchBlockScalar
                  mappingValueBranchA
                  mappingValueBranchC
                ];
              };

          sequenceValueBranchA = [
            { lit = " "; }
            "INLINE_ENTRY"
            "EOL"
          ];
          sequenceValueBranchB = [
            "EOL"
            {
              choice = [
                (nAt "BLOCK_MAPPING" (d + 1))
                (nAt "BLOCK_SEQUENCE" (d + 1))
              ];
            }
          ];
          # "- key: value" where more keys of the SAME mapping follow on
          # continuation lines (the common "list of records" idiom):
          #   items:
          #     - name: a
          #       value: 1
          #     - name: b
          #       value: 2
          # Only reachable when indentStep == 2: the column right after
          # "- " is `indentStep*d + 2`, which equals depth (d+1)'s own
          # indent literal (`indentStep*(d+1)`) only when indentStep == 2
          # -- exactly when that's true, continuation lines are just
          # ordinary MAPPING_ENTRY_(d+1) lines and no new machinery is
          # needed beyond reusing them directly. For any other indentStep,
          # this branch is skipped entirely; the same content must instead
          # use the fully block-nested form ("-\n    key: value\n    ...")
          # via sequenceValueBranchB. `star` (not `plus`) so a single pair
          # with zero continuations also matches here (this branch is a
          # strict superset of the plain "- key: value" shorthand in that
          # case, so it's tried first; INLINE_MAPPING_SHORTHAND inside
          # branchA below still exists for eligibility cases where this
          # branch isn't defined).
          sequenceValueBranchC = [
            { lit = " "; }
            "NOT_SEQ_MARKER"
            "KEY"
            "WS"
            "COLON_SEP"
            (nAt "MAPPING_VALUE" (d + 1))
            { star = nAt "MAPPING_ENTRY" (d + 1); }
          ];
          # A bare "-" with nothing after it at all is implicit null in
          # real YAML (confirmed via PyYAML: "items:\n  - a\n  -\n" ->
          # {items: ["a", None]}), same reasoning as mappingValueBranchC.
          # Tried last for the same reason: it's a prefix-shape of
          # branchB's "EOL <nested block>", so ordered choice already
          # prefers branchB whenever a nested block actually follows.
          sequenceValueBranchD = { action = { e = "EOL"; f = v: null; }; };
          # "- |" / "- >" -- same BLOCK_HEADER/blockScalarBody machinery
          # as mappingValueBranchBlockScalar, just after "- " instead of
          # "key: " (confirmed via PyYAML: "items:\n  - |\n    x\n    y\n
          # - z\n" -> {items: ["x\ny\n", "z"]}).
          sequenceValueBranchBlockScalar = {
            action = {
              e = [
                { lit = " "; }
                "BLOCK_HEADER"
                blockScalarBody
              ];
              f =
                v:
                let
                  header = builtins.elemAt v 1;
                in
                foldBlockScalar {
                  lines = builtins.elemAt v 2;
                  inherit (header) folded chomp;
                };
            };
          };
          continuationEligible = hasNested && indentStep == 2;
          sequenceValueExpr =
            if continuationEligible then
              {
                choice = [
                  sequenceValueBranchC
                  sequenceValueBranchBlockScalar
                  sequenceValueBranchA
                  sequenceValueBranchB
                  sequenceValueBranchD
                ];
              }
            else if hasNested then
              {
                choice = [
                  sequenceValueBranchBlockScalar
                  sequenceValueBranchA
                  sequenceValueBranchB
                  sequenceValueBranchD
                ];
              }
            else
              {
                choice = [
                  sequenceValueBranchBlockScalar
                  sequenceValueBranchA
                  sequenceValueBranchD
                ];
              };
        in
        {
          grammar = {
            "${n "MAPPING_VALUE"}" = mappingValueExpr;
            "${n "MAPPING_ENTRY"}" = [
              { star = "BLANK_LINE"; }
              indentLit
              "NOT_SEQ_MARKER"
              "KEY"
              "WS"
              "COLON_SEP"
              (n "MAPPING_VALUE")
            ];
            "${n "BLOCK_MAPPING"}" = { plus = n "MAPPING_ENTRY"; };

            "${n "SEQUENCE_VALUE"}" = sequenceValueExpr;
            "${n "SEQUENCE_ENTRY"}" = [
              { star = "BLANK_LINE"; }
              indentLit
              { lit = "-"; }
              (n "SEQUENCE_VALUE")
            ];
            "${n "BLOCK_SEQUENCE"}" = { plus = n "SEQUENCE_ENTRY"; };
          };
          handlers = {
            "${n "MAPPING_VALUE"}" =
              v:
              # branchBlockScalar returns a bare string directly (its own
              # action already folds the block scalar to its final
              # value); branchC returns bare `null`; branchA/B are raw
              # (un-action-wrapped) sequences, indexable at 1. A real
              # value can itself be `null` (BOOL/NULL branches) but never
              # a bare Nix string OTHER than via branchBlockScalar, since
              # every other branch's raw shape is a list -- safe to
              # distinguish by `isString`.
              if v == null then
                null
              else if builtins.isString v then
                v
              else
                builtins.elemAt v 1;
            "${n "MAPPING_ENTRY"}" = v: {
              name = builtins.elemAt v 3;
              value = builtins.elemAt v 6;
            };
            "${n "BLOCK_MAPPING"}" =
              v: builtins.listToAttrs ([ (builtins.elemAt v 0) ] ++ (builtins.elemAt v 1));

            "${n "SEQUENCE_VALUE"}" =
              v:
              # branchD (a bare "-" with nothing after) is action-wrapped
              # and returns bare `null` directly; branchBlockScalar
              # returns a bare string directly (already folded) --
              # distinguishable from branchA/B/C's raw (un-action-wrapped)
              # LIST values, no risk of colliding with a real scalar's
              # content. sequenceValueBranchC's raw value is a 7-element
              # sequence ending in a list of already-
              # handler-transformed {name; value;} attrsets
              # (MAPPING_ENTRY_(d+1)'s own handler runs before `star`
              # collects them) -- branchA/B are each a single value at
              # index 1, same as before. Distinguish by length: only
              # branchC's sequence has 7 elements.
              if v == null then
                null
              else if builtins.isString v then
                v
              else if continuationEligible && builtins.length v == 7 then
                builtins.listToAttrs (
                  [
                    {
                      name = builtins.elemAt v 2;
                      value = builtins.elemAt v 5;
                    }
                  ]
                  ++ (builtins.elemAt v 6)
                )
              else
                builtins.elemAt v 1;
            "${n "SEQUENCE_ENTRY"}" = v: builtins.elemAt v 3;
            "${n "BLOCK_SEQUENCE"}" = v: [ (builtins.elemAt v 0) ] ++ (builtins.elemAt v 1);
          };
        };

      perDepth = map rulesForDepth depths;
      depthGrammar = builtins.foldl' (acc: r: acc // r.grammar) { } perDepth;
      depthHandlers = builtins.foldl' (acc: r: acc // r.handlers) { } perDepth;
    in
    {
      grammar = sharedGrammar // depthGrammar;
      handlers = sharedHandlers // depthHandlers;
    };

  default = mkYamlGrammar { };
in
{
  inherit mkYamlGrammar;
  inherit (default) grammar handlers;
}
