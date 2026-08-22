# The JSON-with-comments grammar, ported from the original default.nix's
# embedded grammar, using the new star/opt/cutSeq combinators from
# lib/packrat.nix instead of hand-rolled right-recursive `choice` chains.
#
# Exposes both a `grammarNoCut` variant (Phase 1: top-level `X` is a plain
# ordered choice) and a `grammar` variant (Phase 2: top-level `X` applies
# the cut operator to each alternative, since SET/LIST/STRING/NUMBER/
# BOOL/NULL are first-token-disjoint -- see default.nix / final report for
# why this is semantically safe here, mirroring the cut paper's AC-FIRST
# motivating example in §4.2).
let
  # Rules shared verbatim between the cut and no-cut variants.
  common = {
    WHITESPACE = { star = { regex = "([[:space:]]+)"; }; };
    NULL = { lit = "null"; };
    BOOL = {
      choice = [
        { lit = "true"; }
        { lit = "false"; }
      ];
    };
    NUMBER = { regex = "([0-9]+)"; };

    STRING = [
      { lit = "\""; }
      "STRING_RAW"
      { lit = "\""; }
    ];
    # STRING_FRAG was previously a standalone nonterminal referenced only
    # from here; inlined directly into STRING_RAW's star body. Each
    # nonterminal is a separate field on EVERY Derivs node (one node per
    # input position, ~392000 of them for a 391947-byte file) regardless
    # of whether that position ever uses it, so a single-use wrapper
    # nonterminal like this one was pure per-node thunk-allocation
    # overhead with no benefit -- confirmed by profiling (see the research
    # notes that led to this change): unused grammar fields cost real,
    # measurable memory (each unforced thunk still gets allocated at every
    # node) and a modest amount of time (the extra nonterminal-reference
    # indirection hop), and there is no engine-level fix for that -- Nix's
    # mapAttrs-based lazy field construction can't skip allocating a thunk
    # just because it happens to go unused at a given position, and the
    # engine's memoization design depends on every field being a real,
    # shared attrset field (not something built through a dynamic-dispatch
    # cache). The only real lever is trimming the grammar's own rule
    # count, which is what this change does for STRING_FRAG/COMMENT_CHAR
    # (both single-use, both had no other purpose besides being one
    # `star`'s body).
    STRING_RAW = {
      star = {
        choice = [
          { regex = ''([^\\\"]+)''; }
          { lit = ''\"''; }
          { lit = ''\''; }
        ];
      };
    };

    COMMENT = [
      "WHITESPACE"
      { lit = "#"; }
      "COMMENT_BODY"
      "WHITESPACE"
    ];
    # Self-chunking, matching STRING_RAW/WHITESPACE's pattern: a `star` over
    # single-character regex matches, rather than one greedy `([^\n]+)`
    # match over the whole line. This makes COMMENT robust to a comment
    # line of ANY length regardless of evalRegex's bounded lookahead
    # window at the grammar level, as a second line of defense alongside
    # (not instead of) the engine-level growing-window retry in
    # lib/packrat.nix's evalRegex -- other grammars/rules can still write a
    # plain non-star regex atom and rely on the engine to handle a match
    # longer than the window correctly, but this rule no longer needs to.
    # COMMENT_CHAR was previously a standalone nonterminal referenced only
    # from here; inlined into COMMENT_BODY's star body for the same
    # per-node thunk-allocation reason as STRING_FRAG above.
    COMMENT_BODY = { star = { regex = "([^\n])"; }; };

    # e? lets LIST/SET accept an empty body ("[]"/"{}"), which the original
    # grammar could not parse at all (its LIST_ITEMS/ITEMS required >= 1
    # item). This can only add acceptance, never regress anything that
    # already parsed under the old grammar.
    LIST = [
      { lit = "["; }
      "WHITESPACE"
      { opt = "LIST_ITEMS"; }
      "WHITESPACE"
      { lit = "]"; }
    ];
    LIST_ITEMS = [
      "X"
      {
        star = [
          { lit = ","; }
          "X"
        ];
      }
    ];

    SET = [
      { lit = "{"; }
      "WHITESPACE"
      { opt = "ITEMS"; }
      "WHITESPACE"
      { lit = "}"; }
    ];
    ITEMS = [
      "ITEM"
      {
        star = [
          { lit = ","; }
          "ITEM"
        ];
      }
    ];
    ITEM = {
      choice = [
        [
          "WHITESPACE"
          "STRING"
          "WHITESPACE"
          { lit = ":"; }
          "X"
        ]
        [
          "COMMENT"
          "STRING"
          "WHITESPACE"
          { lit = ":"; }
          "X"
        ]
      ];
    };
  };

  # Order matters for a plain (non-cut) ordered choice: PEG tries branches
  # left-to-right and stops at the first success, so branches matching
  # MORE OFTEN in real input should come first to minimize wasted failed
  # attempts. Measured the actual value-type distribution of a real,
  # large flake.lock (14.2MB, /home/tbereknyei/sources/nix-overlay/
  # flake.lock): of ~362716 X-values, ~60% were strings, ~20% were
  # sets/dicts, ~13% numbers, ~6% bools, and LISTS were under 1% (2074) --
  # the previous order (SET, LIST, STRING, ...) tried the rarest type
  # (LIST) second and the most common type (STRING) third. Reordered to
  # STRING, SET, NUMBER, BOOL, LIST, NULL to match observed frequency.
  xBranches = [
    "STRING"
    "SET"
    "NUMBER"
    "BOOL"
    "LIST"
    "NULL"
  ];

  grammarNoCut = common // {
    X = [
      "WHITESPACE"
      { choice = xBranches; }
      "WHITESPACE"
    ];
  };

  # Phase 2: cut applied to every branch of the top-level choice. Each
  # branch becomes `{ cutSeq = [ "<NAME>" ""]; }` -- e1 is the real
  # sub-parse, e2 is epsilon purely to give the cut something to commit
  # after. Since the branches are first-token-disjoint ("{", "[", "\"",
  # digit, "t"/"f", "n"), committing right after e1 succeeds changes no
  # accept/reject outcome for this grammar; it's here for fidelity to the
  # task/paper, and Phase 3 measures whether it has any actual effect.
  grammar = common // {
    X = [
      "WHITESPACE"
      {
        choice = map (name: { cutSeq = [ name "" ]; }) xBranches;
      }
      "WHITESPACE"
    ];
  };

  # Handlers shared between both variants; only X's differs (see below),
  # since the cut variant's inner choice value is wrapped one level deeper
  # ([branchVal ""] per matched cutSeq branch) than the plain-choice
  # variant's (branchVal directly).
  handlersCommon = {
    WHITESPACE = v: builtins.concatStringsSep "" v;
    STRING_RAW = v: builtins.concatStringsSep "" v;
    # Return a plain Nix string rather than the old `{ string = ...; }`
    # wrapper -- that wrapper is what made every JSON string leaf come back
    # out as an object like `{"string": "..."}` instead of a plain JSON
    # string, breaking round-tripping. A real JSON string decodes to a
    # plain string and must re-encode the same way.
    STRING = v: builtins.elemAt v 1;
    ITEM = v: {
      name = builtins.elemAt v 1;
      value = builtins.elemAt v 4;
    };
    NUMBER = builtins.fromJSON;
    # Likewise for BOOL/NULL: without a handler these pass through as the
    # literally-matched text ("true"/"false"/"null", i.e. Nix strings), not
    # actual Nix `true`/`false`/`null` -- which `builtins.toJSON`/`--json`
    # would then render as quoted strings, also breaking round-tripping.
    BOOL = v: v == "true";
    NULL = v: null;

    LIST =
      v:
      let
        opt = builtins.elemAt v 2;
      in
      if opt == null then [ ] else [ (builtins.elemAt opt 0) ] ++ map (p: builtins.elemAt p 1) (builtins.elemAt opt 1);

    SET =
      v:
      let
        opt = builtins.elemAt v 2;
        items =
          if opt == null then
            [ ]
          else
            [ (builtins.elemAt opt 0) ] ++ map (p: builtins.elemAt p 1) (builtins.elemAt opt 1);
      in
      builtins.listToAttrs items;
  };

  handlersNoCut = handlersCommon // {
    X = v: builtins.elemAt v 1;
  };

  handlers = handlersCommon // {
    # Inner choice value for the cut variant is [branchVal ""] (from the
    # matched cutSeq's [e1val e2val]); unwrap one more level than the
    # no-cut variant.
    X = v: builtins.elemAt (builtins.elemAt v 1) 0;
  };
in
{
  inherit
    grammarNoCut
    grammar
    handlersNoCut
    handlers
    ;
}
