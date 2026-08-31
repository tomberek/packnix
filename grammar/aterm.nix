# A generic ATerm (Annotated Term) grammar for lib/packrat.nix.
#
# ATerm is a language-independent term format from the ASF+SDF
# Meta-Environment / Stratego/XT toolset -- and, closer to home, it's
# what Nix's own `.drv` files are written in (`Derive([...],[...],...)`
# is a plain ATerm application). See grammar/drv.nix for a grammar
# specialized to that exact shape with semantic field decoding; this
# file covers the general term format, all six term kinds, usable for
# any ATerm document, not just `.drv`s.
#
# Term kinds (van den Brand, de Jong, Klint, Olivier, "Efficient
# Annotated Terms", Software: Practice and Experience 2000):
#   - INT:         -?[0-9]+
#   - REAL:        -?[0-9]+.[0-9]+([eE][-+]?[0-9]+)?  -- a decimal point
#                  is what distinguishes a REAL from an INT; no bare
#                  exponent without one (ECMA/JSON-style "1e10" is NOT
#                  a valid ATerm REAL under this grammar).
#   - APPL:        Constructor("(" args ")")?  -- a constructor with no
#                  args at all (bare `foo`, no parens) is still a valid
#                  APPL, distinct from a 0-arity `foo()`.
#   - LIST:        "[" (term ("," term)*)? "]"
#   - TUPLE:       "(" (term ("," term)*)? ")"  -- an anonymous-
#                  constructor application, syntactically identical to
#                  APPL's arg list but with no Constructor prefix at
#                  all. Real and load-bearing: every Nix `.drv`'s
#                  outputs/inputDrvs/env entries are exactly this (e.g.
#                  `("out","/nix/...","sha256","abc...")`), not a
#                  Constructor-prefixed APPL. Real corpus check (500
#                  real `.drv` files): every observed tuple has exactly
#                  2 or 4 elements, never 0 or 1 -- but this grammar
#                  doesn't special-case that, since nothing in the
#                  format itself forbids other arities.
#   - PLACEHOLDER: "<" term ">"  -- used by Stratego for pattern-holes;
#                  not produced by ordinary term construction, but part
#                  of the textual format.
#   - BLOB is deliberately NOT modeled: it's a binary payload with no
#     defined textual syntax in the spec -- every real textual-ATerm
#     writer (including Nix's own) only ever produces the six kinds above.
#
# A Constructor is either a bare identifier ([A-Za-z_][A-Za-z0-9_]*) or
# a quoted string (same syntax as a STRING term, but immediately
# followed by "(" -- a bare quoted string with no trailing "(" is a
# STRING term instead, not an APPL with zero args; the two are only
# distinguished by that lookahead). Some ATerm literature documents a
# lowercase-first Constructor as canonical, but real corpus evidence
# contradicts a hard lowercase-first rule -- Nix's own `.drv` writer
# emits `Derive`, confirmed uppercase-first in every real file sampled
# -- so this grammar accepts either case.
#
# ANNOTATION ("{" term-list "}") can attach to any BasicTerm, tried once
# after the term itself -- optional.
#
# String escapes (confirmed against Nix's own ATerm writer, the one real
# producer available to test against): `\"`, `\\`, `\n`, `\r`, `\t`.
# This grammar accepts exactly that set for any ATerm document, not just
# Nix's own output -- the wider ATerm literature doesn't document a
# fuller escape set either.
let
  ws = {
    opt = {
      regex = "([ \t\r\n]+)";
    };
  };

  # A digit run and everything after it start identically, so REAL is
  # tried before INT in the grammar's choice below via cutSeq: once the
  # digits+"." shape is seen, commit -- an INT can never itself contain
  # a "." (there is no ambiguity to backtrack over, just an ordering to
  # get right).
  intBody = {
    regex = "(-?[0-9]+)";
  };
  realBody = {
    regex = "(-?[0-9]+\\.[0-9]+([eE][-+]?[0-9]+)?)";
  };

  # `[A-Za-z_]` -- accepts any letter or underscore as the first
  # character (see file header for why uppercase-first is allowed).
  identBody = {
    regex = "([A-Za-z_][A-Za-z0-9_]*)";
  };

  # A quoted string body -- fragments of "not quote, not backslash" or a
  # recognized two-character escape, looped via star (unlike
  # grammar/flakelock.nix's single-regex shortcut, a generic ATerm
  # string genuinely can contain any of the five escapes, so this needs
  # the fragment/escape choice grammar/json.nix's STRING already
  # establishes the pattern for).
  stringFragment = {
    choice = [
      { regex = "([^\"\\\\]+)"; }
      {
        action = {
          e = [
            { lit = "\\"; }
            {
              choice = [
                { lit = "\""; }
                { lit = "\\"; }
                { lit = "n"; }
                { lit = "r"; }
                { lit = "t"; }
              ];
            }
          ];
          f =
            v:
            let
              c = builtins.elemAt v 1;
            in
            if c == "n" then
              "\n"
            else if c == "r" then
              "\r"
            else if c == "t" then
              "\t"
            else
              c;
        };
      }
    ];
  };
  quotedBody = {
    action = {
      e = [
        { lit = "\""; }
        { star = stringFragment; }
        { lit = "\""; }
      ];
      f = v: builtins.concatStringsSep "" (builtins.elemAt v 1);
    };
  };

  # `(" term ("," term)* )?` -- shared by APPL's arg list and LIST's
  # element list (the two only differ in their surrounding delimiter).
  termList = {
    action = {
      e = {
        opt = [
          "TERM"
          {
            star = [
              ws
              { lit = ","; }
              ws
              "TERM"
            ];
          }
        ];
      };
      f =
        v:
        if v == null then
          [ ]
        else
          [ (builtins.elemAt v 0) ] ++ map (p: builtins.elemAt p 3) (builtins.elemAt v 1);
    };
  };

  annotation = {
    opt = {
      action = {
        e = [
          { lit = "{"; }
          ws
          termList
          ws
          { lit = "}"; }
        ];
        f = v: builtins.elemAt v 2;
      };
    };
  };

  # Each of REAL/INT/APPL/STRING/LIST/PLACEHOLDER below is referenced
  # from exactly one place -- TERM's own choice, by value, not by a bare
  # nonterminal-reference string -- so each is a plain `let`-bound
  # expression here rather than a named `grammar` field: there's no way
  # for two call sites to collide at the same input position when
  # there's only one (same reasoning grammar/json.nix's header
  # documents for its own single-reference rules). Only TERM itself
  # needs named-rule status, since APPL/LIST/PLACEHOLDER recurse into
  # nested terms via the bare string "TERM".
  real = {
    action = {
      e = realBody;
      f = builtins.fromJSON;
    };
  };
  int = {
    action = {
      e = intBody;
      f = builtins.fromJSON;
    };
  };

  # A quoted-string Constructor immediately followed by "(" is an APPL;
  # the same quoted string NOT followed by "(" is a plain STRING term.
  # This used to be decided via `&(...)` positive lookahead (checking
  # quotedBody+"(" without consuming, then re-matching quotedBody once
  # committed). That lookahead's disambiguation role is reproduced here
  # by ordinary PEG ordered choice instead: the quoted-Constructor
  # branch below is a single atomic sequence (quotedBody immediately
  # followed by a MANDATORY "(" and the rest of the arg list) -- if
  # any part of it fails, the whole branch fails and backtracks to the
  # start, leaving the quoted string wholly unconsumed so `string`
  # (TERM's next alternative below) can still match it as a plain
  # STRING term. This is behaviorally identical to the old lookahead
  # for every well-formed and no-parens-at-all input; the only
  # difference is a malformed/truncated arg list immediately after a
  # quoted Constructor (e.g. `"foo"(1,2` with a missing `)`), where the
  # old version reported a 0-arg APPL (the shared `opt` arg list
  # reverted independently of the already-committed lookahead) and this
  # version instead falls through to STRING -- same consumed length and
  # leftover position either way, so accept/reject of DOCUMENT (or any
  # embedding grammar) is unaffected.
  appl = {
    choice = [
      {
        action = {
          e = [
            quotedBody
            { lit = "("; }
            ws
            termList
            ws
            { lit = ")"; }
          ];
          f = v: {
            constructor = builtins.elemAt v 0;
            args = builtins.elemAt v 3;
          };
        };
      }
      {
        action = {
          e = [
            identBody
            {
              opt = {
                action = {
                  e = [
                    { lit = "("; }
                    ws
                    termList
                    ws
                    { lit = ")"; }
                  ];
                  f = v: builtins.elemAt v 2;
                };
              };
            }
          ];
          f = v: {
            constructor = builtins.elemAt v 0;
            args = if builtins.elemAt v 1 == null then [ ] else builtins.elemAt v 1;
          };
        };
      }
    ];
  };

  string = quotedBody;

  list = {
    action = {
      e = [
        { lit = "["; }
        ws
        termList
        ws
        { lit = "]"; }
      ];
      f = v: builtins.elemAt v 2;
    };
  };

  # An anonymous-constructor application -- syntactically just APPL's
  # own arg-list, but with no Constructor prefix at all. Tried as its
  # own alternative (not folded into `appl`) since a tuple's opening
  # "(" appears with nothing before it, whereas `appl`'s "(" always
  # follows a Constructor -- the two are distinguished by which
  # alternative in TERM's choice below even attempts to match at this
  # position, not by any lookahead within this rule itself.
  tuple = {
    action = {
      e = [
        { lit = "("; }
        ws
        termList
        ws
        { lit = ")"; }
      ];
      f = v: builtins.elemAt v 2;
    };
  };

  placeholder = {
    action = {
      e = [
        { lit = "<"; }
        ws
        "TERM"
        ws
        { lit = ">"; }
      ];
      f = v: {
        placeholder = builtins.elemAt v 2;
      };
    };
  };
in
{
  grammar = {
    # A BasicTerm plus its optional trailing annotation. Tried in this
    # order: REAL/INT start with a digit or "-" (REAL first, since an
    # INT's own body can never itself contain "." -- once digits+"."
    # are seen there's nothing to backtrack over); APPL/STRING both
    # start with a letter or `"` (APPL first, with its own lookahead
    # distinguishing a quoted Constructor from a bare STRING); LIST
    # starts with "["; TUPLE starts with "("; PLACEHOLDER starts with
    # "<". No cut needed at this level -- every alternative's own first
    # token is already disjoint from every sibling's (APPL-vs-STRING is
    # the one exception, handled by APPL's internal lookahead, not by
    # cut here).
    TERM = {
      action = {
        e = [
          {
            choice = [
              real
              int
              appl
              string
              list
              tuple
              placeholder
            ];
          }
          annotation
        ];
        f =
          v:
          let
            t = builtins.elemAt v 0;
            ann = builtins.elemAt v 1;
          in
          # Every one of REAL/INT/APPL/STRING/LIST/TUPLE/PLACEHOLDER's own
          # handler returns a DIFFERENT shape -- APPL/PLACEHOLDER an
          # attrset, everything else a bare value (a number, a string, a
          # list) -- so `t // { annotation = ann; }` only ever worked by
          # accident for the two attrset kinds; it crashed outright for
          # the other five (confirmed via lib/roundtrip.nix's generated
          # samples, e.g. `""{}`/`5{}`/`[1]{}` -- annotations are absent
          # from every real corpus file this grammar was checked against,
          # so this went unnoticed). An annotated term is now uniformly
          # `{ term = t; annotation = ann; }` regardless of kind, so a
          # caller checking `? annotation` gets a consistent answer
          # whether `t` was a number or an attrset.
          if ann == null then
            t
          else
            {
              term = t;
              annotation = ann;
            };
      };
    };

    DOCUMENT = [
      ws
      "TERM"
      ws
      { eof = { }; }
    ];
  };

  handlers = {
    DOCUMENT = v: builtins.elemAt v 1;
  };
}
