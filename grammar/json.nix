# The JSON grammar, using the star/opt/cutSeq/action combinators from
# lib/packrat.nix. Exposes both `grammarNoCut` (top-level `X` is a plain
# ordered choice) and `grammar` (each `X` alternative wrapped in cutSeq,
# since SET/LIST/STRING/NUMBER/BOOL/NULL are first-token-disjoint --
# mirrors the cut paper's AC-FIRST example, PASTE'10 §4.2). Both variants
# are kept for A/B benchmarking (see bench/measure.sh); default.nix picks
# one via `useCut`.
#
# String escapes: `\" \\ \/ \b \f \n \r \t` -- everything JSON's own
# grammar defines EXCEPT `\uXXXX`. That one is deliberately out of scope:
# it needs encoding an arbitrary Unicode codepoint (including
# surrogate-pair-combined astral characters) to UTF-8 bytes, and Nix has
# no `chr`/`ord`/hex-literal builtin to build that from -- a correct
# implementation would mean hand-rolling UTF-8's bit-packing from
# scratch, a separate undertaking from fixing STRING's escape handling
# itself. A `\uXXXX` escape in real input is correctly rejected (this
# grammar's `stringFragment` has no branch for `\u`, so `STRING` fails to
# match a string containing one) rather than silently mishandled --
# same "fail loudly on what's out of scope, don't misparse" discipline as
# every other grammar in this repo.
let
  # Every named rule here is a field on EVERY Derivs node (one per input
  # position), regardless of whether a position ever uses it -- so a rule
  # referenced from exactly one place can always be folded into that call
  # site: there's no way for two call sites to collide at the same
  # position when there's only one. stringFragment/commaSeparated/setItem
  # below are folded this way (each replaces a rule that used to exist,
  # e.g. STRING_RAW/LIST_ITEMS/ITEMS/ITEM).
  #
  # BUGFIX: the escape branches below used to be bare `{ lit = ...; }`
  # atoms with no decoding at all -- STRING's handler just concatenated
  # whatever text matched, so a string containing `\"` decoded to the
  # literal two-character sequence `\"` instead of a bare `"` (confirmed
  # against builtins.fromJSON, which correctly decodes it) -- and the
  # bare-backslash branch matched a LONE `\` with no escape partner,
  # which is never valid JSON at all (a backslash must always be
  # followed by an escape character). Found independently via
  # lib/generate.nix's round-trip testing: no fixture in this repo's
  # corpus happens to contain an escaped quote or backslash, so
  # verify-fixtures.sh's byte-identical-to-fromJSON check never exercised
  # this path. Fixed the same way grammar/aterm.nix's stringFragment
  # already does it: each escape is `{ lit = "\\"; } { choice = [...] }`
  # wrapped in `action`, decoding the matched escape CHARACTER to its
  # real value, and STRING's handler (below) concatenates the DECODED
  # fragments, not the raw matched text.
  stringFragment = {
    choice = [
      { regex = ''([^\\\"]+)''; }
      {
        action = {
          e = [
            { lit = "\\"; }
            {
              choice = [
                { lit = "\""; }
                { lit = "\\"; }
                { lit = "/"; }
                { lit = "b"; }
                { lit = "f"; }
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
            # Nix's OWN string literal syntax only recognizes `\n`/`\r`/
            # `\t`/`\\`/`\"`/`\$` as escapes -- there is no `\b`/`\f`
            # literal at all (confirmed: `"\b"` in Nix source is just the
            # bare character `b`, not a backspace byte -- caught by this
            # exact test failing when first written). Backspace/form-feed
            # are instead obtained by asking `builtins.fromJSON` to decode
            # them itself (a real, single-byte control character each,
            # confirmed via `stringLength`), the one Nix builtin that DOES
            # know what `\b`/`\f` mean.
            if c == "b" then
              builtins.fromJSON ''"\b"''
            else if c == "f" then
              builtins.fromJSON ''"\f"''
            else if c == "n" then
              "\n"
            else if c == "r" then
              "\r"
            else if c == "t" then
              "\t"
            else
              c; # \" \\ \/ decode to themselves minus the backslash
        };
      }
    ];
  };

  # `item ("," item)*`. Cut on the repetition body changes WHY a trailing
  # comma is rejected (whole star fails, vs. plain (e1 e2)* stopping early
  # and the outer "]"/"}" rejecting the leftover ","), not WHETHER -- ","
  # is never a valid start of "]"/"}". Also faster on long runs.
  commaSeparated = item: [
    item
    {
      star = {
        cutSeq = [
          { lit = ","; }
          item
        ];
      };
    }
  ];

  # `"name": value`.
  setItem = [
    "WHITESPACE"
    "STRING"
    "WHITESPACE"
    { lit = ":"; }
    "X"
  ];

  # Rules shared verbatim between the cut and no-cut variants.
  common = {
    # `opt`, not `star`: `[[:space:]]+` already greedily consumes the
    # whole run in one match, so `star`'s recheck loop would only ever
    # fire 0 or 1 times. `opt` gives the same "zero or more" acceptance
    # (a bare `regex` requires >=1 match, which would reject e.g. "[]").
    WHITESPACE = {
      opt = {
        regex = "([[:space:]]+)";
      };
    };

    STRING = [
      { lit = "\""; }
      { star = stringFragment; }
      { lit = "\""; }
    ];
  };

  # NUMBER/BOOL/NULL/LIST/SET carry a real value-transform (unlike the
  # purely structural inlinings above), so they're folded into xBranches
  # via `{ action = { e; f; }; }` (lib/packrat.nix), which lets a
  # transform travel with an inlined expression instead of needing a
  # named field. Each is single-reference (xBranches only) so this is
  # still the same safe inlining as above. STRING/WHITESPACE/X stay named
  # rules -- each is referenced from 2+ places, which would need an
  # actual position-disjointness argument to inline safely, not just a
  # reference count.
  #
  # "false" tried first in BOOL: outnumbers "true" ~14:1 in this repo's
  # fixtures.
  numberBranch = {
    action = {
      e = {
        regex = "([0-9]+)";
      };
      f = builtins.fromJSON;
    };
  };
  boolBranch = {
    action = {
      e = {
        choice = [
          { lit = "false"; }
          { lit = "true"; }
        ];
      };
      f = v: v == "true";
    };
  };
  nullBranch = {
    action = {
      e = {
        lit = "null";
      };
      f = v: null;
    };
  };

  # `opt` lets LIST/SET accept an empty body ("[]"/"{}"). Only ONE
  # WHITESPACE around the body, not two: X already eats its own leading
  # and trailing whitespace, and setItem ends in "X" so it inherits that
  # trailing WHITESPACE too -- a second WHITESPACE before "]"/"}" would
  # always be redundant (the last item already ate it, or if empty, the
  # first WHITESPACE already did).
  #
  # Unwraps `opt`'s leading-item-plus-star-of-pairs into a flat list ([]
  # if the opt didn't match).
  listBranch = {
    action = {
      e = [
        { lit = "["; }
        "WHITESPACE"
        { opt = commaSeparated "X"; }
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
          [ (builtins.elemAt opt 0) ] ++ map (p: builtins.elemAt p 1) (builtins.elemAt opt 1);
    };
  };

  # Each raw item is setItem's shape, [WHITESPACE STRING WHITESPACE lit
  # X] -- pulls name/value straight out of that raw sequence.
  setBranch = {
    action = {
      e = [
        { lit = "{"; }
        "WHITESPACE"
        { opt = commaSeparated setItem; }
        { lit = "}"; }
      ];
      f =
        v:
        let
          opt = builtins.elemAt v 2;
          toPair = item: {
            name = builtins.elemAt item 1;
            value = builtins.elemAt item 4;
          };
        in
        builtins.listToAttrs (
          if opt == null then
            [ ]
          else
            [ (toPair (builtins.elemAt opt 0)) ]
            ++ map (p: toPair (builtins.elemAt p 1)) (builtins.elemAt opt 1)
        );
    };
  };

  # Ordered by real-world value-type frequency (strings/sets most common,
  # lists rarest) -- PEG tries branches left-to-right and stops at the
  # first success.
  xBranches = [
    "STRING"
    setBranch
    numberBranch
    boolBranch
    listBranch
    nullBranch
  ];

  grammarNoCut = common // {
    X = [
      "WHITESPACE"
      { choice = xBranches; }
      "WHITESPACE"
    ];
  };

  # Each branch becomes `{ cutSeq = [ <branch> ""]; }` (e2 = epsilon, just
  # to give the cut something to commit after). Branches are
  # first-token-disjoint, so committing changes no accept/reject outcome.
  grammar = common // {
    X = [
      "WHITESPACE"
      {
        choice = map (b: {
          cutSeq = [
            b
            ""
          ];
        }) xBranches;
      }
      "WHITESPACE"
    ];
  };

  # Shared between both variants; only X differs, since the cut variant's
  # inner choice value is wrapped one level deeper ([branchVal ""]) than
  # the plain-choice variant's (branchVal directly).
  handlersCommon = {
    # `opt`'s raw value is the matched string, or `null` if there was
    # nothing to match.
    WHITESPACE = v: if v == null then "" else v;
    STRING = v: builtins.concatStringsSep "" (builtins.elemAt v 1);
  };

  handlersNoCut = handlersCommon // {
    X = v: builtins.elemAt v 1;
  };

  handlers = handlersCommon // {
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
