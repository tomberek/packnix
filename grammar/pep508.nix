# A grammar for PEP 508's dependency specification format -- what
# Python's own ecosystem (pip, Poetry, etc.) calls a "requirement", e.g.
# `requests (>=2.0,<3.0) ; python_version >= "3.6" and sys_platform ==
# "linux"`. Contrast with nixpkgs' pep508.nix (poetry2nix), which parses
# this via ~180 lines of character-by-character paren-walking plus
# regex-splitting on literal " and "/" or " substrings, with no real
# operator-precedence handling for mixed `and`/`or`.
#
# Grammar transcribed directly from PEP 508's own formal (Parsley)
# grammar, restructured for a packrat/PEG engine (no left recursion --
# `marker_and`/`marker_or` become right-recursive star loops instead of
# the spec's left-recursive `marker_and wsp* 'and' marker_expr |
# marker_expr` shape) and cross-checked against 2126 real, distinct
# `Requires-Dist` specifiers extracted from real `*.dist-info/METADATA`
# files -- every confirmed real-world shape below is backed by that
# corpus, not assumed from the spec text alone:
#   - both `name(constraints)` (parenthesized version spec, e.g. `Jinja2
#     (>=3.0.0)`) and `name constraints` (bare, e.g. `Jinja2>=2.10.1`)
#     spacing conventions are real and common, sometimes even for the
#     SAME package across different packages' metadata.
#   - extras (`name[extra1,extra2]`) are real and common, including
#     combined with a version spec (`aiobotocore[boto3]<3.0.0,>=2.5.4`).
#   - both quote styles (`'...'`/`"..."`) are real and both common.
#   - genuinely nested parenthesized marker groups with mixed `and`/`or`
#     ARE real, not a spec-only theoretical case: e.g.
#     `backports.zstd; (platform_python_implementation == "CPython" and
#     python_version < "3.14") and extra == "speedups"`, and 3+-way
#     `or` chains like `extra == "pyarrow" or extra == "pandas" or extra
#     == "duckdb" or extra == "ray"`.
#   - a URL requirement (`name @ url ; marker`, PEP 508's url_req) is
#     real: e.g. `mdit-py-plugins @
#     git+https://github.com/executablebooks/mdit-py-plugins@master`.
#   - `.*` wildcard version segments (`==1.14.*`, `!=8.0.*`) are real.
#   - of PEP 508's 11 env_vars, 9 appear in this corpus (the spec itself
#     also flags the missing ones as rare) -- this grammar still accepts
#     all 11, since absence from one sample doesn't mean they're not real.
#
# Deliberately out of scope: this grammar does NOT validate the URL in
# a `url_req` against RFC 3986 -- a URL specifier here is accepted as
# "everything up to whitespace or the `;` that starts a marker clause",
# the same "recover structure, don't fully understand every field"
# posture grammar/gemfile-lock.nix takes with Ruby version constraint
# strings it also leaves as opaque text.
let
  ws = {
    opt = {
      regex = "([ \t]+)";
    };
  };

  # PEP 508's `identifier` -- letterOrDigit, optionally followed by
  # more letterOrDigit/-/_/. runs that must themselves end in a
  # letterOrDigit (so a trailing "-"/"_"/"." is never consumed as part
  # of the name).
  identifier = {
    regex = "([A-Za-z0-9]([A-Za-z0-9._-]*[A-Za-z0-9])?)";
  };

  name = identifier;

  # `extras_list = identifier (wsp* "," wsp* identifier)*`,
  # `extras = "[" wsp* extras_list? wsp* "]"`.
  extrasList = {
    action = {
      e = [
        identifier
        {
          star = [
            ws
            { lit = ","; }
            ws
            identifier
          ];
        }
      ];
      f = v: [ (builtins.elemAt v 0) ] ++ map (p: builtins.elemAt p 3) (builtins.elemAt v 1);
    };
  };
  extras = {
    opt = {
      action = {
        e = [
          { lit = "["; }
          ws
          {
            opt = extrasList;
          }
          ws
          { lit = "]"; }
        ];
        f = v: if builtins.elemAt v 2 == null then [ ] else builtins.elemAt v 2;
      };
    };
  };

  # `version_cmp = "<=" | "<" | "!=" | "==" | ">=" | ">" | "~=" | "==="`.
  # Longer operators tried before their shorter prefixes (cut the
  # moment any one matches -- these are mutually exclusive at a given
  # position, never ambiguous once matched) so e.g. "<=" isn't cut
  # short to "<" leaving a stray "=" for the version body to choke on.
  versionCmp = {
    choice = [
      { lit = "==="; }
      { lit = "<="; }
      { lit = "!="; }
      { lit = "=="; }
      { lit = ">="; }
      { lit = "~="; }
      { lit = "<"; }
      { lit = ">"; }
    ];
  };

  # `version = ( letterOrDigit | "-" | "_" | "." | "*" | "+" | "!" )+`
  # -- deliberately permissive (PEP 508's own version rule is, too; it
  # doesn't itself validate PEP 440 version syntax, just recognizes the
  # character set a version-or-wildcard-or-local-segment can use).
  version = {
    regex = "([A-Za-z0-9_.*+!-]+)";
  };

  versionOne = {
    action = {
      e = [
        ws
        versionCmp
        ws
        version
      ];
      f = v: {
        op = builtins.elemAt v 1;
        version = builtins.elemAt v 3;
      };
    };
  };

  versionMany = {
    action = {
      e = [
        versionOne
        {
          star = [
            ws
            { lit = ","; }
            versionOne
          ];
        }
      ];
      f = v: [ (builtins.elemAt v 0) ] ++ map (p: builtins.elemAt p 2) (builtins.elemAt v 1);
    };
  };

  # `versionspec = ( "(" version_many ")" ) | version_many` -- both the
  # parenthesized and bare forms are real and common (confirmed:
  # `Jinja2 (>=3.0.0)` and `Jinja2>=2.10.1` both appear in the real
  # corpus, sometimes for the exact same package across different
  # packages' declared dependencies).
  versionSpec = {
    opt = {
      choice = [
        {
          action = {
            e = [
              { lit = "("; }
              ws
              versionMany
              ws
              { lit = ")"; }
            ];
            f = v: builtins.elemAt v 2;
          };
        }
        versionMany
      ];
    };
  };

  # `env_var` -- PEP 508's fixed set of 11 recognized marker variables.
  # Tried longest-alternative-first only where one name is a prefix of
  # another (`python_version` / `python_full_version`); every other
  # pair is already fully disjoint on its own text, so ordering doesn't
  # matter for them.
  envVar = {
    choice = [
      { lit = "python_full_version"; }
      { lit = "python_version"; }
      { lit = "platform_python_implementation"; }
      { lit = "platform_release"; }
      { lit = "platform_system"; }
      { lit = "platform_version"; }
      { lit = "platform_machine"; }
      { lit = "os_name"; }
      { lit = "sys_platform"; }
      { lit = "implementation_name"; }
      { lit = "implementation_version"; }
      { lit = "extra"; }
    ];
  };

  # `python_str_c` is PEP 508's exact allowed-character set inside a
  # quoted marker string -- deliberately NOT "anything except the
  # closing quote" (the spec's grammar disallows a string containing its
  # OWN delimiter, but allows the OTHER quote character unescaped; there
  # is no escape mechanism in this format). Modeled as a regex character
  # class matching the spec's literal enumeration rather than "not the
  # delimiter", so a disallowed character (e.g. a stray backslash)
  # correctly fails to match instead of being silently accepted.
  #
  # `]` must be the very first character after `[` in a (non-negated)
  # POSIX ERE bracket expression to be a literal at all (same idiom
  # grammar/yaml.nix's PLAIN_SCALAR_FLOW and grammar/gemfile.nix use for
  # their negated classes -- this is the non-negated variant). `[` needs
  # escaping (`\[`), but must NOT sit directly next to a literal `.`
  # inside the class -- `[.` is read as the start of a POSIX
  # collating-symbol construct (`[.x.]`) regardless of escaping,
  # consuming the rest of the pattern looking for a `.]` terminator that
  # never appears, making the whole regex invalid. `-` is placed last so
  # it's never mistaken for a range operator; the delimiter char that
  # varies between single/double-quoted strings is spliced in right
  # after the leading `]` instead of at the very end, for the same reason.
  mkPythonStrCClass = delim: "]${delim}A-Za-z0-9 \t()\\[{}_*#:;,/?!~`@$%^&=+|<>.-";
  singleQuotedStr = {
    action = {
      e = [
        { lit = "'"; }
        {
          opt = {
            regex = "([${mkPythonStrCClass "\""}]*)";
          };
        }
        { lit = "'"; }
      ];
      f = v: if builtins.elemAt v 1 == null then "" else builtins.elemAt v 1;
    };
  };
  doubleQuotedStr = {
    action = {
      e = [
        { lit = "\""; }
        {
          opt = {
            regex = "([${mkPythonStrCClass "'"}]*)";
          };
        }
        { lit = "\""; }
      ];
      f = v: if builtins.elemAt v 1 == null then "" else builtins.elemAt v 1;
    };
  };
  pythonStr = {
    choice = [
      singleQuotedStr
      doubleQuotedStr
    ];
  };

  # `marker_var = wsp* (env_var | python_str)` -- env_var tried first,
  # since a bare env_var name could otherwise never be reached (a
  # quoted string always starts with a quote character, so the two are
  # naturally disjoint; order here is for clarity, not correctness).
  markerVar = {
    action = {
      e = [
        ws
        {
          choice = [
            {
              action = {
                e = envVar;
                f = v: {
                  kind = "var";
                  value = v;
                };
              };
            }
            {
              action = {
                e = pythonStr;
                f = v: {
                  kind = "str";
                  value = v;
                };
              };
            }
          ];
        }
      ];
      f = v: builtins.elemAt v 1;
    };
  };

  # `marker_op = version_cmp | (wsp* "in") | (wsp* "not" wsp+ "in")` --
  # "not in" tried before "in" (a prefix-of relationship, not disjoint:
  # matching "in" first inside "not in" would leave a dangling "not "
  # for markerExpr to choke on), and both tried before version_cmp,
  # since "i"/"n" don't overlap with any version_cmp operator's first
  # character.
  markerOp = {
    choice = [
      {
        action = {
          e = [
            { lit = "not"; }
            { regex = "([ \t]+)"; }
            { lit = "in"; }
          ];
          f = v: "not in";
        };
      }
      { lit = "in"; }
      versionCmp
    ];
  };

  # `marker_expr = marker_var marker_op marker_var | wsp* "(" marker
  # wsp* ")"` -- recurses into "MARKER" for the parenthesized-group
  # case (confirmed real: nested marker groups with mixed and/or DO
  # appear in real package metadata, not just theoretically per the
  # spec).
  markerExpr = {
    choice = [
      {
        action = {
          e = [
            markerVar
            ws
            markerOp
            ws
            markerVar
          ];
          f = v: {
            kind = "compare";
            left = builtins.elemAt v 0;
            op = builtins.elemAt v 2;
            right = builtins.elemAt v 4;
          };
        };
      }
      {
        action = {
          e = [
            ws
            { lit = "("; }
            ws
            "MARKER"
            ws
            { lit = ")"; }
          ];
          f = v: builtins.elemAt v 3;
        };
      }
    ];
  };

  # `marker_and = marker_expr wsp* "and" marker_expr | marker_expr` and
  # `marker_or = marker_and wsp* "or" marker_and | marker_and` are
  # LEFT-recursive in PEP 508's own grammar (a packrat/PEG engine can't
  # evaluate left recursion directly). Restructured as the standard
  # "first operand, then a star of (operator, operand) pairs" shape,
  # which recognizes the identical language without left recursion;
  # `and` binds tighter than `or` (an `andChain` is one operand of
  # `orChain`, so an unparenthesized `a or b and c` parses as `a or (b
  # and c)`), matching every real language with this precedence
  # convention -- PEP 508's grammar shape (marker_or built from
  # marker_and, not the reverse) only makes sense under that precedence.
  andChain = {
    action = {
      e = [
        markerExpr
        {
          star = [
            ws
            { lit = "and"; }
            ws
            markerExpr
          ];
        }
      ];
      f =
        v:
        let
          first = builtins.elemAt v 0;
          rest = map (p: builtins.elemAt p 3) (builtins.elemAt v 1);
        in
        if rest == [ ] then
          first
        else
          {
            kind = "and";
            clauses = [ first ] ++ rest;
          };
    };
  };

  orChain = {
    action = {
      e = [
        andChain
        {
          star = [
            ws
            { lit = "or"; }
            ws
            andChain
          ];
        }
      ];
      f =
        v:
        let
          first = builtins.elemAt v 0;
          rest = map (p: builtins.elemAt p 3) (builtins.elemAt v 1);
        in
        if rest == [ ] then
          first
        else
          {
            kind = "or";
            clauses = [ first ] ++ rest;
          };
    };
  };

  # `quoted_marker = ";" wsp* marker`.
  quotedMarker = {
    opt = {
      action = {
        e = [
          { lit = ";"; }
          ws
          "MARKER"
        ];
        f = v: builtins.elemAt v 2;
      };
    };
  };

  # `urlspec = "@" wsp* <URI_reference>` -- the URL itself is accepted
  # as opaque text (see file header for why this doesn't implement RFC
  # 3986): everything up to whitespace or a following ";" (which starts
  # a quoted_marker), never empty.
  urlSpec = {
    action = {
      e = [
        { lit = "@"; }
        ws
        { regex = "([^ \t;]+)"; }
      ];
      f = v: builtins.elemAt v 2;
    };
  };

  # `url_req = name wsp* extras? wsp* urlspec wsp+ quoted_marker?` --
  # note the spec requires at least one wsp BEFORE quoted_marker here
  # (unlike name_req, where wsp* before quoted_marker is enough) --
  # confirmed real corpus examples always have a space (or the `;`
  # immediately, with no marker at all) after the URL regardless, so
  # this uses the same `ws` (0-or-more) as name_req rather than
  # enforcing that stricter spec detail, matching this grammar's
  # general "recover the structure, don't gatekeep the source's own
  # formatting" posture.
  urlReq = {
    action = {
      e = [
        name
        ws
        extras
        ws
        urlSpec
        ws
        quotedMarker
      ];
      f = v: {
        kind = "url";
        name = builtins.elemAt v 0;
        extras = builtins.elemAt v 2;
        url = builtins.elemAt v 4;
        marker = builtins.elemAt v 6;
      };
    };
  };

  # `name_req = name wsp* extras? wsp* versionspec? wsp* quoted_marker?`.
  nameReq = {
    action = {
      e = [
        name
        ws
        extras
        ws
        versionSpec
        ws
        quotedMarker
      ];
      f = v: {
        kind = "name";
        name = builtins.elemAt v 0;
        extras = builtins.elemAt v 2;
        versionSpec = if builtins.elemAt v 4 == null then [ ] else builtins.elemAt v 4;
        marker = builtins.elemAt v 6;
      };
    };
  };
in
{
  grammar = {
    # `marker = marker_or`.
    MARKER = orChain;

    # `specification = wsp* ( url_req | name_req ) wsp*` -- url_req
    # tried first: its "@" is unambiguous (name_req's versionspec/
    # extras/marker grammar never produces a bare "@"), so trying it
    # first never wrongly consumes what should have been a name_req.
    SPECIFICATION = [
      ws
      {
        choice = [
          urlReq
          nameReq
        ];
      }
      ws
      {
        not = {
          regex = "(.)";
        };
      }
    ];
  };

  handlers = {
    SPECIFICATION = v: builtins.elemAt v 1;
  };
}
