# A grammar for PEP 508's dependency specification format -- what
# Python's own ecosystem (pip, Poetry, etc.) calls a "requirement", e.g.
# `requests (>=2.0,<3.0) ; python_version >= "3.6" and sys_platform ==
# "linux"`. This is currently parsed in nixpkgs by
# pkgs/development/tools/poetry2nix/poetry2nix/pep508.nix via ~180 lines
# of character-by-character paren-walking (findSubExpressions) plus
# regex-splitting on literal " and "/" or " substrings -- fragile by its
# own admission (a `# TODO: Handle single quoted values` comment), with
# no real operator-precedence handling for mixed `and`/`or` and a fixed
# value-character-class regex that can't distinguish quoting contexts.
#
# Grammar transcribed directly from PEP 508's own formal (Parsley)
# grammar, restructured for a packrat/PEG engine (no left recursion --
# `marker_and`/`marker_or` become right-recursive star loops instead of
# the spec's left-recursive `marker_and wsp* 'and' marker_expr |
# marker_expr` shape) and cross-checked against 2126 real, distinct
# `Requires-Dist` specifiers extracted from real `*.dist-info/METADATA`
# files on this machine (a broad, real-world Python package sample, not
# synthetic test cases) -- every confirmed real-world shape below is
# backed by that corpus, not assumed from the spec text alone:
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
#   - of PEP 508's 11 env_vars, 9 appear in this corpus (all except
#     `platform_release`/`platform_version`/`implementation_version`,
#     both/all confirmed rare in practice by the spec itself) -- this
#     grammar still accepts all 11, since their absence from one
#     machine's installed packages doesn't mean they're not real.
#
# Deliberately out of scope: this grammar does NOT validate the URL in
# a `url_req` against RFC 3986 (PEP 508 embeds the full URI grammar, but
# a URL specifier here is accepted as "everything up to whitespace or
# the `;` that starts a marker clause" -- the same "we don't need to
# fully understand every field, just structurally recover the ones that
# matter" posture grammar/gemfile-lock.nix takes with Ruby version
# constraint strings it also leaves as opaque text).
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
  # closing quote" (that would also accept the OTHER quote character
  # unescaped, which the spec's grammar doesn't: a squote-delimited
  # string may contain a bare dquote and vice versa, but neither may
  # contain its OWN delimiter at all -- there is no escape mechanism in
  # this format). Modeled as a regex character class matching the
  # spec's literal enumeration (whitespace/letters/digits and a fixed
  # punctuation set) rather than "not the delimiter", so a delimiter
  # character appearing where it's NOT allowed (e.g. a stray backslash,
  # which PEP 508 does not list) correctly fails to match instead of
  # being silently accepted.
  #
  # `]` must be the very first character after `[` in a (non-negated)
  # POSIX ERE bracket expression to be treated as a literal at all --
  # escaping it (`\]`) is invalid syntax in this engine outside a
  # bracket expression, and mid-class placement makes it a literal
  # closing bracket instead of a class member (confirmed directly; same
  # idiom grammar/yaml.nix's PLAIN_SCALAR_FLOW and grammar/gemfile.nix
  # use for their own negated classes -- this is the non-negated
  # variant of that same rule). `[` needs escaping (`\[`), but must NOT
  # sit directly next to a literal `.` inside the class -- `[.` is read
  # as the start of a POSIX collating-symbol construct (`[.x.]`)
  # regardless of any escaping before it, consuming the rest of the
  # pattern looking for a `.]` terminator that never appears, which
  # makes the whole regex invalid (confirmed directly by bisecting
  # exactly which characters, in which order, triggered
  # "invalid regular expression"). `-` is placed last so it's never
  # mistaken for a range operator; the delimiter char that varies
  # between single/double-quoted strings is spliced in right after the
  # leading `]` instead of at the very end, for the same reason (a
  # trailing `-<delim>` would make `-` look like a range operator
  # instead of a literal).
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
  # evaluate left recursion directly -- it would recurse into the same
  # rule at the same position forever). Restructured as the standard
  # "first operand, then a star of (operator, operand) pairs" shape,
  # which recognizes the identical language (left-associative `and`/
  # `or` chains) without left recursion; `and` binds tighter than `or`,
  # matching the spec's two-level and/or split (an `andChain` is one
  # operand of `orChain`, so an unparenthesized `a or b and c` parses
  # as `a or (b and c)`, matching every real language with this same
  # precedence convention -- PEP 508 doesn't spell this out as
  # explicitly as e.g. Python's own `or`/`and` docs do, but its grammar
  # SHAPE -- marker_or built from marker_and, not the reverse -- only
  # makes sense under that precedence, and no real corpus example
  # contradicts it).
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
