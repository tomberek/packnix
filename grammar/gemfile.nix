# A real (subset of) Ruby Bundler `Gemfile` grammar for lib/packrat.nix --
# NOT `Gemfile.lock` (that's grammar/gemfile-lock.nix; a fixed,
# machine-generated format). A `Gemfile` is technically arbitrary Ruby, so
# this grammar targets only the confirmed-common subset needed to recover
# one specific fact `Gemfile.lock` never records: which Bundler *group*
# each gem belongs to.
#
# Why this matters: `packnix-bundler`'s `mkGemset` (see that repo) needs
# per-gem group membership to make `bundlerEnv`'s `groups` filtering
# actually work -- without it, every gem is tagged `["default"]`
# regardless of the real Gemfile, and `groups` filtering becomes a
# silent no-op (confirmed by reading nixpkgs'
# `bundled-common/functions.nix`: `groupMatches` always matches when
# every gem is tagged `"default"`, since `groups ++ ["default"]` always
# includes it).
#
# Scope confirmed by scanning a nixpkgs checkout's 136-file corpus (the
# same one grammar/gemfile-lock.nix was built from) for every Gemfile
# that uses groups at all (6 files) -- these are the ONLY forms observed:
#   - `group :a, :b do ... end` blocks (bare symbols, comma-separated,
#     may be followed by `, optional: true` -- ignored, this grammar
#     only cares about group names). May nest.
#   - `gem 'name', group: :x` / `group: [:x, :y]` / `groups: [:x, :y]` --
#     THREE real spellings of an inline alternative to the block form,
#     confirmed in gitlab's and sure's real Gemfiles (`groups:` plural is
#     a real, distinct spelling from `group:` singular -- both accepted
#     by Bundler).
#   - `if <cond> ... end`, `unless <cond> ... end`, and
#     `if <cond> ... else ... end` wrapping either of the above
#     (confirmed in discourse/sure/redmine's real Gemfiles). The
#     condition itself (typically `ENV["X"] == "1"`) is captured as raw
#     text but never interpreted -- there is no way to know its value
#     without knowing the environment `bundle install` was actually run
#     under. Both branches' gems are collected (a UNION, not a choice of
#     one branch) -- this never silently drops a gem, though it may
#     over-include relative to one specific real `bundle install` run.
#
# Deliberately out of scope (real, but rare -- 1-2 files each in the
# corpus, and each is a materially bigger feature than group-tracking):
#   - `gemspec` (loads a `.gemspec` file, which can itself declare
#     groups)
#   - `eval_gemfile` / `Dir.glob ... do |f| eval_gemfile f end` (loads
#     OTHER Gemfile-like files)
#   - anything else that's actually arbitrary Ruby (arbitrary method
#     calls, string interpolation, multi-line expressions, heredocs) --
#     EXCEPT top-level `def`/`class`/`module`/`begin`/`case` blocks
#     (confirmed in discourse's real Gemfile: a top-level `def
#     rails_master? ... end` helper), which ARE recognized structurally
#     (as "OPAQUE_BLOCK", matching only the opening keyword and the
#     balanced closing `end` -- their contents are discarded, not
#     parsed) purely so their `end` doesn't get mistaken for a dangling
#     GROUP_BLOCK/IF_BLOCK terminator and fail the whole file's parse. A
#     generic `<arbitrary-expr> do [|args|] ... end` block opener (e.g.
#     redmine's `Dir.glob(...).each do |file|`) is NOT recognized this
#     way -- see OPAQUE_OPENER_LINE's comment for why (evalRegex's
#     bounded lookahead window).
# Every line/construct not specifically recognized is matched by a
# catch-all fallback rule and simply ignored (not a parse failure) --
# unlike grammar/gemfile-lock.nix's "fail the whole parse on anything
# unexpected" discipline, a Gemfile genuinely can't be fully modeled
# short of embedding a Ruby interpreter, so "degrade gracefully, recover
# what we can" is the correct posture here. A gem declared only inside
# an out-of-scope mechanism (gemspec/eval_gemfile/Dir.glob, or one whose
# enclosing `do`-block isn't a recognized OPAQUE_BLOCK opener) simply
# never appears in this grammar's output at all -- there is no
# "half-known" gem entry, only "found with real groups" or "not found."
# If ANY construct in the file can't be matched (most commonly a
# `Dir.glob ... do |f| ... end` opener), the whole file fails to parse
# (`DOCUMENT` returns `false`) -- the caller (`packnix-bundler`'s
# `mkGemset`) decides how to treat that (its plan: fall back to treating
# every gem as `["default"]`, exactly like not having Gemfile group
# information at all).
let
  # No escape sequences are modeled for quoted gem/group names -- real
  # Gemfiles occasionally use string interpolation or escapes in other
  # contexts, but never in a `gem 'name'`/`:symbol` position in the
  # corpus (names are plain package-name-shaped identifiers). A quoted
  # string here is deliberately narrow: no `\`, no interpolation.
  singleQuoted = {
    action = {
      e = [
        { lit = "'"; }
        { regex = "([^'\r\n]*)"; }
        { lit = "'"; }
      ];
      f = v: builtins.elemAt v 1;
    };
  };
  doubleQuoted = {
    action = {
      e = [
        { lit = "\""; }
        { regex = "([^\"\r\n]*)"; }
        { lit = "\""; }
      ];
      f = v: builtins.elemAt v 1;
    };
  };
  quotedString = {
    choice = [
      singleQuoted
      doubleQuoted
    ];
  };

  # A bare `:symbol` -- Ruby identifier rules (letters/digits/underscore,
  # not starting with a digit), which every real group name in the
  # corpus satisfies.
  symbol = {
    action = {
      e = {
        regex = "(:[A-Za-z_][A-Za-z0-9_]*)";
      };
      f = v: builtins.substring 1 (builtins.stringLength v - 1) v; # drop the leading ":"
    };
  };

  ws = {
    opt = {
      regex = "([ \t]+)";
    };
  };

  lineEnd = {
    choice = [
      { regex = "(\r?\n)"; }
      {
        not = {
          regex = "(.)";
        };
      }
    ];
  };
  blankLine = [
    ws
    { regex = "(\r?\n)"; }
  ];
  commentLine = [
    { lit = "#"; }
    {
      opt = {
        regex = "([^\r\n]*)";
      };
    }
    lineEnd
  ];

  # One or more comma-separated symbols: `:a` or `:a, :b, :c`. Used both
  # for `group :a, :b do` and for the array form `[:a, :b]`'s contents.
  symbolList = {
    action = {
      e = [
        symbol
        {
          star = [
            ws
            { lit = ","; }
            ws
            symbol
          ];
        }
      ];
      f = v: [ (builtins.elemAt v 0) ] ++ map (p: builtins.elemAt p 3) (builtins.elemAt v 1);
    };
  };

  # The value on the right of a `key:` in a `gem` call's keyword
  # arguments. Scoped to exactly the shapes confirmed in the corpus:
  # a bare symbol, a quoted string, an array literal of symbols
  # (`[:a, :b]`), or a bare `true`/`false` -- NOT a general Ruby
  # expression. `[^]...]`'s bare (unescaped) closing `]` is deliberate:
  # escaping `]` outside a bracket expression is invalid POSIX ERE in
  # this engine (confirmed directly; only `\[` needs escaping, `]` must
  # stay bare) -- same idiom grammar/yaml.nix's PLAIN_SCALAR_FLOW uses.
  argValue = {
    choice = [
      symbol
      quotedString
      {
        action = {
          e = [
            { lit = "["; }
            ws
            { opt = symbolList; }
            ws
            { lit = "]"; }
          ];
          f = v: if builtins.elemAt v 2 == null then [ ] else builtins.elemAt v 2;
        };
      }
      {
        action = {
          e = {
            lit = "true";
          };
          f = v: true;
        };
      }
      {
        action = {
          e = {
            lit = "false";
          };
          f = v: false;
        };
      }
    ];
  };

  # `key: value` -- the only keys this grammar cares about are
  # `group`/`groups`; every other key (`require:`, `platforms:`,
  # `github:`, `path:`, `feature_category:`, etc.) is still parsed (so
  # the line doesn't fall through to the catch-all and lose its gem name)
  # but its value is discarded.
  kwarg = {
    action = {
      e = [
        { regex = "([A-Za-z_]+)"; }
        ws
        { lit = ":"; }
        ws
        argValue
      ];
      f = v: {
        key = builtins.elemAt v 0;
        value = builtins.elemAt v 4;
      };
    };
  };

  # A positional arg (version constraint string, e.g. `'~> 2.7'`, or a
  # bare symbol for the rarer `gem :name` form) -- accepted and ignored,
  # same "parse so we don't lose the rest of the line, discard the
  # value" reasoning as kwarg's non-group keys.
  positionalArg = {
    choice = [
      quotedString
      symbol
    ];
  };

  gemArg = {
    choice = [
      kwarg
      positionalArg
    ];
  };

  # `gem 'name'[, arg]*` -- collects every `group:`/`groups:` kwarg's
  # symbol(s) into this gem's own inline group declaration (a gem naming
  # `group:` more than once is not valid Ruby, but if it somehow
  # happened, this takes the union rather than picking one).
  GEM_LINE = {
    action = {
      e = [
        { lit = "gem"; }
        ws
        quotedString
        {
          star = [
            ws
            { lit = ","; }
            ws
            gemArg
          ];
        }
        ws
        { opt = commentLine; }
        lineEnd
      ];
      f =
        v:
        let
          name = builtins.elemAt v 2;
          args = map (p: builtins.elemAt p 3) (builtins.elemAt v 3);
          groupArgs = builtins.filter (
            a: builtins.isAttrs a && (a.key or null) == "group" || (a.key or null) == "groups"
          ) args;
          inlineGroups = builtins.concatMap (
            a: if builtins.isList a.value then a.value else [ a.value ]
          ) groupArgs;
        in
        {
          kind = "gem";
          inherit name;
          groups = inlineGroups; # [] if no inline group:/groups: kwarg was present
        };
    };
  };

  # Lines this grammar recognizes as structurally significant but
  # doesn't need the content of -- consumed so the catch-all doesn't
  # have to (and so a `source 'https://...'` line, say, can't be
  # accidentally swallowed by a later, looser catch-all pattern change).
  SOURCE_LINE = [
    { lit = "source"; }
    ws
    quotedString
    ws
    { opt = commentLine; }
    lineEnd
  ];
  GEMSPEC_LINE = [
    { lit = "gemspec"; }
    {
      opt = {
        regex = "([^\r\n]*)";
      };
    }
    lineEnd
  ];

  # Opens an OPAQUE_BLOCK -- a top-level Ruby construct real Gemfiles do
  # contain (confirmed: discourse's has a top-level `def rails_master? ...
  # end` helper method) that this grammar can't interpret, but whose
  # `end` DOES need balancing -- UNRECOGNIZED_LINE deliberately refuses
  # to swallow a bare "end" (GROUP_BLOCK/IF_BLOCK need that refusal to
  # find their OWN terminator), so without this, a dangling `def`'s `end`
  # has nothing to close against and the whole file fails to parse.
  # Recognizes a `def`/`class`/`module`/`begin`/`case` keyword line only
  # -- NOT a generic `<arbitrary-expr> do [|args|]` block opener (e.g.
  # redmine's `Dir.glob(...).each do |file|`): `evalRegex`'s bounded
  # lookahead window only re-tries with a wider window when a match
  # fills the window exactly (i.e. might be truncated), not when no match
  # is found at all in the window -- so a pattern needing to scan past an
  # arbitrary-length expression before finding " do" can silently fail to
  # match a longer real line even though a truly unbounded regex would.
  # `Dir.glob ... do |f| ... end` is already out of scope per this file's
  # header (falls back to `UNRECOGNIZED_LINE`'s failure -> `mkGemset`'s
  # whole-file fallback), so this only needs to cover the keyword forms,
  # which are always short enough to fit the window. Deliberately tried
  # AFTER GROUP_BLOCK/IF_BLOCK in itemExpr's choice (a real `group :x
  # do`/`if ...` line must be recognized by ITS OWN rule first; this is
  # only reached once those have already failed).
  OPAQUE_OPENER_LINE = [
    {
      regex = "((def|class|module|begin|case)([ \t?!(][^\r\n]*)?)";
    }
    lineEnd
  ];

  # Catch-all: any line this grammar doesn't otherwise recognize --
  # matched and discarded, not a parse failure (see file header for why
  # a Gemfile can't realistically be fully modeled). Must be tried LAST
  # in every choice it appears in. Explicitly refuses to match a bare
  # "end", "else", or blank line, since those are structurally
  # significant to GROUP_BLOCK/IF_BLOCK's own termination/branching and
  # must never be silently swallowed here.
  UNRECOGNIZED_LINE = {
    action = {
      e = [
        {
          not = {
            choice = [
              { lit = "end"; }
              { lit = "else"; }
              { regex = "(\r?\n)"; }
            ];
          };
        }
        { regex = "([^\r\n]+)"; }
        lineEnd
      ];
      f = v: { kind = "unrecognized"; };
    };
  };

  # A single line inside an OPAQUE_BLOCK's body -- anything except a bare
  # "end" (which must terminate this block, or a nested OPAQUE_BLOCK, and
  # so must never be swallowed here) or a nested block opener (handled by
  # "OPAQUE_BLOCK" itself, tried first in OPAQUE_ITEM's choice below). No
  # attempt is made to recognize `gem`/`group` lines inside an opaque
  # block -- a call inside a `def`'s body never executes at Gemfile-eval
  # time, so there is nothing to extract here even in principle.
  OPAQUE_LINE = {
    action = {
      e = [
        ws
        {
          not = {
            choice = [
              { lit = "end"; }
              { regex = "(\r?\n)"; }
            ];
          };
        }
        { regex = "([^\r\n]+)"; }
        lineEnd
      ];
      f = v: null;
    };
  };
  OPAQUE_ITEM = {
    action = {
      e = [
        ws
        {
          choice = [
            "OPAQUE_BLOCK"
            OPAQUE_LINE
          ];
        }
      ];
      f = v: null;
    };
  };

  # A single top-level "thing" inside the document, a GROUP_BLOCK, or an
  # IF_BLOCK's body -- tries every specifically-recognized form before
  # the catch-all. `commentLine`/`blankLine` produce no item at all
  # (filtered out by ITEM_LIST's handler), everything else produces
  # exactly one `{kind;...}` value (GEM_LINE items carry name/groups;
  # GROUP_BLOCK/IF_BLOCK items carry their own nested `items` list,
  # flattened by ITEM_LIST -- see below). Defined as a plain value here
  # (not a named grammar rule) since it's referenced from ITEM_LIST via
  # `{ star = itemExpr; }` -- an inlined expression, not a nonterminal
  # name -- which is fine: ITEM doesn't need to recurse into ITSELF, only
  # GROUP_BLOCK/IF_BLOCK (which DO need named-rule status, since
  # ITEM_LIST recurses into them and they recurse back into ITEM_LIST).
  # Leads with `ws`: `group`/`if`/`gem` bodies are conventionally
  # indented, and neither `lit`/`regex` atoms nor `blankLine`/
  # `commentLine` skip leading whitespace on their own, so this strips
  # it once, up front, rather than teaching every alternative to do it
  # itself.
  itemExpr = {
    action = {
      e = [
        ws
        {
          choice = [
            {
              action = {
                e = commentLine;
                f = v: null;
              };
            }
            {
              action = {
                e = blankLine;
                f = v: null;
              };
            }
            GEM_LINE
            SOURCE_LINE_ITEM
            GEMSPEC_LINE_ITEM
            "GROUP_BLOCK"
            "IF_BLOCK"
            "OPAQUE_BLOCK"
            UNRECOGNIZED_LINE
          ];
        }
      ];
      f = v: builtins.elemAt v 1;
    };
  };
  SOURCE_LINE_ITEM = {
    action = {
      e = SOURCE_LINE;
      f = v: null;
    };
  };
  GEMSPEC_LINE_ITEM = {
    action = {
      e = GEMSPEC_LINE;
      f = v: null;
    };
  };

  # Zero or more items, with GROUP_BLOCK/IF_BLOCK's nested item lists
  # flattened into the parent's own flat list, and null (comment/blank/
  # source/gemspec) entries dropped. Every GEM_LINE inside a GROUP_BLOCK
  # already has that block's groups unioned into it by GROUP_BLOCK's own
  # handler before it ever reaches this flattening step, so this itself
  # does no group-tagging -- it only concatenates. `itemListExpr` is a
  # plain expression (not `{star = "ITEM_LIST";}`, which wouldn't make
  # sense -- ITEM_LIST itself is the named rule GROUP_BLOCK/IF_BLOCK
  # reference, defined via this expression down in `grammar` below).
  itemListExpr = {
    action = {
      e = {
        star = itemExpr;
      };
      f =
        v:
        builtins.concatMap (
          item:
          if item == null then
            [ ]
          else if item.kind == "block" then
            item.items
          else if item.kind == "gem" then
            [ item ]
          else
            [ ] # "unrecognized"
        ) v;
    };
  };
in
{
  grammar = {
    # `group :a, :b[, optional: true] do <items> end` -- recursive via
    # ITEM_LIST -> itemExpr -> "GROUP_BLOCK" (a genuine named-rule
    # reference, not `action`-inlined, since actual recursion needs a
    # lazy nonterminal reference, not a compile-time-inlined expression
    # -- see lib/packrat.nix's `compile`: bare strings resolve through
    # `derivs.${expr}`, which is how self-reference works at all).
    # Every gem line found (transitively, through nested blocks) gets
    # this block's groups UNIONED into its own -- a gem's final groups
    # is the union of every enclosing GROUP_BLOCK plus any inline
    # `group:`/`groups:` kwarg on the gem line itself.
    GROUP_BLOCK = {
      action = {
        e = [
          { lit = "group"; }
          ws
          symbolList
          {
            opt = [
              ws
              { lit = ","; }
              ws
              kwarg
            ];
          }
          ws
          { lit = "do"; }
          {
            opt = {
              regex = "([^\r\n]*)";
            };
          }
          lineEnd
          "ITEM_LIST"
          { regex = "([ \t]*)"; }
          { lit = "end"; }
          lineEnd
        ];
        f =
          v:
          let
            groups = builtins.elemAt v 2;
            items = builtins.elemAt v 8;
          in
          {
            kind = "block";
            items = map (item: item // { groups = item.groups ++ groups; }) items;
          };
      };
    };

    # `(if|unless) <cond, ignored> <items> [else <items>] end` -- both
    # branches' gems are collected (a union), per this file's header.
    IF_BLOCK = {
      action = {
        e = [
          {
            choice = [
              { lit = "if"; }
              { lit = "unless"; }
            ];
          }
          { regex = "([^\r\n]*)"; }
          lineEnd
          "ITEM_LIST"
          {
            opt = [
              { regex = "([ \t]*)"; }
              { lit = "else"; }
              {
                opt = {
                  regex = "([^\r\n]*)";
                };
              }
              lineEnd
              "ITEM_LIST"
            ];
          }
          { regex = "([ \t]*)"; }
          { lit = "end"; }
          lineEnd
        ];
        f =
          v:
          let
            ifItems = builtins.elemAt v 3;
            elseOpt = builtins.elemAt v 4;
            elseItems = if elseOpt == null then [ ] else builtins.elemAt elseOpt 4;
          in
          {
            kind = "block";
            items = ifItems ++ elseItems;
          };
      };
    };

    # A construct this grammar can't interpret (`def`/`class`/`module`/
    # `begin`/`case`, or a method-call `do |args| ... end` block) whose
    # `end` must still balance -- see OPAQUE_OPENER_LINE/OPAQUE_ITEM's
    # comments for why. Recurses into itself (via "OPAQUE_BLOCK", for
    # nesting) rather than "ITEM_LIST" -- a `gem`/`group` line inside a
    # `def`'s body is never actually evaluated when Bundler loads a
    # Gemfile, so this deliberately does NOT recognize them; everything
    # inside is discarded, matching this file's stated scope (arbitrary
    # Ruby method bodies aren't modeled).
    OPAQUE_BLOCK = {
      action = {
        e = [
          OPAQUE_OPENER_LINE
          { star = OPAQUE_ITEM; }
          ws
          { lit = "end"; }
          lineEnd
        ];
        f = v: null;
      };
    };

    # The one named rule GROUP_BLOCK/IF_BLOCK recurse into -- itself
    # recurses back into them (a genuine mutual-recursion cycle,
    # resolved lazily the same way every other self-referential rule in
    # this engine's grammars is -- see lib/packrat.nix's header comment).
    ITEM_LIST = itemListExpr;

    # The whole file: a flat ITEM_LIST, requiring the entire input
    # consumed (not just a parseable prefix) -- same "fail cleanly on a
    # genuine structural mismatch" discipline as
    # grammar/gemfile-lock.nix, even though individual UNRECOGNIZED
    # lines are tolerated; an unbalanced `do`/`end` (a real syntax error
    # in the Gemfile itself) should still surface as a parse failure,
    # not silently truncate.
    DOCUMENT = [
      "ITEM_LIST"
      {
        not = {
          regex = "(.)";
        };
      }
    ];
  };

  handlers = {
    DOCUMENT = v: builtins.elemAt v 0;
  };
}
