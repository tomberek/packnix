# A real (subset of) Ruby Bundler `Gemfile` grammar for lib/packrat.nix --
# NOT `Gemfile.lock` (that's grammar/gemfile-lock.nix, a fixed,
# machine-generated format). A `Gemfile` is technically arbitrary Ruby, so
# this grammar targets only the confirmed-common subset needed to recover
# one specific fact `Gemfile.lock` never records: which Bundler *group*
# each gem belongs to.
#
# Why this matters: `packnix-bundler`'s `mkGemset` needs per-gem group
# membership to make `bundlerEnv`'s `groups` filtering actually work --
# without it, every gem is tagged `["default"]` and `groups` filtering
# becomes a silent no-op.
#
# Scope confirmed by scanning a nixpkgs checkout's 136-file corpus for
# every Gemfile that uses groups at all (6 files) -- these are the ONLY
# forms observed:
#   - `group :a, :b do ... end` blocks (bare symbols, comma-separated,
#     may be followed by `, optional: true` -- ignored). May nest.
#   - `gem 'name', group: :x` / `group: [:x, :y]` / `groups: [:x, :y]` --
#     three real spellings of an inline alternative to the block form.
#   - `if <cond> ... end`, `unless <cond> ... end`, and
#     `if <cond> ... else ... end` wrapping either of the above. The
#     condition itself is captured as raw text but never interpreted --
#     there's no way to know its value without knowing the environment
#     `bundle install` was run under. Both branches' gems are collected
#     (a union, not a choice of one branch) -- this never silently drops
#     a gem, though it may over-include relative to one specific real
#     `bundle install` run.
#
# Deliberately out of scope (real, but rare -- 1-2 files each in the
# corpus):
#   - `gemspec` (loads a `.gemspec` file, which can itself declare
#     groups)
#   - `eval_gemfile` / `Dir.glob ... do |f| eval_gemfile f end` (loads
#     other Gemfile-like files)
#   - anything else that's actually arbitrary Ruby (method calls, string
#     interpolation, multi-line expressions, heredocs) -- EXCEPT
#     top-level `def`/`class`/`module`/`begin`/`case` blocks (a top-level
#     `def rails_master? ... end` helper is real, found in discourse's
#     Gemfile), which ARE recognized structurally as "OPAQUE_BLOCK":
#     matching only the opening keyword and the balanced closing `end`,
#     contents discarded, purely so their `end` doesn't get mistaken for
#     a dangling GROUP_BLOCK/IF_BLOCK terminator. A generic
#     `<arbitrary-expr> do [|args|] ... end` opener (e.g. redmine's
#     `Dir.glob(...).each do |file|`) is NOT recognized this way -- see
#     OPAQUE_OPENER_LINE for why.
#
# Every line/construct not specifically recognized is matched by a
# catch-all fallback rule and simply ignored (not a parse failure) --
# a Gemfile can't realistically be fully modeled short of embedding a
# Ruby interpreter, so "degrade gracefully, recover what we can" is the
# right posture here. A gem declared only inside an out-of-scope
# mechanism just never appears in this grammar's output. If any
# construct in the file can't be matched at all (most commonly a
# `Dir.glob ... do |f| ... end` opener), the whole file fails to parse
# (`DOCUMENT` returns `false`) -- `packnix-bundler`'s `mkGemset` treats
# that as "no group info, fall back to `["default"]`".
let
  # No escape sequences modeled for quoted gem/group names -- real
  # Gemfiles never use `\`/interpolation in a `gem 'name'`/`:symbol`
  # position.
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

  # A bare `:symbol` -- Ruby identifier rules.
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

  # One or more comma-separated symbols: `:a` or `:a, :b, :c`. Used for
  # `group :a, :b do` and for the array form `[:a, :b]`'s contents.
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
  # arguments: a bare symbol, a quoted string, an array literal of
  # symbols (`[:a, :b]`), or a bare `true`/`false` -- NOT a general Ruby
  # expression. `[^]...]`'s bare closing `]` is deliberate: escaping `]`
  # outside a bracket expression is invalid POSIX ERE in this engine
  # (same idiom grammar/yaml.nix's PLAIN_SCALAR_FLOW uses).
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
  # `github:`, `path:`, etc.) is still parsed, so the line doesn't fall
  # through to the catch-all and lose its gem name, but its value is
  # discarded.
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

  # A positional arg (version constraint string, or a bare symbol for
  # the rarer `gem :name` form) -- accepted and ignored.
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
  # symbol(s) into this gem's own inline group declaration (takes the
  # union if `group:` somehow appears more than once).
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
  # swallow them instead.
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
  # contain (discourse's has `def rails_master? ... end`) that this
  # grammar can't interpret, but whose `end` DOES need balancing --
  # UNRECOGNIZED_LINE refuses to swallow a bare "end" (GROUP_BLOCK/
  # IF_BLOCK need that refusal to find their OWN terminator), so a
  # dangling `def`'s `end` would otherwise have nothing to close against.
  # Recognizes a `def`/`class`/`module`/`begin`/`case` keyword line only
  # -- NOT a generic `<arbitrary-expr> do [|args|]` opener (e.g.
  # redmine's `Dir.glob(...).each do |file|`): evalRegex's bounded
  # lookahead window only retries wider when a match fills the window
  # exactly, not when no match is found at all, so a pattern needing to
  # scan past an arbitrary-length expression before finding " do" can
  # silently fail on a longer real line. That form is already out of
  # scope (falls back to UNRECOGNIZED_LINE's failure -> mkGemset's
  # whole-file fallback). Tried AFTER GROUP_BLOCK/IF_BLOCK in itemExpr's
  # choice.
  OPAQUE_OPENER_LINE = [
    { regex = "((def|class|module|begin|case)([ \t?!(][^\r\n]*)?)"; }
    lineEnd
  ];

  # Catch-all: any line this grammar doesn't otherwise recognize --
  # matched and discarded, not a parse failure. Must be tried LAST in
  # every choice it appears in. Refuses to match a bare "end", "else",
  # or blank line, since those are structurally significant to
  # GROUP_BLOCK/IF_BLOCK's own termination/branching.
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
  # "end" or a nested block opener (handled by "OPAQUE_BLOCK" itself,
  # tried first in OPAQUE_ITEM's choice). No attempt is made to recognize
  # `gem`/`group` lines inside an opaque block -- a call inside a `def`'s
  # body never executes at Gemfile-eval time.
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
  # IF_BLOCK's body. `commentLine`/`blankLine` produce no item at all
  # (filtered out by ITEM_LIST's handler); everything else produces one
  # `{kind;...}` value (GEM_LINE carries name/groups; GROUP_BLOCK/
  # IF_BLOCK carry their own nested `items`, flattened by ITEM_LIST).
  # Defined as a plain value here, not a named grammar rule, since
  # ITEM_LIST references it via `{ star = itemExpr; }` (an inlined
  # expression) -- fine, since itemExpr doesn't need to recurse into
  # itself, only GROUP_BLOCK/IF_BLOCK (which DO need named-rule status,
  # since ITEM_LIST recurses into them and they recurse back). Leads
  # with `ws`: `group`/`if`/`gem` bodies are conventionally indented, and
  # no atom here skips leading whitespace on its own.
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
  # flattened into the parent's own flat list, and null entries dropped.
  # Every GEM_LINE inside a GROUP_BLOCK already has that block's groups
  # unioned into it by GROUP_BLOCK's own handler, so this itself does no
  # group-tagging, only concatenation. `itemListExpr` is a plain
  # expression, not `{star = "ITEM_LIST";}` -- ITEM_LIST itself is the
  # named rule GROUP_BLOCK/IF_BLOCK reference, defined via this
  # expression down in `grammar` below.
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
    # reference, since actual recursion needs a lazy nonterminal
    # reference, not a compile-time-inlined expression -- bare strings
    # resolve through `derivs.${expr}` in lib/packrat.nix's `compile`,
    # which is how self-reference works at all). Every gem line found,
    # transitively through nested blocks, gets this block's groups
    # UNIONED into its own -- a gem's final groups is the union of every
    # enclosing GROUP_BLOCK plus any inline `group:`/`groups:` kwarg on
    # the gem line itself.
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
    # `begin`/`case`) whose `end` must still balance -- see
    # OPAQUE_OPENER_LINE/OPAQUE_ITEM above for why. Recurses into itself
    # (via "OPAQUE_BLOCK", for nesting) rather than "ITEM_LIST" -- a
    # `gem`/`group` line inside a `def`'s body is never actually
    # evaluated when Bundler loads a Gemfile, so this deliberately does
    # NOT recognize them; everything inside is discarded.
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
    # recurses back into them, resolved lazily the same way every other
    # self-referential rule in this engine's grammars is.
    ITEM_LIST = itemListExpr;

    # The whole file: a flat ITEM_LIST, requiring the entire input
    # consumed, not just a parseable prefix -- an unbalanced `do`/`end`
    # (a real syntax error in the Gemfile itself) should surface as a
    # parse failure, not silently truncate.
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
