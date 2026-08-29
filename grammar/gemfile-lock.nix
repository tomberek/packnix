# A real Gemfile.lock (Ruby Bundler lockfile) grammar for lib/packrat.nix.
# Not JSON/YAML/TOML -- a bespoke, line-oriented, fixed-indent format with
# its own top-level sections. Schema confirmed by surveying 136 real
# Gemfile.lock files (a nixpkgs checkout's pkgs/ tree), not derived from
# Bundler's docs alone.
#
# Why this grammar matters for nixpkgs: converting a Gemfile.lock into a
# Nix-consumable `gemset.nix` (as `bundlerEnv` expects) today requires
# running `bundix`, an external Ruby tool needing network access to
# compute each gem's sha256. But Bundler >=2.7's `CHECKSUMS` section
# already embeds a hex sha256 per gem, and that hash is EXACTLY what
# bundix ends up storing (`nix hash convert --to base32 --hash-algo
# sha256 <hex from CHECKSUMS>` produces the identical string bundix
# writes into gemset.nix). For any lockfile with a CHECKSUMS section, a
# pure-Nix parser can read the whole dependency graph AND every gem's
# fetch hash directly -- no bundix, no network needed at eval time.
#
# Top-level shape (fixed order, confirmed across the whole corpus):
#   [ (GEM|GIT|PATH source block)* ]
#   PLATFORMS
#     <platform-name-line>+
#   DEPENDENCIES
#     <dependency-line>+
#   [ CHECKSUMS
#       <checksum-line>+ ]
#   [ RUBY VERSION
#       <version-line> ]
#   BUNDLED WITH
#     <version-line>
#
# Confirmed structural facts driving this grammar's design:
#   - indentation is fixed and NOT configurable like grammar/yaml.nix's
#     indentStep -- Bundler always emits exactly 2/4/6 spaces (section
#     body / spec name / spec dependency), and 3 spaces for the one-line
#     RUBY VERSION / BUNDLED WITH values.
#   - a document can have MULTIPLE GEM/GIT/PATH blocks (3 of 136 corpus
#     files have 2+ GEM blocks; large multi-gem monorepo lockfiles like
#     GitLab's have dozens of PATH blocks) -- modeled as `star`, not a
#     fixed count.
#   - a GEM/GIT/PATH block's `specs:` list is never actually empty in
#     valid output, but an EMPTY "specs:" with a trailing blank line
#     immediately after DOES occur (one corpus file has a bare
#     "GEM\n  specs:\n\n" block) -- modeled as `star`, not `plus`, on spec
#     entries.
#   - nesting is capped at exactly one level: a spec has direct
#     dependencies listed under it (6-space indent), but those dependency
#     lines are BARE gem names/constraints, never further-nested specs of
#     their own (no line in the corpus is indented past 6 spaces) --
#     unlike grammar/yaml.nix, this format needs no depth-indexed rule
#     generation at all, just three fixed indent literals.
#   - a spec's version can be platform-qualified (e.g. "ffi
#     (1.17.1-x86_64-linux-gnu)") -- the version and platform suffix are
#     both free-form and separated by "-", and gem names themselves
#     legitimately contain "-", so this grammar does NOT try to split
#     platform from version; the whole parenthesized text is kept as one
#     opaque "version" string (matches how CHECKSUMS keys must match a
#     spec's version text exactly, so keeping it unsplit is also what a
#     consumer needs for that lookup).
#   - DEPENDENCIES lines may have a trailing "!" (means "path/git source,
#     not from the primary GEM index" in Bundler's own semantics) and/or
#     one-or-more comma-separated version constraints in parens (e.g.
#     "sys-filesystem (~> 1.5, >= 1.5.5)") -- both optional and
#     independent of each other.
#   - version constraint operators confirmed in use: = != < <= > >= ~>
#   - CHECKSUMS entries can have NO hash at all (path-sourced gems have a
#     checksum line with just "name (version)", no " sha256=...") --
#     modeled as an optional trailing field.
#   - GIT blocks always have remote+revision; PATH blocks only ever have
#     remote (no revision -- there's nothing to pin, it's a local path).
#     GIT blocks MAY additionally have exactly one of tag:/ref:/branch:
#     (branch: is a real Bundler field not seen in this corpus but
#     supported for completeness, same shape as tag:/ref:).
#   - no escape sequences anywhere in this format -- gem names, versions,
#     remotes, revisions are all plain text with a restricted charset (no
#     quoting/escaping syntax exists in Bundler's lockfile serializer).
#
# Deliberately out of scope:
#   - PLUGIN SOURCES / plugin-specific sections (rare Bundler plugin
#     feature, not seen anywhere in the 136-file corpus)
#   - preserving comment lines (Bundler's lockfile format has none to
#     preserve -- unlike YAML, there is no comment syntax at all here)
# A malformed or differently-structured Gemfile.lock correctly fails to
# parse rather than silently mis-parsing, same discipline as every other
# grammar in this repo.
let
  # Bundler's charset for gem/platform names, remotes, revisions, and
  # RUBY VERSION/BUNDLED WITH version text: no escapes exist in this
  # format at all, so every leaf is a plain greedy run stopping only at
  # the newline (or, for names appearing before a parenthesized version,
  # at "(" / a following space too).
  restOfLine = {
    regex = "([^\r\n]+)";
  };

  # "end of line": an actual newline, or end of input (the file's last
  # line need not have a trailing newline).
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

  blankLine = {
    lit = "\n";
  };

  # A gem/platform name: letters, digits, and the punctuation Bundler
  # allows in a gem name (confirmed charset from the corpus: a-zA-Z0-9,
  # plus "_.-"). Stops before "(" or a space, since a spec line's name is
  # immediately followed by " (version)".
  bareName = {
    regex = "([A-Za-z0-9_.-]+)";
  };

  # "(text)" with arbitrary non-")" text inside -- used for a spec's
  # version (possibly platform-qualified, e.g. "1.17.1-x86_64-linux-gnu")
  # and for a dependency's constraint list (e.g. "~> 1.5, >= 1.5.5"). Kept
  # as one opaque string in both cases (see file header for why version
  # text is never split further).
  parenText = {
    action = {
      e = [
        { lit = "("; }
        { regex = "([^)]*)"; }
        { lit = ")"; }
      ];
      f = v: builtins.elemAt v 1;
    };
  };

  # A single spec dependency line, e.g. "multipart-post (>= 1.2, < 3)" or
  # bare "json" with no constraint at all -- six-space indent, under a
  # spec's own four-space-indented name line.
  specDependency = {
    action = {
      e = [
        { lit = "      "; }
        bareName
        {
          opt = [
            { lit = " "; }
            parenText
          ];
        }
        lineEnd
      ];
      f = v: {
        name = builtins.elemAt v 1;
        constraint =
          let
            c = builtins.elemAt v 2;
          in
          if c == null then null else builtins.elemAt c 1;
      };
    };
  };

  # A single spec entry: "  name (version)\n" followed by zero or more
  # of its own dependency lines. Four-space indent for the name itself.
  specEntry = {
    action = {
      e = [
        { lit = "    "; }
        bareName
        { lit = " "; }
        parenText
        lineEnd
        { star = specDependency; }
      ];
      f = v: {
        name = builtins.elemAt v 1;
        version = builtins.elemAt v 3;
        dependencies = builtins.elemAt v 5;
      };
    };
  };

  # "  key: value\n" -- used for remote:/revision:/tag:/ref:/branch: lines
  # inside a GEM/GIT/PATH block header.
  headerField = key: {
    action = {
      e = [
        { lit = "  ${key}: "; }
        restOfLine
        lineEnd
      ];
      f = v: builtins.elemAt v 1;
    };
  };

  gemBlock = {
    action = {
      e = [
        { lit = "GEM\n"; }
        { opt = headerField "remote"; }
        { lit = "  specs:\n"; }
        { star = specEntry; }
      ];
      f = v: {
        type = "gem";
        remote = builtins.elemAt v 1;
        specs = builtins.elemAt v 3;
      };
    };
  };

  # GIT's optional third header field: exactly one of tag:/ref:/branch:,
  # if present at all (confirmed: never more than one of these three
  # co-occurs in a single GIT block).
  gitRefField = {
    opt = {
      choice = [
        (headerField "tag")
        (headerField "ref")
        (headerField "branch")
      ];
    };
  };

  gitBlock = {
    action = {
      e = [
        { lit = "GIT\n"; }
        (headerField "remote")
        (headerField "revision")
        gitRefField
        { lit = "  specs:\n"; }
        { star = specEntry; }
      ];
      f = v: {
        type = "git";
        remote = builtins.elemAt v 1;
        revision = builtins.elemAt v 2;
        ref = builtins.elemAt v 3;
        specs = builtins.elemAt v 5;
      };
    };
  };

  pathBlock = {
    action = {
      e = [
        { lit = "PATH\n"; }
        (headerField "remote")
        { lit = "  specs:\n"; }
        { star = specEntry; }
      ];
      f = v: {
        type = "path";
        remote = builtins.elemAt v 1;
        specs = builtins.elemAt v 3;
      };
    };
  };

  # Every source block ends in exactly one blank line before the next
  # section (confirmed: blank lines never occur mid-block, only ever as
  # the block/section separator).
  sourceBlock = [
    {
      choice = [
        gemBlock
        gitBlock
        pathBlock
      ];
    }
    blankLine
  ];

  platformLine = {
    action = {
      e = [
        { lit = "  "; }
        restOfLine
        lineEnd
      ];
      f = v: builtins.elemAt v 1;
    };
  };

  # A DEPENDENCIES line: "  name[ (constraints)][!]\n" -- the "!" marker
  # (means "resolved from a GIT/PATH source, not the primary GEM index")
  # and the constraint list are both optional and independent of each
  # other (confirmed: both combinations, and neither, appear in the
  # corpus).
  dependencyLine = {
    action = {
      e = [
        { lit = "  "; }
        bareName
        {
          opt = [
            { lit = " "; }
            parenText
          ];
        }
        {
          opt = {
            lit = "!";
          };
        }
        lineEnd
      ];
      f = v: {
        name = builtins.elemAt v 1;
        constraint =
          let
            c = builtins.elemAt v 2;
          in
          if c == null then null else builtins.elemAt c 1;
        pinned = builtins.elemAt v 3 != null;
      };
    };
  };

  # A CHECKSUMS line: "  name (version)[ sha256=hex]\n" -- path-sourced
  # gems have no hash at all (confirmed in the corpus).
  checksumLine = {
    action = {
      e = [
        { lit = "  "; }
        bareName
        { lit = " "; }
        parenText
        {
          opt = [
            { lit = " sha256="; }
            { regex = "([0-9a-f]+)"; }
          ];
        }
        lineEnd
      ];
      f = v: {
        name = builtins.elemAt v 1;
        version = builtins.elemAt v 3;
        sha256 =
          let
            m = builtins.elemAt v 4;
          in
          if m == null then null else builtins.elemAt m 1;
      };
    };
  };

  # RUBY VERSION's and BUNDLED WITH's one-line value: always exactly
  # 3-space indented, distinct from every other section's 2-space body
  # indent (confirmed universal across the corpus -- a genuine quirk of
  # Bundler's own serializer, not a typo to normalize away).
  threeSpaceValue = {
    action = {
      e = [
        { lit = "   "; }
        restOfLine
        lineEnd
      ];
      f = v: builtins.elemAt v 1;
    };
  };

  document = [
    { star = sourceBlock; }
    { lit = "PLATFORMS\n"; }
    { star = platformLine; }
    { lit = "\nDEPENDENCIES\n"; }
    { star = dependencyLine; }
    {
      opt = [
        { lit = "\nCHECKSUMS\n"; }
        { star = checksumLine; }
      ];
    }
    {
      opt = [
        { lit = "\nRUBY VERSION\n"; }
        threeSpaceValue
      ];
    }
    { lit = "\nBUNDLED WITH\n"; }
    threeSpaceValue
    {
      opt = {
        regex = "(\r?\n)";
      };
    } # a lockfile's final line may or may not end in a newline
    {
      not = {
        regex = "(.)";
      };
    } # require the ENTIRE input consumed, not just a prefix
  ];

  documentHandler =
    v:
    let
      checksumsOpt = builtins.elemAt v 5;
      rubyVersionOpt = builtins.elemAt v 6;
    in
    {
      # `sourceBlock` is a plain (non-action) sequence [blockValue "\n"],
      # so `star`'s collected list is a list of those raw pairs -- unwrap
      # each to just its blockValue.
      sources = map (pair: builtins.elemAt pair 0) (builtins.elemAt v 0);
      platforms = builtins.elemAt v 2;
      dependencies = builtins.elemAt v 4;
      checksums = if checksumsOpt == null then null else builtins.elemAt checksumsOpt 1;
      rubyVersion = if rubyVersionOpt == null then null else builtins.elemAt rubyVersionOpt 1;
      bundledWith = builtins.elemAt v 8;
    };

  grammar = {
    DOCUMENT = document;
  };
  handlers = {
    DOCUMENT = documentHandler;
  };
in
{
  inherit grammar handlers;
}
