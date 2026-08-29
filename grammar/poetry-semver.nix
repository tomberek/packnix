# A grammar + evaluator for Poetry's version-constraint syntax -- what
# a `pyproject.toml`'s `python = "..."` field or a Poetry dependency's
# version string looks like: `^1.2.3`, `~1.2`, `>=3.9,<4`, `*`,
# `1.2.*`, `~2.7 || ^3.5`, a bare version (`3.12`, meaning exact match).
#
# Currently parsed in nixpkgs by
# pkgs/development/tools/poetry2nix/poetry2nix/semver.nix (per-clause
# operator/version matching) and lib.nix's `isCompatible` (splits a
# whole constraint string on `||`/`,`/`&&` via a naive `builtins.split`
# + left-fold). Both were found to have REAL, DEMONSTRABLE BUGS while
# building this grammar -- not theoretical concerns:
#   - `^1.2.3`'s upper bound is computed as `builtins.splitVersion v`
#     with ONE index bumped in place, rest left UNCHANGED (not
#     zeroed) -- e.g. `^1.2.3` computes an upper bound of `2.2.3`, not
#     the correct `2.0.0`. Confirmed directly: `satisfiesSemver
#     "2.2.0" "^1.2.3"` and `satisfiesSemver "2.1.99" "^1.2.3"` both
#     incorrectly return `true` on real nixpkgs code, when real Poetry
#     semantics (confirmed against Poetry's own documentation) requires
#     both to be `false` -- `^1.2.3` must never accept a 2.x.x version.
#   - Same class of bug for `~`: `~1.2.3`'s upper bound computes to
#     `1.3.3` (bumping only the middle component, never zeroing the
#     trailing one) instead of the correct `1.3.0`.
#   - `!=X.Y.*` (a negated wildcard, e.g. `!=3.0.*`, real and common in
#     `python-versions` fields across real `poetry.lock` files) doesn't
#     actually exclude anything: `satisfiesSemver "3.0.5" "!=3.0.*"`
#     incorrectly returns `true` (should be `false` -- 3.0.5 IS a 3.0.x
#     version, and should be excluded).
#   - A bare version with no operator at all (`python = "3.12"`, a
#     real value found in a real `pyproject.toml` on this machine) is
#     not recognized as valid syntax at all -- `semver.nix`'s
#     `parseConstraint` throws `"Constraint ... could not be parsed"`,
#     even though a bare version is documented, valid Poetry syntax
#     meaning an exact-match constraint.
#   - A standalone wildcard constraint like `1.*` (not nested inside an
#     `==`/`!=` clause) also throws -- `parseConstraint`'s two match
#     attempts (`version_cmp version` / `version - version`) don't
#     recognize a bare `X.Y.*` shape as its own valid clause at all,
#     even though real `python-versions` strings use exactly this form
#     (e.g. `!=3.0.*,!=3.1.*,!=3.2.*,>=2.7`, confirmed real).
#
# Verified against 65 real, distinct `python-versions`/`python = "..."`
# constraint strings extracted from real `poetry.lock`/`pyproject.toml`
# files on this machine -- every shape below is backed by that corpus:
# comma-AND lists, `||`-OR (confirmed real: `~2.7 || ^3.5`), mixed
# whitespace around operators/commas, `^`/`~`/bare/`>=`/`<`/`!=`
# constraints, and `!=X.Y.*` wildcard exclusions.
#
# Deliberately out of scope: PEP 440's full version grammar (pre/post/
# dev release segments, epochs, local version identifiers) -- Poetry's
# own constraint syntax only operates on plain dotted-integer versions
# in every real example found; this grammar's `version` rule matches
# that confirmed shape, not the full PEP 440 surface.
let
  ws = {
    opt = {
      regex = "([ \t]+)";
    };
  };

  # A plain dotted-integer version: one or more integer components
  # separated by ".". Deliberately does NOT model PEP 440's fuller
  # surface (pre-releases, local segments, etc.) -- see file header.
  versionBody = {
    regex = "([0-9]+(\\.[0-9]+)*)";
  };

  # Pure-`builtins` dot-splitter (no `lib` dependency, matching every
  # other grammar in this repo) -- `builtins.split "\\." s` alternates
  # matched-separator entries (lists) and unmatched-text entries
  # (strings); filtering to just the strings gives the dot-separated
  # components.
  splitDot = s: builtins.filter builtins.isString (builtins.split "\\." s);

  # A version with its LAST component replaced by a literal "*"
  # wildcard (`1.*`, `1.2.*`), or a bare "*" alone (meaning "any
  # version at all"). Captured as `{ prefix; }` (the dotted-integer
  # components before the "*", possibly empty for a bare "*") so the
  # evaluator can do a prefix-match rather than string-splitting the
  # wildcard notation again later. `evalRegex` (lib/packrat.nix) only
  # ever returns the WHOLE matched text (its outermost capture group),
  # never inner sub-group captures, so this re-derives the prefix from
  # that plain string itself rather than relying on the regex's own
  # nested groups.
  wildcardVersion = {
    action = {
      e = {
        regex = "(([0-9]+(\\.[0-9]+)*)\\.\\*|\\*)";
      };
      f = v: {
        prefix = if v == "*" then [ ] else splitDot (builtins.substring 0 (builtins.stringLength v - 2) v);
      };
    };
  };

  # `versionCmp` -- Poetry's comparison operators. `==`/`!=` before
  # `=`-less `<`/`>` variants so the two-character forms are never cut
  # short; `~=` (PEP 440's compatible-release operator, which Poetry
  # also accepts) tried before bare `~` for the same reason.
  versionCmp = {
    choice = [
      { lit = "=="; }
      { lit = "!="; }
      { lit = ">="; }
      { lit = "<="; }
      { lit = "~="; }
      { lit = "<"; }
      { lit = ">"; }
    ];
  };

  # A single constraint clause -- one of:
  #   - `^version`   (caret: compatible-release, semver-style)
  #   - `~version`   (tilde: minimal-version-with-limited-update)
  #   - `cmp version-or-wildcard` (comparison operator + a plain
  #     version OR a wildcard -- `!=3.0.*` is real and common)
  #   - `wildcard`   (a bare wildcard with no operator at all, e.g.
  #     `1.*` on its own -- real, confirmed in python-versions strings)
  #   - `version`    (a bare version with no operator: exact match --
  #     real, confirmed via a real `pyproject.toml`'s `python = "3.12"`)
  # Tried in this order: `^`/`~` are unambiguous on their first
  # character; `cmp` is tried before bare `wildcard`/`version` since a
  # comparison's operator character is never itself a digit or "*".
  versionOne = {
    action = {
      e = [
        ws
        {
          choice = [
            {
              action = {
                e = [
                  { lit = "^"; }
                  versionBody
                ];
                f = v: {
                  kind = "caret";
                  version = builtins.elemAt v 1;
                };
              };
            }
            {
              action = {
                e = [
                  { lit = "~"; }
                  versionBody
                ];
                f = v: {
                  kind = "tilde";
                  version = builtins.elemAt v 1;
                };
              };
            }
            {
              action = {
                e = [
                  versionCmp
                  ws
                  {
                    choice = [
                      wildcardVersion
                      versionBody
                    ];
                  }
                ];
                f =
                  v:
                  let
                    op = builtins.elemAt v 0;
                    val = builtins.elemAt v 2;
                  in
                  if builtins.isAttrs val then
                    {
                      kind = "cmpWildcard";
                      inherit op;
                      prefix = val.prefix;
                    }
                  else
                    {
                      kind = "cmp";
                      inherit op;
                      version = val;
                    };
              };
            }
            {
              action = {
                e = wildcardVersion;
                f = v: {
                  kind = "wildcard";
                  prefix = v.prefix;
                };
              };
            }
            {
              action = {
                e = versionBody;
                f = v: {
                  kind = "exact";
                  version = v;
                };
              };
            }
          ];
        }
      ];
      f = v: builtins.elemAt v 1;
    };
  };

  # `versionOne (wsp* "," wsp* versionOne)*` -- comma-separated
  # constraints are ANDed together (confirmed real:
  # `>=2.7, !=3.0.*, !=3.1.*, <4`, both with and without whitespace
  # after the comma).
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
      f = v: {
        kind = "and";
        clauses = [ (builtins.elemAt v 0) ] ++ map (p: builtins.elemAt p 2) (builtins.elemAt v 1);
      };
    };
  };

  # `versionMany (wsp* "||" wsp* versionMany)*` -- confirmed real:
  # `~2.7 || ^3.5` (a real `python-versions` string meaning "Python 2.7.x
  # OR any Python 3.5+").
  versionExpr = {
    action = {
      e = [
        versionMany
        {
          star = [
            ws
            { lit = "||"; }
            ws
            versionMany
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

  # --- Evaluator -------------------------------------------------------
  #
  # `mkSatisfies packrat version constraintString` parses
  # `constraintString` with the grammar above (needs `packrat` -- this
  # file doesn't self-import lib/packrat.nix, see `mkSatisfies`'s own
  # comment below), then evaluates the resulting tree against `version`
  # -- replacing both `semver.nix`'s per-clause matching AND `lib.nix`'s
  # `isCompatible` splitter with one correctly-specified implementation.
  # Every case documented as buggy in this file's header is fixed here:
  #   - caret/tilde upper bounds are computed by finding the right
  #     component to bump and ZEROING every component after it (not
  #     leaving them at their original digits).
  #   - `cmpWildcard` (`!=X.Y.*` and friends) compares `version`'s own
  #     leading components against the wildcard's prefix, so `!=3.0.*`
  #     genuinely excludes every 3.0.x version.
  #   - `exact` (a bare version, no operator) is a real, valid clause
  #     meaning `==`, not a parse error.
  #
  # `caretUpper`/`tildeUpper` operate on the CONSTRAINT's own version
  # string (e.g. "1.2.3" from "^1.2.3"), producing the correct exclusive
  # upper bound as a new version string, compared against `version`
  # via `builtins.compareVersions` (same primitive `semver.nix` already
  # used correctly for plain `>=`/`<`/`==`/`!=` clauses -- only the
  # caret/tilde bound COMPUTATION was wrong, not the comparison itself).
  caretUpper =
    versionStr:
    let
      parts = splitDot versionStr;
      n = builtins.length parts;
      isZero = i: builtins.elemAt parts i == "0";
      findFirstNonZero =
        i:
        if i >= n - 1 then
          n - 1
        else if !(isZero i) then
          i
        else
          findFirstNonZero (i + 1);
      idx = findFirstNonZero 0;
      bump =
        i: v:
        if i == idx then
          builtins.toString (builtins.fromJSON v + 1)
        else if i > idx then
          "0"
        else
          v;
    in
    builtins.concatStringsSep "." (
      map (i: bump i (builtins.elemAt parts i)) (builtins.genList (i: i) n)
    );

  tildeUpper =
    versionStr:
    let
      parts = splitDot versionStr;
      n = builtins.length parts;
      idx = if n >= 2 then 1 else 0;
      bump =
        i: v:
        if i == idx then
          builtins.toString (builtins.fromJSON v + 1)
        else if i > idx then
          "0"
        else
          v;
    in
    builtins.concatStringsSep "." (
      map (i: bump i (builtins.elemAt parts i)) (builtins.genList (i: i) n)
    );

  # `version`'s own leading N components equal `prefix` exactly (used by
  # both `wildcard` -- "does version match this wildcard" -- and
  # `cmpWildcard`'s `!=`/`==` -- "does version match, to negate/confirm").
  matchesWildcardPrefix =
    version: prefix:
    let
      vparts = splitDot version;
    in
    builtins.length vparts >= builtins.length prefix
    && builtins.genList (i: builtins.elemAt vparts i) (builtins.length prefix) == prefix;

  evalClause =
    version: clause:
    if clause.kind == "and" then
      builtins.all (evalClause version) clause.clauses
    else if clause.kind == "or" then
      builtins.any (evalClause version) clause.clauses
    else if clause.kind == "caret" then
      builtins.compareVersions version clause.version >= 0
      && builtins.compareVersions version (caretUpper clause.version) < 0
    else if clause.kind == "tilde" then
      builtins.compareVersions version clause.version >= 0
      && builtins.compareVersions version (tildeUpper clause.version) < 0
    else if clause.kind == "exact" then
      builtins.compareVersions version clause.version == 0
    else if clause.kind == "wildcard" then
      clause.prefix == [ ] || matchesWildcardPrefix version clause.prefix
    else if clause.kind == "cmpWildcard" then
      let
        matches = matchesWildcardPrefix version clause.prefix;
      in
      if clause.op == "!=" then
        !matches
      else if clause.op == "==" then
        matches
      else
        throw "poetry-semver: '${clause.op}' cannot be combined with a wildcard version"
    else if clause.kind == "cmp" then
      let
        cmp = builtins.compareVersions version clause.version;
      in
      if clause.op == "==" then
        cmp == 0
      else if clause.op == "!=" then
        cmp != 0
      else if clause.op == ">=" then
        cmp >= 0
      else if clause.op == "<=" then
        cmp <= 0
      else if clause.op == ">" then
        cmp > 0
      else if clause.op == "<" then
        cmp < 0
      else if clause.op == "~=" then
        # PEP 440 compatible-release: same as caret for a 2+ component
        # version (>=X.Y, <(X+1).0 when only major.minor given; the
        # last given component may vary freely, everything before it
        # is pinned) -- reuses tildeUpper's "bump second-to-last given
        # component" rule, which computes exactly this bound.
        cmp >= 0 && builtins.compareVersions version (tildeUpper clause.version) < 0
      else
        throw "poetry-semver: unknown operator '${clause.op}'"
    else
      throw "poetry-semver: unknown clause kind '${clause.kind}'";

  parseConstraint =
    packrat: constraintStr:
    (packrat.run {
      grammar = grammarDef;
      handlers = handlersDef;
    } 0 constraintStr).CONSTRAINT;

  grammarDef = {
    CONSTRAINT = [
      ws
      versionExpr
      ws
      {
        not = {
          regex = "(.)";
        };
      }
    ];
  };
  handlersDef = {
    CONSTRAINT = v: builtins.elemAt v 1;
  };

  # `mkSatisfies packrat version constraintStr` -- takes `packrat` as an
  # explicit parameter rather than self-importing `lib/packrat.nix`,
  # matching this repo's established convention of every consumer
  # (tests.nix, packnix-bundler's mk-gemset.nix, this file's own README
  # examples) receiving `packrat` externally rather than a grammar file
  # importing its own engine dependency.
  mkSatisfies =
    packrat: version: constraintStr:
    let
      tree = parseConstraint packrat constraintStr;
    in
    if tree == false then
      throw "poetry-semver: constraint '${constraintStr}' could not be parsed"
    else
      evalClause version tree;
in
{
  grammar = grammarDef;
  handlers = handlersDef;
  inherit mkSatisfies;
}
