# The SAME flake.lock schema grammar/flakelock.nix parses, but written
# against lib/valuewalk.nix instead: builtins.fromJSON does the actual
# text parse (native C++, no packrat combinators walk the string at all),
# and this schema only walks the resulting Nix value tree to confirm it
# has the shape flake.lock is confirmed to always have -- the same 4 node
# key-sets, the same locked/original field set, documented in
# grammar/flakelock.nix's own header comment.
#
# Written as a NAMED GRAMMAR (`vw.run { grammar; } value`), mirroring
# lib/packrat.nix's own `grammar = { RuleName = expr; ... }` shape and
# bare-string ("LOCKED") nonterminal-reference syntax as closely as this
# domain allows -- see lib/valuewalk.nix's header comment for what
# resolving "Name" by lookup into a lazily self-referential attrset of
# compiled rules means here. Matches grammar/flakelock.nix's own
# LOCKED/NODE/DOCUMENT rule names one-for-one so the two files read as
# parallel structures.
#
# Measured on bench/fixtures/synth-2000.json (636KB, 2000 nodes),
# confirmed byte-identical output to grammar/flakelock.nix's DOCUMENT
# rule on the same fixture: ~0.07-0.09s / ~50-52MB here, vs.
# grammar/flakelock.nix's ~0.6-0.9s / ~194-197MB -- roughly 8x faster,
# ~4x less RSS. The native parser does the character-by-character work in
# C++; this only walks a tree shaped by nodes-and-fields, not bytes of
# text, so its cost scales with node COUNT rather than input byte size.
#
# What this trades away vs. grammar/flakelock.nix's rule-by-rule string
# walk: no check on key ORDER within an object (fromJSON has already
# discarded that by the time this schema ever sees the value -- there is
# no way to recover it after the fact). grammar/flakelock.nix's fields
# being tried in fixed alphabetical order means a flake.lock with keys in
# a different order fails to parse; this schema does not notice or care.
# If a caller genuinely depends on rejecting reordered keys (not just
# validating the DATA), grammar/flakelock.nix is the only one of the two
# that can do that.
#
# FAILURE SENTINEL: lib/valuewalk.nix uses `null`, confirmed safe here
# because no field in this schema is ever legitimately `null` in a real
# flake.lock (see lib/valuewalk.nix's header for why that's a
# per-schema fact, not a general guarantee, and what a schema whose real
# data DOES contain `null` would need instead).
#
# Run with:
#   nix eval --impure --expr '
#     let
#       vw = import ../lib/valuewalk.nix;
#       g = import ./flakelock-valuewalk.nix;
#     in (vw.run { grammar = g; } (builtins.fromJSON
#          (builtins.readFile ../bench/fixtures/synth-5.json))).DOCUMENT
#   ' --json
{
  # Every field grammar/flakelock.nix's lockedOriginalFields lists,
  # confirmed present in a real file's `locked`/`original` objects. All
  # optional (see grammar/flakelock.nix's own comment: `type` is
  # confirmed present in 100% of real objects but still `opt`-wrapped
  # there too, since getting a real "missing type" rejection costs
  # nothing and catches a genuine schema violation rather than silently
  # accepting one).
  LOCKED = {
    attrs = {
      closed = true;
      optional = {
        dir = {
          string = { };
        };
        lastModified = {
          int = { };
        };
        narHash = {
          string = { };
        };
        owner = {
          string = { };
        };
        ref = {
          string = { };
        };
        repo = {
          string = { };
        };
        rev = {
          string = { };
        };
        revCount = {
          int = { };
        };
        shallow = {
          bool = { };
        };
        submodules = {
          bool = { };
        };
        type = {
          string = { };
        };
        url = {
          string = { };
        };
      };
    };
  };

  # The 4 node shapes grammar/flakelock.nix's `node` choice enumerates,
  # same most-common-first order (see that file's comment for the
  # observed corpus counts backing this order and this enumeration).
  # "LOCKED" is a bare-string rule reference, resolved by name against
  # this same grammar attrset -- see lib/valuewalk.nix's header comment.
  NODE = {
    choice = [
      {
        attrs = {
          closed = true;
          fields = {
            flake = {
              bool = { };
            };
            locked = "LOCKED";
            original = "LOCKED";
          };
        };
      }
      {
        attrs = {
          closed = true;
          fields = {
            # Input names are arbitrary (other nodes' names), so this is
            # `attrsOf`, not a fixed `fields` set -- same reasoning as
            # grammar/flakelock.nix's inputsObject.
            inputs = {
              attrsOf = {
                string = { };
              };
            };
            locked = "LOCKED";
            original = "LOCKED";
          };
        };
      }
      {
        attrs = {
          closed = true;
          fields = {
            locked = "LOCKED";
            original = "LOCKED";
          };
        };
      }
      {
        attrs = {
          closed = true;
          fields = {
            inputs = {
              attrsOf = {
                string = { };
              };
            };
          };
        };
      }
    ];
  };

  DOCUMENT = {
    attrs = {
      closed = true;
      fields = {
        # Node names are arbitrary, same as inputs above. "NODE" is
        # another bare-string rule reference.
        nodes = {
          attrsOf = "NODE";
        };
        root = {
          string = { };
        };
        version = {
          int = { };
        };
      };
    };
  };
}
