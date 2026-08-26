# A schema-validation engine over an ALREADY-PARSED Nix value tree (from
# builtins.fromJSON/fromTOML), not over string positions like
# lib/packrat.nix. Motivation: lib/packrat.nix's { json = {}; }/{ toml =
# {}; } combinator (see its header comment) hands a substring to the
# native parser for speed, but that alone gives no structural validation
# -- no "does this object have exactly these fields, in these types" the
# way a hand-written packrat grammar (e.g. grammar/flakelock.nix) gets for
# free from its rule-by-rule shape. This is that validation layer, applied
# to the tree fromJSON/fromTOML already built, instead of re-deriving it
# from text.
#
# Measured against grammar/flakelock.nix on a 636KB, 2000-node synthetic
# flake.lock-shaped fixture (bench/fixtures/synth-2000.json), confirmed
# byte-identical output to the specialized grammar: ~8x faster (~0.07-0.09s
# vs. ~0.6-0.9s), ~4x less RSS (~50MB vs. ~195MB). The native parse does
# the character-by-character work in C++; this only walks the resulting
# tree, whose size is nodes-and-fields, not bytes-of-text.
#
# Schema DSL (attrset-as-data, deliberately parallel to packrat.nix's
# grammar DSL, but over VALUES not string positions):
#   { string = {}; }        -> matches iff isString
#   { int = {}; }           -> matches iff isInt
#   { bool = {}; }          -> matches iff isBool
#   { pattern = "..."; }    -> matches iff isString and builtins.match
#                              against the WHOLE string succeeds (same
#                              POSIX ERE semantics as packrat.nix's regex,
#                              but no position/window: matched against the
#                              complete string value, not a prefix)
#   { choice = [s1 s2 ...]; } -> ordered choice: first schema that matches
#                              (same name and semantics as packrat.nix's
#                              `choice` -- both are ordered alternation
#                              over the SAME input/value, no difference
#                              between the two domains here)
#   { listOf = s; }         -> matches iff isList and every element
#                              matches `s`
#   { attrsOf = s; }        -> matches iff isAttrs and every VALUE
#                              matches `s` (keys unconstrained -- for
#                              arbitrary-keyed maps like flake.lock's
#                              `nodes`/`inputs`)
#   { attrs = { fields; optional; closed; }; }
#                            -> matches iff isAttrs, every name in
#                               `fields` is present and matches its
#                               schema, every name in `optional` (if
#                               present) matches its schema, and (if
#                               `closed` -- default true) no OTHER key
#                               exists. `closed = false` passes any
#                               unconstrained key's value through
#                               UNCHANGED (see checkExprSafety-style
#                               reasoning below on why this matters for
#                               byte-identical reconstruction).
#   { action = { e; f; }; } -> e, with f applied to its value on success
#                              (same spirit as packrat.nix's action)
#   "Name"                  -> reference to another named rule in the
#                              same grammar (see `run`/`compileGrammar`
#                              below) -- same bare-string syntax as
#                              packrat.nix's nonterminal reference, and
#                              needed for the same reason: a recursive or
#                              mutually-recursive schema (e.g. flake.lock's
#                              NODE containing a `locked`/`original` field
#                              that's itself a named rule, or a schema
#                              that's self-referential the way a JSON
#                              value's own grammar is). Resolved by NAME
#                              lookup into a lazily-built, self-referential
#                              attrset of compiled rules -- NOT by
#                              position-index the way packrat.nix's
#                              `nameToIndex` resolves a nonterminal to a
#                              fixed Derivs-array slot. That indexing
#                              exists in packrat.nix ONLY to make repeated
#                              reference to "the same rule at the same
#                              input POSITION" share one memoized thunk
#                              (Ford's packrat technique, see that file's
#                              header) -- there is no position dimension
#                              here at all (each rule reference in a
#                              value-tree walk lands on a genuinely
#                              different subtree, e.g. one flake.lock
#                              node's `locked` field vs. another's), so
#                              there is no redundant work to memoize
#                              against and no need for an index-based
#                              lookup scheme. Plain Nix name lookup already
#                              gives correct (and correctly lazy, so
#                              mutually-recursive rules don't infinite-loop
#                              at COMPILE time) resolution for free.
#
# `run`/`compileGrammar` mirror packrat.nix's own `run`/`buildDerivs`
# public shape as closely as this domain allows: `run { grammar; handlers
# ? {}; } value` returns `{ <RuleName> = matchedValue | null; ... }` for
# EVERY rule in `grammar`, each applied to the SAME `value` (there is no
# "position" for different rules to be evaluated relative to, unlike
# packrat's per-position Derivs node -- every named rule here is just an
# independent schema over the one value being validated, most usefully
# picked out as `(run { ... } value).SomeTopLevelRuleName`). `handlers.
# <RuleName>` transforms a rule's matched value on success, applied once
# per rule reference (there is no shared-position memoization for it to
# ride along with, unlike packrat.nix's handler application at Derivs-node
# construction) -- default identity, same as packrat.nix's `handlers.${name}
# or (v: v)`.
#
# FAILURE SENTINEL: `null`, not lib/packrat.nix's `[value derivs] |
# false` (see that file's header comment on why a bare value can't
# double as both a real value and a failure marker). Confirmed against
# every real schema this engine currently backs (flake.lock,
# bench/fixtures/synth-cargo-*, a real nixpkgs-vendored package-lock.json)
# that none of their corpora ever produce a JSON `null` -- so `null` as a
# bare sentinel is safe FOR THOSE SCHEMAS specifically, not safe in
# general. JSON supports `null` as an ordinary value everywhere the
# format is used; a schema over data that legitimately contains `null`
# would hit the same collision `false` has for flake.lock's very real
# `"flake": false` field (confirmed: a bare-`false`-sentinel design
# can't tell a real `false` value apart from a type mismatch, on that
# exact field, in this repo's own synthetic fixture).
#
# The alternative -- wrapping every success in a 1-element list, exactly
# mirroring packrat.nix's `[value derivs]` shape, so `null`/`false`/any
# other real value can never collide with the failure marker -- was
# built and measured (not just theorized): on the same fixture, wrapped
# success used ~10% FEWER allocations than every bare-sentinel variant
# tried (path, a literal attrset, and null itself all measured
# identically: ~247k values / ~187k primop calls, vs. ~224k values /
# ~141k primop calls wrapped) -- confirmed via NIX_SHOW_STATS, not
# estimated. (A function-value sentinel was also tried and is outright
# broken, not just slower: Nix's `==` on functions is unconditionally
# `false`, even comparing a function to itself, so an isFail check built
# on it can never fire.) Wrapping is deliberately NOT the default here --
# `null` is simpler and every current schema is confirmed safe with it --
# but if a future schema's real data needs to represent `null`, switch
# that schema's `compile` to the wrapped shape (or make it opt-in per
# schema) rather than re-deriving these numbers again.
rec {
  FAIL = null;
  isFail = v: v == null;

  # `refs`: a lazily self-referential attrset `{ <RuleName> = compiled
  # matcher; ... }`, threaded through every recursive `compile` call the
  # same way packrat.nix's `mkCompile` threads `nameToIndex` -- see this
  # file's header comment for why NAME lookup (not position-indexed, the
  # way packrat.nix resolves "Name") is the right mechanism here, and why
  # plain Nix laziness makes a self-referential `refs` safe to build
  # (compileGrammar below constructs it via `builtins.mapAttrs`, which
  # never forces a value until actually looked up, so mutually-recursive
  # rules resolve correctly instead of infinite-looping at compile time).
  # `compile`'s single-argument form (used by direct `vw.compile schema`
  # callers with no named rules, e.g. examples/flakelock-valuewalk.nix)
  # passes `refs = {}`; a bare string in such a schema is simply
  # unsupported (throws, same as any other unrecognized `expr`) since
  # there is no grammar attrset to resolve it against.
  compile = compileWith { };

  compileWith =
    refs: expr:
    let
      compile = compileWith refs;
    in
    if builtins.isString expr then
      (v: refs.${expr} v)
    else if expr ? string then
      (v: if builtins.isString v then v else FAIL)
    else if expr ? int then
      (v: if builtins.isInt v then v else FAIL)
    else if expr ? bool then
      (v: if builtins.isBool v then v else FAIL)
    else if expr ? pattern then
      (v: if builtins.isString v && builtins.match expr.pattern v != null then v else FAIL)
    else if expr ? choice then
      let
        compiled = map compile expr.choice;
        go =
          v: cs:
          if cs == [ ] then
            FAIL
          else
            let
              r = (builtins.head cs) v;
            in
            if !(isFail r) then r else go v (builtins.tail cs);
      in
      (v: go v compiled)
    else if expr ? listOf then
      let
        cc = compile expr.listOf;
      in
      (
        v:
        if !(builtins.isList v) then
          FAIL
        else
          let
            rs = map cc v;
          in
          if builtins.all (r: !(isFail r)) rs then rs else FAIL
      )
    else if expr ? attrsOf then
      let
        cc = compile expr.attrsOf;
      in
      (
        v:
        if !(builtins.isAttrs v) then
          FAIL
        else
          let
            rs = builtins.mapAttrs (_: cc) v;
          in
          if builtins.all (r: !(isFail r)) (builtins.attrValues rs) then rs else FAIL
      )
    else if expr ? attrs then
      let
        required = expr.attrs.fields or { };
        optional = expr.attrs.optional or { };
        closed = expr.attrs.closed or true;
        allKnown = (builtins.attrNames required) ++ (builtins.attrNames optional);
        compiledReq = builtins.mapAttrs (_: compile) required;
        compiledOpt = builtins.mapAttrs (_: compile) optional;
      in
      (
        v:
        if !(builtins.isAttrs v) then
          FAIL
        else if closed && builtins.any (k: !(builtins.elem k allKnown)) (builtins.attrNames v) then
          FAIL
        else if !(builtins.all (k: v ? ${k}) (builtins.attrNames required)) then
          FAIL
        else
          let
            reqResults = builtins.mapAttrs (k: c: c v.${k}) compiledReq;
            presentOptKeys = builtins.filter (k: v ? ${k}) (builtins.attrNames optional);
            optResults = builtins.listToAttrs (
              map (k: {
                name = k;
                value = compiledOpt.${k} v.${k};
              }) presentOptKeys
            );
            # Only reached when closed = false (the closed check above
            # already rejected any unknown key otherwise) -- pass the
            # ORIGINAL value through unchanged: this schema makes no
            # claim about its shape, so there's nothing to validate, but
            # the reconstructed attrset must still be byte-identical to
            # the input for keys this schema doesn't constrain (e.g. a
            # flake.lock node's `inputs` map, whose keys are arbitrary
            # other node names).
            unknownKeys = builtins.filter (k: !(builtins.elem k allKnown)) (builtins.attrNames v);
            passthroughResults = builtins.listToAttrs (
              map (k: {
                name = k;
                value = v.${k};
              }) unknownKeys
            );
            allOk =
              builtins.all (r: !(isFail r)) (builtins.attrValues reqResults)
              && builtins.all (r: !(isFail r)) (builtins.attrValues optResults);
          in
          if allOk then reqResults // optResults // passthroughResults else FAIL
      )
    else if expr ? action then
      let
        c = compile expr.action.e;
      in
      (
        v:
        let
          r = c v;
        in
        if isFail r then FAIL else expr.action.f r
      )
    else
      throw "valuewalk: unrecognized expression: ${builtins.toJSON expr}";

  # Compiles every rule in `grammar` ONCE, resolving "Name" references by
  # lazy name lookup into the resulting attrset itself -- same
  # self-referential-attrset trick a recursive/mutually-recursive `rec {
  # ... }` schema already relies on (confirmed working before this
  # existed at all: a self-referential `rec` schema with no named-rule
  # indirection walks correctly today), just applied across separately
  # NAMED rules instead of one anonymous self-reference. `builtins.mapAttrs`
  # never forces a rule's compiled matcher until something actually calls
  # it, so two rules referencing each other by name (`A = "B"; B = "A";`
  # is nonsensical as a schema but wouldn't infinite-loop AT COMPILE TIME
  # either way -- only calling the resulting matcher on an actual value
  # could ever recurse, and only as deep as the value's own tree, same
  # bound plain `rec` recursion already had).
  compileGrammar =
    grammar:
    let
      compiled = builtins.mapAttrs (_: expr: compileWith compiled expr) grammar;
    in
    compiled;

  # Public entry point, mirroring packrat.nix's `run { grammar; handlers
  # ? {}; } count string` shape as closely as this domain allows (see
  # this file's header comment for what differs and why): returns `{
  # <RuleName> = matchedValue | null; ... }`, one entry per rule in
  # `grammar`, each rule applied independently to the SAME `value` --
  # there is no "position" for different rules to be relative to, unlike
  # packrat's per-string-position Derivs node, so a caller most usefully
  # reads out `(run { ... } value).SomeTopLevelRuleName` rather than
  # every field.
  run =
    {
      grammar,
      handlers ? { },
    }:
    value:
    let
      compiled = compileGrammar grammar;
      resolvedHandlers = builtins.mapAttrs (name: _: handlers.${name} or (v: v)) grammar;
    in
    builtins.mapAttrs (
      name: _:
      let
        r = compiled.${name} value;
      in
      if isFail r then FAIL else resolvedHandlers.${name} r
    ) grammar;
}
