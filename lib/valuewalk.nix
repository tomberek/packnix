# A schema-validation engine over an ALREADY-PARSED Nix value tree (from
# builtins.fromJSON/fromTOML), not over string positions like
# lib/packrat.nix. lib/packrat.nix's `{ json = {}; }`/`{ toml = {}; }`
# combinator hands a substring to the native parser for speed, but gives
# no structural validation of the result -- no "does this object have
# exactly these fields, in these types" the way a hand-written packrat
# grammar gets for free from its rule-by-rule shape. This is that
# validation layer, applied to the tree fromJSON/fromTOML already built.
#
# Measured against grammar/flakelock.nix on a 636KB, 2000-node synthetic
# flake.lock-shaped fixture: byte-identical output to the specialized
# grammar, ~8x faster and ~4x less RSS. The native parse does the
# character-by-character work in C++; this only walks the resulting tree,
# whose size is nodes-and-fields, not bytes-of-text.
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
#                              complete string value)
#   { choice = [s1 s2 ...]; } -> ordered choice: first schema that matches
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
#                               UNCHANGED (needed for byte-identical
#                               reconstruction of keys this schema doesn't
#                               constrain -- see the `attrs` case below).
#   { action = { e; f; }; } -> e, with f applied to its value on success
#   "Name"                  -> reference to another named rule in the
#                              same grammar (see `run`/`compileGrammar`
#                              below), needed for recursive or
#                              mutually-recursive schemas (e.g. flake.lock's
#                              NODE containing a `locked`/`original` field
#                              that's itself a named rule). Resolved by
#                              NAME lookup into a lazily-built,
#                              self-referential attrset of compiled rules
#                              -- NOT by position-index the way
#                              packrat.nix's `nameToIndex` resolves a
#                              nonterminal to a fixed Derivs-array slot.
#                              That indexing exists in packrat.nix to
#                              share one memoized thunk for repeated
#                              reference to "the same rule at the same
#                              input POSITION"; there is no position
#                              dimension here (each rule reference in a
#                              value-tree walk lands on a genuinely
#                              different subtree), so plain Nix name
#                              lookup already gives correct (and lazy, so
#                              mutually-recursive rules don't infinite-loop
#                              at COMPILE time) resolution for free.
#
# `run`/`compileGrammar` mirror packrat.nix's own `run`/`buildDerivs`
# public shape as closely as this domain allows: `run { grammar; handlers
# ? {}; } value` returns `{ <RuleName> = matchedValue | null; ... }` for
# EVERY rule in `grammar`, each applied to the SAME `value` (there is no
# "position" for different rules to be evaluated relative to -- every
# named rule here is just an independent schema over the one value being
# validated, most usefully picked out as
# `(run { ... } value).SomeTopLevelRuleName`). `handlers.<RuleName>`
# transforms a rule's matched value on success, default identity.
#
# FAILURE SENTINEL: `null`, not lib/packrat.nix's `[value derivs] |
# false`. Confirmed safe against every real schema this engine currently
# backs (none of their corpora ever produce a JSON `null`) -- not safe in
# general, since JSON supports `null` as an ordinary value; a schema over
# data that legitimately contains `null` would hit the same collision
# `false` has for flake.lock's very real `"flake": false` field.
#
# The alternative -- wrapping every success in a 1-element list, exactly
# mirroring packrat.nix's `[value derivs]` shape, so `null`/`false`/any
# other real value can never collide with the failure marker -- was
# measured on the same fixture: wrapped success used ~10% FEWER
# allocations than every bare-sentinel variant tried (path, attrset, and
# null itself all measured identically). (A function-value sentinel is
# outright broken, not just slower: Nix's `==` on functions is
# unconditionally `false`, even comparing a function to itself.)
# Wrapping is deliberately NOT the default here -- `null` is simpler and
# every current schema is confirmed safe with it -- but if a future
# schema's real data needs to represent `null`, switch that schema's
# `compile` to the wrapped shape (or make it opt-in per schema) rather
# than re-deriving these numbers again.
rec {
  FAIL = null;
  isFail = v: v == null;

  # `refs`: a lazily self-referential attrset `{ <RuleName> = compiled
  # matcher; ... }`, threaded through every recursive `compile` call.
  # `compileGrammar` below constructs it via `builtins.mapAttrs`, which
  # never forces a value until actually looked up, so mutually-recursive
  # rules resolve correctly instead of infinite-looping at compile time.
  # `compile`'s single-argument form (no named rules, e.g.
  # examples/flakelock-valuewalk.nix) passes `refs = {}`; a bare string in
  # such a schema is simply unsupported (throws).
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
  # lazy name lookup into the resulting attrset itself. `builtins.mapAttrs`
  # never forces a rule's compiled matcher until something actually calls
  # it, so two mutually-referencing rules don't infinite-loop AT COMPILE
  # TIME -- only calling the resulting matcher on an actual value can
  # recurse, and only as deep as the value's own tree.
  compileGrammar =
    grammar:
    let
      compiled = builtins.mapAttrs (_: expr: compileWith compiled expr) grammar;
    in
    compiled;

  # Public entry point, mirroring packrat.nix's `run` shape as closely as
  # this domain allows: returns `{ <RuleName> = matchedValue | null; ... }`,
  # one entry per rule in `grammar`, each rule applied independently to the
  # SAME `value` -- there is no "position" for different rules to be
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
