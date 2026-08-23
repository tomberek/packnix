# Recognition-path optimization investigation (2026-08-22)

Six parallel agents investigated candidate optimizations within the packrat
engine's *recognition* path (matching/sequencing/choice/star/cut dispatch and
the `Derivs` array itself) — the ~94% of cost that a prior measurement showed
is NOT materialization/value-storage. That prior measurement: swapping every
grammar rule's handler to `v: null` (discard all parsed values, keep full
recognition logic) drops `lock-large.json` RSS from ~224MB to ~206MB, i.e.
materialization is only ~6% of total cost; recognition is the other ~94%.

Baseline at time of investigation: `lock-large.json` (391947 bytes) at
~213MB RSS / ~0.55-0.74s wall (5-8 run means vary slightly by agent/run).

4 of 6 investigations completed; 2 failed on transient API errors (server
error mid-response on one; structured-output retry cap exceeded on another)
— not a reflection of the ideas themselves, just infrastructure hiccups.
Not yet re-run: `mkNode`'s per-node field-dispatch mechanism (string-compare
vs. attrset-lookup dispatch), and one other area from the original six.

All findings below were verified by each agent via: (1) an isolated
microbenchmark first, (2) a real-engine ablation in a `/tmp` scratch copy of
`lib/packrat.nix`/`grammar/json.nix` (never touching the repo), confirming
byte-identical output vs. the real engine on `lock-large.json` and
`tests.nix`'s `allPassed: true` against the scratch copy, before trusting any
number.

## Ranked by potential

### 1. Grammar rule inlining (NULL/BOOL/NUMBER/LIST/SET/WHITESPACE/STRING → X) — HIGH, ~18% RSS/time

Continues the exact "one fewer field per Derivs node" lever already applied
this session (STRING_RAW, LIST_ITEMS/ITEMS, ITEM), pushed further: fold the
remaining non-recursive rules into `X`'s choice branches, leaving only `X`
as a named rule (it can't be inlined — it's genuinely recursive via
LIST/SET referencing "X").

Progressive real-engine measurements:
- baseline (8 rules): RSS mean ~213.1MB, wall ~0.66s
- inline NULL/BOOL/NUMBER/LIST/SET into X (3 rules left): ~191.1MB (-10.3%)
- + inline WHITESPACE (2 rules left): ~179.7MB (-15.7%)
- + inline STRING (1 rule left, just X): ~173.8MB (-18.4%), wall ~0.54s (-18%)

**Requires a new engine primitive**: an `action` combinator in `compile`
(~12 lines) so a value-transform (handler) can travel with an inlined
sub-expression instead of needing a named rule to hang off of — NULL/BOOL/
NUMBER/LIST/SET carry real handlers (`v: v == "true"`, etc.), unlike the
purely-structural ITEM/LIST_ITEMS inlinings already done.

**Real correctness caveat, not just complexity**: inlining sacrifices
packrat's per-position memoization sharing for whatever gets inlined — a
named rule's field is computed once per Derivs node and reused by every
caller reaching that position; an inlined expression recompiles
independently per call site. If two call sites of the same inlined
expression ever evaluated at the *identical* input position within one
parse, work would silently duplicate instead of memoize (still correct,
but risks packrat's O(n) time bound, not just a style concern). Verified
safe for *this* grammar today (disjoint input-position paths, checked via
byte-identical output + hand-tested whitespace edge cases), but a future
grammar change introducing more sharing/ambiguity across inlined
expressions must re-verify this invariant.

Recommendation: implement, but incrementally (simplest atoms first — NUMBER/
BOOL/NULL, then LIST/SET, then WHITESPACE; leave STRING inlined last or not
at all, since it's the one most worth keeping visually separate for
readability even though it measures fine inlined), one rule at a time with
verification at each step, matching this session's established style.
Get explicit sign-off on the `action` combinator + memoization trade-off
before starting, given it changes engine semantics, not just grammar data.

### 2. `compileSeq`'s O(k²) `foldl'`+`++` → hand-unrolled seq3/seq4/seq5 — MEDIUM, ~6% RSS / ~15% time

`compileSeq` builds a sequence's result via `foldl'` where each step does
`elemAt acc 0 ++ [(elemAt r 0)]` — an O(current length) copy per step, so
O(k²) total for a k-element sequence. This grammar's real sequence lengths
(STRING/X=3, LIST/SET=4, ITEM-via-commaSeparated=5) are small, but the
quadratic-vs-linear gap is already clearly visible at these sizes, not
swamped by `++`'s constant factor:

| k | old (foldl'+++) | hand-unrolled |
|---|---|---|
| 3 | 299MB / 0.90s | 205MB / 0.58s |
| 4 | 367MB / 1.35s | 238MB / 0.75s |
| 5 | 434MB / 1.36s | 273MB / 0.92s |

(A generic non-hardcoded O(k) alternative via self-referential `genList` was
tried and was *worse* than the original — no free generic fix exists;
specialization is the only path that wins.)

Real-engine ablation (scratch `lib/packrat.nix` with hardcoded `seq3`/`seq4`/
`seq5`, falling back to the original `foldl'` for any other length):
baseline 212588-213512 KB / 0.58-0.61s → scratch 200148-200660 KB / 0.49-0.51s
— consistent ~6% RSS drop, ~15% wall-time drop. `nrFunctionCalls`
1,803,591→1,492,265, `nrPrimOpCalls` 1,903,274→1,624,746, `list.bytes`
15,011,328→9,823,840 (`list.concats` 157,578→22,000).

Risk: low. Pure allocation optimization, no semantics change, byte-identical
output + tests.nix pass confirmed. Caveats: (1) each arity is hand-transcribed
— a copy-paste index slip (`elemAt r 0`/`r 1`/…) is the realistic bug, keep
the byte-identical + tests.nix checks as the acceptance gate; (2) if the
grammar's sequence lengths drift (this session's git log shows that
happening repeatedly via inlining), add the new arity or accept the slower
generic fallback — no correctness risk either way, just a missed speedup;
(3) `cutSeq`'s arity is fixed at 2 and doesn't need this.

**Recommendation: implement now.** Best risk-adjusted win of the four —
non-negligible impact, no architecture change, safe fallback path.

### 3. `compileStarPlain`'s attrset → list — LOW, ~0.4% RSS (StarPlain only)

`cheapChunk`'s `{hitLimit;values;d;}` per-iteration attrset → an equivalent
3-element list `[values d hitLimit]`. Pure representation change, same
pattern already applied to the engine's `[value derivs]` result shape.
Real-engine: RSS mean 212,329KB vs. baseline 213,169KB over 8 runs (~0.39%).
Byte-identical output, tests pass. cpuTime statistically tied (no wall-time
win).

**Do NOT mirror this onto `compileStarCut`** — tested and found to be a
**regression** (+1.0% RSS): `compileStarCut` deliberately avoids bounded
`acc ++ [x]` accumulation because 2 of this fixture's 2029 SET occurrences
have 654/663 comma-items, and reintroducing chunked accumulation there
reintroduces the exact O(n²) list-append cost `genericClosure` was
introduced to avoid. A narrower "probe once, escalate only if already
matched" fast path recovers a smaller, real ~0.24%, but at the cost of one
extra function call on every one of the ~98.8% of calls that aren't actually
zero-iteration — likely not worth the added code-path complexity given the
low call volume (~2029 vs. StringPlain's ~15,919) caps the aggregate benefit
regardless.

**Recommendation**: implement the StarPlain list-threading change
opportunistically (cheap, safe, matches an established pattern). Leave
StarCut alone — the complexity/regression-risk isn't justified by a sub-1%,
time-neutral gain.

### 4. `evalLit`/`evalRegex` first-char pre-checks — NONE

Tested and rejected empirically, despite looking promising in an isolated
microbenchmark. This grammar's literals are almost all 1 character already
(10 of 13 literal sites: `,` `:` `"` `[` `]` `{` `}` — the 1-char slice *is*
the full comparison, no "check first char, skip the rest" case exists to
exploit). The only >1-char literals (`true`/`false`/`null`) fail rarely
(~689 occurrences out of 391,947 bytes / 9,980 X-invocations, since the
cut-enabled choice is already ordered by real frequency and first-token-
disjoint). For `evalRegex`, all three regexes (WHITESPACE, stringFragment,
NUMBER) succeed far more often than they fail in this fixture (e.g.
stringFragment succeeds ~14 times per string body for every 1 terminating
failure), so a speculative failure-fast pre-check taxes the dominant success
path to save on a rare failure path — measured net negative
(`evalLit` +0.013% values.bytes / statistically-tied wall time;
`evalRegex` +1.58% values.bytes) on the real engine.

**Recommendation: do not implement either variant.** If revisiting this
area, the remaining ~94% recognition cost almost certainly lives in
combinator dispatch/sequencing/choice machinery (Env/thunk allocation per
compileSeq/compileChoice/compileStar step, cutSeq bookkeeping), not leaf-level
substring/match primop calls.

## Not yet investigated (agent failures, not rejected ideas)

The two areas below failed on transient infrastructure errors (server error
mid-response; structured-output retry-cap exhaustion) in the first
investigation round and were re-run in a second round. Both came back
low-value.

### 5. `mkNode`'s per-node field-dispatch (string-compare vs. attrset-lookup) — LOW, ~0.8% RSS

Re-derived fresh on the engine as it stood *before* the compileSeq/
compileStarPlain/grammar-inlining changes below (baseline: ~200MB). Pure
string-comparison cost (`name == "count"`) is not measurable in isolation —
negligible next to everything else. A pre-baked attrset-lookup dispatch
(closures built once per `buildDerivs` call, `mkNode` becomes a uniform
`fn count` call with no branch) gives a small, real, byte-identical
~0.8% RSS win (`nrPrimOpCalls` drops as the `==` calls disappear). Also
re-confirmed the two-pass `mapAttrs`+`//` idea (rejected earlier this
session at a larger field count) is *still* rejected at today's smaller
field count — the `//` merge's per-node Bindings-array copy cost
(`nrOpUpdates` 3→93,667) outweighs any dispatch savings by a wide margin
regardless of field count.

**Recommendation**: not worth implementing on its own — smaller than every
other finding in this document, and would need re-measuring again after the
grammar-inlining change below (which further shrinks the field count this
mechanism operates over, likely shrinking the win further). Revisit only if
looking for the last few tenths of a percent.

### 6. `compileChoice`'s head/tail traversal + cut vs. no-cut re-check — LOW, ~0.8-1% RSS

Real `X`-invocation count on `lock-large.json`: 9,980 (5,966 str, 2,008
SET, 1,296 num, 689 bool, 21 list, 0 null) — confirms `xBranches`'
frequency ordering is correct. Isolated microbenchmark comparing
head/tail traversal, index-based (`elemAt bs i`), and a fully-unrolled
seq3/seq4/seq5-style 6-way dispatch: unrolled wins (~1.36M KB vs ~1.63-
1.75M KB RSS at 4M calls), because it avoids allocating/holding
intermediate `tail bs` sublists — head/tail and `elemAt`-index cost
statistically the same as each other, contrary to the initial hypothesis
that head/tail's extra allocation would show up distinctly.

Real-engine ablation of an unrolled `choice6` (+ `choice3` for BOOL/
stringFragment's shape): ~0.8% RSS, wall time statistically tied.
**Recommendation: not worth it** — `X` is this grammar's only high-volume
choice site (9,980 invocations, an order of magnitude below `compileSeq`'s
sequence-site volume that made *that* specialization worth ~6%), so the
return on an implementation+verification cycle here is much smaller than
`compileSeq`'s was.

Also re-confirmed cut vs. `grammarNoCut` on the engine state at time of
this investigation (prior checks, done earlier in the session on older
engine states, had found both neutral): **this flipped** — cut now
measurably costs ~0.8% *more* RSS than no-cut, not neutral, though wall
time is still statistically tied/noisy. Small effect, but worth knowing
if the cut/no-cut question comes up again — re-measure fresh rather than
trusting either the old "neutral" or this "cut costs slightly more"
finding, since the engine keeps changing.

## Implemented so far (this document is now a mix of history and open items)

- **#2 (`compileSeq` specialization)**: implemented and committed
  (`70858c1`, then flattened for readability in `14d2ce5`). ~5.7% RSS,
  ~15-18% wall time, confirmed on the real engine.
- **#3 (`compileStarPlain`'s attrset → list)**: implemented and committed
  (`e508dba`). ~0.4% RSS (small enough to be within whole-process RSS
  sampling noise, confirmed instead via `NIX_SHOW_STATS`'s deterministic
  `sets.number`). Explicitly does NOT extend to `compileStarCut` — that
  mirror was tested and found to be a regression.
- **#1 (grammar rule inlining), partial**: implemented for the
  single-reference rules only (`NUMBER`/`BOOL`/`NULL`/`LIST`/`SET`,
  each referenced from exactly one place — `xBranches` — making it
  structurally impossible for two call sites to collide at the same
  input position). Added an `action` combinator to `lib/packrat.nix`
  (`{ action = { e; f; }; }`) so each rule's value-transform travels
  with its inlined expression. `WHITESPACE`/`STRING`/`X` remain named
  rules (multi-reference; would need an actual position-disjointness
  argument, not just a reference count, to inline safely) — deliberately
  NOT attempted in this pass. Measured ~12.1% RSS reduction (larger than
  the original ~10.3% ablation estimate, likely because the engine
  changed further via #2/#3 since that estimate was made).

## Derivs node representation: list instead of attrset (implemented)

Prompted by "can we make a lazy attrset" — real Nix attrsets can't defer
spine construction (the Bindings/Attr-slot array is always built eagerly,
only field *values* stay lazy), so no true "lazy attrset" exists. Two
alternatives were prototyped instead, entirely in `/tmp` scratch copies,
each verified byte-identical + `tests.nix` 17/17 before trusting any number:

**Rejected: struct-of-arrays.** Replace the one shared `at` array (of
per-position nodes) with one full-length array *per rule name*, indexed by
position directly; a nonterminal reference becomes `elemAt ruleArrays.NAME
pos` instead of `derivs.NAME`, and lit/range/regex jumps become pure
arithmetic (`pos + n`) with no lookup. On this grammar's already-inlined
3-rule shape: ~1.7% RSS win (small but real, confirmed n=17, high z-score),
wall time inconclusive (noisy, borderline-to-not-significant). But tested
against `examples/json-simple.nix` (13 rules, i.e. a grammar written the
normal way, not pre-inlined) it reverses badly: **+43.6% RSS, +22.5% wall**
(n=17, byte-identical output confirmed both times). Root cause: this design
pays for N full-length `genList` spines instead of 1 shared spine, so its
fixed cost scales with rule count × string length — wins only on a grammar
already inlined down to a handful of rules, loses on anything written the
way a person would normally write a grammar. **Not implemented.**

**Implemented: list-node.** Keep the shared-spine architecture (one `at`
array, each element built lazily on first access, same as today) but make
each node a LIST — `[count field_1 ... field_N]` — instead of an attrset,
with nonterminal references baked to a fixed slot index (`nameToIndex`) at
COMPILE time rather than looked up by name per call. A list element costs
~24 bytes vs. an attrset Attr slot's ~56 bytes for the same "eager spine,
lazy values" behavior. Unlike struct-of-arrays this *improves* as rule
count grows (single shared spine, not N of them): −2.1%/−4.2% RSS
(unoptimized first attempt) → **−6.2%/−11.1% RSS** after fixing a
correctness-preserving perf bug (see below), on the 3-rule and 13-rule
grammars respectively (n=17, z-scores −111/−236, clearly real), with wall
time now *also* slightly improved (−1.4%/−4.3%, though still within noise
— not a regression either way).

Perf bug caught before shipping: the first list-node prototype's `mkNode`
used an index-driven `genList (i: ... (elemAt at count) ...)`, re-deriving
`elemAt at count` (the position's own node — always the same value) once
per rule slot inside that lambda, plus paying `elemAt compiledFieldsInOrder
(i-1)` index arithmetic that the old attrset `mapAttrs` never needed
(`nrPrimOpCalls` +24-30%, no offsetting savings). Fix: hoist `derivsNode =
elemAt at count` into a single `let`, and build the field list via `[count]
++ map (field: field derivsNode) compiledFieldsInOrder` (`map` walks the
list, no per-element index math) — this alone cut wall time ~18-20% vs. the
unfixed version and improved RSS further too.

Ported into `lib/packrat.nix`: `mkCompile` gains a `nameToIndex` parameter
(nonterminal refs resolve to `elemAt derivs idx` instead of `derivs.${expr}`
— baked once per reference site, not looked up per call); `evalLit`/
`evalRange`/`evalRegex` read `count` via `elemAt derivs 0` instead of
`derivs.count`; `buildDerivs` computes `names`/`nameToIndex` once and
returns `{ at; nameToIndex; }`; `run` looks up a rule's slot via
`built.nameToIndex.${name}`. Every combinator that only threads `derivs`
opaquely (`compileSeq`/`seq3`-`seq5`, `compileChoice`, `compileStar*`,
`compileOpt`/`And`/`Not`, `compileAction`) is untouched. Confirmed on this
machine post-port: `lock-large.json` RSS 175.9MB → 165.0MB (5-run means),
consistent with the ~6% figure measured in the scratch prototype.

