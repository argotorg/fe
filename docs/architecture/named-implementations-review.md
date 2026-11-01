# Named implementations through trait evidence: review brief

This replacement follows Sean's proposed architecture on merged master
`bfc795bb45875ec3ef9decfc0122dadd9b101c63`, including trait-evidence PR #1848
(merged head `06f37c55651396d33b23752ad66c2142d4ba1f92`).
Lexical choices prioritize implementation candidates, recursive prerequisites
inherit those choices, and calls carry only their declared bound dictionaries.
A bounded generic call can observe 70 while an unbounded concrete helper
observes 100 in the same calling scope.

The core does not port the previous choice checker. The retained surface is
named-implementation syntax, formatting, name visibility, associated-output
agreement and stable identities. Proofs retain selected implementation arguments
and local captures even when those arguments do not occur in the trait header.
Supertrait projections follow the subtrait's actual prerequisite dictionaries.

## First-version rulings

The [language specification](../language/named-implementations.md) gives examples.

- **Ordinary escape:** `with (<Type as Trait>)` opens a uniquely resolvable
  ordinary implementation, letting a named wrapper call the ordinary method.
  Resolving this operand is independent of lexical choices and bound assumptions;
  its subsequently proved conditions still use the surrounding solve context.
- **Supertrait agreement:** a subtrait proof owns its implied dictionaries.
  Repeated paths to a bound, including an explicitly repeated supertrait, must
  agree. Conflicts are rejected rather than resolved by bound order.
- **Ordinary output representative:** a named implementation with associated
  outputs requires a source-declared ordinary implementation covering its header
  under its conditions, with equal outputs at all header overlaps. Ordinary
  type normalization can then obtain the outputs of every admitted named proof.
  Method-only traits need no ordinary counterpart. Compiler-generated output
  representatives remain outside the first version's comparison support.

Choice-dependent type formation remains rejected. Normalization through
evidence, public `Evidence` values, `prove` and `Constraint` are deferred.
No expression interpreter or check-time execution mechanism is introduced.

## Repairs and integration

The review repairs prevent unresolved inference variables from entering fresh
choice-matching tables, preserve trait-keyed effect-provider proofs, and include
all instantiated implementation arguments in stable evidence identities.
Transitive supertrait projection extends actual prerequisite dictionaries and
checks agreement along repeated paths. Methods and constants consume those
slots; constant defaults share calls' missing-proof validation. Selected private
prerequisites are checked at their producer. These repairs use the existing proof engine; no second resolver is introduced.

The integration retains upstream projected-goal lookup, effect-key `Self`
handling, tuple-struct identities and diagnostics. Sean's proof representation
and instance transport match the earlier prototype base. The combined parser
WASM is freshly generated with the pinned toolchain and checked against its
input stamp. Private names now report the visibility diagnostic. Associated
output errors blame the nonconforming named declaration; agreement through the
ordinary representatives avoids duplicate blame on an agreeing declaration.

## Indexed storage and proof ownership

Indexing records its `StateIndex::locate` and `LaneCodec` callables alongside the
checked expression. Substitution and bound-evidence binding visit those calls;
access analysis and MIR consume the same finalized instance keys. They no longer
reconstruct core-method selections independently. Shifted storage now observes
`(5, 6)` after ordinary and chosen writes, and a chosen encoding preserves its
neighboring packed lane. Alias analysis separates different keys only when their
complete dispatch identities agree; different dictionaries can map different
keys to the same location, so subsequent distinct fields cannot prove separation.

A codec obligation can become decidable only after substitution establishes a
packed layout. Its instance finalizer then records the missing proof, including
prerequisites, before applying the shared call finalizer. Existing selected proofs
are retained. The compiler's provisional whole-slot `SolLanes<T>` fallback is not
recorded as a selected implementation while `T` is unknown: its negative premise
can change when `T` becomes a packed element. An internal deferred-codec leaf
retains that ordinary obligation inside any already selected parent proof.
Substitution transports it; the shared instance finalizer completes just the
leaf. Completion failure cannot become an absent bound and trigger parent
reselection. Declared bounds and actual implementation
proofs remain valid. An unavailable lexical codec proof is retained separately
from this deliberate deferral: packed-layout admission reports its failure instead
of selecting an ordinary codec. A whole-slot access does not require unused
codec methods. This distinction is tested for generic, supplied-bound,
concrete packed and concrete unpacked cases, plus a named parent whose deferred
prerequisite closes after substitution and unrelated choices around opaque
generic storage codecs.

This is the follow-up consolidation rule: name the decision's producer and its
result. Failed and deliberately deferred obligations are results too; discarding
that distinction must not authorize a different selection. Consumers transport,
substitute, project or validate the recorded result. A
newly closed obligation needs an explicit owner; it does not justify replacing
an existing proof. Method-declaration conformance and call-argument validation
remain separate checks because they establish different facts.

## Verification

At source commit `4c4fc0d49028101d03ea111de5e4bb6078dee07f`, the local
optimized workspace suite passes **4,130 tests**, with one pre-existing skipped
fuzz test. The 170-test focused run includes named storage at O0/O1, failed and
deferred codec controls, Solidity arrays, all Sonatina IR snapshots and
Rust/tree-sitter agreement. Workspace documentation tests, strict Clippy across
all targets/features, formatting and diff checks pass. Pinned tree-sitter 0.24.7
ABI 14 regeneration and the browser parser's input stamp are unchanged.
Public named-implementation visibility agrees in both parsers; selected type
reads use diagnostic 8-0138, distinct from invalid `mut?` usage.

The first merged-master full run preserved four ordinary Solidity-array failures
from the provisional-codec mistake and four changed IR snapshots. The repair is
covered by the same failing runtime tests plus the direct proof regression.
The snapshot changes reduce duplicate helper bodies; normalized instruction
bodies and call-graph structure agree, modulo generated symbols and duplication.
A subsequent adversarial codec case exposed failed lexical proof being
mistaken for ordinary deferral. Its first repair also rejected two valid generic
cases; those failed histories remain preserved. The final repair passes these
controls and the complete workspace rerun.

Local native tests disable process core dumps because this host's core-dump pipe
can delay expected traps past test deadlines. No compiler behavior, assertion,
deadline or test filter is changed. These are local workspace results, not a
claim that every platform or external CI job has run.

The optimized CLI is compared with the exact merged-master binary using
alternating process-CPU pairs: one warmup and seven measured pairs per fixture.
Both binaries produce identical diagnostics on both fixtures.

| Fixture | Paired median change | Bootstrap 95% interval |
| --- | ---: | ---: |
| `growing_recursion` | +3.14% | [+1.34%, +5.45%] |
| `library_state_index` | +1.57% | [-7.10%, +18.62%] |

The recursion fixture shows a small measured increase; its interval does not
include zero. The storage fixture is inconclusive at this sample size. This is
a bounded two-fixture comparison on a shared host, not a claim of unchanged
compiler throughput or a broad performance guarantee. The earlier candidate's
samples remain preserved and are not substituted for these final results.

## Remaining boundaries

A generic associated constant whose initializer calls a bound const method can
trigger 3-0051 even without named implementations; this also reproduces on the
earlier exact PR #1848 base. Generic named-only method lookup can require explicit
`<Type as Trait>::method()` qualification where `Type::method()` does not discover
the implementation. Normalization through evidence remains deferred.

The recommendation is to replace the previous named-implementation stack after
independent review of this verified candidate. The original `b6767480` review
candidate, `121a340e` repair and `f364bc7a` earlier PR integration remain preserved.
Higher-level quotation, reflection and derive ports are separate work; this core
does not claim their parity or reintroduce their former choice checker.
