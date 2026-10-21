# Named implementations through trait evidence: review brief

This replacement follows Sean's proposed architecture on his standalone
trait-evidence PR #1848, at `af26cd9fb9cdb97adfc58d757dea176dff62a658`.
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
prerequisites are checked at their producer. These repairs add neither a second
resolver nor a new evidence kind.

The integration retains upstream projected-goal lookup, effect-key `Self`
handling, tuple-struct identities and diagnostics. Sean's proof representation
and instance transport match the earlier prototype base. The combined parser
WASM is freshly generated with the pinned toolchain and checked against its
input stamp. Private names now report the visibility diagnostic. Associated
output errors blame the nonconforming named declaration; agreement through the
ordinary representatives avoids duplicate blame on an agreeing declaration.

## Verification

On the integrated Linux source checkpoint `682039dea`, all 4,100 workspace tests
pass, including 198 HIR trait tests, 34 focused named-evidence cases and four
stable-identity tests. Native and Sonatina/EVM regressions pass at O0/O1.
One pre-existing diagnostics fuzz test remains ignored. Strict workspace Clippy
with all targets and features, formatting, diff checks and parser freshness pass.
The doctest command passes with two ignored examples and no executable examples.

The first unfiltered run preserved one native arithmetic timeout: the host's
core-dump pipe delayed exit after the expected trap. The same test binary passed
with a process-local constructor disabling core dumps, and the complete workspace
rerun under that control passed. No compiler behavior, assertion, deadline or
test filter changed. These are local workspace results, not a claim that every
platform or external CI job has run.

Seven alternating measured process pairs on the optimized `growing_recursion`
stress case give median CPU times of 1.823 seconds upstream and 1.842 seconds
for the candidate, with identical diagnostics. The paired median change is
+0.31%; its bootstrap interval spans -2.30% to +4.21%. This detects no regression
beyond run-to-run variation on that case, not a general throughput guarantee.

## Remaining boundaries

Indexed storage is an open correctness finding. Generated `StateIndex::locate`
calls reconstruct proofs instead of consuming the dictionary selected for the
indexed operation. A StorageMap probe with ordinary and shifted StorageKey
implementations observes `(6, 6)` after two writes where honoring the selected
dictionaries requires `(5, 6)`. This also loses a dictionary supplied through an
inherent method's declared bound; fixed contract field layout does not explain
it. The next bounded repair must preserve the indexing proof through the
normalized place and make access analysis and lowering consume the same result,
including its consequences for place identity. Silent fallback is not an
approved storage rule.

Two separate limitations remain documented. A generic associated constant whose
initializer calls a bound const method triggers 3-0051 even without named
implementations; this also reproduces on the exact PR #1848 base, without named implementations.
Generic named-only method lookup can require explicit
`<Type as Trait>::method()` qualification where `Type::method()` does not discover
the implementation. Normalization through evidence remains deferred.

The recommendation is to replace the previous named-implementation stack after
independent review and resolution of the indexed-place transport finding.
The original `b67674801d39f4a7cd52c7e728357ed7a281cc0b` review candidate and clean
`121a340eda824410cc496dda284ddf00db9b701c` repair remain preserved. No PR closure,
base-branch rewrite or publication is part of this integration checkpoint.
