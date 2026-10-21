# Named implementations through trait evidence: review brief

This candidate realizes Sean's proposed direction on trait-evidence base
`2c6673819a0ed2a718593a0053f50786ba72a22b`. Lexical choices prioritize
implementation candidates, recursive prerequisites inherit those choices,
and calls carry only the evidence required by declared bounds. A generic
`T::price()` can therefore observe 70 while an unbounded concrete helper
observes 100 in the same calling scope.

The core does not port the previous choice checker. The retained surface is
named-implementation syntax, formatting, name visibility, associated-output
agreement and stable identities. Selected proofs remain meaningful after
types become concrete, retain implementation arguments absent from the trait
header, and preserve local implementations' captured generic arguments.
Supertrait projections use the subtrait's proof and its prerequisite evidence.

## Recommended first-version rulings

The [language specification](../language/named-implementations.md) gives the
behavior and examples. These three rulings complete this candidate's design:

- **Ordinary escape:** `with (<Type as Trait>)` opens a uniquely resolvable
  ordinary implementation. A named wrapper can call the ordinary method and
  add one without recursive self-dispatch. Resolving the operand requires an
  ordinary candidate independently of lexical choices and bound assumptions;
  its subsequently proved conditions still use the surrounding solve context.
- **Supertrait agreement:** a subtrait proof owns its implied supertrait
  dictionary. Repeated paths to the same bound, including an explicitly
  repeated supertrait, must agree. Conflicts are rejected instead of selecting
  according to bound order. This preserves both dictionary semantics and a
  single unambiguous dictionary per callee bound.
- **Ordinary output representative:** a named implementation of a trait with
  associated outputs requires a source-declared ordinary implementation
  covering its header under its conditions, with equal outputs at all header
  overlaps. This prevents evidence from proving a bound whose outputs ordinary
  type normalization cannot obtain. Method-only traits need no ordinary
  counterpart. Compiler-generated output representatives remain outside this
  first version's comparison support.

Choice-dependent type formation remains rejected. Public `Evidence`, `prove`
and `Constraint`, and normalization through evidence, are deferred. No new
expression interpreter or check-time execution mechanism is introduced.

## Verification and review scope

The original verification below covers
`b67674801d39f4a7cd52c7e728357ed7a281cc0b`. The post-review identity repair is
`040bb0cd4116c1acf6eca065ab898d5cda6a2fea`; its additional checks are recorded
separately below.

- 191 HIR trait tests and the focused 27-test named-evidence suite passed,
  including failed/cyclic chosen prerequisites, inference ordering, captured
  arguments, closed proofs, supertrait conflicts and associated-output rules.
- Three MIR identity tests passed. The runtime fixture passed native and
  Sonatina/EVM execution at both O0 and O1. Parser, formatter and MIR package
  tests also passed earlier in the candidate's development.
- Strict workspace Clippy passed with all targets and all features. Formatting
  and release-fragment validation passed. This is not a full workspace test
  suite or CI claim.
- Seven alternating process pairs on the ordinary `growing_recursion` stress
  case measured median CPU time of 38.84 seconds at the base and 37.73 seconds
  at the candidate, with identical diagnostics. This supports no observed
  regression on that unoptimized case, not a general speedup claim.

The original numbered review probes were unavailable; the committed tests
are explicit semantic equivalents, not claimed literal reruns. Review can
start with `crates/hir/tests/traits/named_evidence.rs` and the executable
`crates/fe/tests/fixtures/fe_test/named_implementations.fe`.

Independent review found that stable evidence identities omitted selected
implementation arguments absent from the trait header. The repair includes
every instantiated implementation argument through the existing stable type
and constant renderer. A paired-call regression fails before the repair and
passes afterward for explicit type arguments, constant arguments and captured
generic arguments. All four stable-key tests pass, preserving ordinary sharing
and allocation-order independence. The expanded 7/9 runtime fixture passes
native execution at O0/O1 both before and after the repair, so this establishes
an identity collision, not a demonstrated runtime miscompile. Strict workspace
Clippy and formatting pass after the repair.

Sean's subsequent review exposed two additional blockers: unresolved inference
variables could enter fresh-table choice matching, and trait-keyed effect
providers discarded their chosen bound proofs. The repairs retain ordinary
inference diagnostics and transport resolved provider proofs through the same
bound-evidence slots used by generic calls. All three provider/choice nesting
forms, forwarding and failed selected prerequisites have discriminating tests.

The transitive projection repair extends an implementation's actual prerequisite
dictionaries with their implied dictionaries. Projection follows the existing
proof DAG, checks agreement along repeated paths, and supplies the resulting
slots to implementation methods and constants. Selected private prerequisites
are checked at their producer too. Constant defaults use the same missing-proof
validation as calls. This corrects both the coherent diamond rejection and the
conflicting constant acceptance; it adds no second resolver or evidence kind.

After these repairs, all 197 HIR trait tests pass, including 33 focused
named-evidence tests. All four stable-identity tests pass. The expanded runtime
fixture passes native and Sonatina/EVM execution at O0 and O1; strict workspace
Clippy with all targets and features, formatting and diff checks pass. These
are scoped checks on the repair, before integration onto PR #1848. The earlier
CPU comparison has not been repeated on this repair.

The bounded probes also retain two separate limitations for review. A generic
associated constant whose initializer calls a bound const method can trigger
3-0051 in both the earlier candidate and the original trait-evidence baseline,
with every named declaration removed; that existing CTFE issue is outside these
repairs. Generic named-only method lookup
can require explicit `<Type as Trait>::method()` qualification where abbreviated
`Type::method()` fails to discover the implementation. Normalization through
evidence remains deferred. These checks do not establish arbitrary compiler-wide
proof preservation or a full workspace CI result.

The recommendation is to replace the previous named-implementation stack
with this evidence-based candidate after independent review and integration
onto Sean's standalone evidence PR #1848. The three rulings above are concrete
recommendations. Prior branches and repair checkpoints remain preserved.
