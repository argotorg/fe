# CTFE deferral implementation audit

The architecture specification distinguishes the initial correctness cutover
(phases 0–7) from immutable representation consolidation (phase 8). This audit
covers both. Phases 0–8 are implemented, including the corrective findings
below. The full release and CI configuration gates pass. Source-level borrowing
admission remains separately scoped as described below.

## Architecture and ownership

| Area | Implemented boundary |
| --- | --- |
| Outcomes | `EvalOutcome::{Ready, Blocked, Failed}` separates missing facts from semantic failures. `FoldAttempt` separately describes optional optimization misses. |
| Replay | `ConstComputationId` records the original entry, owned input descriptions, result type, parameter owner and origin before execution. A blocked attempt discards its private machine state. |
| Values | `SemConstId` is the authoritative immutable scalar/aggregate/enum payload. `VerifiedConstValueId` validates its recursive type and payload shape. Machine storage and live references remain private. |
| Type integration | `ConstTyData` references verified values, partial immutable descriptions or a `ConstDesc` with its source instance. Inference variables, layout holes and formal runtime evidence remain separate. |
| Source identity | Canonical terms are separate from occurrence provenance. Source descriptions retain ordered operand origins and call frames until required forcing. Identity/display views may project the canonical term. |
| Specialization | Declaration templates are instantiated once at `instantiate_const_template`. Reification of already instantiated values/types does not apply positional substitution again. Scoped request specialization traverses inputs, result types, provider/impl environments and provenance. |
| Execution | Whole-body execution uses admitted raw semantic bodies. Retained arithmetic, unary, cast, invocation, selected constant, repeat, index and field terms delegate to the ordered CTFE service. |
| Normalization | Extraction accepts only a bounded, obligation-preserving whole expression. It rejects control flow, mutation, discarded operations and duplicate operand consumption; declaration recurrence stops inlining even when generic arguments change. |
| Budgets | Term operands and ordinary invocation inputs share one attempt budget. Cached body outcomes retain their work cost. Term depth is bounded; machine and term repeats charge one step per allocated element before construction. The machine retains its existing separate const-reference query policy. |
| Runtime | Preparation forces descriptions and imports verified values. Only explicitly classified formal layout evidence uses the evidence path. No scalar interpreter or unreified-success fallback remains in MIR lowering. |

Bodyless user extern calls retain their established symbolic **type identity**.
This does not establish a value or mark an unsupported operation as dependent:
forcing the invocation reports an unsupported-operation failure after evaluating
its arguments. Extern identity regressions verify distinct calls remain distinct.

## Corrective review findings

| Reproduced finding | Correction and regression |
| --- | --- |
| Caller-owned expression arguments were substituted twice, producing 1 instead of 7 | Explicit template boundary; `expression_arguments_preserve_caller_parameter_ownership` covers swapped, repeated, nested and inherited impl/trait parameters. |
| Expanding generic recursion overflowed the host stack during extraction | Declaration recurrence, extraction depth/work limits and operand consumption; subprocess regression also covers a finite 80-call chain. Recursive calls and const references dispatch before entering the large value-operation frame, keeping finite debug execution within the configured limit. |
| Generic alias specialization lost division/overflow failures and operation spans | Preserve `ConstDesc` and source instance at the type boundary; test arithmetic/unary faults, both fault orders, nested calls and two alias layers. Layout instantiation uses the same declaration-scoped unevaluated-body environment backfill as Binder. |
| Valid retained casts could not be forced by the common service | Common cast/repeat/index/field paths; specialization, exact value/type and reification round trips, including zero extents and unselected faulting fields. |
| Closed arithmetic succeeded with a zero-step budget | Shared term/body accounting, cached work cost and recursion checks; zero/low/high budgets, nested terms, partial aggregates, repeated invocation inputs and repeat allocation are covered. |
| Machine scalar/extent demands treated closed descriptions as dependencies | Force through the common service in the current environment; bodyless externs fail, genuine parameters block, and unrepresentable/over-budget extents fail before allocation. |
| Empty repeats could hide a malformed element type | Check the element's declared array type before allocation, even at length zero; verify complete aggregate operands before projection. |

The first source-adapter full run also found missing handling of the new adapter
at selected-constant and identity boundaries. Targeted tests now preserve generic
trait-projection repeat admission, omitted const defaults in effect keys,
bodyless extern identities and recursive generic associated-constant recovery.
Resolved constants lower to explicit references before execution, allowing the
existing const-reference cycle detector to run after semantic body construction.
Bare parameter and projection forwarding retain their established type identity;
arithmetic selected bodies retain the original selection until forcing.
Existing diagnostic snapshots are preserved; no changed snapshot has been
accepted merely to make the tests pass.

## Phase and acceptance coverage

| Spec phases | Implementation and evidence |
| --- | --- |
| 0 | Source/consumer inventory, reproduced review probes, regression matrix and this audit. |
| 1–3 | Immutable requests, distinct outcomes and optional folds, shared primitives, strict concrete machine, isolated root replay and verified publication. |
| 4 | Scope-owned descriptions, once-only template instantiation, bounded whole-expression extraction and ordered provenance. |
| 5 | Required forcing retains diagnostics; optional misses retain operations; runtime boundaries accept verified values or explicit formal evidence. |
| 6 | Differential and fixed-expectation tests, staged environments, selected context, cycle recovery and cached resource accounting. |
| 7 | Independent symbolic evaluators and fallback success paths removed. Final repository gates and measurements are listed below. |
| 8 | `SemConstId` owns immutable payloads; `ConstDesc`/`ConstComputationId` own description/replay metadata; type-system adapters reference them. Checked conversion/reification and explicit evidence replace legacy value transport. |

Section 20's behavioral criteria map to the architecture and regression tables.
The consolidated schema uses the spec's permitted authoritative constructors and
views: no second immutable scalar/aggregate payload enum remains in the type
system. Mutable machine values are separate. Remaining source/type inference
adapters are listed explicitly below; none provides an alternative execution
protocol. Final verification below passes for both gates within the documented
borrowing scope; environment differences are reported explicitly.

## Regression traceability

The T labels refer to the specification's regression matrix. The principal
integration tests live in `crates/hir/tests/semantic_ctfe_deferral.rs`.

| Requirements | Principal coverage |
| --- | --- |
| T01–T06 | `generic_{branch,match,cast}_declaration_replays_after_specialization`, `differential_generic_branch_match_cast_and_integer_term`, alias diagnostic tests |
| T07–T19 | `differential_strictness_projection_branch_and_loop_matrix`, `discarded_index_keeps_its_bounds_obligation`, `pure_term_replay_keeps_source_assignment_order`, retained aggregate tests |
| T20–T21, T56–T57 | `differential_unused_generics_arguments_and_symbolic_equality`, `whole_expression_terms_preserve_generic_identity` |
| T22–T26 | Checked/wrapping/saturating, signed-cast and division/remainder/exponent/shift differential groups, with fixed expected values/faults |
| T27–T28 | User-named `add` body/result/assertion regressions and user extern intrinsic-identity rejection |
| T29–T33 | `blocked_mutating_requests_restart_from_original_state`; machine internal reference/mutation tests; `semantic_ctfe_array_repeat.rs`; executable `const_symbolic_array_repeat` fixture |
| T34–T38, T58 | Caller ownership, staged specialization, selected trait constants, invocation context and repeated invocation identity tests; effect-key and extern identity fixtures |
| T39–T41 | `differential_nested_record_enum_array_tuple_and_fixed_string`, reification tests in all three CTFE integration files, retained-term round trips |
| T42–T46 | Formal-evidence, optional-fold and mutation-invalidation tests in `semantic_ctfe.rs`; layout-evidence and runtime fixtures in the complete suite |
| T47–T49 | Unused-generics/unsupported-argument differential tests, extern identity rejection, const-body admission tests, provider-borrow rejection control |
| T50–T54 | Stable const cycles, expanding function recursion, resource/cache environment tests, term depth/step tests and shared invocation budgets and machine scalar/extent demand classification |
| T55 | Request/value shape unit tests, scalar/array reification rejection and retained aggregate operand validation |

The differential harness compares direct machine execution against description,
specialization and forcing. It compares exact nested types and payloads, and
semantic failure categories/origins. Fixed expectations prevent two paths from
agreeing on an incorrect shared primitive implementation.

## Hard cutover and intentional adapters

Removed: repeated positional demand substitution; machine symbolic-result
continuation; reference snapshot replay; value-attached deferred-origin state;
name-based user-call arithmetic; `ConstExpr::LocalBinding`; independent
aggregate/type-level integer interpreters; MIR symbolic scalar execution and
reification fallback success.

Remaining adapters have explicit policies:

- `evaluate_type_level_int_const_expr` is an optional scalar reduction adapter
  to the common service. A blocked or failed reduction preserves the original
  operation for required forcing; it does not produce a value.
- The early HIR integer/literal helper supports type inference before semantic
  body construction. It uses the shared primitive implementation and does not
  execute user function bodies.
- `ConstTyData::UnEvaluated` retains validated original body/environment metadata
  at the type-checking boundary. Execution delegates to the same body service.
- `NotConstEvaluable` denotes unsupported operations; it is not a dependency
  protocol. Optional fold misses may retain an unsupported operation.
- Checked value conversion/retyping may return `None`; required materialization
  handles that failure rather than silently using the input as a successful value.
- Formal evidence recognition remains exact. A symbolic arithmetic description
  does not become evidence because its payload happens to contain a parameter.

## Borrowing scope

Borrows of a local in an evaluating frame (`ref x`, `mut x`, a projection of
one, or such a borrow passed to or returned from a callee) are admitted since
#1582. A borrow through a pointer or of a provider-backed local still fails with
`InvalidProviderUse`, since CTFE has no memory outside its frames. Internal
machine tests cover admitted reference lifetime, typed reads, mutation,
discard/replay behavior and the pointer rejection. The generic stdlib
`SolArraySuffix` declaration is covered; before #1582 its additional concrete
execution probe reached the borrowing restriction while resolving an array
extent, and it is not claimed as a passing concrete CTFE case.

## Verification and performance

### Master rebase

The 25 unpublished development commits were consolidated into an implementation
commit and this audit, then the nine-commit branch was rebased onto master
`e2fa7e53c`. The implementation is now `5e5073c18`. Backup refs retain both
original and consolidated histories. Consolidation preserved the exact final
file tree before the rebase.

`git range-diff` confirms the seven original patches are unchanged. The sole
content conflict combined the CTFE numeric intrinsic classifier with master's
trusted host I/O and clock declarations. Three incoming benchmark functions also
received the repository's existing scoped allowance for intentional measurement
output, fixing the stricter Clippy gate without changing test behavior.

Rebase verification:

- Full release suite: **3,458 passed, one skipped**, in 501.121 s.
- CI all-feature configuration: build passed; **3,397 tests passed, zero
  skipped**, in 307.401 s, with the standard language-server/bench exclusions.
- Focused release integration group: **91 passed**, including both corelib/stdlib
  profiles and host-import identity checks.
- Strict and CI Clippy, nightly/stable formatting, wasm/wasi and release-note
  validation pass. The parser was regenerated for master's updated grammar.

Both full configurations use pinned solc 0.8.30. Local Node remains 26.5.0
rather than CI's Node 20; verification is local macOS ARM64. Logs use
`/tmp/fe-ctfe-rebase-{full,focused,ci-build,ci-tests,final-clippy,clippy-ci,wasm,wasi,treesitter}.log`.
The exact full-suite and Clippy commands are recorded below.

### Implementation verification before the rebase

The original corrective commits were `b92c2b84c`, `bb8cb9b3c` and `ab970e91f`.
The following results were executed locally on macOS ARM64 before the master
rebase; the section above records verification of the current branch.

| Gate | Result |
| --- | --- |
| Debug `semantic_ctfe_deferral` integration binary | 36 passed, including the subprocess recursion regression. |
| Focused release integration/consumer group | 80 passed, including corelib and stdlib under both profiles. |
| `cargo nextest r --release --no-fail-fast` | 3,418 passed, one skipped; 294.347 s. No package exclusions. |
| `cargo +nightly fmt --all -- --check` | Passed. |
| `cargo fmt --all -- --check` | Passed. |
| Strict workspace Clippy, command below | Passed. |
| CI Clippy, command below | Passed. |
| `make treesitter-generate` | Passed with the pinned CLI after retrying outside the network sandbox. |
| `make check-wasm` | Passed. |
| `make check-wasi` | Passed. |
| `python3 newsfragments/validate_files.py` | Passed; existing `1556.bugfix.md` covers this behavior. |
| CI all-feature build/test commands below | Build passed; 3,325 tests passed, zero skipped; 303.380 s. Standard language-server/bench exclusions. |

Exact Clippy commands:

```sh
cargo clippy --workspace --all-targets --all-features -- \
  -D warnings -A clippy::upper-case-acronyms -A clippy::large-enum-variant \
  -W clippy::print_stdout -W clippy::print_stderr
cargo clippy --locked --workspace --all-targets --all-features -- -D clippy::all
```

CI feature configuration, with its standard package exclusions and pinned solc:

```sh
FE_SOLC_PATH=/tmp/fe-ctfe-solc-0.8.30 cargo test --release --workspace \
  --all-features --no-run --locked --exclude fe-language-server --exclude fe-bench
FE_SOLC_PATH=/tmp/fe-ctfe-solc-0.8.30 cargo nextest run --release --workspace \
  --all-features --no-fail-fast --locked --exclude fe-language-server --exclude fe-bench
```

Foundry is 1.5.1, matching CI. The full unexcluded run used local solc 0.8.35;
the CI configuration uses the repository-pinned 0.8.30. Parser generation used
local Node 26.5.0; CI's Node 20 environment was not reproduced. These local
checks do not claim execution of the Linux/Windows CI matrix.

Logs are `/tmp/fe-ctfe-latest-deferral.log`,
`/tmp/fe-ctfe-final-focused-3.log`, `/tmp/fe-ctfe-final-full-3.log`,
`/tmp/fe-ctfe-final-clippy-{strict,ci}-3.log`,
`/tmp/fe-ctfe-final-{wasm,wasi}-3.log`,
`/tmp/fe-ctfe-final-ci-{build,tests}.log` and
`/tmp/fe-ctfe-final-treesitter-2.log`.
Earlier full checkpoints exposed eight and then five integration failures;
those failures were corrected, and all pass in the final full run. No diagnostic
snapshots were changed.

### Representative measurements

Three optimized samples were run before the master rebase, after the full
suite finished and without a concurrent build. Each sample creates a fresh,
already type-checked database.
These are current-implementation sanity measurements, not cold compiler startup
or a controlled before/after comparison. Timings are total elapsed time per row.

| Workload | Observed range |
| --- | --- |
| First concrete arithmetic force in the pretyped database | 2.006–2.337 ms |
| Cached concrete forcing ×10,000 | 0.514–0.546 ms |
| Generic term description ×1,000 | 7.185–7.233 ms |
| 32 distinct branch specializations and forces | 1.001–1.115 ms |
| Cached blocked forcing ×10,000 | 0.532–0.536 ms |
| Repeated nested aggregate reification ×1,000 | 0.072–0.075 ms |

The cached workloads show no repeated body replay under unchanged input. The
resource regression separately proves cache hits still charge recorded work to
a parent forcing attempt. No speedup claim is made without a comparable baseline.
The harness and output are `/tmp/fe-ctfe-performance.rs` and
`/tmp/fe-ctfe-final-performance.log`.

History consolidation, the master rebase and pushing were subsequently
authorized. The rebase verification above applies to the current branch.
