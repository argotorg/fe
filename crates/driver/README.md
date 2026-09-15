# Explicit scalar generation

`generation::generate_scalar_function` evaluates an ordinary checked Fe
provider and fills a function body in a separate, checked standalone source
stage. It is a driver API; the `fe` command does not discover or invoke these
providers automatically.

A provider computes the designated core descriptor:

```fe
use core::meta::FunctionBody

const fn provide() -> FunctionBody<u256> {
    let mut result: u256 = 40
    let slot = mut result
    slot += 2
    FunctionBody { value: result }
}
```

The target source supplies the entire declaration and can include a caller:

```fe
pub const fn answer() -> u256 {}
const fn consume() -> u256 { answer() }
```

After loading the provider file into a `DriverDataBase` initialized with builtin
core/std and resolving `provide` to a `hir::hir_def::Func`, invoke the API:

```rust,ignore
use fe_driver::generation::{
    FunctionTemplate, GenerationBudget, generate_scalar_function,
};

let mut budget = GenerationBudget::new(1, 64 * 1024);
let artifact = generate_scalar_function(
    &provider_db,
    provider_func,
    FunctionTemplate {
        url: "file:///generated/answer.fe".parse()?,
        source: target_source.to_owned(),
        function_name: "answer".to_owned(),
    },
    &mut budget,
)?;
```

`artifact.source()` contains the completed source. Ordinary compiler consumers
use `artifact.database()`, `artifact.file()`, and `artifact.function()`. The
artifact owns its canonical database, and generation performs no filesystem
writes. Integration tests in `tests/scalar_generation.rs` evaluate callers from
that database and check declaration requirements with positive and negative
controls.

The initial entry accepts a safe, top-level, nullary const provider in a
standalone ingot with only builtin dependencies. Root generics, where clauses,
and effects are excluded; ordinary generic helpers retain normal call-site
checking. Descriptor values must use the actual builtin
`core::meta::FunctionBody<bool>` or `FunctionBody<u256>` identity. Type aliases
are accepted, same-shaped user definitions are not.

The selected target must have an explicit return type and an empty body. Only
that body's parsed byte range is replaced, preserving attributes, modifiers,
parameters, labels, generics, requirements, effects, and supporting items. The
target return type must equal the descriptor's scalar type after normalization.
The whole completed target stage passes HIR, semantic borrow, and layout checks.

Provenance retains owned provider and template sources and their byte ranges,
plus the generated body range and request-local invocation number. Output
diagnostics retain that provenance. This stage receipt does not add generated
HIR origins or implicit cross-stage references. The target uses default compiler
options and builtin core/std; it does not inherit the provider's dependencies.

Budgets count each emitted source even when CTFE is cached. Emitted source is
charged before target checking, including attempts rejected by that checking.
The input template is also checked against the source limit before parsing.
These are logical source/function limits, not a bound on all compiler memory or
provider evaluation work. CTFE's existing execution limits still apply.

## Aggregate values and request identity

For tuples and fixed arrays, use an explicit `GenerationSession`:

```rust,ignore
use fe_driver::generation::{GenerationRequest, GenerationSession};

let mut session = GenerationSession::new(
    "schema-stage".to_owned(),
    GenerationBudget::new(8, 64 * 1024),
);
let artifact = session.generate_value_function(
    &provider_db,
    provider_func,
    GenerationRequest {
        key: "constants".to_owned(),
        template,
    },
)?;
```

This entry accepts `FunctionBody<T>` where `T` is recursively `bool`, `u256`, a
tuple (including unit), or a fixed array. For example, a provider can compute
`FunctionBody<([u256; 2], bool)>` for a template returning `([u256; 2], bool)`.
The source body contains only canonical literal syntax. Ordinary target checking
still validates its complete declaration and all callers.

The transport retains a structural type independently of its contents. Empty
`[bool; 0]` and `[u256; 0]` values therefore remain different contracts. User
nominal types, references, pointers and unsupported scalar types are rejected,
even inside zero-length arrays. Target aliases normalize normally, but fitting
integer literals cannot silently narrow the descriptor's element type.

Transport is bounded to 32 levels, 4,096 type nodes, and 4,096 expanded value
occurrences. Expanded array
cost uses saturating arithmetic before the explicit provider evaluation request;
an enclosing empty
array does not materialize its latent element values. Literal rendering checks
the remaining aggregate source budget before each append. These bounds apply to
the transport, not all work in ordinary checking or CTFE.

Every attempted request key is reserved before validation. Reusing a key in the
same session returns `DuplicateRequest`, even if the first attempt failed. Failed
attempts before source emission do not consume the output budget; emitted but
invalid target stages do. A duplicate consumes neither output nor execution.
The attempted-key ledger is not limited by the output budget; callers control
session admission and lifetime.

Receipts and errors retain `RequestIdentity { stage, key }`. The existing
`invocation` field is an emission ordinal only. Reversing request order changes
that ordinal but not the caller-supplied identity. Equal emitted text does not
merge distinct requests. Two fresh sessions may evaluate the same stage/key
against different sources; identity equality is not evidence of equal artifacts
or permission to reuse one. A shared stage key does not make the artifacts in
separate databases mutually visible. The original scalar entry retains `None` for request
identity and continues to reject aggregates.

## Frozen exports and bound calls

`generation::imports` supports one explicit call to a selected function from a
checked generated artifact:

```rust,ignore
use fe_driver::generation::imports::{FrozenArtifact, bind_function};

let frozen = FrozenArtifact::new(artifact);
let selected = frozen.export("apply")?;
let bound = bind_function(template, &selected, &[1, 0], &mut budget)?;
```

The template selects a top-level function with an explicit return type and an
empty body. The index list forwards its parameters in the selected export's
argument order; labels come from the export. For example, `[1, 0]` forwards the
second template parameter first. Repeated indices are allowed for these Copy
scalar values. There is no arbitrary argument-expression string input.

Exports must be public, safe, monomorphic const functions over `bool` and `u256`,
without effects or where clauses. Ordinary View/Own parameter adaptation is
checked by the compiler. Exact scalar types must match after normalization;
references, nominal types and generic interfaces are outside this entry.

The handle retains its particular immutable artifact incarnation, even after the
original wrapper is dropped. Reusing a logical request identity does not rebind
an existing handle. Materialization copies the complete frozen source into a
separate dependency ingot, retaining root paths and private supporting context.
The dependency exposes its whole public surface, not an export whitelist.

The driver constructs the call slot and checks the completed compilation. It
then compares that call's resolved function against the selected materialized
export in the receiving database. Shadowing cannot silently redirect a returned
bound artifact. This check covers the designated direct call; it does not prove
a transitive call graph or establish a general hygiene mechanism.

`BoundFunction` exposes its immutable database, source, selected function and
receipt. The receipt retains the frozen source, original generation provenance,
selected export, completed consumer source and emitted call range. Post-emission
errors retain this receipt after the temporary database is dropped. The operation performs no filesystem
writes and uses builtin dependencies with default compiler options. Custom
dependency closures and persistent serialized handles remain future work.

Each emission charges one function and the bytes of both the frozen package and
completed consumer source, including emissions rejected during checking. These
are logical source limits, not a bound on all compiler allocations. Request
identities continue to name logical work rather than imported declarations.

## End-to-end validator example

Run the complete in-memory example from the repository root:

```sh
cargo run --release -p fe-driver --example generated_validator
```

The Fe provider computes inclusive bounds `(10, 20)` as
`FunctionBody<(u256, u256)>`. Generation fills a private `bounds()` function in
a package with a public `within(value: u256) -> bool` helper. A frozen export
of that helper then supplies the body of `validate(value: u256) -> bool` in a
separate consumer. Ordinary Fe callers evaluate the boundary cases:

```text
below: false
lower: true
inside: true
upper: true
above: false
```

The example needs no manual editing of generated source. The helper reads the
provider-computed bounds from its retained package context, so this application
uses the existing value-generation and bound-call operations. It does not yet
need a general expression builder or literal arguments in the call protocol.
The host supplies explicit templates, selects the export and sequences the two
stages; providers are not automatically discovered.

The example's tests run in the normal Cargo test suite. They compare a provider
edit in a reused database with fresh compilation and verify that a retained old
export keeps its original behavior. This is a focused edit oracle for this
pipeline, not a general incremental correctness or performance claim.

## Nominal identity and frozen package graphs

`generation::imports::packages` composes ordinary checked Fe packages while
preserving the identity of their captured dependency graph:

```rust,ignore
use fe_driver::generation::imports::packages::{
    FrozenPackage, PackageLimits, compose_packages,
};

let records = FrozenPackage::from(frozen_artifact);
let result = compose_packages(
    consumer_source.to_owned(),
    &[("left", &records), ("right", &records)],
    PackageLimits::new(16, 64 * 1024),
)?;
```

The result exposes `database()`, `file()`, `source()` and owned materialization
receipts. It can itself become a dependency of another composition. Clones and
repeated conversions from the same cloned `FrozenArtifact` retain one recipe
identity. Each unique recipe is materialized once per receiving compilation,
so two aliases of that package refer to the same nominal types. Independently
created artifacts stay distinct even when their sources and logical request
identities are equal. A diamond preserves sharing through the captured graph.

For example, a generated package can contain a private computed `limit()`, a
public `Record`, a `make(value) -> Record` constructor and a
`valid(record: Record) -> bool` function. The consumer can pass
`left::make(value: 15)` to `right::valid` when both aliases select the same
package. Separate same-shaped `Record` definitions fail ordinary type checking.
The executable examples are in `tests/frozen_packages.rs`; the underlying
resolver controls are in `tests/nominal_package_identity.rs`.

This entry exposes ordinary package imports. Public visibility and alias
shadowing retain their normal behavior. It does not extend the scalar
`bind_function` contract to nominal interfaces or verify every authored call's
selected target. Nominal compatibility is decided by the existing compiler,
not by a parallel structural comparison in the package layer.

Composition captures immutable source/edge recipes; it does not retain every
intermediate composition's checked database. Generated leaves retain their
original artifact. The returned root owns its fresh checked database, with
builtin core/std and default compiler options. This API constructs finite DAGs;
it does not specify general cyclic dependency or provider-discovery semantics.

Limits count the new root and unique transitive packages, their source bytes,
and each dependency alias once per unique parent. They do not bound all compiler
allocations. Every materialized package is checked. Receipts on output errors
retain sources and dependency URLs after the temporary database is dropped.
Materialization URLs are local to that compilation, not persistent package IDs.
No filesystem or network access is performed.

## Ground const where predicates

Ordinary compilation now checks boolean `where` conditions on declarations
without in-scope generic parameters:

```fe
const LIMIT: u256 = 8
const fn allowed(_ size: u256) -> bool { size < LIMIT }

fn operation() where allowed(3), !false {}
```

Every condition is checked at its declaration, even if nobody uses the item.
Only evaluation to `true` succeeds. Type errors, nonconst operations, failed
execution, recursion, and exhausted CTFE limits are errors. Conditions enter
ordinary semantic borrow and layout checking as anonymous const bodies.
They neither add solver assumptions nor filter impl candidates.

Type/trait predicates keep their existing syntax and meaning. Parenthesize a
block condition, as in `where ({ ... })`, to distinguish it from the item's
body. Generic type, trait, impl and associated-function contexts remain
explicitly unsupported, including a trait's implicit `Self` scope.

Generated target templates retain these conditions and receive the same checks.
The existing provider and frozen-export protocols continue to exclude root
requirements, including ground const predicates.


## Generic function const requirements

Top-level generic functions can retain parameter-dependent boolean conditions:

```fe
const fn bounded<const N: usize>() -> u256 where N > 0 { 42 }

const fn forward<const COUNT: usize>() -> u256 where COUNT > 0 {
    bounded<COUNT>()
}

const fn answer() -> u256 { forward<1>() }
```

The declarations check the predicates' types and const-language operations.
Each function use discharges its requirements after inference. `bounded<0>()`
is rejected even though its body does not use `N`. Function values and calls in
constant initializers, array lengths and other anonymous constant bodies use
the same check. A ground condition is still checked on an unused generic
function; parameter-dependent conditions are obligations for its callers.

Concrete conditions use ordinary CTFE and its execution limits. Generic
forwarding currently accepts an identical resolved, typed expression after
scoped substitution. Parameter spelling is irrelevant; declaration identities,
parameter positions, operation identities and arithmetic mode matter. Supported
symbolic expressions are literals, const paths, unary/binary operations, casts
and ordinary const calls. This is exact forwarding, not algebraic implication:
`N > 1` does not automatically establish `N > 0`. Blocks and control flow can
be evaluated with concrete arguments but cannot yet be forwarded symbolically.
Anonymous constants in an ordinary function's signature or executable body can
forward that function's checked conditions. Predicate formation cannot use any
of the enclosing function's conditions, including from constants nested inside
a predicate. This rule is independent of clause order. Nested declarations do
not inherit function premises, and associated functions remain outside the
supported generic requirement scope. Recursive requirements cannot establish
themselves.

For example, a constrained helper can compute an array length in a signature:

```fe
const fn empty_length<const N: usize>() -> usize where N > 0 { 0 }
const fn consume<const COUNT: usize>(
    _ values: [u8; { empty_length<COUNT>() }]
) -> u256 where COUNT > 0 { 42 }
const fn answer() -> u256 { consume<1>([]) }
```

The matching condition permits the signature's helper call. `consume<0>([])`
still fails, even though the helper returns zero for every input. This extends
requirement forwarding, not the supported symbolic array evaluation language.

Generated target templates support the same generic declarations and ordinary
callers. Providers and selected frozen exports keep their existing root
restrictions. This does not add automatic provider discovery or a generic
cross-stage export protocol.

Inference templates and requirement discharge have separate queries so CTFE
can consume completed inference while checking a condition. Raw inference,
semantic templates and CTFE results are not certificates that a program meets
its requirements. Use normal compiler diagnostics, as generation does, before
admitting a program to lowering. The implementation is shared frontend logic;
no backend has a separate const-requirement evaluator.
