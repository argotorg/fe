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

Cross-stage item references remain unsupported. Request identities name logical
work, not compiler declarations, source snapshots, or imported symbols. A future
reference protocol must bind a particular checked artifact incarnation, exported
item and receiving compilation context before ordinary name/type checking.
