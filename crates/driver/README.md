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
