# IMPORTANT: READ THIS FIRST

**Date**: 2025-10-23
**Status**: STARTING FRESH
**Branch**: `hir-api-rework-realigned-oct-23`
**Starting Point**: `merge-hir-with-hir-analysis` (commit 0bd749662)

**Purpose**: This document defines the architecture and patterns for the HIR refactoring. We are starting fresh after a failed first attempt (Oct 21-23). This document is intentionally stateless regarding progress - rely on todo tracking and code inspection to determine what's been completed.

---

## 1. The Vision: A Context-Rich, Traversable HIR

The primary goal of this refactoring is to **eliminate the manual threading of context** (like `scope` and `assumptions`) throughout the analysis code. We will achieve this by moving analysis logic out of intermediate "wrapper" structs and into methods directly on the HIR items themselves.

The desired end state is a "context-rich traversal API" where HIR nodes are self-aware and analysis queries can be composed naturally.

From the meeting with Sean:
> "...if we go back to the discussion about how the language server needs to be careful to do things in the same way that the compiler does, that's basically because of all these functions like lower HIR tie and lower importate and stuff... where you have to pass in these arguments like the scope and the assumptions basically... ideally we can just... hide the uses of them internally in these node like things."

This means analysis logic should be encapsulated within the HIR items, not in external `lower_*` functions or wrapper structs.

---

## 2. The Correct Implementation Pattern

### The WRONG Pattern (Standalone Functions)

This pattern is incorrect because the analysis logic still lives outside the HIR item, even if the wrapper struct is gone. It does not create a traversable, object-oriented API.

```rust
// in analysis/ty/trait_def.rs

// Standalone function that takes the HIR item as a parameter
#[salsa::tracked]
pub fn trait_methods<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: Trait<'db> // Takes Trait as parameter
) -> IndexMap<...> {
    // ... implementation ...
}
```

**Why it's wrong:** Analysis logic is external to the HIR item. Users must know to call this function and pass the right parameters.

### The CORRECT Pattern (`impl` Block Methods)

All new analysis logic must be implemented as methods on the HIR items directly, using `impl` blocks in `hir_def/item.rs`.

```rust
// in hir_def/item.rs

#[salsa::tracked]
pub struct Trait<'db> {
    // Internal/syntactic fields
    pub(crate) raw_methods: Vec<Func<'db>>,
    // ... other fields ...
}

// The implementation block for the HIR item
impl<'db> Trait<'db> {
    // Clean public API - analysis complexity is hidden
    #[salsa::tracked(return_ref)]
    pub fn methods(self, db: &'db dyn HirAnalysisDb) -> IndexMap<...> {
        // Internally does the analysis work
        let mut methods = IndexMap::default();
        for method in self.raw_methods(db) {
            // ... actual implementation here ...
        }
        methods
    }
}
```

**Why it's right:**
- Analysis logic is a method on the item itself
- Creates a natural, discoverable API
- Salsa automatically caches results
- No wrapper struct needed

### API Naming Guidelines

**Public API should have clean, straightforward names:**
- `.ty()` - returns the analyzed type (not `.analyzed_ty()` or `.trait_ty()`)
- `.methods()` - returns analyzed methods (not `.analyzed_methods()`)
- `.ingot()` - returns the containing ingot (not `.trait_ingot()`)
- `.name()` - returns the name (not `.trait_name()`)

**Name for what it *is*, not the process that creates it:**
- Avoid process-oriented prefixes like `lower_` or `analyzed_`. The fact that a method performs lowering or analysis is an implementation detail.
- **GOOD**: `impl_trait.self_ty(db)` or `impl_trait.data(db)`
- **BAD**: `impl_trait.lowered_self_ty(db)`

**Complexity goes into the internal implementation:**
- Use `pub(crate)` or private fields for raw syntactic data (e.g., `raw_type: TypeId`)
- Use `pub(crate)` helper functions for complex analysis logic
- The public method name is clean, doing analysis internally

**Principle:** The public API wraps analysis functionality in an easy-to-use interface.

```rust
// GOOD: Clean public API
impl<'db> Field<'db> {
    #[salsa::tracked]
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        // Internally calls lower_hir_ty with correct scope/assumptions
        lower_hir_ty(db, self.raw_type(db), self.scope(db), ...)
    }
}

// BAD: Wordy, redundant names
impl<'db> Field<'db> {
    pub fn analyzed_field_type(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> { ... }
}
```

### Module Evolution

As we perform this refactoring, the roles of our modules will evolve:

- **`hir_def` module**: Will become the container of the new **public traversal API**. HIR items will expose rich, context-aware methods that hide implementation complexity.

- **`analysis` module**: Will evolve into a **toolbox of internally useful queries and lightweight data structures**, designed to be exposed primarily through the public traversal API in `hir_def`, not used directly by external consumers.

This separation ensures a clean public API while maintaining internal flexibility for complex analysis queries.

### Leaning into Salsa Caching

A key part of this refactoring is to lean into Salsa's caching and incremental computation capabilities. By moving analysis logic into `#[salsa::tracked]` methods on the HIR items, we gain several benefits:

*   **On-Demand Computation:** Analysis is only performed when a method is called, not eagerly in a constructor.
*   **Memoization:** Salsa automatically caches the results of tracked methods. If the inputs haven't changed, the cached result is returned immediately, making subsequent queries extremely fast.
*   **Incremental Updates:** When a piece of source code changes, Salsa can determine exactly which queries need to be re-run, avoiding a full re-analysis of the entire project.

This is why the `impl` block pattern is so important. It allows us to associate these cached, on-demand computations directly with the data they operate on, which is the core of the "context-rich traversal API" we are building.

**Regarding Collections:**

Iterators themselves are not directly trackable by Salsa. However, you can have a `#[salsa::tracked]` function that *returns* an iterator.

*   **The function call is cached:** Salsa memoizes the call to the function that produces the iterator. If the inputs are the same, the function isn't re-executed.
*   **The iterator is not cached:** The iterator itself is created fresh on each call.

The main benefit of this pattern is avoiding the upfront cost of collecting all items into a `Vec`. If the consumer of the iterator only needs a few items, this can be more memory-efficient. However, if the entire collection is always needed, returning a `#[salsa::tracked(return_ref)] Vec<...>` might be more performant, as Salsa can cache the entire vector. The choice depends on the use case.

**Moving forward, all wrapper eliminations MUST use the CORRECT `impl` block pattern.**

---

## 3. Core Refactoring Principles

- **High-Level Goal**: Flatten the `analysis` module wrappers around HIR items. There is roughly one wrapper per HIR item (`TraitDef` for `Trait`, `Implementor` for `ImplTrait`, etc.).
- **Wrapper Purpose**: These wrappers currently serve two functions: providing an API surface for analysis-level logic and containing complex constructor logic that performs analysis.
- **The Problem**: This leads to brittle, hard-to-maintain code where consumers like the Language Server must manually pass around context (`scope`, `assumptions`) and re-implement analysis logic.
- **The Solution**: Decompose the logic from the wrapper constructors into methods on the HIR items themselves (e.g., `ImplTrait`). This makes the API more robust and easier to use.
- **Implementation**: Transition to the new API by removing `pub` visibility on HIR item fields (where appropriate) and providing `#[salsa::tracked]` methods for traversal and analysis.
- **Phased Approach**: Get larger chunks working first. Take a step back on fine-grained node wrappers for `expr`/`pat`/`stmt` until the primary item-level wrappers are handled.
- **Scope-level Wrappers**: There are also analysis-like structs for `ScopeId` variants (e.g., `GenericParamTypeSet`). These are more complex and should be addressed after the item-level wrappers are complete.

---

## 4. Inventory of `hir::analysis` and Consolidation Pointers

This is an inventory of the key abstractions in the `hir::analysis` module and pointers on how to consolidate their logic into the new traversal API.

### Item-Level Wrappers (Primary Targets)

*   **`analysis::ty::trait_def::TraitDef`**
    *   **Purpose**: A pure wrapper around a `Trait` to provide analysis methods.
    *   **Consolidation Pointer**: Move methods like `methods()`, `super_traits()` directly into an `impl` block on the `Trait` struct.
    *   **Example**: `trait.methods(db)` instead of `lower_trait(db, trait).methods(db)`

*   **`analysis::ty::func_def::FuncDef`**
    *   **Purpose**: Wraps a `Func` or `EnumVariant` to cache lowered types and provide analysis methods.
    *   **Consolidation Pointer**: Move logic for lowering `arg_tys` and `ret_ty` into `#[salsa::tracked]` methods on `Func` and `EnumVariant`.
    *   **Example**: `func.arg_tys(db)` and `func.ret_ty(db)`

*   **`analysis::ty::trait_def::Implementor`**
    *   **Purpose**: Represents a lowered, semantic representation of an `impl Trait` block, caching resolved types and constraints.
    *   **Consolidation Pointer**: The logic will be moved into methods on `ImplTrait`. The `Implementor` struct will be renamed to `TraitImplData` and become a `pub(crate)` implementation detail.
    *   **Complexity**: HIGH. This is the most complex part of the refactor and was the primary cause of the previous failure.

**CRITICAL COMPLEXITY: The `Binder` and the Syntactic/Semantic Divide**

-   A generic `impl<T> Foo for T` is a template. The `Binder` is the mechanism that allows the compiler to substitute `T` with a concrete type (e.g., `u32`) to check if the `impl` applies.
-   To do this, the `Binder` must wrap a **semantic**, `TyFoldable` data structure. It needs to know about `TyId`s and other semantic types to perform substitutions.
-   The `ImplTrait` struct is **syntactic**; it contains `PathId`s and other source-level information. It is not `TyFoldable`. You cannot substitute a semantic `TyId` into a syntactic `PathId`.
-   This means an intermediate, semantic data structure **must exist**. This is the role of `Implementor`. The previous refactor failed when it tried to eliminate this structure without a correct replacement, leading to inconsistent type substitutions.

**The Correct Strategy: Containment, Not Elimination**

The goal is to make the semantic data structure an invisible implementation detail.

1.  **Rename and Contain**: Rename `Implementor` to `TraitImplData` and make it `pub(crate)`. Its purpose is to be the semantic, `TyFoldable` payload for the `Binder`.
2.  **Isolate Lowering**: The `lower_impl_trait` function, which creates the `Binder<TraitImplData>`, will be kept but also made `pub(crate)`.
3.  **Create the Public API**:
    -   A `pub(crate)` `#[salsa::tracked]` method will be added to `ImplTrait`: `fn data(self, db) -> Binder<TraitImplData>`. This will be the single internal entry point to the lowered form.
    -   All **public** analysis methods (`self_ty`, `trait_inst`, `methods`, etc.) will be added to `ImplTrait`.
    -   These public methods will use `self.data(db)` internally to get the semantic information they need.

This design makes `ImplTrait` the single, context-rich touchpoint for consumers of the API, while correctly handling the complexities of generic `impl` blocks under the hood.

*   **`analysis::ty::adt_def::AdtDef`**
    *   **Purpose**: Wraps `Struct`, `Contract`, or `Enum` to provide a unified analysis view.
    *   **Consolidation Pointer**: Decompose and move logic to the respective HIR items. For example, `struct.fields_with_types(db)`. The `AdtField` sub-wrapper is a good pattern to follow.

### Key Salsa Queries & Abstractions

These are not one-to-one wrappers, but represent significant analysis logic that can be consolidated.

*   **`lower_hir_ty` (`ty_lower.rs`)**
    *   **Purpose**: The main entry point for lowering a `HirTyId` to a `TyId`.
    *   **Consolidation Pointer**: This function should be "hidden" from the public API. Instead of calling it directly, users should call methods on HIR items that have a `HirTyId` field, for example `field.ty(db)`, which would call `lower_hir_ty` internally with the correct `scope` and `assumptions`.

*   **`collect_generic_params` (`ty_lower.rs`)**
    *   **Purpose**: Collects and lowers generic parameters for an item.
    *   **Consolidation Pointer**: Move this logic into a `generic_params()` method on the `GenericParamOwner` items (`Func`, `Struct`, etc.).

*   **`resolve_imports` (`name_resolution/import_resolver.rs`)**
    *   **Purpose**: Resolves all `use` statements in an ingot.
    *   **Consolidation Pointer**: A larger task, but the logic could be moved to methods on `Use` and `Mod` items, like `use_item.resolved_item(db)`.

*   **`check_func_body` (`ty_check/mod.rs`)**
    *   **Purpose**: Performs type checking for a function body.
    *   **Consolidation Pointer**: The logic will be encapsulated within a single high-level analysis method, such as `func.type_check(db)`. This method will be the public API for body analysis.

*   **`TyCheckEnv` (`ty_check/env.rs`)**
    *   **Purpose**: A stateful environment for type checking a function body.
    *   **Consolidation Pointer**: This will be replaced by a temporary, `pub(crate)` context struct (e.g., `TyCheckCtxt`). This context object will be created and used exclusively as an *internal implementation detail* of the main `func.type_check(db)` method. It will be passed through the internal expression/statement traversal, but will not be exposed to consumers of the HIR API. This avoids creating new persistent wrappers for every expression and statement.

---

## 5. What Went Wrong in the First Attempt

**Timeline:** Started Oct 21, 2025. By Oct 23, the branch (`hir-api-rework`) was in a broken state with cascading test failures.

### Mistake #1: Initially Using Wrong Pattern

The first implementations used standalone functions (`trait_methods(db, trait_)`) instead of impl methods. This was later "corrected," but wasted time and created confusion about what pattern to follow.

### Mistake #2: Going Too Fast

Multiple wrappers were partially eliminated simultaneously without ensuring each one was fully working first. This created a web of broken dependencies that was hard to untangle.

**Example:** `TraitDef` was "eliminated" with standalone functions, then `Implementor` work began, then we went back to "correct" `TraitDef` to use impl methods, all while other code was breaking.

### Mistake #3: Ignoring Test Failures

Test regressions were introduced but not immediately fixed:
- `constraints_standalone__specialized` - failed due to `Binder<Implementor>` elimination losing type substitutions
- `early_path_resolution_standalone__enum_self_variant` - failed for unclear reasons, never fully investigated

Instead of stopping to fix each regression, work continued, creating a pile of broken tests.

### Mistake #4: Getting Lost in Complex Fixes

When `constraints_standalone__specialized` failed, we spent hours debugging complex trait resolution and method selection logic instead of stepping back to understand the root cause.

**What actually happened:** The `Binder<Implementor>` struct was a single, semantic unit. When `Binder::instantiate` was called on it, it correctly substituted generic type parameters across all its fields (`self_ty`, `trait_inst`, `constraints`) within a single, shared unification context. The attempt to eliminate `Implementor` by creating separate `Binder<TyId>`, `Binder<TraitInstId>`, etc., on the `ImplTrait` node broke this atomicity. Each `instantiate` call created a new, independent unification context, leading to inconsistent substitutions and incorrect type inference.

**What we did:** Tried complex workarounds in method selection logic, creating new candidate types and verification strategies, getting deeper into the weeds.

**What we should have done:** Recognized that eliminating `Binder<Implementor>` was premature and required a different strategy. Should have reverted and planned more carefully.

### Mistake #5: Breaking Commit Pattern

Commits eliminated core functionality without a complete replacement strategy. This created regressions that cascaded through subsequent work.

### Mistake #6: Incomplete Elimination

The "corrected" TraitDef and FuncDef eliminations left behind:
- Standalone functions that weren't deleted
- Mixed calling patterns (some using methods, some using old functions)
- Unclear boundaries between old and new patterns

### Key Lessons Learned

1. **Go depth-first, one wrapper at a time:** Fully complete one elimination before starting the next
2. **Keep all tests green:** If a test breaks, stop and fix it immediately
3. **Don't eliminate what you don't understand:** The `Binder<Implementor>` pattern was more subtle than initially recognized
4. **Simple beats clever:** When debugging, step back to understand the root cause instead of adding complexity
5. **Complete deletion, not coexistence:** Delete old patterns completely once new ones work
6. **Commit frequently with working code:** Don't let broken state accumulate

---

## 6. The Fresh Start Strategy

### Starting Point

- **Branch:** `hir-api-rework-realigned-oct-23`
- **Reset to:** `merge-hir-with-hir-analysis` (commit 0bd749662)
- **All tests pass:** Starting from a known-good state

### Elimination Order (Simplest to Hardest)

1. **TraitDef** - Sean called this "the most egregious" - it's just a namespace wrapper
2. **FuncDef** - Similar to TraitDef, but with some cached computation
3. **Implementor** - Complex due to `Binder` usage; requires careful strategy
4. **AdtDef** - Sean said this "needs to be decomposed" - may require different approach

### Per-Wrapper Process

For each wrapper:

1. **Create a detailed plan document** (e.g., `TRAITDEF_ELIMINATION_PLAN.md`)
2. **Audit the wrapper:** Understand every method, every usage, every call site
3. **Add impl methods to HIR item:** Move logic from wrapper to HIR item
4. **Test incrementally:** Ensure new methods work before changing call sites
5. **Update call sites systematically:** One file at a time, keep tests passing
6. **Delete wrapper completely:** No coexistence of old and new patterns
7. **Clean up:** Remove unused imports, standalone functions, old documentation

### Critical Rules

- **One wrapper at a time:** Don't start the next until the current is complete
- **Keep tests green:** Run tests frequently, fix failures immediately
- **Complete deletion:** No leaving old functions around "just in case"
- **Clean API naming:** Use straightforward names like `.ty()`, `.methods()`, `.ingot()`
- **Verification checklist** before moving on:
  - [ ] All standalone functions deleted
  - [ ] All call sites updated
  - [ ] All unused imports removed
  - [ ] Compilation succeeds with no warnings
  - [ ] All tests pass

---

## 7. Critical Implementation Details

### Complete Deletion, Not Coexistence

After adding impl methods, you MUST delete the old standalone functions. Don't leave them around "for compatibility" or "just in case". The goal is elimination, not duplication.

```rust
// After moving logic to impl methods, DELETE this:
#[salsa::tracked(return_ref)]
pub fn trait_methods<'db>(db: &'db dyn HirAnalysisDb, trait_: Trait<'db>) -> ... {
    // DELETE THIS ENTIRE FUNCTION
}
```

### Update ALL Call Sites

Every single usage of the old standalone function must be updated to use the new method. Use grep/ripgrep to find all occurrences:

```bash
# Find all usages
rg 'trait_methods\(' --type rust

# Update from standalone function:
let methods = trait_methods(db, my_trait);

# To method call:
let methods = my_trait.methods(db);
```

### Clean Up Unused Imports

After updating call sites, remove the now-unused imports of the standalone functions:

```rust
// DELETE these after migration:
use crate::analysis::ty::trait_def::{
    trait_methods,      // Not needed anymore
    trait_super_traits, // Not needed anymore
    trait_ingot,        // Not needed anymore
};
```

### Don't Just Delegate

When we say "move" the logic, we mean **inline the actual implementation** into the impl method, not just create a delegation wrapper that calls the old function. The goal is to eliminate the old pattern entirely, not add another layer of indirection.

```rust
// BAD: Just delegating to old function (adds indirection)
impl<'db> Trait<'db> {
    pub fn methods(self, db: &'db dyn HirAnalysisDb) -> ... {
        trait_methods(db, self)  // Just calling old function
    }
}

// GOOD: Actually moving the implementation
impl<'db> Trait<'db> {
    #[salsa::tracked(return_ref)]
    pub fn methods(self, db: &'db dyn HirAnalysisDb) -> IndexMap<...> {
        let mut methods = IndexMap::default();
        for method in self.raw_methods(db) {
            // ... actual implementation here ...
        }
        methods
    }
}
```

### Verification Checklist

Before considering a wrapper elimination complete:

- [ ] All standalone functions have been deleted
- [ ] All call sites have been updated to use methods
- [ ] All unused imports have been removed
- [ ] Compilation succeeds with no warnings about unused code
- [ ] All tests pass (minus any pre-existing failures)
- [ ] Documentation updated to reflect new API

---

## 8. Wrapper Details

### TraitDef (START HERE - Simplest)

**Location:** `crates/hir/src/analysis/ty/trait_def.rs`

**Structure:**
```rust
pub struct TraitDef<'db> {
    pub trait_: Trait<'db>,  // Just wraps the HIR item!
}
```

**Constructor:** `lower_trait(db, trait_)` - literally just calls `TraitDef::new(db, trait_)`

**Methods to migrate:**
- `methods()` → `trait.methods(db)`
- `params()` → `trait.params(db)`
- `param_set()` → (internal helper)
- `self_param()` → `trait.self_param(db)`
- `original_params()` → `trait.params(db)` (explicit params)
- `expected_implementor_kind()` → (possibly internal)
- `ingot()` → `trait.ingot(db)`
- `super_traits()` → `trait.super_traits(db)`
- `name()` → `trait.name(db)`

**Complexity:** LOW - Pure namespace wrapper with minimal logic

---

### FuncDef (Second Priority)

**Location:** `crates/hir/src/analysis/ty/func_def.rs`

**Purpose:** Wraps `Func` or `EnumVariant`, caches lowered types

**Methods to migrate:**
- `arg_tys()` → `func.arg_tys(db)`
- `ret_ty()` → `func.ret_ty(db)`
- Similar methods for EnumVariant

**Complexity:** LOW-MEDIUM - Has some constructor logic for lowering types

---

### Implementor (Complex - Plan Carefully)

**Location:** `crates/hir/src/analysis/ty/trait_def.rs`

**Structure:**
```rust
pub(crate) struct Implementor<'db> {
    trait_: TraitInstId<'db>,
    params: Vec<TyId<'db>>,
    types: IndexMap<IdentId<'db>, TyId<'db>>,
    hir_impl_trait: ImplTrait<'db>,
}
```

**Constructor:** `lower_impl_trait(db, impl_trait)` - performs significant analysis

**CRITICAL COMPLEXITY: `Binder<Implementor>`**

- `Implementor` is almost always used as `Binder<Implementor>`
- `Binder` allows generic trait implementations to be instantiated with concrete types
- `Implementor` implements `TyFoldable`, `TyVisitable`, `Unifiable` - required for `Binder`
- **We cannot simply replace `Binder<Implementor>` with `Binder<ImplTrait>`**

**Strategy (Must Plan Before Implementing):**

1. **Phase 0: In-depth Analysis (CRITICAL)**: Before writing code, choose a strategy for handling `Binder`. Most likely path: create a new, minimal, non-salsa-tracked struct that can be wrapped in `Binder`. Methods on `ImplTrait` would create this struct on-the-fly.
2. **Phase 1: Add `impl` block to `ImplTrait`**
3. **Phase 2: Migrate Logic** to new methods
4. **Phase 3: Address `Binder`** - Update all `Binder<Implementor>` usage sites
5. **Phase 4: Final Elimination**

**Complexity:** MEDIUM-HIGH

---

### AdtDef (Plan Carefully)

**Location:** `crates/hir/src/analysis/ty/adt_def.rs`

**Purpose:** Unified interface over `Struct`, `Contract`, `Enum`

**Complexity:** HIGH - Sean said this "needs to be decomposed"

May require decomposition rather than simple elimination. The `AdtField` sub-wrapper already demonstrates some desired patterns.

---

## 9. Testing Strategy

### Test After Every Change

Don't accumulate untested changes. After each meaningful step:

```bash
cargo test --workspace --quiet
```

### Fix Regressions Immediately

If a test breaks:
1. **Stop** making changes
2. **Understand** the root cause
3. **Fix** the regression
4. **Verify** all tests pass
5. **Then continue**

### Test Targets

The following test suites must remain green:
- Constraint resolution tests
- Path resolution tests
- Type checking tests
- All CLI output tests

---

## 10. Documentation Policy

- **This document is the single source of truth** for refactoring vision and patterns
- **Per-wrapper plans** (e.g., `TRAITDEF_ELIMINATION_PLAN.md`) contain detailed execution plans
- Create new plan documents as needed for each wrapper elimination
- Update CLAUDE.md to reference this document

---

## 11. Success Criteria

The refactoring is successful when:

1. ✅ All wrapper structs eliminated (`TraitDef`, `FuncDef`, `Implementor`, `AdtDef`)
2. ✅ All analysis methods live on HIR items as impl methods
3. ✅ Public API uses clean, straightforward method names
4. ✅ All tests pass with no regressions
5. ✅ No compiler warnings about unused code
6. ✅ Language server can use HIR traversal API naturally
7. ✅ Manual `scope` and `assumptions` threading is eliminated (or significantly reduced)

---

## References

- **Meeting transcript:** `iffy-transcription-of-fe-meeting-oct-21.srt` (in this repo)
- **Failed first attempt branch:** `hir-api-rework` (for reference only)

---

## Ongoing Learnings

**Run Targeted Tests During Refactoring**
Don't wait until the end. After changing a file, run its specific tests immediately:
```bash
cargo test <specific_test_name> --quiet
```
Catch regressions instantly rather than debugging them later.

**One Refactoring Pattern at a Time**
Separate type substitutions from semantic changes:
- Type substitution: `TraitDef` → `Trait` (mechanical)
- Semantic change: removing `lower_trait` calls (changes meaning)

Do these in separate passes when possible.

**Document Non-Obvious Code**
When encountering special-case logic, ask "Why?" before changing it. If the answer isn't obvious, it's a potential trap.

**Bottom Line**: Understand the code first (via targeted tests + tracing through examples), then refactor. Changing code we don't fully understand causes churn.
