# HIR API Refactor - Prereview Study (2025-11-03)

## Context
This document summarizes investigation and analysis performed ahead of reviewing the 10-29 review session transcription. We're working on the `hir-api-rework-realigned-oct-23` branch, which aims to eliminate wrapper types and create a more traversable HIR API.

## Current State Assessment

### Code Size Analysis
Comparing `hir-api-rework-BASE` to current `HEAD`:
- **Total change**: 32 Rust files changed, +247 net lines (1299 insertions, 1052 deletions)
- **🚩 Problem**: We should have a NET LOSS, not a gain - the refactor aims to eliminate code duplication

### Files with Most Changes
1. **crates/hir/src/hir_def/item.rs**: +449 added lines (net +509 lines changed)
2. **crates/hir/src/analysis/ty/trait_def.rs**: 326 lines changed
3. **crates/hir/src/analysis/ty/func_def.rs**: 233 lines changed
4. **crates/hir/src/analysis/ty/def_analysis.rs**: 227 lines changed
5. **crates/hir/src/analysis/ty/adt_def.rs**: 226 lines changed

### Git History
Recent commits show progress but incomplete elimination:
```
c3f163b96 little sketch during meeting
6fea8b6e0 dissolve AdtDef
689527a8e further alignment in briefing
cc94f8c00 Expose public semantic API on ImplTrait: rename to raw_ty/ty() pattern
37645fbeb Make ImplTrait::instantiated() generic over UnificationStore and update all call sites
```

## Core Problem: `lower_*` Functions

### Two Categories Identified

#### Category 1: AST → HIR Lowering (✅ These are FINE)
Located in `crates/hir/src/lower/`:
- **Pattern**: `fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: AstNode) -> HirNode`
- **Purpose**: Legitimate compilation pipeline - converting parsed AST to HIR
- **Examples**:
  - `crates/hir/src/lower/types.rs:7` - `Type::lower_ast()`
  - `crates/hir/src/lower/expr.rs:12` - `Expr::lower_ast()`
  - `crates/hir/src/lower/body.rs:13` - `Body::lower_ast()`
- **Action**: Keep these - they're correct

#### Category 2: HIR → Semantic Analysis (🚩 PROBLEMATIC)
Located in `crates/hir/src/analysis/`:

**🚩 `lower_hir_ty`** - The most egregious example
- **Location**: `crates/hir/src/analysis/ty/ty_lower.rs:22`
- **Signature**:
  ```rust
  #[salsa::tracked]
  pub fn lower_hir_ty<'db>(
      db: &'db dyn HirAnalysisDb,
      ty: HirTyId<'db>,              // Takes HIR type
      scope: ScopeId<'db>,            // ⚠️ Manual scope threading
      assumptions: PredicateListId<'db>, // ⚠️ Manual assumptions threading
  ) -> TyId<'db>                     // Returns semantic type
  ```
- **Call count**: 12+ direct calls found
- **Problem**: Converts HIR-level type representation to semantic analysis type, but requires manual scope/assumptions threading
- **Called from**: `crates/hir/src/hir_def/item.rs` - methods on Func, Struct, ImplTrait, etc.

**🚩 `lower_trait_ref`**
- **Location**: `crates/hir/src/analysis/ty/trait_lower.rs`
- **Signature**:
  ```rust
  #[salsa::tracked]
  pub(crate) fn lower_trait_ref<'db>(
      db: &'db dyn HirAnalysisDb,
      self_ty: TyId<'db>,
      trait_ref: TraitRefId<'db>,     // Takes HIR trait reference
      scope: ScopeId<'db>,            // ⚠️ Manual scope threading
      assumptions: PredicateListId<'db>, // ⚠️ Manual assumptions threading
  ) -> Result<TraitInstId<'db>, ...>
  ```
- **Call count**: 5+ call sites
- **Problem**: Same pattern - requires explicit scope/assumptions

**🚩 `lower_type_alias`**
- **Location**: `crates/hir/src/analysis/ty/ty_lower.rs:148`
- **Signature**:
  ```rust
  #[salsa::tracked(return_ref, cycle_fn=..., cycle_initial=...)]
  pub(crate) fn lower_type_alias<'db>(
      db: &'db dyn HirAnalysisDb,
      alias: HirTypeAlias<'db>,       // Takes HIR item
  ) -> TyAlias<'db>                  // Returns wrapper
  ```

### Search Results Summary
- **Total `lower_*` function calls**: 346 across codebase
- **Files with most calls**:
  - `crates/hir/src/lower/item.rs`: 108 calls (mostly AST→HIR, OK)
  - `crates/hir/src/lower/params.rs`: 52 calls (mostly AST→HIR, OK)
  - `crates/hir/src/lower/expr.rs`: 33 calls (mostly AST→HIR, OK)
  - `crates/hir/src/analysis/ty/def_analysis.rs`: 20 calls (⚠️ analysis layer)
  - `crates/hir/src/analysis/ty/ty_lower.rs`: 16 calls (⚠️ analysis layer)

## The Problematic Pattern

### Current (Wrong) Pattern:
```rust
// In crates/hir/src/hir_def/item.rs (example from ImplTrait methods)
let ty = self
    .ret_ty(db) // Access the field
    .map(|ty| lower_hir_ty(db, ty, self.scope(), assumptions))
    .unwrap_or_else(|| TyId::unit(db));
```

**Problems**:
1. Callers must manually obtain `scope` via `self.scope()`
2. Callers must manually obtain/thread `assumptions`
3. Duplicated pattern appears in multiple places
4. Easy to pass wrong scope or assumptions
5. Not ergonomic - requires knowledge of internals

### Desired (Correct) Pattern:
```rust
// The HIR item method should handle this internally
impl ImplTrait {
    pub fn ret_ty(self, db: &dyn HirAnalysisDb) -> TyId {
        // Internally gathers scope and assumptions
        let assumptions = collect_constraints(db, self.into()).instantiate_identity();
        self.ret_ty_raw(db)
            .map(|hir_ty| lower_hir_ty(db, hir_ty, self.scope(), assumptions))
            .unwrap_or_else(|| TyId::unit(db))
    }
}
```

Even better - eventually `lower_hir_ty` should become a method on `HirTyId` itself.

## Wrapper Types to Eliminate

From October 21st meeting transcription and current code:

### Already Dissolved (according to git log):
- ❌ `AdtDef` - commit 6fea8b6e0 "dissolve AdtDef"

### Still Present (need elimination):
Based on `crates/hir/src/analysis/ty/`:

1. **`TraitDef`** (trait_def.rs:326 lines changed)
   - Most egregious - literally just wraps HIR Trait with no added value
   - All its methods should move to `impl Trait` in hir_def/item.rs
   - Referenced in: def_analyzer, trait_lower.rs

2. **`FuncDef`** (func_def.rs:233 lines changed)
   - Wraps HIR Func
   - Methods should move to `impl Func`

3. **`Implementer`** (mentioned in Oct 21 transcription)
   - Wraps `ImplTrait`
   - Used in Binder contexts
   - More complex due to usage in TyFoldable, TyVisitable, Unifiable traits

### Wrapper Elimination Strategy (from Oct 21 meeting):

1. **Start with easiest**: `TraitDef`
   - Move all methods to HIR `Trait` item
   - Make internal fields private
   - Update all call sites
   - Delete wrapper struct

2. **Move to moderate**: `FuncDef`, then `Implementer`
   - Similar process but more complex due to trait implementations

3. **Handle scope-level wrappers later**:
   - `GenericParamTypeSet` and related types are more complex
   - Deal with these after item-level wrappers are gone

## Key Insights from Oct 21 Meeting

### The Core Motivation
> "The language server needs to be careful to do things in the same way that the compiler does - that's basically because of all these functions like `lower_hir_ty` and `lower_impl_trait` and stuff... where you have to pass in these arguments like the scope and the assumptions basically."

### The Vision
Create a "top down hierarchy of objects that represent the source code":
- Each HIR item is an object with enough context
- Can traverse downward (func → params → types)
- Can traverse upward (field → struct)
- No need to manually thread scope/assumptions

### What We Want to Eliminate
From def_analyzer.rs and similar files:
- Calls to `lower_trait()` that build wrappers
- Calls to `lower_hir_ty()` with manual scope/assumptions
- Calls to `lower_impl_trait()` with manual context
- All the wrapper construction functions

## Specific Action Items

### Immediate Investigation Needed
1. ✅ Identify all `lower_*` functions in analysis layer (DONE)
2. ✅ Count usage sites (346 total found, ~20-40 problematic ones in analysis/)
3. ⏳ Wait for Oct 29 transcription to complete for latest context
4. 🔲 Understand why LoC increased (+247) instead of decreased
5. 🔲 Identify specific duplicated scope/assumptions gathering code

### Refactor Strategy
Based on Oct 21 meeting guidance:

1. **Phase 1: TraitDef elimination** (depth-first approach)
   - Move all `TraitDef` methods to `impl Trait` in hir_def/item.rs
   - Make Trait fields private that should be private
   - Update all call sites in analysis crate
   - Delete TraitDef wrapper
   - Verify tests pass

2. **Phase 2: FuncDef elimination**
   - Same process as TraitDef

3. **Phase 3: Implementer elimination**
   - More complex - used in Binder, TyFoldable, etc.
   - May need to use `ImplTrait` directly in those trait impls

4. **Phase 4: Cleanup `lower_*` functions**
   - Convert `lower_hir_ty` to method on `HirTyId`
   - Convert `lower_trait_ref` to method on `TraitRefId`
   - Hide or eliminate public `lower_*` functions

### Files to Focus On

**Primary targets** (where duplicated scope/assumptions gathering likely occurs):
- `crates/hir/src/analysis/ty/def_analysis.rs` (227 lines changed, 20 `lower_*` calls)
- `crates/hir/src/hir_def/item.rs` (+449 lines - check for duplication)
- `crates/hir/src/analysis/ty/trait_def.rs` (326 lines - wrapper to eliminate)
- `crates/hir/src/analysis/ty/func_def.rs` (233 lines - wrapper to eliminate)

**Secondary targets**:
- `crates/hir/src/analysis/ty/ty_lower.rs` (16 calls - contains `lower_hir_ty`)
- `crates/hir/src/analysis/ty/trait_lower.rs` (132 lines - contains `lower_trait_ref`)

## Questions for Oct 29 Review Transcription

When the transcription completes, look for:
1. Why did LoC increase instead of decrease?
2. Are there specific patterns of scope/assumptions duplication identified?
3. What's the current status of wrapper elimination?
4. Are there any blockers or complications discovered?
5. What should be the next concrete step?

## Reference: Example Call Site Pattern

Typical problematic pattern found in `crates/hir/src/hir_def/item.rs`:
```rust
// Around line ~400-500 (in various impl blocks)
.map(|ty| lower_hir_ty(db, ty, self.scope(), assumptions))
```

This appears in:
- `Func::ret_ty()` methods
- `Struct` field type methods
- `ImplTrait` type methods
- Various other HIR item analysis methods

Each one manually gathers scope via `self.scope()` and threads `assumptions` - this is the duplication to eliminate.

---

**Document created**: 2025-11-03
**Branch**: hir-api-rework-realigned-oct-23
**Status**: Awaiting Oct 29 transcription completion
