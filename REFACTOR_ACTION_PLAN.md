# HIR Refactor Action Plan

**Date**: 2025-11-03 (Updated after Oct 29 review)
**Branch**: `hir-api-rework-realigned-oct-23`
**Status**: New API exists, old API needs deletion

---

## The Core Strategy (Survives Compaction)

### The Break-Then-Fix Pattern

**Sean's recommended approach** (the `raw_ty` example):

1. **Make fields private** to break external usage
2. **Let compilation errors guide you** to call sites
3. **Replace old patterns with new API** at each error site
4. **Delete redundant code** that becomes obsolete
5. **Discover opportunities** for additional API methods

This is NOT just about making things compile - it's about using the compiler as a guide to systematically migrate the codebase.

### Why This Works

- **Compiler finds all usage sites** - no manual searching needed
- **Forces complete migration** - can't leave old patterns around
- **Reveals redundant code** - external analysis logic becomes obviously unnecessary
- **Exposes API gaps** - if something is hard to migrate, you need a better method

---

## Current State (Oct 29 Review Findings)

**Problem**: New methods exist on HIR items, but old code paths still exist too
- Result: +247 LoC instead of expected deletion
- Root cause: Didn't complete the "delete old patterns" step

**What needs to happen**:
- Make internal fields private (force breakage)
- Fix compilation errors by using new methods
- Delete now-redundant external analysis code
- Achieve net LoC deletion

---

## Immediate Action Items (Priority Order)

### 1. Make `raw_ty` Fields Private (START HERE)

**Files**: `crates/hir/src/hir_def/item.rs`

**Change**:
```rust
// From:
#[salsa::tracked]
pub struct ImplTrait<'db> {
    pub(crate) raw_ty: Option<TypeId<'db>>,  // ← Currently accessible
    // ...
}

// To:
#[salsa::tracked]
pub struct ImplTrait<'db> {
    raw_ty: Option<TypeId<'db>>,  // ← Now private
    // ...
}
```

**Expected breakages**: Anywhere that accesses `impl_trait.raw_ty(db)` directly

**How to fix each breakage**:
```rust
// Old pattern (BREAKS after making raw_ty private):
let ty = impl_trait.raw_ty(db);
let scope = impl_trait.scope();
let assumptions = collect_constraints(db, impl_trait.into()).instantiate_identity();
let analyzed_ty = ty.map(|t| lower_hir_ty(db, t, scope, assumptions))
    .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other));

// New pattern (USE THIS):
let analyzed_ty = impl_trait.self_ty(db);
```

**Result**: Delete 3-5 lines of boilerplate at each call site, replace with 1 line

**Apply to all HIR items**: `Func`, `Struct`, `Trait`, etc. - any with `raw_ty` or similar fields

**Important**: After fixing all breakages, you may need to relax the visibility back to `pub(crate)`:
- If HIR visitors (in `visitor.rs`) need raw syntax access, make the field `pub(crate)`
- The pattern: visitors traverse syntax trees (need `raw_ty`), analysis uses lowered types (uses `.ty()` method)
- Two legitimate use cases: syntax traversal vs semantic analysis
- Start with fully private, see what breaks, then adjust visibility as needed

---

### 2. Focus on `def_analysis.rs` for Deletions

**File**: `crates/hir/src/analysis/ty/def_analysis.rs`

**What Sean meant**: This file heavily uses the OLD API patterns. It's not just about deleting redundant code - it's about:

1. **Replacing old API usage** with new methods
2. **Discovering what external analysis shouldn't exist** anymore
3. **Finding opportunities** for new methods if the migration is awkward

#### Complete Inventory of Manual Lowering Calls

**`lower_hir_ty` calls (8 total):**

1. **Line 149** - `analyze_trait`: Lowering trait associated type defaults
   - Context: `assoc_type.default` (from `trait_.types(db)`)
   - **Action**: Create `AssocTyDecl::default_ty(db) -> Option<TyId>`
   - **Difficulty**: Easy - standalone trait associated types
   - **Impact**: Medium - eliminates boilerplate in trait analysis

2. **Line 362** - `DefAnalyzer::verify_term_type_kind`: Internal validation helper
   - Context: Checking if a type has `*` kind and is not const
   - **Action**: Keep as-is (internal helper method) OR create HIR type method
   - **Difficulty**: Low priority - already encapsulated in DefAnalyzer
   - **Impact**: Low - internal implementation detail

3. **Line 380** - `DefAnalyzer::verify_self_type`: Impl self type validation
   - Context: Verifying self type matches in impl blocks
   - **Action**: Keep as-is (internal helper) OR inline into caller
   - **Difficulty**: Low priority - already encapsulated
   - **Impact**: Low - internal validation logic

4. **Line 542** - `DefAnalyzer::visit_ty`: Visitor callback for type checking
   - Context: Visitor pattern - checking every type node for well-formedness
   - **Action**: Cannot eliminate - this IS the lowering point for visiting
   - **Difficulty**: N/A - fundamental visitor behavior
   - **Impact**: None - keep as-is

5. **Line 572** - `DefAnalyzer::visit_where_predicate`: Where clause validation
   - Context: Visitor pattern - validating where clause predicates
   - **Action**: Cannot eliminate - visitor needs to lower for validation
   - **Difficulty**: N/A - fundamental visitor behavior
   - **Impact**: None - keep as-is

6. **Line 612** - `DefAnalyzer::visit_field_def`: Field vs const param type matching
   - Context: Checking if field type matches const type parameter
   - **Action**: Keep as-is - visitor needs both types for comparison
   - **Difficulty**: Low priority - specialized check
   - **Impact**: Low - internal validation logic

7. **Line 698** - `DefAnalyzer::visit_generic_param`: Generic default forward ref check
   - Context: Ensuring generic defaults don't reference later params
   - **Action**: Possibly create `GenericParam::default_ty(db)` method
   - **Difficulty**: Medium - generic param handling
   - **Impact**: Low-Medium - could simplify but complex logic remains

8. **Line 885** - `DefAnalyzer::visit_impl_trait`: ImplTrait assoc type definitions
   - Context: Lowering `impl_trait.types()` for well-formedness checking
   - **Action**: Create `AssocTyDef::ty_lowered(db) -> TyId` method
   - **Difficulty**: Easy-Medium - similar to AssocTyDecl
   - **Impact**: Medium - cleaner impl trait handling

**`lower_trait_ref` calls (5 total):**

1. **Line 158** - `analyze_trait`: Trait assoc type default bounds checking
   - Context: Checking if default type satisfies trait bounds
   - **Action**: Possibly create method on AssocTyDecl
   - **Difficulty**: Medium - bound checking logic
   - **Impact**: Medium - part of trait analysis

2. **Line 803** - `DefAnalyzer::visit_where_predicate`: Where clause trait bound lowering
   - Context: Visitor lowering trait bounds in where clauses
   - **Action**: Cannot eliminate - visitor needs this
   - **Difficulty**: N/A - fundamental visitor behavior
   - **Impact**: None - keep as-is

3. **Line 1107** - `check_trait_ref_wf`: Standalone trait ref well-formedness check
   - Context: Helper function checking trait reference validity
   - **Action**: Could be method on some HIR type (need to investigate caller)
   - **Difficulty**: Medium - needs analysis of usage
   - **Impact**: Low-Medium - helper function

4. **Line 1232** - `analyze_impl_trait_specific_error`: ImplTrait trait lowering ⭐
   - Context: **THE FUNCTION SEAN WANTS TO DELETE/SHRINK**
   - **Action**: Create `ImplTrait::trait_inst(db) -> Result<TraitInstId, TraitRefLowerError>`
   - **Difficulty**: High priority - big LoC savings opportunity
   - **Impact**: **HIGH** - Sean specifically called this out

5. **Line 1347** - `analyze_impl_trait_specific_error`: Checking impl assoc type bounds
   - Context: Inside the function Sean wants to shrink
   - **Action**: Part of the above - may become method or stay internal
   - **Difficulty**: Medium - bound checking
   - **Impact**: Medium - within targeted function

#### Priority Order for Elimination

**🔥 High Priority (Start Here):**

1. **Line 1232** - `analyze_impl_trait_specific_error`: Create `ImplTrait::trait_inst(db)`
   - Sean explicitly called out this function for deletion/shrinking
   - High LoC deletion potential

2. **Line 149** - Trait associated type defaults: Create `AssocTyDecl::default_ty(db)`
   - Clean standalone case
   - Immediate LoC savings

3. **Line 885** - ImplTrait assoc types: Create `AssocTyDef::ty_lowered(db)`
   - Similar pattern to above
   - Clean API improvement

**⚠️ Medium Priority:**

4. **Line 158** - Trait bound checking for assoc type defaults
   - Part of trait analysis logic
   - Could be method or keep as helper

5. **Line 698** - Generic param defaults: Create `GenericParam::default_ty(db)`
   - Some complexity in forward ref checking
   - Moderate improvement

6. **Line 1107** - `check_trait_ref_wf`: Investigate callers and context
   - Might be eliminable depending on usage

**✅ Keep As-Is (Low/No Priority):**

7. Lines 362, 380, 612 - Internal DefAnalyzer helpers
   - Already encapsulated within analyzer
   - Low value in moving to HIR items

8. Lines 542, 572, 803 - Visitor pattern lowering
   - Fundamental visitor behavior
   - Cannot/should not eliminate

**Pattern to look for**:
```rust
// Functions that do external analysis of HIR items
fn analyze_impl_trait_specific_error(
    db: &dyn HirAnalysisDb,
    impl_trait: ImplTrait,
) -> ... {
    let assumptions = collect_constraints(db, impl_trait.into()).instantiate_identity();
    let scope = impl_trait.scope();
    let self_ty = impl_trait.raw_ty(db)  // ← This breaks after step 1
        .map(|ty| lower_hir_ty(db, ty, scope, assumptions))
        .unwrap_or_else(...);
    // ... more external analysis
}
```

**Questions to ask**:
- Is this logic now in a method? → Delete the function, use the method
- Is this logic needed but no method exists? → Create the method
- Can call sites just use `impl_trait.self_ty(db)` directly? → Delete the function entirely

**Expected outcome**: Significant LoC deletion as external analysis functions become obsolete

---

### 3. Eliminate `TraitImplData` (Sean's Specific Strategy)

**The Pattern Sean Described**:

**Current State**:
```rust
#[salsa::interned]
pub(crate) struct TraitImplData<'db> {
    pub self_ty: TyId<'db>,
    pub trait_inst: Option<TraitInstId<'db>>,
    pub constraints: PredicateListId<'db>,
}

impl ImplTrait {
    #[salsa::tracked]
    pub(crate) fn trait_impl_data(self, db: ...) -> TraitImplData {
        // Complex analysis logic here
    }
}

// Usage:
let self_ty = impl_trait.trait_impl_data(db).self_ty(db);
```

**Sean's Approach - "Cut and Paste"**:

> "Like, there's, there's no real reason to bundle it together into this one struct... A method to get the params. A method to get the trait against. That's basically it... Self-ty."

> "Every place where there's a comment that says, don't need to cache the query... You just, you just cut out that block of code... paste it in there."

**Step-by-step**:

1. **For each field in `TraitImplData`**, create a direct method on `ImplTrait`:

```rust
impl ImplTrait {
    #[salsa::tracked]  // Salsa caches this automatically!
    pub fn self_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        // Cut the self_ty logic from trait_impl_data and paste here
        let scope = self.scope();
        let assumptions = collect_constraints(db, self.into()).instantiate_identity();
        self.raw_ty(db)
            .map(|ty| lower_hir_ty(db, ty, scope, assumptions))
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
    }

    #[salsa::tracked]
    pub fn trait_inst(self, db: &'db dyn HirAnalysisDb) -> Option<TraitInstId<'db>> {
        // Cut the trait_inst logic and paste here
        let scope = self.scope();
        let assumptions = collect_constraints(db, self.into()).instantiate_identity();
        let self_ty = self.self_ty(db);  // Can call our other method!
        self.trait_ref(db).to_opt()
            .and_then(|tr| lower_trait_ref(db, self_ty, tr, scope, assumptions).ok())
    }

    // Similar for constraints, params, etc.
}
```

2. **Update call sites**:
```rust
// Old:
let data = impl_trait.trait_impl_data(db);
let self_ty = data.self_ty(db);
let trait_inst = data.trait_inst(db);

// New:
let self_ty = impl_trait.self_ty(db);
let trait_inst = impl_trait.trait_inst(db);
```

3. **Delete `TraitImplData`** struct entirely

4. **Delete `trait_impl_data()` query**

**Key Insight from Oct 29**:
> "like, this instantiate identity thing is just, like, an unwrap, like a unbind or something... I think this is all just fine to split it up."

The "atomic instantiation" concern from the briefing was overcautious - the fields don't need bundling for correctness.

---

### 4. Search and Replace `lower_*` Patterns

**Files to audit**: Anywhere calling these functions

**Pattern search**:
```bash
# Find manual scope/assumptions threading
rg 'lower_hir_ty.*scope.*assumptions' --type rust

# Find all lower_hir_ty calls
rg 'lower_hir_ty\(' --type rust

# Find all lower_trait_ref calls
rg 'lower_trait_ref\(' --type rust
```

**Replacement strategy**:
- If called on a HIR item field → use item's method instead
- If called standalone → check if the item has a method that does this
- If no method exists → consider creating one

**Goal**: Most `lower_hir_ty` calls should disappear from high-level code. They should only exist:
- Inside HIR item methods (internal implementation)
- In low-level analysis utilities

---

## Secondary Actions (After Primary Items Complete)

### 5. Rename `TypeId` → `TypeRef`

**Why**: Confusion with `TyId` (semantic type)

**Scope**: Mechanical rename throughout codebase

**When**: After step 1-4 are done and tests pass

### 6. Clean Up Visibility

**Pattern**:
```rust
// Make truly internal things private (no pub at all)
pub(crate) raw_methods: Vec<Func<'db>>  // ← Change to:
raw_methods: Vec<Func<'db>>             // ← Private

// Public API stays public
pub fn methods(self, db: ...) -> ...    // ← Keep public
```

**Rationale**: Since this is a mega-crate, `pub(crate)` doesn't enforce boundaries. Use `private` for module-level encapsulation.

### 7. Verify Net LoC Deletion

**Success metric** from Sean:
> "And ideally that'll be a, a nice diff where we're deleting some lines of code."

**Check**:
```bash
git diff hir-api-rework-BASE..HEAD --stat
```

Should see negative net lines in analysis-heavy files.

---

## What NOT to Do

### ❌ Don't Just Delegate

```rust
// BAD - Just wrapping old function
impl Trait {
    pub fn methods(self, db: ...) -> ... {
        trait_methods(db, self)  // Still calling old pattern!
    }
}
```

**Why bad**: Adds indirection, doesn't eliminate old code

### ❌ Don't Leave Both Patterns

```rust
// BAD - Coexistence
pub fn trait_methods(...) { }  // Old standalone function
impl Trait {
    pub fn methods(...) { }     // New method
}
```

**Why bad**: Confusion, duplication, unclear which to use

**Do instead**: Delete the old standalone function entirely

### ❌ Don't Try to Preserve `TraitImplData` as `pub(crate)`

**Sean's verdict**: Eliminate it entirely by splitting into direct methods

**Why**: The bundling isn't needed - each field can be its own cached method

---

## Validation Checklist (Before Calling It Done)

- [ ] `raw_ty` and similar fields are private across all HIR items
- [ ] All compilation errors from step 1 have been fixed
- [ ] `def_analysis.rs` has significant LoC reduction
- [ ] `TraitImplData` struct is deleted
- [ ] `trait_impl_data()` query is deleted
- [ ] Search for `lower_hir_ty` shows mostly internal usage
- [ ] No standalone `lower_trait`, `lower_impl_trait`, etc. functions remain
- [ ] All tests pass
- [ ] Net LoC deletion achieved (check with `git diff --stat`)

---

## Next Phase (After Item-Level Complete)

### Scope-Level Children

From both Oct 21 and Oct 29:
> "get, get the first level into a presentable state... And then proceed on."

**What this means**:
- Function parameters
- Struct fields
- Generic parameters
- Associated types

**Same pattern applies**: Make parent item fields private, use methods to access children with full context.

**Defer until**: Item-level wrappers are eliminated and tests pass with net LoC deletion.

---

## Key Principles (The Philosophy)

### 1. The Compiler is Your Guide

Don't hunt for usage manually - make things private and let compilation errors show you where to update.

### 2. Delete, Don't Accumulate

Every migration should result in LoC deletion. If it doesn't, you're adding indirection instead of eliminating it.

### 3. Methods Over Functions

```rust
// Old: Functions that take items as parameters
fn analyze(db: &dyn Db, item: Item, scope: Scope, assumptions: Assumptions) -> Result

// New: Methods on items that encapsulate context
impl Item {
    fn analyze(self, db: &dyn Db) -> Result  // Context is self.scope(), self.assumptions()
}
```

### 4. Cut and Paste, Not Reorganize

When Sean says "cut out that block of code, paste it in there", he means:
- **Literally move the logic** from external functions into methods
- **Don't abstract or reorganize** - just relocate
- **Salsa handles caching** - you don't need to manually cache in structs

### 5. Trust Salsa

Don't pre-compute and materialize results in tracked structs. Let methods compute on-demand - Salsa caches them automatically.

---

## Communication Pattern

When reporting progress:

**Good**: "Made `raw_ty` private in `ImplTrait`, fixed 12 call sites in `def_analysis.rs`, deleted `analyze_impl_trait_specific_error` function. Net -47 LoC."

**Bad**: "Worked on ImplTrait, made some changes, tests pass."

**Focus on**:
- What broke (made private)
- How many places needed fixing
- What got deleted
- Net LoC change

This makes progress tangible and verifiable.

---

## The End Goal (Sean's Vision)

> "I'm imagining like a you know kind of top down hierarchy of objects um that represent the source code... from that object you can dig downward... or you can climb upward basically"

**For users of the API** (LSP, compiler driver, future contributors):

```rust
// Instead of this mess:
let scope = impl_trait.scope();
let assumptions = collect_constraints(db, impl_trait.into()).instantiate_identity();
let ty = impl_trait.raw_ty(db)
    .map(|t| lower_hir_ty(db, t, scope, assumptions))
    .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other));

// Just this:
let ty = impl_trait.self_ty(db);
```

**That's the entire point.**

---

**Last Updated**: 2025-11-03
**Based On**: Oct 29 review with Sean + Oct 21 planning session
