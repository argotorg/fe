//! Definition analysis entrypoints that delegate to the semantic traversal API.
//! The legacy visitor-based analyzer has been removed. Keep only slim helpers
//! and public entrypoints that compose semantic diagnostics.

use rustc_hash::FxHashMap;
use smallvec1::SmallVec;

use crate::analysis::HirAnalysisDb;
use crate::hir_def::{Func, GenericParamOwner, IdentId, Impl as HirImpl, ImplTrait, Trait};

use super::{
    adt_def::{AdtDef, AdtRef},
    diagnostics::TyDiagCollection,
};

/// Analyze structs/enums/contracts via semantic views.
#[salsa::tracked(return_ref)]
pub fn analyze_adt<'db>(
    db: &'db dyn HirAnalysisDb,
    adt_ref: AdtRef<'db>,
) -> Vec<TyDiagCollection<'db>> {
    match adt_ref {
        AdtRef::Struct(s) => s.analyze(db),
        AdtRef::Enum(e) => e.analyze(db),
        AdtRef::Contract(c) => c.analyze(db),
    }
}

/// Analyze trait definition via semantic views.
#[salsa::tracked(return_ref)]
pub fn analyze_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: Trait<'db>,
) -> Vec<TyDiagCollection<'db>> {
    trait_.analyze(db)
}

/// Analyze inherent impl via semantic views.
#[salsa::tracked(return_ref)]
pub fn analyze_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_: HirImpl<'db>,
) -> Vec<TyDiagCollection<'db>> {
    impl_.analyze(db)
}

/// Analyze function definition via semantic views + generic-param diags.
#[salsa::tracked(return_ref)]
pub fn analyze_func<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let mut diags = func.analyze(db);
    let owner = GenericParamOwner::Func(func);
    diags.extend(owner.diags_const_param_types(db));
    diags.extend(owner.diags_params_defined_in_parent(db));
    diags.extend(owner.diags_kind_bounds(db));
    diags.extend(owner.diags_trait_bounds(db));
    diags.extend(owner.diags_non_trailing_defaults(db));
    diags.extend(owner.diags_default_forward_refs(db));
    diags
}

/// Analyze trait impl definition. Keeps early error handling and method conformance
/// glue while most checks are performed by semantic views.
#[salsa::tracked(return_ref)]
pub fn analyze_impl_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Vec<TyDiagCollection<'db>> {
    // Early path/domain/WF checks; bail out on errors to avoid noisy follow-ups
    let (implementor_opt, validity_diags) = impl_trait.diags_implementor_validity(db);
    
    let Some(implementor) = implementor_opt else {
        return validity_diags;
    };

    let mut diags = validity_diags;

    // Method conformance diagnostics
    diags.extend(impl_trait.diags_method_conformance(db, implementor));

    // Trait-ref WF and super-trait constraints
    diags.extend(impl_trait.diags_trait_ref_and_wf(db));

    // Associated types diagnostics (WF + presence + bounds)
    diags.extend(impl_trait.diags_assoc_types_wf(db));
    diags.extend(impl_trait.diags_missing_assoc_types(db));
    diags.extend(impl_trait.diags_assoc_types_bounds(db));

    // Generic parameter diagnostics
    let owner = GenericParamOwner::ImplTrait(impl_trait);
    diags.extend(owner.diags_params_defined_in_parent(db));
    diags.extend(owner.diags_kind_bounds(db));
    diags.extend(owner.diags_non_trailing_defaults(db));
    diags.extend(owner.diags_default_forward_refs(db));
    diags.extend(owner.diags_trait_bounds(db));

    diags
}

/// Shared helper for duplicate name diagnostics used by semantic views.
pub(crate) fn check_duplicate_names<'db, F>(
    names: impl Iterator<Item = Option<IdentId<'db>>>,
    create_diag: F,
) -> SmallVec<[TyDiagCollection<'db>; 2]>
where
    F: Fn(SmallVec<[u16; 4]>) -> TyDiagCollection<'db>,
{
    let mut defs = FxHashMap::<IdentId<'db>, SmallVec<[u16; 4]>>::default();
    for (i, name) in names.enumerate() {
        if let Some(name) = name {
            defs.entry(name).or_default().push(i as u16);
        }
    }
    defs.into_iter()
        .filter_map(|(_name, idxs)| (idxs.len() > 1).then_some(create_diag(idxs)))
        .collect()
}

// Minimal public struct retained for downstream diagnostics that refer to cycle members.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AdtCycleMember<'db> {
    pub adt: AdtDef<'db>,
    pub field_idx: u16,
    pub ty_idx: u16,
}
