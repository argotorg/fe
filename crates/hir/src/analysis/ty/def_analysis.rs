//! Definition analysis entrypoints that delegate to the semantic traversal API.
//! The legacy visitor-based analyzer has been removed. Keep only slim helpers
//! and public entrypoints that compose semantic diagnostics.

use rustc_hash::FxHashMap;
use smallvec1::SmallVec;

use crate::analysis::HirAnalysisDb;
use crate::core::semantic::Diagnosable;
use crate::hir_def::{Func, IdentId, Impl as HirImpl, ImplTrait, Trait};

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
    trait_.diags(db)
}

/// Analyze inherent impl via semantic views.
#[salsa::tracked(return_ref)]
pub fn analyze_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_: HirImpl<'db>,
) -> Vec<TyDiagCollection<'db>> {
    impl_.diags(db)
}

/// Analyze function definition via semantic views + generic-param diags.
#[salsa::tracked(return_ref)]
pub fn analyze_func<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Vec<TyDiagCollection<'db>> {
    func.diags(db)
}

/// Analyze trait impl definition. Keeps early error handling and method conformance
/// glue while most checks are performed by semantic views.
#[salsa::tracked(return_ref)]
pub fn analyze_impl_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Vec<TyDiagCollection<'db>> {
    impl_trait.diags(db)
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
