use hir::hir_def::{scope_graph::ScopeId, IdentId};

use crate::{
    name_resolution::{
        method_selection::{select_method_candidate, MethodCandidate},
        PathRes,
    },
    ty::{func_def::FuncDef, trait_resolution::PredicateListId},
    HirAnalysisDb,
};

/// High-level façade for method lookup. Wraps the low-level selector and
/// provides a stable API for consumers.
/// Returns the function definition of the selected method if resolution succeeds.
pub fn find_method_id<'db>(
    db: &'db dyn HirAnalysisDb,
    receiver_ty: crate::ty::canonical::Canonical<crate::ty::ty_def::TyId<'db>>,
    method_name: IdentId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<FuncDef<'db>> {
    match select_method_candidate(db, receiver_ty, method_name, scope, assumptions) {
        Ok(MethodCandidate::InherentMethod(fd)) => Some(fd),
        Ok(MethodCandidate::TraitMethod(tm)) | Ok(MethodCandidate::NeedsConfirmation(tm)) => {
            Some(tm.method.0)
        }
        Err(_) => None,
    }
}

/// Extract the underlying function definition for a resolved method PathRes.
/// Returns None if the PathRes is not a method.
pub fn method_func_def_from_res<'db>(
    res: &crate::name_resolution::PathRes<'db>,
) -> Option<FuncDef<'db>> {
    match res {
        PathRes::Method(_, cand) => match cand {
            MethodCandidate::InherentMethod(fd) => Some(*fd),
            MethodCandidate::TraitMethod(tm) | MethodCandidate::NeedsConfirmation(tm) => {
                Some(tm.method.0)
            }
        },
        _ => None,
    }
}
