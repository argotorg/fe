use hir::hir_def::scope_graph::ScopeId;
use hir::hir_def::PathId;

use crate::ty::trait_resolution::PredicateListId;
use crate::{
    name_resolution::{resolve_path, PathRes, PathResError},
    HirAnalysisDb,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DomainPreference {
    Value,
    Type,
    Either,
}

/// Thin facade over `resolve_path` that hides the boolean tail-domain flag
/// and allows callers to express intent declaratively.
pub fn resolve_with_policy<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    pref: DomainPreference,
) -> Result<PathRes<'db>, PathResError<'db>> {
    match pref {
        DomainPreference::Value => resolve_path(db, path, scope, assumptions, true),
        DomainPreference::Type => resolve_path(db, path, scope, assumptions, false),
        DomainPreference::Either => {
            // Try value first, then type.
            match resolve_path(db, path, scope, assumptions, true) {
                ok @ Ok(_) => ok,
                Err(_) => resolve_path(db, path, scope, assumptions, false),
            }
        }
    }
}
