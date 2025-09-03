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

/// Convenience wrapper over PathRes with helper methods for common data access.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Resolution<'db> {
    pub path_res: PathRes<'db>,
}

impl<'db> Resolution<'db> {
    pub fn scope(&self, db: &'db dyn HirAnalysisDb) -> Option<hir::hir_def::scope_graph::ScopeId<'db>> {
        self.path_res.as_scope(db)
    }

    pub fn name_span(&self, db: &'db dyn HirAnalysisDb) -> Option<hir::span::DynLazySpan<'db>> {
        self.path_res.name_span(db)
    }

    pub fn kind_name(&self) -> &'static str { self.path_res.kind_name() }

    pub fn pretty_path(&self, db: &'db dyn HirAnalysisDb) -> Option<String> { self.path_res.pretty_path(db) }
}

/// Variant of `resolve_with_policy` returning a richer Resolution wrapper.
pub fn resolve_with_policy_ex<'db>(
    db: &'db dyn HirAnalysisDb,
    path: hir::hir_def::PathId<'db>,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    pref: DomainPreference,
) -> Result<Resolution<'db>, PathResError<'db>> {
    resolve_with_policy(db, path, scope, assumptions, pref).map(|path_res| Resolution { path_res })
}
