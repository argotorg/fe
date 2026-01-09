use hir::{
    analysis::HirAnalysisDb,
    analysis::ty::ty_def::TyId,
    hir_def::{Func, HirIngot, scope_graph::ScopeId},
};

/// Returns a scope suitable for normalizing types under the given generic arguments.
pub(crate) fn normalization_scope_for_args<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    args: &[TyId<'db>],
) -> ScopeId<'db> {
    args.iter()
        .find_map(|ty| ty.ingot(db))
        .map(|ingot| ingot.root_mod(db).scope())
        .unwrap_or_else(|| func.scope())
}
