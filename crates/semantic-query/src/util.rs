use hir::hir_def::{ItemKind, scope_graph::ScopeId};
use hir::SpannedHirDb;
use hir_analysis::diagnostics::SpannedHirAnalysisDb;
use hir_analysis::ty::func_def::FuncDef;
use hir_analysis::ty::trait_resolution::PredicateListId;

pub(crate) fn enclosing_func<'db>(
    db: &'db dyn SpannedHirDb,
    mut scope: ScopeId<'db>,
) -> Option<hir::hir_def::item::Func<'db>> {
    for _ in 0..16 {
        if let Some(item) = scope.to_item() {
            if let ItemKind::Func(f) = item { return Some(f); }
        }
        if let Some(parent) = scope.parent(db) { scope = parent; } else { break; }
    }
    None
}

pub(crate) fn resolve_method_call<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    func: hir::hir_def::item::Func<'db>,
    receiver: hir::hir_def::ExprId,
    method_name: hir::hir_def::IdentId<'db>,
    scope: ScopeId<'db>,
) -> Option<FuncDef<'db>> {
    use hir_analysis::ty::{ty_check::check_func_body, canonical::Canonical};
    let (_diags, typed) = check_func_body(db, func).clone();
    let recv_ty = typed.expr_prop(db, receiver).ty;
    let assumptions = PredicateListId::empty_list(db);
    hir_analysis::name_resolution::find_method_id(
        db,
        Canonical::new(db, recv_ty),
        method_name,
        scope,
        assumptions,
    )
}
