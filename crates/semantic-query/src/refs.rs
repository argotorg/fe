use hir_analysis::diagnostics::SpannedHirAnalysisDb;
use hir_analysis::ty::{func_def::FuncDef, trait_resolution::PredicateListId};

use hir::hir_def::{scope_graph::ScopeId, IdentId, ItemKind, TopLevelMod};

pub(crate) fn implementing_methods_for_trait_method<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    fd: FuncDef<'db>,
) -> Vec<FuncDef<'db>> {
    let Some(func) = fd.hir_func_def(db) else {
        return Vec::new();
    };
    let Some(parent) = func.scope().parent(db) else {
        return Vec::new();
    };
    let trait_item = match parent {
        ScopeId::Item(ItemKind::Trait(t)) => t,
        _ => return Vec::new(),
    };
    let name: IdentId<'db> = fd.name(db);
    let assumptions = PredicateListId::empty_list(db);
    let mut out = Vec::new();
    for it in top_mod.all_impl_traits(db) {
        let Some(tr_ref) = it.trait_ref(db).to_opt() else {
            continue;
        };
        let hir::hir_def::Partial::Present(path) = tr_ref.path(db) else {
            continue;
        };
        let Ok(hir_analysis::name_resolution::PathRes::Trait(tr_inst)) =
            hir_analysis::name_resolution::resolve_with_policy(
                db,
                path,
                it.scope(),
                assumptions,
                hir_analysis::name_resolution::DomainPreference::Type,
            )
        else {
            continue;
        };
        if tr_inst.def(db).trait_(db) != trait_item {
            continue;
        }
        for child in it.children_non_nested(db) {
            if let ItemKind::Func(impl_fn) = child {
                if impl_fn.name(db).to_opt() == Some(name) {
                    if let Some(fd2) = hir_analysis::ty::func_def::lower_func(db, impl_fn) {
                        out.push(fd2);
                    }
                }
            }
        }
    }
    out
}

