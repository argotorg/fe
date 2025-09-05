use hir_analysis::diagnostics::SpannedHirAnalysisDb;
use hir_analysis::ty::{trait_resolution::PredicateListId, func_def::FuncDef};

use hir::span::DynLazySpan;
use hir::source_index::unified_occurrence_rangemap_for_top_mod;
use hir::hir_def::{IdentId, TopLevelMod, ItemKind, scope_graph::ScopeId};

use crate::anchor::anchor_for_scope_match;
use crate::util::enclosing_func;

pub fn implementing_methods_for_trait_method<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    fd: FuncDef<'db>,
) -> Vec<FuncDef<'db>> {
    let Some(func) = fd.hir_func_def(db) else { return Vec::new() };
    let Some(parent) = func.scope().parent(db) else { return Vec::new() };
    let trait_item = match parent { ScopeId::Item(ItemKind::Trait(t)) => t, _ => return Vec::new() };
    let name: IdentId<'db> = fd.name(db);
    let assumptions = PredicateListId::empty_list(db);
    let mut out = Vec::new();
    for it in top_mod.all_impl_traits(db) {
        let Some(tr_ref) = it.trait_ref(db).to_opt() else { continue };
        let hir::hir_def::Partial::Present(path) = tr_ref.path(db) else { continue };
        let Ok(hir_analysis::name_resolution::PathRes::Trait(tr_inst)) = hir_analysis::name_resolution::resolve_with_policy(
            db,
            path,
            it.scope(),
            assumptions,
            hir_analysis::name_resolution::DomainPreference::Type,
        ) else { continue };
        if tr_inst.def(db).trait_(db) != trait_item { continue; }
        for child in it.children_non_nested(db) {
            if let ItemKind::Func(impl_fn) = child {
                if impl_fn.name(db).to_opt() == Some(name) {
                    if let Some(fd2) = hir_analysis::ty::func_def::lower_func(db, impl_fn) { out.push(fd2); }
                }
            }
        }
    }
    out
}

pub fn method_refs_in_mod<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    fd: FuncDef<'db>,
) -> Vec<DynLazySpan<'db>> {
    let mut out: Vec<DynLazySpan<'db>> = Vec::new();

    // Method calls by typed identity
    for occ in unified_occurrence_rangemap_for_top_mod(db, top_mod).iter() {
        if let hir::source_index::OccurrencePayload::MethodName { scope, body, receiver, ident, span: name_span } = &occ.payload {
            if let Some(func) = enclosing_func(db, body.scope()) {
                if let Some(cand) = crate::util::resolve_method_call(db, func, *receiver, *ident, *scope) {
                    if cand == fd { out.push(name_span.clone()); }
                }
            }
        }
    }

    // UFCS/associated paths resolving to the same method or its scope
    let func_scope = fd.scope(db);
    let assumptions = PredicateListId::empty_list(db);
    for occ in unified_occurrence_rangemap_for_top_mod(db, top_mod).iter() {
        let (p, s, path_lazy) = match &occ.payload {
            hir::source_index::OccurrencePayload::PathSeg { path, scope, path_lazy, .. } => (*path, *scope, path_lazy.clone()),
            _ => continue,
        };
        let Ok(res) = hir_analysis::name_resolution::resolve_with_policy(db, p, s, assumptions, hir_analysis::name_resolution::DomainPreference::Either) else { continue };
        let matches_fd = match hir_analysis::name_resolution::method_func_def_from_res(&res) {
            Some(mfd) => mfd == fd,
            None => false,
        };
        if matches_fd || res.as_scope(db) == Some(func_scope) {
            let view = hir::path_view::HirPathAdapter::new(db, p);
            let span = if res.as_scope(db) == Some(func_scope) {
                anchor_for_scope_match(db, &view, path_lazy.clone(), p, s, func_scope)
            } else {
                let anchor = hir::path_anchor::AnchorPicker::pick_unresolved_tail(&view);
                hir::path_anchor::map_path_anchor_to_dyn_lazy(path_lazy.clone(), anchor)
            };
            out.push(span);
        }
    }
    out
}
