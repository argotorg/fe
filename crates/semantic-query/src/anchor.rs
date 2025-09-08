use hir_analysis::diagnostics::SpannedHirAnalysisDb;
use hir_analysis::name_resolution::{resolve_with_policy, DomainPreference};

use hir::hir_def::{scope_graph::ScopeId, PathId};

pub(crate) fn anchor_for_scope_match<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    view: &hir::path_view::HirPathAdapter<'db>,
    lazy_path: hir::span::path::LazyPathSpan<'db>,
    p: PathId<'db>,
    s: ScopeId<'db>,
    target_sc: ScopeId<'db>,
) -> hir::span::DynLazySpan<'db> {
    use hir_analysis::ty::trait_resolution::PredicateListId;
    let assumptions = PredicateListId::empty_list(db);
    let tail = p.segment_index(db);
    for i in 0..=tail {
        let seg_path = p.segment(db, i).unwrap_or(p);
        if let Ok(seg_res) =
            resolve_with_policy(db, seg_path, s, assumptions, DomainPreference::Either)
        {
            if seg_res.as_scope(db) == Some(target_sc) {
                let anchor = hir::path_anchor::AnchorPicker::pick_visibility_error(view, i);
                return hir::path_anchor::map_path_anchor_to_dyn_lazy(lazy_path.clone(), anchor);
            }
        }
    }
    let anchor = hir::path_anchor::AnchorPicker::pick_unresolved_tail(view);
    hir::path_anchor::map_path_anchor_to_dyn_lazy(lazy_path.clone(), anchor)
}
