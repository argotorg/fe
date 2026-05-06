use common::ingot::Ingot;
use hir::core::semantic::symbol::ReferenceIndex;
use hir::hir_def::HirIngot;

use crate::extract::DocExtractor;

/// Layer 2: per-ingot reference index, assembled from Layer 1 per-module data.
/// Salsa caches this — builtins compute once and never again.
#[salsa::tracked(return_ref)]
pub fn ingot_references<'db>(
    db: &'db dyn hir::analysis::HirAnalysisDb,
    ingot: Ingot<'db>,
) -> ReferenceIndex<'db> {
    hir::core::semantic::index::build_reference_index_from_cached(db, ingot)
}

/// Per-ingot doc extraction, cached by salsa.
/// Builtins never recompute (their inputs never change).
#[salsa::tracked(return_ref)]
pub fn docs_for_ingot<'db>(
    db: &'db dyn hir::SpannedHirDb,
    ingot: Ingot<'db>,
) -> Vec<fe_web::model::DocItem> {
    let extractor = DocExtractor::new(db);
    let mut items = Vec::new();

    for top_mod in ingot.all_modules(db) {
        for item in top_mod.children_nested(db) {
            if let Some(doc_item) = extractor.extract_item_for_ingot(item, ingot) {
                items.push(doc_item);
            }
        }
    }

    items
}

/// Per-ingot module tree extraction, cached by salsa.
#[salsa::tracked(return_ref)]
pub fn module_tree_for_ingot<'db>(
    db: &'db dyn hir::SpannedHirDb,
    ingot: Ingot<'db>,
) -> Vec<fe_web::model::DocModuleTree> {
    let extractor = DocExtractor::new(db);
    let root_mod = ingot.root_mod(db);
    extractor.build_module_tree_for_ingot(ingot, root_mod)
}

/// Per-ingot trait impl links, cached by salsa.
#[salsa::tracked(return_ref)]
pub fn trait_impl_links_for_ingot<'db>(
    db: &'db dyn hir::SpannedHirDb,
    ingot: Ingot<'db>,
) -> Vec<(String, fe_web::model::DocTraitImpl)> {
    let extractor = DocExtractor::new(db);
    extractor.extract_trait_impl_links(ingot)
}
