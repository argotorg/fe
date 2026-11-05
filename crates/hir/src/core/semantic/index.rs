//! Per-module semantic index queries, cached by salsa.
//!
//! Layer 1: per-module reference data that downstream consumers compose into
//! per-ingot outputs. Salsa caches each module independently.

use common::ingot::Ingot;

use crate::analysis::HirAnalysisDb;
use crate::core::semantic::reference::resolver::resolved_item_scope_targets;
use crate::core::semantic::symbol::{IndexedReference, ReferenceIndex};
use crate::hir_def::{HirIngot, TopLevelMod, scope_graph::ScopeId};
use crate::span::DynLazySpan;

use rayon::prelude::*;
use rustc_hash::FxHashMap;

/// A single resolved reference: target scope + source span + metadata.
#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct ResolvedRef<'db> {
    pub target: ScopeId<'db>,
    pub span: DynLazySpan<'db>,
    pub is_self_ty: bool,
}

/// Per-module: all references originating from this module's items.
/// Cached by salsa — only recomputes when this module's resolved targets change.
#[salsa::tracked]
pub fn module_references<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<ResolvedRef<'db>> {
    let mut refs = Vec::new();

    for &item in top_mod.all_items(db) {
        for resolved in resolved_item_scope_targets(db, item) {
            refs.push(ResolvedRef {
                target: resolved.scope,
                span: resolved.span.clone(),
                is_self_ty: resolved.is_self_ty,
            });
        }
    }

    refs
}

/// Pre-warm the per-module reference cache in parallel.
///
/// Must be called outside any salsa tracked query: each worker thread queries
/// its own clone of `db`.
///
/// After this returns, every module's `module_references` result is cached in
/// salsa's memo table, so subsequent reads (e.g., from `build_reference_index_from_cached`)
/// are cheap hash lookups.
pub fn pre_warm_module_references<DB>(db: &DB, ingot: Ingot<'_>)
where
    DB: HirAnalysisDb + Clone,
{
    ingot
        .all_modules(db)
        .par_iter()
        .for_each_with(db.clone(), |db, &top_mod| {
            module_references(db, top_mod);
        });
}

/// Build a ReferenceIndex for an ingot from cached per-module data.
///
/// Each module's references are salsa-cached -- only changed modules recompute.
/// This function assembles the full inverted index from those cached slices.
///
/// For best cold-start performance, call `pre_warm_module_references` before
/// the tracked query that invokes this function.
pub fn build_reference_index_from_cached<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> ReferenceIndex<'db> {
    let mut index: FxHashMap<ScopeId<'db>, Vec<IndexedReference<'db>>> = FxHashMap::default();

    for top_mod in ingot.all_modules(db) {
        for resolved_ref in module_references(db, *top_mod) {
            index
                .entry(resolved_ref.target)
                .or_default()
                .push(IndexedReference {
                    span: resolved_ref.span.clone(),
                    is_self_ty: resolved_ref.is_self_ty,
                    module: *top_mod,
                });
        }
    }

    ReferenceIndex::from_map(index)
}
