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

use rustc_hash::FxHashMap;

/// A single resolved reference: target scope + source span + metadata.
#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct ResolvedRef<'db> {
    pub target: ScopeId<'db>,
    pub span: DynLazySpan<'db>,
    pub is_self_ty: bool,
}

/// Per-module: all references originating from this module's items.
/// Cached by salsa — only recomputes when this module's resolved targets change.
#[salsa::tracked(return_ref)]
pub fn module_references<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<ResolvedRef<'db>> {
    let scope_graph = top_mod.scope_graph(db);
    let mut refs = Vec::new();

    for item in scope_graph.items_dfs(db) {
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
/// Must be called OUTSIDE any salsa tracked query -- `salsa::par_map` forks the
/// database internally and worker threads attach their own forks. Calling from
/// inside a tracked query would conflict with the existing attachment.
///
/// After this returns, every module's `module_references` result is cached in
/// salsa's memo table, so subsequent reads (e.g., from `build_reference_index_from_cached`)
/// are cheap hash lookups.
pub fn pre_warm_module_references<'db>(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) {
    let modules = ingot.all_modules(db);
    if modules.is_empty() {
        return;
    }

    // Ensure the HirAnalysisDb view is registered before par_map forks the db.
    // Views are normally registered lazily by the first tracked function call,
    // but par_map needs them registered up front for as_view on forked dbs.
    HirAnalysisDb::zalsa_register_downcaster(db);

    let _: Vec<()> = salsa::par_map(db, modules.to_vec(), |db, top_mod| {
        let _ = module_references(db, top_mod);
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
