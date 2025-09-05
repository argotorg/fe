mod util;
mod identity;
mod anchor;
mod goto;
mod hover;
mod refs;

 
use hir::SpannedHirDb;
use hir::LowerHirDb;
use hir::Ingot;
use hir::{
    hir_def::{scope_graph::ScopeId, PathId, TopLevelMod},
    source_index::{
        unified_occurrence_rangemap_for_top_mod,
        OccurrencePayload,
    },
    span::{DynLazySpan, LazySpan},
};
// method_func_def_from_res no longer used here
use hir_analysis::name_resolution::{resolve_with_policy, DomainPreference};
use crate::identity::{OccTarget, occurrence_symbol_target};
use crate::anchor::anchor_for_scope_match;
use hir_analysis::ty::func_def::FuncDef;
use hir_analysis::ty::trait_resolution::PredicateListId;
// (ty_check imports trimmed; not needed here)
use hir_analysis::HirAnalysisDb;
use hir_analysis::diagnostics::SpannedHirAnalysisDb;
use parser::TextSize;
use rustc_hash::FxHashMap;

/// High-level semantic queries (goto, hover, refs). This thin layer composes
/// HIR + analysis to produce IDE-facing answers without LS coupling.
pub struct SemanticIndex;

/// Small ergonomic wrapper around `SemanticQueryDb` to avoid repeating
/// both `db` and `spanned` in every call site.
pub struct Api<'db, DB: SemanticQueryDb + ?Sized> {
    db: &'db DB,
}

impl<'db, DB: SemanticQueryDb> Api<'db, DB> {
    pub fn new(db: &'db DB) -> Self { Self { db } }

    pub fn goto_candidates_at_cursor(
        &self,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Vec<DefinitionLocation<'db>> {
        SemanticIndex::goto_candidates_at_cursor(self.db, top_mod, cursor)
    }

    pub fn hover_info_for_symbol_at_cursor(
        &self,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Option<HoverData<'db>> {
        SemanticIndex::hover_info_for_symbol_at_cursor(self.db, top_mod, cursor)
    }

    pub fn symbol_identity_at_cursor(
        &self,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Option<SymbolKey<'db>> {
        symbol_at_cursor(self.db, top_mod, cursor)
    }

    pub fn definition_for_symbol(
        &self,
        key: SymbolKey<'db>,
    ) -> Option<(TopLevelMod<'db>, DynLazySpan<'db>)> {
        def_span_for_symbol(self.db, key)
    }

    pub fn references_for_symbol(
        &self,
        top_mod: TopLevelMod<'db>,
        key: SymbolKey<'db>,
    ) -> Vec<Reference<'db>> {
        find_refs_for_symbol(self.db, top_mod, key)
    }

    pub fn find_references_at_cursor(
        &self,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Vec<Reference<'db>> {
        SemanticIndex::find_references_at_cursor(self.db, top_mod, cursor)
    }

    pub fn find_references_at_cursor_best(
        &self,
        origin_top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Vec<Reference<'db>> {
        use std::collections::HashSet;
        let Some(key) = symbol_at_cursor(self.db, origin_top_mod, cursor) else { return Vec::new() };
        // Build module set from ingot if possible
        let ing = origin_top_mod.ingot(self.db);
        let view = ing.files(self.db);
        let mut modules: Vec<TopLevelMod<'db>> = Vec::new();
        for (_u, f) in view.iter() {
            if f.kind(self.db) == Some(common::file::IngotFileKind::Source) {
                modules.push(hir::lower::map_file_to_mod(self.db, f));
            }
        }
        if modules.is_empty() { modules.push(origin_top_mod); }
        // Use indexed lookup for all indexable keys
        let use_index = to_index_key(&key).is_some();
        let mut out: Vec<Reference<'db>> = if use_index {
            SemanticIndex::indexed_references_for_symbol_in_ingot(self.db, ing, key)
        } else {
            SemanticIndex::references_for_symbol_across(self.db, &modules, key)
        };
        // Dedup by (file, range)
        let mut seen: HashSet<(common::file::File, parser::TextSize, parser::TextSize)> = HashSet::new();
        out.retain(|r| match r.span.resolve(self.db) {
            Some(sp) => seen.insert((sp.file, sp.range.start(), sp.range.end())),
            None => true,
        });
        out
    }
}

pub struct DefinitionLocation<'db> {
    pub top_mod: TopLevelMod<'db>,
    pub span: DynLazySpan<'db>,
}

// Legacy HoverInfo removed; use structured HoverData instead

/// Structured hover data for public API consumption. Semantic, not presentation.
pub struct HoverData<'db> {
    pub top_mod: TopLevelMod<'db>,
    pub span: DynLazySpan<'db>,
    pub signature: Option<String>,
    pub documentation: Option<String>,
    pub kind: &'static str,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct Reference<'db> {
    pub top_mod: TopLevelMod<'db>,
    pub span: DynLazySpan<'db>,
}


impl SemanticIndex {
    pub fn new() -> Self {
        Self
    }

    /// Return all definition candidates at cursor (includes ambiguous/not-found buckets).
    /// Find all possible goto definition locations for a cursor position.
    /// Uses the occurrence-based resolution system with clean delegation to occurrence handlers.
    pub fn goto_candidates_at_cursor<'db>(
        db: &'db dyn SpannedHirAnalysisDb,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Vec<DefinitionLocation<'db>> {
        // Use a centralized façade for occurrence-based goto. If any result is produced, return it.
        let mut out = Vec::new();
        if let Some(occ) = pick_best_occurrence_at_cursor(db, top_mod, cursor) {
            let spans = crate::goto::goto_candidates_for_occurrence(db, &occ, top_mod);
            for span in spans.into_iter() {
                if let Some(tm) = span.top_mod(db) { out.push(DefinitionLocation { top_mod: tm, span }); }
                else { out.push(DefinitionLocation { top_mod, span }); }
            }
        }
        out
    }

    /// Convenience: goto definition from a cursor within a module.
    /// Applies a simple module-local preference policy when multiple candidates exist.
    pub fn goto_definition_at_cursor<'db>(
        db: &'db dyn SpannedHirAnalysisDb,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Option<DefinitionLocation<'db>> {
        if let Some(key) = symbol_at_cursor(db, top_mod, cursor) {
            if let Some((tm, span)) = def_span_for_symbol(db, key) {
                return Some(DefinitionLocation { top_mod: tm, span });
            }
        }
        // Fall back to occurrence-based candidates: callers that want multiple
        // can use `goto_candidates_at_cursor`; we do not collapse here to avoid
        // masking ambiguity.
        None
    }

    /// Resolve the given path in the provided scope and return the definition location if any.
    /// This expects the caller to pass an appropriate `scope` for the path occurrence.
    pub fn goto_definition_for_path<'db>(
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        path: PathId<'db>,
    ) -> Option<DefinitionLocation<'db>> {
        let assumptions = PredicateListId::empty_list(db);
        let res =
            resolve_with_policy(db, path, scope, assumptions, DomainPreference::Value).ok()?;
        let span = res.name_span(db)?;
        let top_mod = span.top_mod(db)?;
        Some(DefinitionLocation { top_mod, span })
    }

    /// Find the HIR path under the given cursor within the smallest enclosing item.
    /// Uses the unified occurrence index to pick the smallest covering PathSeg span.
    pub fn at_cursor<'db>(
        db: &'db dyn SpannedHirDb,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Option<(PathId<'db>, ScopeId<'db>, usize, DynLazySpan<'db>)> {
        use hir::source_index::occurrences_at_offset;
        let mut best: Option<(PathId<'db>, ScopeId<'db>, usize, DynLazySpan<'db>, TextSize)> = None;
        for occ in occurrences_at_offset(db, top_mod, cursor) {
            if let OccurrencePayload::PathSeg { path, scope, seg_idx, span, .. } = occ {
                if let Some(sp) = span.clone().resolve(db) {
                    let w: TextSize = sp.range.end() - sp.range.start();
                    match best {
                        None => best = Some((path, scope, seg_idx, span, w)),
                        Some((_, _, _, _, bw)) if w < bw => best = Some((path, scope, seg_idx, span, w)),
                        _ => {}
                    }
                }
            }
        }
        best.map(|(p, s, i, span, _)| (p, s, i, span))
    }

    // legacy hover_at_cursor removed; use hover_info_for_symbol_at_cursor instead

    /// Structured hover data (signature, docs, kind) for the symbol at cursor.
    pub fn hover_info_for_symbol_at_cursor<'db>(
        db: &'db dyn SpannedHirAnalysisDb,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Option<HoverData<'db>> {
        let occ = pick_best_occurrence_at_cursor(db, top_mod, cursor)?;
        let hs = crate::hover::hover_for_occurrence(db, &occ, top_mod)?;
        Some(HoverData { top_mod, span: hs.span, signature: hs.signature, documentation: hs.documentation, kind: hs.kind })
    }

    /// Public identity API for consumers.
    pub fn symbol_identity_at_cursor<'db>(
        db: &'db dyn SpannedHirAnalysisDb,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Option<SymbolKey<'db>> {
        symbol_at_cursor(db, top_mod, cursor)
    }

    /// Public definition API for consumers.
    pub fn definition_for_symbol<'db>(
        db: &'db dyn SpannedHirAnalysisDb,
        key: SymbolKey<'db>,
    ) -> Option<(TopLevelMod<'db>, DynLazySpan<'db>)> {
        def_span_for_symbol(db, key)
    }

    /// Public references API for consumers.
    pub fn references_for_symbol<'db>(
        db: &'db dyn SpannedHirAnalysisDb,
        top_mod: TopLevelMod<'db>,
        key: SymbolKey<'db>,
    ) -> Vec<Reference<'db>> {
        find_refs_for_symbol(db, top_mod, key)
    }

    /// Find references to the symbol under the cursor, within the given top module.
    /// Identity-first: picks a SymbolKey at the cursor, then resolves refs.
    pub fn find_references_at_cursor<'db>(
        db: &'db dyn SpannedHirAnalysisDb,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Vec<Reference<'db>> {
        if let Some(key) = symbol_at_cursor(db, top_mod, cursor) {
            return find_refs_for_symbol(db, top_mod, key);
        }
        Vec::new()
    }

    /// Workspace-level: get references for the symbol under cursor across many modules.
    /// `modules` should be a deduplicated list of `TopLevelMod` to search.
    pub fn find_references_at_cursor_across<'db>(
        db: &'db dyn SpannedHirAnalysisDb,
        origin_top_mod: TopLevelMod<'db>,
        modules: &[TopLevelMod<'db>],
        cursor: TextSize,
    ) -> Vec<Reference<'db>> {
        if let Some(key) = symbol_at_cursor(db, origin_top_mod, cursor) {
            return references_for_symbol_across(db, modules, key);
        }
        Vec::new()
    }

    /// Workspace-level: get references for a symbol identity across many modules.
    /// `modules` should be a deduplicated list of `TopLevelMod` to search.
    pub fn references_for_symbol_across<'db>(
        db: &'db dyn SpannedHirAnalysisDb,
        modules: &[TopLevelMod<'db>],
        key: SymbolKey<'db>,
    ) -> Vec<Reference<'db>> {
        references_for_symbol_across(db, modules, key)
    }

    /// Build a per-module symbol index keyed by semantic identity. Not cached yet.
    pub fn build_symbol_index_for_modules<'db>(
        db: &'db dyn SpannedHirAnalysisDb,
        modules: &[TopLevelMod<'db>],
    ) -> FxHashMap<SymbolKey<'db>, Vec<Reference<'db>>> {
        let mut map: FxHashMap<SymbolKey<'db>, Vec<Reference<'db>>> = FxHashMap::default();
        for &m in modules {
            for (key, r) in collect_symbol_refs_for_module(db, m).into_iter() {
                map.entry(key).or_default().push(r);
            }
        }
        map
    }

}

// Note: Ranking/ordering of candidates is left to callers.

/// Pick the smallest covering occurrence at the cursor across all occurrence kinds.
fn kind_priority(occ: &OccurrencePayload<'_>) -> u8 {
    match occ {
        OccurrencePayload::PathExprSeg { .. } | OccurrencePayload::PathPatSeg { .. } => 0,
        OccurrencePayload::MethodName { .. }
        | OccurrencePayload::FieldAccessName { .. }
        | OccurrencePayload::PatternLabelName { .. }
        | OccurrencePayload::UseAliasName { .. }
        | OccurrencePayload::UsePathSeg { .. } => 1,
        OccurrencePayload::PathSeg { .. } => 2,
        OccurrencePayload::ItemHeaderName { .. } => 3,
    }
}

fn pick_best_occurrence_at_cursor<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: TextSize,
) -> Option<OccurrencePayload<'db>> {
    use hir::source_index::occurrences_at_offset;
    
    // SIMPLIFIED: Only check exact cursor position, no fallbacks
    // This ensures half-open range semantics are respected
    let occs = occurrences_at_offset(db, top_mod, cursor);
    
    
    // Find the best occurrence at this exact position
    let mut best: Option<(OccurrencePayload<'db>, TextSize, u8)> = None;
    for occ in occs {
        let span = match &occ {
            OccurrencePayload::PathSeg { span, .. }
            | OccurrencePayload::UsePathSeg { span, .. }
            | OccurrencePayload::UseAliasName { span, .. }
            | OccurrencePayload::MethodName { span, .. }
            | OccurrencePayload::FieldAccessName { span, .. }
            | OccurrencePayload::PatternLabelName { span, .. }
            | OccurrencePayload::PathExprSeg { span, .. }
            | OccurrencePayload::PathPatSeg { span, .. }
            | OccurrencePayload::ItemHeaderName { span, .. } => span.clone(),
        };
        let w = if let Some(sp) = span.resolve(db) { sp.range.end() - sp.range.start() } else { TextSize::from(1u32) };
        let pr = kind_priority(&occ);
        
        // Prefer lower priority (better), then smaller width  
        match best { 
            None => best = Some((occ, w, pr)),
            Some((_, bw, bpr)) if pr < bpr || (pr == bpr && w < bw) => best = Some((occ, w, pr)), 
            _ => {} 
        }
    }
    
    best.map(|(occ, _, _)| occ)
}

/// Shared: collect symbol references for a single module by scanning the unified
/// occurrence rangemap and resolving each occurrence to a SymbolKey and span.
fn collect_symbol_refs_for_module<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    m: TopLevelMod<'db>,
) -> Vec<(SymbolKey<'db>, Reference<'db>)> {
    let mut out: Vec<(SymbolKey<'db>, Reference<'db>)> = Vec::new();
    for occ in unified_occurrence_rangemap_for_top_mod(db, m).iter() {
        // Skip header occurrences - we only want references, not definitions
        match &occ.payload { 
            OccurrencePayload::ItemHeaderName { .. } => continue,
            _ => {}
        }
        
        // Use the canonical occurrence interpreter to get the symbol target
        if let Some(target) = occurrence_symbol_target(db, m, &occ.payload) {
            if let Some(key) = occ_target_to_symbol_key(target) {
                let span = compute_reference_span(db, &occ.payload, target, m);
                out.push((key, Reference { top_mod: m, span }));
            }
        }
    }
    out
}

fn compute_reference_span<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    occ: &OccurrencePayload<'db>,
    target: OccTarget<'db>,
    _m: TopLevelMod<'db>,
) -> DynLazySpan<'db> {
    match occ {
        // For PathSeg, use smart anchoring based on the target
        OccurrencePayload::PathSeg { path, scope, path_lazy, .. } => {
            let view = hir::path_view::HirPathAdapter::new(db, *path);
            match target {
                OccTarget::Scope(sc) => anchor_for_scope_match(db, &view, path_lazy.clone(), *path, *scope, sc),
                _ => {
                    let anchor = hir::path_anchor::AnchorPicker::pick_unresolved_tail(&view);
                    hir::path_anchor::map_path_anchor_to_dyn_lazy(path_lazy.clone(), anchor)
                }
            }
        }
        // For all other occurrence types, use the occurrence's own span
        OccurrencePayload::PathExprSeg { span, .. }
        | OccurrencePayload::PathPatSeg { span, .. }
        | OccurrencePayload::FieldAccessName { span, .. }
        | OccurrencePayload::PatternLabelName { span, .. }
        | OccurrencePayload::MethodName { span, .. }
        | OccurrencePayload::UseAliasName { span, .. }
        | OccurrencePayload::UsePathSeg { span, .. }
        | OccurrencePayload::ItemHeaderName { span, .. } => span.clone(),
    }
}

// (unused helper functions removed)

// ---------- Tracked, per-ingot symbol index ----------

// We define a tiny DB marker for semantic-query so we can expose
// a cached, tracked index without bloating hir-analysis.
#[salsa::db]
pub trait SemanticQueryDb: SpannedHirAnalysisDb + LowerHirDb {}

#[salsa::db]
impl<T> SemanticQueryDb for T where T: SpannedHirAnalysisDb + LowerHirDb {}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct IndexedRefsEntry<'db> {
    pub key: IndexKey<'db>,
    pub refs: Vec<Reference<'db>>,
}

/// Tracked per-ingot symbol index as a list to satisfy Salsa's Update bounds.
#[salsa::tracked(return_ref)]
pub fn symbol_index_for_ingot<'db>(
    db: &'db dyn SemanticQueryDb,
    ingot: Ingot<'db>,
) -> Vec<IndexedRefsEntry<'db>> {
    use common::file::IngotFileKind;
    use hir::lower::map_file_to_mod;

    // Accumulate in a local map, then convert to a Vec of entries.
    let mut map: FxHashMap<IndexKey<'db>, Vec<Reference<'db>>> = FxHashMap::default();

    // Enumerate all source modules in the ingot
    let view = ingot.files(db);
    let mut modules = Vec::new();
    for (_u, f) in view.iter() {
        if f.kind(db) == Some(IngotFileKind::Source) {
            modules.push(map_file_to_mod(db, f));
        }
    }

    // Build index via shared collector
    for &m in &modules {
        for (skey, r) in collect_symbol_refs_for_module(db, m).into_iter() {
            if let Some(ikey) = to_index_key(&skey) {
                map.entry(ikey).or_default().push(r);
            }
        }
    }

    // Convert to a Vec of entries for tracked return type.
    map.into_iter()
        .map(|(key, refs)| IndexedRefsEntry { key, refs })
        .collect()
}

impl SemanticIndex {
    /// Lookup references for a symbol identity using the tracked per-ingot index.
    /// Falls back to empty Vec if the key is missing.
    pub fn indexed_references_for_symbol_in_ingot<'db>(
        db: &'db dyn SemanticQueryDb,
        ingot: Ingot<'db>,
        key: SymbolKey<'db>,
    ) -> Vec<Reference<'db>> {
        if let Some(ikey) = to_index_key(&key) {
            symbol_index_for_ingot(db, ingot)
                .iter()
                .find(|e| e.key == ikey)
                .map(|e| e.refs.clone())
                .unwrap_or_default()
        } else {
            Vec::new()
        }
    }
}


// (header name helpers are superseded by ItemHeaderName occurrences)

// (variant header helper superseded by ItemHeaderName occurrences)

// (func header helper superseded by ItemHeaderName occurrences)



// (enclosing func helpers not used here)

// reverse span index helpers deleted in favor of unified occurrence index

// hover helpers removed; analysis façade provides semantic hover data

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolKey<'db> {
    Scope(hir::hir_def::scope_graph::ScopeId<'db>),
    EnumVariant(hir::hir_def::EnumVariant<'db>),
    FuncParam(hir::hir_def::ItemKind<'db>, u16),
    Method(FuncDef<'db>),
    // Local binding within a function
    Local(
        hir::hir_def::item::Func<'db>,
        hir_analysis::ty::ty_check::BindingKey<'db>,
    ),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum IndexKey<'db> {
    Scope(hir::hir_def::scope_graph::ScopeId<'db>),
    EnumVariant(hir::hir_def::EnumVariant<'db>),
    FuncParam(hir::hir_def::ItemKind<'db>, u16),
    Method(FuncDef<'db>),
}

fn to_index_key<'db>(key: &SymbolKey<'db>) -> Option<IndexKey<'db>> {
    match *key {
        SymbolKey::Scope(sc) => Some(IndexKey::Scope(sc)),
        SymbolKey::EnumVariant(v) => Some(IndexKey::EnumVariant(v)),
        SymbolKey::FuncParam(item, idx) => Some(IndexKey::FuncParam(item, idx)),
        SymbolKey::Method(fd) => Some(IndexKey::Method(fd)),
        SymbolKey::Local(..) => None,
    }
}

// (symbol_key_from_res removed)

// Duplicate of analysis façade implementing_methods_for_trait_method removed.

// Unified identity at cursor
fn symbol_at_cursor<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    cursor: TextSize,
) -> Option<SymbolKey<'db>> {
    // Use simplified analysis bridge that respects half-open range semantics
    if let Some(id) = hir_analysis::lookup::identity_at_offset(db, top_mod, cursor) {
        use hir_analysis::lookup::SymbolIdentity as I;
        let key = match id {
            I::Scope(sc) => SymbolKey::Scope(sc),
            I::EnumVariant(v) => SymbolKey::EnumVariant(v),
            I::FuncParam(item, idx) => SymbolKey::FuncParam(item, idx),
            I::Method(fd) => SymbolKey::Method(fd),
            I::Local(func, bkey) => SymbolKey::Local(func, bkey),
        };
        return Some(key);
    }
    None
}

fn occ_target_to_symbol_key<'db>(t: OccTarget<'db>) -> Option<SymbolKey<'db>> {
    match t {
        OccTarget::Scope(sc) => Some(SymbolKey::Scope(sc)),
        OccTarget::EnumVariant(v) => Some(SymbolKey::EnumVariant(v)),
        OccTarget::FuncParam(item, idx) => Some(SymbolKey::FuncParam(item, idx)),
        OccTarget::Method(fd) => Some(SymbolKey::Method(fd)),
        OccTarget::Local(func, bkey) => Some(SymbolKey::Local(func, bkey)),
    }
}

// Definition span for a SymbolKey
fn def_span_for_symbol<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    key: SymbolKey<'db>,
) -> Option<(TopLevelMod<'db>, DynLazySpan<'db>)> {
    match key {
        SymbolKey::Local(func, bkey) => {
            let span = hir_analysis::ty::ty_check::binding_def_span_in_func(db, func, bkey)?;
            let tm = span.top_mod(db)?;
            Some((tm, span))
        }
        SymbolKey::Method(fd) => {
            if let Some(span) = fd.scope(db).name_span(db) {
                let tm = span.top_mod(db)?;
                Some((tm, span))
            } else if let Some(item) = fd.scope(db).to_item() {
                let lazy = DynLazySpan::from(item.span());
                let tm = lazy.top_mod(db)?;
                Some((tm, lazy))
            } else {
                None
            }
        }
        SymbolKey::EnumVariant(v) => {
            let sc = v.scope();
            let span = sc.name_span(db)?;
            let tm = span.top_mod(db)?;
            Some((tm, span))
        }
        SymbolKey::Scope(sc) => {
            let span = sc.name_span(db)?;
            let tm = span.top_mod(db)?;
            Some((tm, span))
        }
        SymbolKey::FuncParam(item, idx) => {
            let sc = hir::hir_def::scope_graph::ScopeId::FuncParam(item, idx);
            let span = sc.name_span(db)?;
            let tm = span.top_mod(db)?;
            Some((tm, span))
        }
    }
}

// Unified references by identity
fn find_refs_for_symbol<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    key: SymbolKey<'db>,
) -> Vec<Reference<'db>> {
    use std::collections::HashSet;
    let mut out: Vec<Reference<'db>> = Vec::new();
    let mut seen: HashSet<(common::file::File, parser::TextSize, parser::TextSize)> = HashSet::new();

    // 1) Always include def-site first when available.
    if let Some((tm, def_span)) = def_span_for_symbol(db, key.clone()) {
        if let Some(sp) = def_span.resolve(db) {
            seen.insert((sp.file, sp.range.start(), sp.range.end()));
        }
        out.push(Reference { top_mod: tm, span: def_span });
    }

    // 2) Single pass over occurrence index for this module.
    for occ in unified_occurrence_rangemap_for_top_mod(db, top_mod).iter() {
        // Skip header-name occurrences; def-site is already injected above.
        match &occ.payload { OccurrencePayload::ItemHeaderName { .. } => continue, _ => {} }

        // Resolve occurrence to a symbol identity and anchor appropriately.
        let Some(target) = occurrence_symbol_target(db, top_mod, &occ.payload) else { continue };
        // Custom matcher to allow associated functions (scopes) to match method occurrences
        let matches = match (key, target) {
            (SymbolKey::Scope(sc), OccTarget::Scope(sc2)) => sc == sc2,
            (SymbolKey::Scope(sc), OccTarget::Method(fd)) => fd.scope(db) == sc,
            (SymbolKey::EnumVariant(v), OccTarget::EnumVariant(v2)) => v == v2,
            (SymbolKey::FuncParam(it, idx), OccTarget::FuncParam(it2, idx2)) => it == it2 && idx == idx2,
            (SymbolKey::Method(fd), OccTarget::Method(fd2)) => fd == fd2,
            (SymbolKey::Local(func, bkey), OccTarget::Local(func2, bkey2)) => func == func2 && bkey == bkey2,
            _ => false,
        };
        if !matches { continue; }

        let span: DynLazySpan<'db> = match &occ.payload {
            OccurrencePayload::PathSeg { path, scope, path_lazy, .. } => {
                match target {
                    OccTarget::Scope(sc) => {
                        let view = hir::path_view::HirPathAdapter::new(db, *path);
                        anchor_for_scope_match(db, &view, path_lazy.clone(), *path, *scope, sc)
                    }
                    _ => {
                        let view = hir::path_view::HirPathAdapter::new(db, *path);
                        let anchor = hir::path_anchor::AnchorPicker::pick_unresolved_tail(&view);
                        hir::path_anchor::map_path_anchor_to_dyn_lazy(path_lazy.clone(), anchor)
                    }
                }
            }
            // For expression and pattern path segments, use the occurrence span.
            OccurrencePayload::PathExprSeg { span, .. } | OccurrencePayload::PathPatSeg { span, .. } => span.clone(),
            // Name-based occurrences anchor at the name span directly.
            OccurrencePayload::UseAliasName { span, .. }
            | OccurrencePayload::UsePathSeg { span, .. }
            | OccurrencePayload::MethodName { span, .. }
            | OccurrencePayload::FieldAccessName { span, .. }
            | OccurrencePayload::PatternLabelName { span, .. } => span.clone(),
            OccurrencePayload::ItemHeaderName { .. } => unreachable!(),
        };

        if let Some(sp) = span.resolve(db) {
            let k = (sp.file, sp.range.start(), sp.range.end());
            if !seen.insert(k) { continue; }
        }
        out.push(Reference { top_mod, span });
    }

    // 3) Method extras: include method refs via façade (covers UFCS and method-call),
    // and if trait method, include implementing method def headers in this module.
    if let SymbolKey::Method(fd) = key {
        // Direct method references in this module
        for span in crate::refs::method_refs_in_mod(db, top_mod, fd) {
            if let Some(sp) = span.resolve(db) {
                let k = (sp.file, sp.range.start(), sp.range.end());
                if !seen.insert(k) { continue; }
            }
            out.push(Reference { top_mod, span });
        }
        for m in crate::refs::implementing_methods_for_trait_method(db, top_mod, fd) {
            if let Some(span) = m.scope(db).name_span(db) {
                if let Some(sp) = span.resolve(db) {
                    let k = (sp.file, sp.range.start(), sp.range.end());
                    if !seen.insert(k) { continue; }
                }
                if let Some(tm) = span.top_mod(db) { out.push(Reference { top_mod: tm, span }); }
            }
        }
    }

    out
}

fn references_for_symbol_across<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    modules: &[TopLevelMod<'db>],
    key: SymbolKey<'db>,
) -> Vec<Reference<'db>> {
    use std::collections::HashSet;
    let mut out = Vec::new();
    let mut seen: HashSet<(common::file::File, parser::TextSize, parser::TextSize)> = HashSet::new();
    for &m in modules {
        for r in find_refs_for_symbol(db, m, key.clone()) {
            if let Some(sp) = r.span.resolve(db) {
                let key = (sp.file, sp.range.start(), sp.range.end());
                if seen.insert(key) {
                    out.push(r);
                }
            } else {
                // Unresolvable spans are rare; keep them without dedup
                out.push(r);
            }
        }
    }
    out
}
