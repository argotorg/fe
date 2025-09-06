mod anchor;
mod hover;
mod identity;
mod refs;

use crate::identity::{occurrence_symbol_target, occurrence_symbol_targets, OccTarget};
use hir::{
    hir_def::{scope_graph::ScopeId, HirIngot, TopLevelMod},
    source_index::{unified_occurrence_rangemap_for_top_mod, OccurrencePayload},
    span::{DynLazySpan, LazySpan},
};
use hir_analysis::diagnostics::SpannedHirAnalysisDb;
use hir_analysis::ty::func_def::FuncDef;
use parser::TextSize;
use rustc_hash::{FxHashMap, FxHashSet};

/// Unified semantic query API. Performs occurrence lookup once and provides
/// all IDE features (goto, hover, references) from that single resolution.
pub struct SemanticQuery<'db> {
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,

    // Cached results from single occurrence lookup
    occurrence: Option<OccurrencePayload<'db>>,
    symbol_key: Option<SymbolKey<'db>>,
}

impl<'db> SemanticQuery<'db> {
    pub fn at_cursor(
        db: &'db dyn SpannedHirAnalysisDb,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Self {
        let occurrence = pick_best_occurrence_at_cursor(db, top_mod, cursor);
        let symbol_key = occurrence
            .as_ref()
            .and_then(|occ| occurrence_symbol_target(db, top_mod, occ))
            .map(occ_target_to_symbol_key);

        Self {
            db,
            top_mod,
            occurrence,
            symbol_key,
        }
    }

    pub fn goto_definition(&self) -> Vec<DefinitionLocation<'db>> {
        // Always check for all possible identities (including ambiguous cases)
        if let Some(ref occ) = self.occurrence {
            let identities =
                hir_analysis::lookup::identity_for_occurrence(self.db, self.top_mod, occ);

            let mut definitions = Vec::new();
            for identity in identities {
                let key = identity_to_symbol_key(identity);
                if let Some((top_mod, span)) = def_span_for_symbol(self.db, key) {
                    definitions.push(DefinitionLocation { top_mod, span });
                }
            }
            return definitions;
        }

        Vec::new()
    }

    pub fn hover_info(&self) -> Option<HoverData<'db>> {
        let occ = self.occurrence.as_ref()?;
        let hs = crate::hover::hover_for_occurrence(self.db, occ, self.top_mod)?;
        Some(HoverData {
            top_mod: self.top_mod,
            span: hs.span,
            signature: hs.signature,
            documentation: hs.documentation,
            kind: hs.kind,
        })
    }

    pub fn find_references(&self) -> Vec<Reference<'db>> {
        let Some(key) = self.symbol_key else {
            return Vec::new();
        };
        find_refs_for_symbol(self.db, self.top_mod, key)
    }

    pub fn find_rename_locations(&self) -> Vec<Reference<'db>> {
        let Some(key) = self.symbol_key else {
            return Vec::new();
        };

        // Check for special cases that should block or require special handling
        if self.is_rename_blocked(&key) {
            return Vec::new();
        }

        // For rename, we want only actual symbol name occurrences, not semantic references
        self.find_symbol_occurrences()
    }

    pub fn find_symbol_occurrences(&self) -> Vec<Reference<'db>> {
        let Some(key) = self.symbol_key else {
            return Vec::new();
        };

        // Use the shared implementation, filtering for rename-allowed occurrences only
        find_refs_for_symbol_with_filter(self.db, self.top_mod, key, true)
    }

    pub fn find_implementations(&self) -> Vec<DefinitionLocation<'db>> {
        let Some(key) = self.symbol_key else {
            return Vec::new();
        };

        match key {
            SymbolKey::Method(fd) => {
                // Find implementing methods for trait methods
                let mut implementations = Vec::new();
                for impl_method in
                    crate::refs::implementing_methods_for_trait_method(self.db, self.top_mod, fd)
                {
                    if let Some(span) = impl_method.scope(self.db).name_span(self.db) {
                        if let Some(tm) = span.top_mod(self.db) {
                            implementations.push(DefinitionLocation { top_mod: tm, span });
                        }
                    }
                }
                implementations
            }
            SymbolKey::Scope(scope_id) => {
                // Check if this is a trait scope
                if let Some(hir::hir_def::ItemKind::Trait(trait_def)) = scope_id.to_item() {
                    return self.find_trait_implementations(trait_def);
                }
                Vec::new()
            }
            _ => {
                // For other symbol types, there are no implementations
                Vec::new()
            }
        }
    }

    fn find_trait_implementations(
        &self,
        trait_def: hir::hir_def::item::Trait<'db>,
    ) -> Vec<DefinitionLocation<'db>> {
        let mut implementations = Vec::new();

        // Find all impl blocks that implement this trait
        for impl_trait in self.top_mod.all_impl_traits(self.db) {
            let Some(trait_ref) = impl_trait.trait_ref(self.db).to_opt() else {
                continue;
            };
            let hir::hir_def::Partial::Present(path) = trait_ref.path(self.db) else {
                continue;
            };

            // Resolve the trait reference to see if it matches our trait
            let assumptions =
                hir_analysis::ty::trait_resolution::PredicateListId::empty_list(self.db);
            let Ok(hir_analysis::name_resolution::PathRes::Trait(trait_inst)) =
                hir_analysis::name_resolution::resolve_with_policy(
                    self.db,
                    path,
                    impl_trait.scope(),
                    assumptions,
                    hir_analysis::name_resolution::DomainPreference::Type,
                )
            else {
                continue;
            };

            if trait_inst.def(self.db).trait_(self.db) == trait_def {
                // This impl block implements our trait - use the trait_ref span
                let span = impl_trait.span().trait_ref();
                if let Some(tm) = span.top_mod(self.db) {
                    implementations.push(DefinitionLocation {
                        top_mod: tm,
                        span: hir::span::DynLazySpan::from(span),
                    });
                }
            }
        }

        implementations
    }

    pub fn symbol_key(&self) -> Option<SymbolKey<'db>> {
        self.symbol_key
    }

    /// Check if renaming this symbol should be blocked or requires special handling
    fn is_rename_blocked(&self, key: &SymbolKey<'db>) -> bool {
        match key {
            SymbolKey::Scope(scope_id) => {
                // Check if this is a module scope that might need special handling
                if let Some(item) = scope_id.to_item() {
                    if let hir::hir_def::ItemKind::Mod(_mod_def) = item {
                        // Get the module name to check for special cases
                        if let Some(name) = item.name(self.db) {
                            let name_str = name.data(self.db);

                            // Block renaming if this looks like an ingot root or special module
                            if name_str == "ingot" || name_str == "main" || name_str == "lib" {
                                return true;
                            }
                        }

                        // For now, block all module renames as they may require file system operations
                        // TODO: In the future, implement proper module rename with file operations
                        return true;
                    }
                }
                false
            }
            SymbolKey::Method(_func_def) => {
                // Allow renaming all functions including main
                false
            }
            // Allow renaming other symbols (EnumVariant, FuncParam, Local)
            _ => false,
        }
    }

    // Test support methods
    pub fn definition_for_symbol(
        db: &'db dyn SpannedHirAnalysisDb,
        key: SymbolKey<'db>,
    ) -> Option<(TopLevelMod<'db>, DynLazySpan<'db>)> {
        def_span_for_symbol(db, key)
    }

    pub fn references_for_symbol(
        db: &'db dyn SpannedHirAnalysisDb,
        top_mod: TopLevelMod<'db>,
        key: SymbolKey<'db>,
    ) -> Vec<Reference<'db>> {
        find_refs_for_symbol(db, top_mod, key)
    }

    pub fn build_symbol_index_for_modules(
        db: &'db dyn SpannedHirAnalysisDb,
        modules: &[TopLevelMod<'db>],
    ) -> FxHashMap<SymbolKey<'db>, Vec<Reference<'db>>> {
        let mut map: FxHashMap<SymbolKey<'db>, Vec<Reference<'db>>> = FxHashMap::default();
        for &m in modules {
            for occ in unified_occurrence_rangemap_for_top_mod(db, m).iter() {
                // Skip header occurrences - we only want references, not definitions
                if matches!(&occ.payload, OccurrencePayload::ItemHeaderName { .. }) {
                    continue;
                }

                // Use the canonical occurrence interpreter to get all symbol targets (including ambiguous)
                let targets = occurrence_symbol_targets(db, m, &occ.payload);
                for target in targets {
                    let key = occ_target_to_symbol_key(target);
                    let span = compute_reference_span(db, &occ.payload, target, m);
                    map.entry(key)
                        .or_default()
                        .push(Reference { top_mod: m, span });
                }
            }
        }
        map
    }
}

pub struct DefinitionLocation<'db> {
    pub top_mod: TopLevelMod<'db>,
    pub span: DynLazySpan<'db>,
}

/// Structured hover data for public API consumption. Semantic, not presentation.
pub struct HoverData<'db> {
    pub top_mod: TopLevelMod<'db>,
    pub span: DynLazySpan<'db>,
    pub signature: Option<String>,
    pub documentation: Option<String>,
    pub kind: &'static str,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Reference<'db> {
    pub top_mod: TopLevelMod<'db>,
    pub span: DynLazySpan<'db>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolKey<'db> {
    Scope(ScopeId<'db>),
    EnumVariant(hir::hir_def::EnumVariant<'db>),
    FuncParam(hir::hir_def::ItemKind<'db>, u16),
    Method(FuncDef<'db>),
    Local(
        hir::hir_def::item::Func<'db>,
        hir_analysis::ty::ty_check::BindingKey<'db>,
    ),
}

// Simple helper functions
fn pick_best_occurrence_at_cursor<'db>(
    db: &'db dyn hir::SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: TextSize,
) -> Option<OccurrencePayload<'db>> {
    use hir::source_index::occurrences_at_offset;

    let occs = occurrences_at_offset(db, top_mod, cursor);
    let mut best: Option<(OccurrencePayload<'db>, TextSize, u8)> = None;

    for occ in occs {
        let span = crate::hover::get_span_from_occurrence(&occ);
        let w = if let Some(sp) = span.resolve(db) {
            sp.range.end() - sp.range.start()
        } else {
            TextSize::from(1u32)
        };
        let pr = kind_priority(&occ);

        match best {
            None => best = Some((occ, w, pr)),
            Some((_, bw, bpr)) if pr < bpr || (pr == bpr && w < bw) => best = Some((occ, w, pr)),
            _ => {}
        }
    }

    best.map(|(occ, _, _)| occ)
}

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

fn occ_target_to_symbol_key<'db>(t: OccTarget<'db>) -> SymbolKey<'db> {
    match t {
        OccTarget::Scope(sc) => SymbolKey::Scope(sc),
        OccTarget::EnumVariant(v) => SymbolKey::EnumVariant(v),
        OccTarget::FuncParam(item, idx) => SymbolKey::FuncParam(item, idx),
        OccTarget::Method(fd) => SymbolKey::Method(fd),
        OccTarget::Local(func, bkey) => SymbolKey::Local(func, bkey),
    }
}

fn identity_to_symbol_key<'db>(
    identity: hir_analysis::lookup::SymbolIdentity<'db>,
) -> SymbolKey<'db> {
    match identity {
        hir_analysis::lookup::SymbolIdentity::Scope(sc) => SymbolKey::Scope(sc),
        hir_analysis::lookup::SymbolIdentity::EnumVariant(v) => SymbolKey::EnumVariant(v),
        hir_analysis::lookup::SymbolIdentity::FuncParam(item, idx) => {
            SymbolKey::FuncParam(item, idx)
        }
        hir_analysis::lookup::SymbolIdentity::Method(fd) => SymbolKey::Method(fd),
        hir_analysis::lookup::SymbolIdentity::Local(func, bkey) => SymbolKey::Local(func, bkey),
    }
}

// Definition span lookup - needed by goto
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
            let sc = ScopeId::FuncParam(item, idx);
            let span = sc.name_span(db)?;
            let tm = span.top_mod(db)?;
            Some((tm, span))
        }
    }
}

// References - needed by find_references
fn find_refs_for_symbol<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    key: SymbolKey<'db>,
) -> Vec<Reference<'db>> {
    find_refs_for_symbol_with_filter(db, top_mod, key, false)
}

fn find_refs_for_symbol_with_filter<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    key: SymbolKey<'db>,
    only_rename_allowed: bool,
) -> Vec<Reference<'db>> {
    let mut out: Vec<Reference<'db>> = Vec::new();
    let mut seen: FxHashSet<(common::file::File, parser::TextSize, parser::TextSize)> =
        FxHashSet::default();

    // 1) Always include def-site first when available.
    if let Some((tm, def_span)) = def_span_for_symbol(db, key) {
        if let Some(sp) = def_span.resolve(db) {
            seen.insert((sp.file, sp.range.start(), sp.range.end()));
        }
        out.push(Reference {
            top_mod: tm,
            span: def_span,
        });
    }

    // 2) Search across all modules in the ingot for references.
    for &module in top_mod.ingot(db).all_modules(db) {
        for occ in unified_occurrence_rangemap_for_top_mod(db, module).iter() {
            // Skip header-name occurrences; def-site is already injected above.
            if let OccurrencePayload::ItemHeaderName { .. } = &occ.payload {
                continue;
            }

            // Resolve occurrence to a symbol identity and anchor appropriately.
            let Some(target) = occurrence_symbol_target(db, module, &occ.payload) else {
                continue;
            };
            // Custom matcher to allow associated functions (scopes) to match method occurrences
            let matches = match (key, target) {
                (SymbolKey::Scope(sc), OccTarget::Scope(sc2)) => sc == sc2,
                (SymbolKey::Scope(sc), OccTarget::Method(fd)) => fd.scope(db) == sc,
                (SymbolKey::EnumVariant(v), OccTarget::EnumVariant(v2)) => v == v2,
                (SymbolKey::FuncParam(it, idx), OccTarget::FuncParam(it2, idx2)) => {
                    it == it2 && idx == idx2
                }
                (SymbolKey::Method(fd), OccTarget::Method(fd2)) => fd == fd2,
                (SymbolKey::Local(func, bkey), OccTarget::Local(func2, bkey2)) => {
                    func == func2 && bkey == bkey2
                }
                _ => false,
            };
            if !matches {
                continue;
            }

            // If filtering for rename operations, check if this occurrence allows renaming
            if only_rename_allowed && !occ.payload.rename_allowed(db) {
                continue;
            }

            let span = compute_reference_span(db, &occ.payload, target, module);

            if let Some(sp) = span.resolve(db) {
                let k = (sp.file, sp.range.start(), sp.range.end());
                if !seen.insert(k) {
                    continue;
                }
            }
            out.push(Reference {
                top_mod: module,
                span,
            });
        }
    }

    // 3) Method extras: include implementing method def headers in this module for trait methods.
    if let SymbolKey::Method(fd) = key {
        for m in crate::refs::implementing_methods_for_trait_method(db, top_mod, fd) {
            if let Some(span) = m.scope(db).name_span(db) {
                if let Some(sp) = span.resolve(db) {
                    let k = (sp.file, sp.range.start(), sp.range.end());
                    if !seen.insert(k) {
                        continue;
                    }
                }
                if let Some(tm) = span.top_mod(db) {
                    out.push(Reference { top_mod: tm, span });
                }
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
        OccurrencePayload::PathSeg {
            path,
            scope,
            path_lazy,
            ..
        } => {
            let view = hir::path_view::HirPathAdapter::new(db, *path);
            match target {
                OccTarget::Scope(sc) => crate::anchor::anchor_for_scope_match(
                    db,
                    &view,
                    path_lazy.clone(),
                    *path,
                    *scope,
                    sc,
                ),
                _ => {
                    let anchor = hir::path_anchor::AnchorPicker::pick_unresolved_tail(&view);
                    hir::path_anchor::map_path_anchor_to_dyn_lazy(path_lazy.clone(), anchor)
                }
            }
        }
        // For all other occurrence types, use the occurrence's own span
        _ => crate::hover::get_span_from_occurrence(occ),
    }
}
