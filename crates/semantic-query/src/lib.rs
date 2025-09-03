

use hir::HirDb;
use hir::{
    hir_def::{scope_graph::ScopeId, ItemKind, PathId, TopLevelMod, IdentId, ExprId, PatId},
    source_index::{
        unified_occurrence_rangemap_for_top_mod,
        OccurrencePayload,
    },
    span::{DynLazySpan, LazySpan},
    SpannedHirDb,
};
use hir_analysis::name_resolution::method_func_def_from_res;
use hir_analysis::name_resolution::{resolve_with_policy, DomainPreference};
use hir_analysis::ty::canonical::Canonical;
use hir_analysis::ty::func_def::FuncDef;
use hir_analysis::ty::trait_resolution::PredicateListId;
use hir_analysis::ty::ty_check::{check_func_body, RecordLike};
use hir_analysis::HirAnalysisDb;
use parser::{TextRange, TextSize};

/// High-level semantic queries (goto, hover, refs). This thin layer composes
/// HIR + analysis to produce IDE-facing answers without LS coupling.
pub struct SemanticIndex;

pub struct DefinitionLocation<'db> {
    pub top_mod: TopLevelMod<'db>,
    pub span: DynLazySpan<'db>,
}

pub struct HoverInfo<'db> {
    pub top_mod: TopLevelMod<'db>,
    pub span: DynLazySpan<'db>,
    pub contents: String,
}

/// Structured hover data for public API consumption. Semantic, not presentation.
pub struct HoverData<'db> {
    pub top_mod: TopLevelMod<'db>,
    pub span: DynLazySpan<'db>,
    pub signature: Option<String>,
    pub documentation: Option<String>,
    pub kind: &'static str,
}

pub struct Reference<'db> {
    pub top_mod: TopLevelMod<'db>,
    pub span: DynLazySpan<'db>,
}

// Local helper hits derived from unified OccurrencePayload; keeps hir lean
#[derive(Clone)]
struct FieldAccessHit<'db> {
    scope: ScopeId<'db>,
    receiver: ExprId,
    ident: IdentId<'db>,
    name_span: DynLazySpan<'db>,
}

#[derive(Clone)]
struct PatternLabelHit<'db> {
    scope: ScopeId<'db>,
    ident: IdentId<'db>,
    name_span: DynLazySpan<'db>,
    constructor_path: Option<PathId<'db>>,
}

#[derive(Clone)]
struct PathExprSegHit<'db> {
    scope: ScopeId<'db>,
    expr: ExprId,
    path: PathId<'db>,
    seg_idx: usize,
    span: DynLazySpan<'db>,
}

#[derive(Clone)]
struct PathPatSegHit<'db> {
    scope: ScopeId<'db>,
    pat: PatId,
    path: PathId<'db>,
    seg_idx: usize,
    span: DynLazySpan<'db>,
}

impl SemanticIndex {
    pub fn new() -> Self {
        Self
    }

    /// Return all definition candidates at cursor (includes ambiguous/not-found buckets).
    /// REVISIT: This function has a fair bit of branching and duplication.
    /// Consider extracting small helpers like `def_loc_from_res` and
    /// `def_loc_from_name_res` to flatten control flow and centralize the
    /// name-span vs item-span fallback logic (needed for file modules).
    /// Keep the segment-subpath resolution, and later switch `at_cursor`
    /// to a tracked rangemap in `hir` for sublinear lookups.
    pub fn goto_candidates_at_cursor<'db>(
        db: &'db dyn HirAnalysisDb,
        spanned: &'db dyn SpannedHirDb,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Vec<DefinitionLocation<'db>> {
        if let Some(key) = symbol_at_cursor(db, spanned, top_mod, cursor) {
            if let Some((tm, span)) = def_span_for_symbol(db, spanned, key) {
                return vec![DefinitionLocation { top_mod: tm, span }];
            }
        }
        // Method call support: if cursor is on a method name in a call, jump to its definition.
        if let Some(call) = find_method_name_at_cursor(spanned, top_mod, cursor) {
            // Ascend to nearest enclosing function for typing the receiver.
            let mut sc = call.body.scope();
            let mut func_item = None;
            for _ in 0..8 {
                if let Some(item) = sc.to_item() {
                    if let ItemKind::Func(f) = item {
                        func_item = Some(f);
                        break;
                    }
                }
                if let Some(parent) = sc.parent(spanned) {
                    sc = parent;
                } else {
                    break;
                }
            }
            if let Some(func) = func_item {
                let (_diags, typed) = check_func_body(db, func).clone();
                let recv_ty = typed.expr_prop(db, call.receiver).ty;
                let assumptions = PredicateListId::empty_list(db);
                if let Some(fd) = hir_analysis::name_resolution::find_method_id(
                    db,
                    Canonical::new(db, recv_ty),
                    call.ident,
                    call.scope,
                    assumptions,
                ) {
                    // Map method FuncDef to its name span
                    if let Some(span) = fd.scope(db).name_span(db) {
                        if let Some(tm) = span.top_mod(db) {
                            return vec![DefinitionLocation { top_mod: tm, span }];
                        }
                    }
                    if let Some(item) = fd.scope(db).to_item() {
                        let lazy = DynLazySpan::from(item.span());
                        let tm = lazy.top_mod(db).unwrap_or(top_mod);
                        return vec![DefinitionLocation {
                            top_mod: tm,
                            span: lazy,
                        }];
                    }
                }
            }
            // Fall through on failure.
        }
        // Field access support: if cursor is on a record field accessor ident, resolve its field definition.
        if let Some(f) = find_field_access_at_cursor(spanned, top_mod, cursor) {
            // Ascend to nearest enclosing function to type the receiver.
            let mut sc = f.scope;
            let mut func_item = None;
            for _ in 0..8 {
                if let Some(item) = sc.to_item() {
                    if let ItemKind::Func(func) = item {
                        func_item = Some(func);
                        break;
                    }
                }
                if let Some(parent) = sc.parent(spanned) {
                    sc = parent;
                } else {
                    break;
                }
            }
            if let Some(func) = func_item {
                let (_diags, typed) = check_func_body(db, func).clone();
                let recv_ty = typed.expr_prop(db, f.receiver).ty;
                if let Some(field_scope) =
                    RecordLike::from_ty(recv_ty).record_field_scope(db, f.ident)
                {
                    if let Some(span) = field_scope.name_span(db) {
                        if let Some(tm) = span.top_mod(db) {
                            return vec![DefinitionLocation { top_mod: tm, span }];
                        }
                    }
                    if let Some(item) = field_scope.to_item() {
                        let lazy = DynLazySpan::from(item.span());
                        let tm = lazy.top_mod(db).unwrap_or(top_mod);
                        return vec![DefinitionLocation {
                            top_mod: tm,
                            span: lazy,
                        }];
                    }
                }
            }
            // If typing fails, fall through to path logic.
        }
        let Some((path, scope, seg_idx, _dyn_span)) = Self::at_cursor(spanned, top_mod, cursor)
        else {
            return vec![];
        };
        // Use the segment-specific subpath so intermediate segments resolve correctly.
        let tail_idx = path.segment_index(spanned);
        let is_tail = seg_idx == tail_idx;
        // Locals/params goto: if we're on the tail segment of a bare ident inside a function body,
        // jump to the local declaration (let/param) if found. This runs before generic path logic.
        if is_tail && path.parent(spanned).is_none() {
            // Expr reference: typed identity and early return
            if let Some(seg) = find_path_expr_seg_at_cursor(spanned, top_mod, cursor) {
                if let Some(func) = find_enclosing_func(spanned, seg.scope) {
                    if let Some(span) =
                        hir_analysis::ty::ty_check::binding_def_span_for_expr(db, func, seg.expr)
                    {
                        if let Some(tm) = span.top_mod(spanned) {
                            return vec![DefinitionLocation { top_mod: tm, span }];
                        }
                    }
                }
            } else if let Some(pseg) = find_path_pat_seg_at_cursor(spanned, top_mod, cursor) {
                // Pattern declaration: the ident itself is the def
                if let Some(tm) = pseg.span.top_mod(spanned) {
                    return vec![DefinitionLocation {
                        top_mod: tm,
                        span: pseg.span.clone(),
                    }];
                }
                return vec![DefinitionLocation {
                    top_mod,
                    span: pseg.span.clone(),
                }];
            }
        }
        // Pattern label goto: clicking a record pattern label should jump to the field definition.
        if let Some(label) = find_pattern_label_at_cursor(spanned, top_mod, cursor) {
            let assumptions = PredicateListId::empty_list(db);
            if let Some(p) = label.constructor_path {
                if let Ok(res) =
                    resolve_with_policy(db, p, label.scope, assumptions, DomainPreference::Either)
                {
                    use hir_analysis::name_resolution::PathRes;
                    let target_scope = match res {
                        PathRes::EnumVariant(v) => {
                            RecordLike::from_variant(v).record_field_scope(db, label.ident)
                        }
                        PathRes::Ty(ty) => {
                            RecordLike::from_ty(ty).record_field_scope(db, label.ident)
                        }
                        PathRes::TyAlias(_, ty) => {
                            RecordLike::from_ty(ty).record_field_scope(db, label.ident)
                        }
                        _ => None,
                    };
                    if let Some(sc) = target_scope {
                        if let Some(span) = sc.name_span(db) {
                            if let Some(tm) = span.top_mod(db) {
                                return vec![DefinitionLocation { top_mod: tm, span }];
                            }
                        }
                    }
                }
            }
        }
        let seg_path = if is_tail {
            path
        } else {
            path.segment(spanned, seg_idx).unwrap_or(path)
        };
        let assumptions = PredicateListId::empty_list(db);
        let pref = if is_tail {
            DomainPreference::Value
        } else {
            DomainPreference::Either
        };
        match resolve_with_policy(db, seg_path, scope, assumptions, pref) {
            Ok(res) => {
                // If on tail, prefer function/method identity when available
                if is_tail {
                    if let Some(fd) = method_func_def_from_res(&res) {
                        if let Some(span) = fd.scope(db).name_span(db) {
                            if let Some(tm) = span.top_mod(db) {
                                return vec![DefinitionLocation { top_mod: tm, span }];
                            }
                        }
                        if let Some(item) = fd.scope(db).to_item() {
                            let lazy = DynLazySpan::from(item.span());
                            let tm = lazy.top_mod(db).unwrap_or(top_mod);
                            return vec![DefinitionLocation {
                                top_mod: tm,
                                span: lazy,
                            }];
                        }
                    }
                    // Prefer enum variant identity when present
                    if let hir_analysis::name_resolution::PathRes::EnumVariant(v) = &res {
                        let sc = v.variant.scope();
                        if let Some(span) = sc.name_span(db) {
                            if let Some(tm) = span.top_mod(db) {
                                return vec![DefinitionLocation { top_mod: tm, span }];
                            }
                        }
                    }
                }
                // Prefer the canonical name span; fallback to the item's full span (e.g., file modules).
                if let Some(span) = res.name_span(db) {
                    if let Some(tm) = span.top_mod(db) {
                        return vec![DefinitionLocation { top_mod: tm, span }];
                    }
                }
                // Handle functions/methods that don't expose a scope name_span directly (non-tail cases)
                if let Some(fd) = method_func_def_from_res(&res) {
                    if let Some(span) = fd.scope(db).name_span(db) {
                        if let Some(tm) = span.top_mod(db) {
                            return vec![DefinitionLocation { top_mod: tm, span }];
                        }
                    }
                    if let Some(item) = fd.scope(db).to_item() {
                        let lazy = DynLazySpan::from(item.span());
                        let tm = lazy.top_mod(db).unwrap_or(top_mod);
                        return vec![DefinitionLocation {
                            top_mod: tm,
                            span: lazy,
                        }];
                    }
                }
                if let Some(sc) = res.as_scope(db) {
                    if let Some(item) = sc.to_item() {
                        let lazy = DynLazySpan::from(item.span());
                        let tm = lazy.top_mod(db).unwrap_or(top_mod);
                        return vec![DefinitionLocation {
                            top_mod: tm,
                            span: lazy,
                        }];
                    }
                }
                vec![]
            }
            Err(err) => {
                use hir_analysis::name_resolution::PathResErrorKind;
                match err.kind {
                    PathResErrorKind::NotFound { bucket, .. } => {
                        let mut out = Vec::new();
                        for nr in bucket.iter_ok() {
                            // Prefer name span; fallback to item full span (e.g., file modules)
                            if let Some(span) = nr.kind.name_span(db) {
                                if let Some(tm) = span.top_mod(db) {
                                    out.push(DefinitionLocation { top_mod: tm, span });
                                }
                                continue;
                            }
                            if let Some(sc) = nr.scope() {
                                if let Some(item) = sc.to_item() {
                                    let lazy = DynLazySpan::from(item.span());
                                    let tm = lazy.top_mod(db).unwrap_or(top_mod);
                                    out.push(DefinitionLocation {
                                        top_mod: tm,
                                        span: lazy,
                                    });
                                }
                            }
                        }
                        out
                    }
                    PathResErrorKind::Ambiguous(vec) => {
                        let mut out = Vec::new();
                        for nr in vec.into_iter() {
                            if let Some(span) = nr.kind.name_span(db) {
                                if let Some(tm) = span.top_mod(db) {
                                    out.push(DefinitionLocation { top_mod: tm, span });
                                }
                                continue;
                            }
                            if let Some(sc) = nr.scope() {
                                if let Some(item) = sc.to_item() {
                                    let lazy = DynLazySpan::from(item.span());
                                    let tm = lazy.top_mod(db).unwrap_or(top_mod);
                                    out.push(DefinitionLocation {
                                        top_mod: tm,
                                        span: lazy,
                                    });
                                }
                            }
                        }
                        out
                    }
                    _ => vec![],
                }
            }
        }
    }

    /// Convenience: goto definition from a cursor within a module.
    /// REVISIT: apply AnchorPolicy to choose best span if multiple candidates.
    pub fn goto_definition_at_cursor<'db>(
        db: &'db dyn HirAnalysisDb,
        spanned: &'db dyn SpannedHirDb,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Option<DefinitionLocation<'db>> {
        if let Some(key) = symbol_at_cursor(db, spanned, top_mod, cursor) {
            if let Some((tm, span)) = def_span_for_symbol(db, spanned, key) {
                return Some(DefinitionLocation { top_mod: tm, span });
            }
        }
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
    /// Currently scans item headers and bodies via a visitor.
    /// REVISIT: replace with OccurrenceIndex-backed rangemap for reverse lookups.
    pub fn at_cursor<'db>(
        db: &'db dyn SpannedHirDb,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Option<(PathId<'db>, ScopeId<'db>, usize, DynLazySpan<'db>)> {
        // REVISIT: cache per-top_mod reverse index for fast lookup.
        let idx = build_span_reverse_index(db, top_mod);
        find_path_at_cursor_from_index(db, &idx, cursor)
    }

    /// Produce simple hover info at the cursor by resolving the path and summarizing it.
    /// REVISIT: enrich contents (signature, type params, docs) once available.
    pub fn hover_at_cursor<'db>(
        db: &'db dyn HirAnalysisDb,
        spanned: &'db dyn SpannedHirDb,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Option<HoverInfo<'db>> {
        // Use at_cursor via SpannedHirDb to pick the path + scope and the dyn span to highlight.
        let (path, scope, _seg_idx, dyn_span) = Self::at_cursor(spanned, top_mod, cursor)?;
        let res = resolve_with_policy(
            db,
            path,
            scope,
            PredicateListId::empty_list(db),
            DomainPreference::Either,
        )
        .ok()?;
        let mut parts: Vec<String> = Vec::new();

        // REVISIT: fetch richer docs (processed Markdown, extern docs) once available.
        if let Some(sc) = res.as_scope(db) {
            if let Some(pretty) = sc.pretty_path(spanned) {
                parts.push(format!("```fe\n{}\n```", pretty));
            }
            if let Some(doc) = get_docstring(spanned, sc) {
                parts.push(doc);
            }
            if let Some(item) = sc.to_item() {
                if let Some(def) = get_item_definition_markdown(spanned, item) {
                    parts.push(def);
                }
            }
        }

        if parts.is_empty() {
            parts.push(summarize_resolution(db, &res));
        }
        Some(HoverInfo {
            top_mod,
            span: dyn_span,
            contents: parts.join("\n\n"),
        })
    }

    /// Structured hover data (signature, docs, kind) for the symbol at cursor.
    pub fn hover_info_for_symbol_at_cursor<'db>(
        db: &'db dyn HirAnalysisDb,
        spanned: &'db dyn SpannedHirDb,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Option<HoverData<'db>> {
        let (path, scope, _seg_idx, dyn_span) = Self::at_cursor(spanned, top_mod, cursor)?;
        let res = resolve_with_policy(
            db,
            path,
            scope,
            PredicateListId::empty_list(db),
            DomainPreference::Either,
        )
        .ok()?;
        let kind = res.kind_name();
        let signature = res.pretty_path(db);
        let documentation = res
            .as_scope(db)
            .and_then(|sc| get_docstring(spanned, sc));
        Some(HoverData { top_mod, span: dyn_span, signature, documentation, kind })
    }

    /// Public identity API for consumers.
    pub fn symbol_identity_at_cursor<'db>(
        db: &'db dyn HirAnalysisDb,
        spanned: &'db dyn SpannedHirDb,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Option<SymbolKey<'db>> {
        symbol_at_cursor(db, spanned, top_mod, cursor)
    }

    /// Public definition API for consumers.
    pub fn definition_for_symbol<'db>(
        db: &'db dyn HirAnalysisDb,
        spanned: &'db dyn SpannedHirDb,
        key: SymbolKey<'db>,
    ) -> Option<(TopLevelMod<'db>, DynLazySpan<'db>)> {
        def_span_for_symbol(db, spanned, key)
    }

    /// Public references API for consumers.
    pub fn references_for_symbol<'db>(
        db: &'db dyn HirAnalysisDb,
        spanned: &'db dyn SpannedHirDb,
        top_mod: TopLevelMod<'db>,
        key: SymbolKey<'db>,
    ) -> Vec<Reference<'db>> {
        find_refs_for_symbol(db, spanned, top_mod, key)
    }

    /// Find references to the symbol under the cursor, within the given top module.
    /// Identity-first: picks a SymbolKey at the cursor, then resolves refs.
    pub fn find_references_at_cursor<'db>(
        db: &'db dyn HirAnalysisDb,
        spanned: &'db dyn SpannedHirDb,
        top_mod: TopLevelMod<'db>,
        cursor: TextSize,
    ) -> Vec<Reference<'db>> {
        if let Some(key) = symbol_at_cursor(db, spanned, top_mod, cursor) {
            return find_refs_for_symbol(db, spanned, top_mod, key);
        }
        Vec::new()
    }
}

// (unused helper functions removed)

// ---------- Reverse Span Index (structural backbone)
// REVISIT: Replace Vec-based index with a tracked rangemap/interval tree in hir for sublinear lookups.

#[derive(Debug, Clone)]
struct SpanEntry<'db> {
    start: TextSize,
    end: TextSize,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    seg_idx: usize,
    span: DynLazySpan<'db>,
}

/// Build a per-module reverse span index of all path segment spans.
/// REVISIT: move to `hir` as a tracked query keyed by top_mod with granular invalidation.
fn build_span_reverse_index<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<SpanEntry<'db>> {
    let mut entries: Vec<SpanEntry<'db>> = Vec::new();
    // Use unified rangemap; entries are sorted by (start, width).
    for e in unified_occurrence_rangemap_for_top_mod(db, top_mod).iter() {
        if let OccurrencePayload::PathSeg { path, scope, seg_idx, span, .. } = &e.payload {
            entries.push(SpanEntry {
                start: e.start,
                end: e.end,
                path: *path,
                scope: *scope,
                seg_idx: *seg_idx,
                span: span.clone(),
            });
        }
    }
    entries
}

fn find_method_name_at_cursor<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: TextSize,
) -> Option<hir::source_index::MethodCallEntry<'db>> {
    let mut best: Option<(hir::source_index::MethodCallEntry<'db>, TextSize)> = None;
    for e in unified_occurrence_rangemap_for_top_mod(db, top_mod).iter() {
        if let OccurrencePayload::MethodName { scope, body, ident, receiver, span } = &e.payload {
            if let Some(sp) = span.clone().resolve(db) {
                let range = sp.range;
                if range.contains(cursor) {
                    let width: TextSize = range.end() - range.start();
                    let entry = hir::source_index::MethodCallEntry { scope: *scope, body: *body, receiver: *receiver, ident: *ident, name_span: span.clone() };
                    best = match best {
                        None => Some((entry, width)),
                        Some((_, bw)) if width < bw => Some((entry, width)),
                        Some(b) => Some(b),
                    };
                }
            }
        }
    }
    best.map(|(e, _)| e)
}

fn find_header_name_scope_at_cursor<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: TextSize,
) -> Option<hir::hir_def::scope_graph::ScopeId<'db>> {
    let mut best: Option<(hir::hir_def::ItemKind<'db>, TextSize)> = None;
    for item in top_mod.all_items(db).iter() {
        if let Some(name_span) = item.name_span() {
            if let Some(sp) = name_span.resolve(db) {
                if sp.range.contains(cursor) {
                    let w: TextSize = sp.range.end() - sp.range.start();
                    best = match best {
                        None => Some((*item, w)),
                        Some((_it, bw)) if w < bw => Some((*item, w)),
                        Some(b) => Some(b),
                    };
                }
            }
        }
    }
    best.map(|(it, _)| hir::hir_def::scope_graph::ScopeId::from_item(it))
}

fn find_variant_decl_at_cursor<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: TextSize,
) -> Option<hir::hir_def::EnumVariant<'db>> {
    let mut best: Option<(hir::hir_def::EnumVariant<'db>, TextSize)> = None;
    for item in top_mod.all_items(db).iter() {
        if let ItemKind::Enum(e) = *item {
            let variants = e.variants(db);
            for (idx, vdef) in variants.data(db).iter().enumerate() {
                if vdef.name.to_opt().is_none() {
                    continue;
                }
                let v = hir::hir_def::EnumVariant::new(e, idx);
                if let Some(span) = v.span().name().resolve(db) {
                    if span.range.contains(cursor) {
                        let w: TextSize = span.range.end() - span.range.start();
                        best = match best {
                            None => Some((v, w)),
                            Some((_vb, bw)) if w < bw => Some((v, w)),
                            Some(b) => Some(b),
                        };
                    }
                }
            }
        }
    }
    best.map(|(v, _)| v)
}

fn find_func_def_name_at_cursor<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: TextSize,
) -> Option<(hir::hir_def::item::Func<'db>, DynLazySpan<'db>)> {
    for item in top_mod.all_items(db).iter() {
        if let ItemKind::Func(f) = *item {
            let lazy_name = f.span().name();
            if let Some(sp) = lazy_name.resolve(db) {
                if sp.range.contains(cursor) {
                    return Some((f, lazy_name.into()));
                }
            }
        }
    }
    None
}

fn find_field_access_at_cursor<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: TextSize,
) -> Option<FieldAccessHit<'db>> {
    let mut best: Option<(FieldAccessHit<'db>, TextSize)> = None;
    for e in unified_occurrence_rangemap_for_top_mod(db, top_mod).iter() {
        if let OccurrencePayload::FieldAccessName { scope, body: _, ident, receiver, span } = &e.payload {
            if let Some(sp) = span.clone().resolve(db) {
                let range = sp.range;
                if range.contains(cursor) {
                    let width: TextSize = range.end() - range.start();
                    let entry = FieldAccessHit { scope: *scope, receiver: *receiver, ident: *ident, name_span: span.clone() };
                    best = match best {
                        None => Some((entry, width)),
                        Some((_, bw)) if width < bw => Some((entry, width)),
                        Some(b) => Some(b),
                    };
                }
            }
        }
    }
    best.map(|(e, _)| e)
}

fn find_pattern_label_at_cursor<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: TextSize,
) -> Option<PatternLabelHit<'db>> {
    let mut best: Option<(PatternLabelHit<'db>, TextSize)> = None;
    for e in unified_occurrence_rangemap_for_top_mod(db, top_mod).iter() {
        if let OccurrencePayload::PatternLabelName { scope, body: _, ident, constructor_path, span } = &e.payload {
            if let Some(sp) = span.clone().resolve(db) {
                let range = sp.range;
                if range.contains(cursor) {
                    let width: TextSize = range.end() - range.start();
                    let entry = PatternLabelHit { scope: *scope, ident: *ident, name_span: span.clone(), constructor_path: *constructor_path };
                    best = match best {
                        None => Some((entry, width)),
                        Some((_, bw)) if width < bw => Some((entry, width)),
                        Some(b) => Some(b),
                    };
                }
            }
        }
    }
    best.map(|(e, _)| e)
}

fn find_path_expr_seg_at_cursor<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: TextSize,
) -> Option<PathExprSegHit<'db>> {
    let mut best: Option<(PathExprSegHit<'db>, TextSize)> = None;
    for e in unified_occurrence_rangemap_for_top_mod(db, top_mod).iter() {
        if let OccurrencePayload::PathExprSeg { scope, body: _, expr, path, seg_idx, span } = &e.payload {
            if let Some(sp) = span.clone().resolve(db) {
                let range = sp.range;
                if range.contains(cursor) {
                    let width: TextSize = range.end() - range.start();
                    let entry = PathExprSegHit { scope: *scope, expr: *expr, path: *path, seg_idx: *seg_idx, span: span.clone() };
                    best = match best {
                        None => Some((entry, width)),
                        Some((_, bw)) if width < bw => Some((entry, width)),
                        Some(b) => Some(b),
                    };
                }
            }
        }
    }
    best.map(|(e, _)| e)
}

fn find_path_pat_seg_at_cursor<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: TextSize,
) -> Option<PathPatSegHit<'db>> {
    let mut best: Option<(PathPatSegHit<'db>, TextSize)> = None;
    for e in unified_occurrence_rangemap_for_top_mod(db, top_mod).iter() {
        if let OccurrencePayload::PathPatSeg { scope, body: _, pat, path, seg_idx, span } = &e.payload {
            if let Some(sp) = span.clone().resolve(db) {
                let range = sp.range;
                if range.contains(cursor) {
                    let width: TextSize = range.end() - range.start();
                    let entry = PathPatSegHit { scope: *scope, pat: *pat, path: *path, seg_idx: *seg_idx, span: span.clone() };
                    best = match best {
                        None => Some((entry, width)),
                        Some((_, bw)) if width < bw => Some((entry, width)),
                        Some(b) => Some(b),
                    };
                }
            }
        }
    }
    best.map(|(e, _)| e)
}

fn find_enclosing_func_item<'db>(
    db: &'db dyn SpannedHirDb,
    mut scope: ScopeId<'db>,
) -> Option<ItemKind<'db>> {
    for _ in 0..16 {
        if let Some(item) = scope.to_item() {
            if matches!(item, ItemKind::Func(_)) {
                return Some(item);
            }
        }
        if let Some(parent) = scope.parent(db) {
            scope = parent;
        } else {
            break;
        }
    }
    None
}

fn find_enclosing_func<'db>(
    db: &'db dyn SpannedHirDb,
    scope: ScopeId<'db>,
) -> Option<hir::hir_def::item::Func<'db>> {
    match find_enclosing_func_item(db, scope) {
        Some(ItemKind::Func(f)) => Some(f),
        _ => None,
    }
}

fn find_path_at_cursor_from_index<'db>(
    _db: &'db dyn SpannedHirDb,
    index: &[SpanEntry<'db>],
    cursor: TextSize,
) -> Option<(PathId<'db>, ScopeId<'db>, usize, DynLazySpan<'db>)> {
    // Binary search on start positions, then scan local neighborhood for the smallest covering range.
    if index.is_empty() {
        return None;
    }
    // Find first entry with start > cursor
    let mut lo = 0usize;
    let mut hi = index.len();
    while lo < hi {
        let mid = (lo + hi) / 2;
        if index[mid].start <= cursor {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    let mut best_i: Option<usize> = None;
    let mut best_w: Option<TextSize> = None;

    // Scan left from insertion point
    let mut i = lo;
    while i > 0 {
        i -= 1;
        let e = &index[i];
        if e.start > cursor {
            break;
        }
        if TextRange::new(e.start, e.end).contains(cursor) {
            let w = e.end - e.start;
            match best_w {
                None => {
                    best_w = Some(w);
                    best_i = Some(i);
                }
                Some(bw) if w < bw => {
                    best_w = Some(w);
                    best_i = Some(i);
                }
                _ => {}
            }
        } else if e.end < cursor {
            // Since entries are sorted by start, once end < cursor on a start <= cursor, earlier entries won't contain cursor
            // But they could still overlap; keep scanning a few steps back just in case
            // We opt to continue one more iteration; break when a gap is clearly before cursor
            if i == 0 || index[i - 1].end < cursor {
                break;
            }
        }
    }
    // Scan rightwards among entries that start == cursor
    let mut j = lo;
    while j < index.len() {
        let e = &index[j];
        if e.start != cursor {
            break;
        }
        if TextRange::new(e.start, e.end).contains(cursor) {
            let w = e.end - e.start;
            match best_w {
                None => {
                    best_w = Some(w);
                    best_i = Some(j);
                }
                Some(bw) if w < bw => {
                    best_w = Some(w);
                    best_i = Some(j);
                }
                _ => {}
            }
        }
        j += 1;
    }
    best_i.map(|k| {
        let e = &index[k];
        (e.path, e.scope, e.seg_idx, e.span.clone())
    })
}

fn summarize_resolution<'db>(
    db: &'db dyn HirAnalysisDb,
    res: &hir_analysis::name_resolution::PathRes<'db>,
) -> String {
    use hir_analysis::name_resolution::PathRes;
    match res {
        PathRes::Ty(ty) => format!("type: {}", ty.pretty_print(db)),
        PathRes::TyAlias(alias, _) => {
            let name = alias
                .alias
                .name(db)
                .to_opt()
                .map(|i| i.data(db))
                .map(|s| s.as_str())
                .unwrap_or("_");
            format!("type alias: {}", name)
        }
        PathRes::Func(ty) => format!("function: {}", ty.pretty_print(db)),
        PathRes::Const(ty) => format!("const: {}", ty.pretty_print(db)),
        PathRes::Trait(inst) => {
            let def = inst.def(db);
            let name = def
                .trait_(db)
                .name(db)
                .to_opt()
                .map(|i| i.data(db))
                .map(|s| s.as_str())
                .unwrap_or("<trait>");
            format!("trait: {}", name)
        }
        PathRes::EnumVariant(v) => {
            let n = v.variant.name(db).unwrap_or("<variant>");
            format!("enum variant: {}", n)
        }
        PathRes::Mod(scope) => format!("module: {:?}", scope),
        PathRes::Method(..) => "method".into(),
        PathRes::FuncParam(item, idx) => {
            let n = match item {
                ItemKind::Func(f) => f
                    .name(db)
                    .to_opt()
                    .map(|i| i.data(db))
                    .map(|s| s.as_str())
                    .unwrap_or("<func>"),
                _ => "<item>",
            };
            format!("function param {} of {}", idx, n)
        }
    }
}

fn get_docstring(db: &dyn HirDb, scope: hir::hir_def::scope_graph::ScopeId) -> Option<String> {
    use hir::hir_def::Attr;
    scope
        .attrs(db)?
        .data(db)
        .iter()
        .filter_map(|attr| match attr {
            Attr::DocComment(doc) => Some(doc.text.data(db).clone()),
            _ => None,
        })
        .reduce(|a, b| a + "\n" + &b)
}

fn get_item_definition_markdown(db: &dyn SpannedHirDb, item: ItemKind) -> Option<String> {
    // REVISIT: leverage AST-side helpers to avoid string slicing.
    let span = item.span().resolve(db)?;
    let mut start: usize = span.range.start().into();
    // If the item has a body or children, cut that stuff out; else use full span end.
    let end: usize = match item {
        ItemKind::Func(func) => func.body(db)?.span().resolve(db)?.range.start().into(),
        ItemKind::Mod(module) => module
            .scope()
            .name_span(db)?
            .resolve(db)?
            .range
            .end()
            .into(),
        _ => span.range.end().into(),
    };

    // Start at the beginning of the line where the name is defined.
    if let Some(name_span) = item.name_span()?.resolve(db) {
        let mut name_line_start: usize = name_span.range.start().into();
        let file_text = span.file.text(db).as_str();
        while name_line_start > 0
            && file_text.chars().nth(name_line_start - 1).unwrap_or('\n') != '\n'
        {
            name_line_start -= 1;
        }
        start = name_line_start;
    }

    let file_text = span.file.text(db).as_str();
    let item_def = &file_text[start..end];
    Some(format!("```fe\n{}\n```", item_def.trim()))
}

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

fn symbol_key_from_res<'db>(
    db: &'db dyn HirAnalysisDb,
    res: &hir_analysis::name_resolution::PathRes<'db>,
) -> Option<SymbolKey<'db>> {
    use hir_analysis::name_resolution::PathRes;
    match res {
        PathRes::Ty(_)
        | PathRes::Func(_)
        | PathRes::Const(_)
        | PathRes::TyAlias(..)
        | PathRes::Trait(_)
        | PathRes::Mod(_) => res.as_scope(db).map(SymbolKey::Scope),
        PathRes::EnumVariant(v) => Some(SymbolKey::EnumVariant(v.variant)),
        PathRes::FuncParam(item, idx) => Some(SymbolKey::FuncParam(*item, *idx)),
        PathRes::Method(..) => method_func_def_from_res(res).map(SymbolKey::Method),
    }
}

/// Public API: Return implementing methods for a trait method FuncDef, limited to the given top module.
/// If `fd` is not a trait method, returns an empty Vec.
pub fn equivalent_methods<'db>(
    db: &'db dyn HirAnalysisDb,
    spanned: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    fd: FuncDef<'db>,
) -> Vec<FuncDef<'db>> {
    let Some(func) = fd.hir_func_def(db) else { return Vec::new() };
    let Some(parent) = func.scope().parent(spanned) else { return Vec::new() };
    let ScopeId::Item(ItemKind::Trait(trait_item)) = parent else { return Vec::new() };
    let name = fd.name(db);
    let assumptions = PredicateListId::empty_list(db);
    let mut out = Vec::new();
    for it in top_mod.all_impl_traits(spanned) {
        let Some(tr_ref) = it.trait_ref(spanned).to_opt() else { continue };
        let hir::hir_def::Partial::Present(path) = tr_ref.path(spanned) else { continue };
        let Ok(hir_analysis::name_resolution::PathRes::Trait(tr_inst)) = resolve_with_policy(
            db,
            path,
            it.scope(),
            assumptions,
            DomainPreference::Type,
        ) else { continue };
        if tr_inst.def(db).trait_(db) != trait_item { continue; }
        for child in it.children_non_nested(spanned) {
            if let ItemKind::Func(impl_fn) = child {
                if impl_fn.name(spanned).to_opt() == Some(name) {
                    if let Some(fd2) = hir_analysis::ty::func_def::lower_func(db, impl_fn) {
                        out.push(fd2);
                    }
                }
            }
        }
    }
    out
}

// Unified identity at cursor
fn symbol_at_cursor<'db>(
    db: &'db dyn HirAnalysisDb,
    spanned: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    cursor: TextSize,
) -> Option<SymbolKey<'db>> {
    // 1) Method call name
    if let Some(call) = find_method_name_at_cursor(spanned, top_mod, cursor) {
        // Ascend to function to type receiver
        if let Some(func) = find_enclosing_func(spanned, call.body.scope()) {
            let (_diags, typed) = check_func_body(db, func).clone();
            let recv_ty = typed.expr_prop(db, call.receiver).ty;
            let assumptions = PredicateListId::empty_list(db);
            if let Some(fd) = hir_analysis::name_resolution::find_method_id(
                db,
                Canonical::new(db, recv_ty),
                call.ident,
                call.scope,
                assumptions,
            ) {
                return Some(SymbolKey::Method(fd));
            }
        }
    }

    // 1b) Function/method definition header name → if it's a method, use Method identity
    if let Some((func_item, _name_span)) = find_func_def_name_at_cursor(spanned, top_mod, cursor) {
        if let Some(fd) = hir_analysis::ty::func_def::lower_func(db, func_item) {
            if fd.is_method(db) {
                return Some(SymbolKey::Method(fd));
            } else {
                // Associated function def header: treat as function scope identity
                return Some(SymbolKey::Scope(func_item.scope()));
            }
        }
    }

    // 2) Path expr segment
    if let Some(seg) = find_path_expr_seg_at_cursor(spanned, top_mod, cursor) {
        // Local binding first
        if let Some(func) = find_enclosing_func(spanned, seg.scope) {
            if let Some(bkey) =
                hir_analysis::ty::ty_check::expr_binding_key_for_expr(db, func, seg.expr)
            {
                return Some(SymbolKey::Local(func, bkey));
            }
        }
        // Else use resolution
        let seg_path = seg.path.segment(spanned, seg.seg_idx).unwrap_or(seg.path);
        if let Ok(res) = resolve_with_policy(
            db,
            seg_path,
            seg.scope,
            PredicateListId::empty_list(db),
            DomainPreference::Either,
        ) {
            if let Some(k) = symbol_key_from_res(db, &res) {
                return Some(k);
            }
        }
    }

    // 3) Path pattern segment → Local declaration
    if let Some(pseg) = find_path_pat_seg_at_cursor(spanned, top_mod, cursor) {
        // find enclosing function for coherence (not strictly needed for def span)
        if let Some(func) = find_enclosing_func(spanned, pseg.scope) {
            return Some(SymbolKey::Local(
                func,
                hir_analysis::ty::ty_check::BindingKey::LocalPat(pseg.pat),
            ));
        }
    }

    // 4) Field accessor name → field scope
    if let Some(f) = find_field_access_at_cursor(spanned, top_mod, cursor) {
        if let Some(func) = find_enclosing_func(spanned, f.scope) {
            let (_diags, typed) = check_func_body(db, func).clone();
            let recv_ty = typed.expr_prop(db, f.receiver).ty;
            if let Some(field_scope) = RecordLike::from_ty(recv_ty).record_field_scope(db, f.ident)
            {
                return Some(SymbolKey::Scope(field_scope));
            }
        }
    }

    // 5) Variant header name
    if let Some(variant) = find_variant_decl_at_cursor(spanned, top_mod, cursor) {
        return Some(SymbolKey::EnumVariant(variant));
    }
    // 6) Item header name
    if let Some(sc) = find_header_name_scope_at_cursor(spanned, top_mod, cursor) {
        return Some(SymbolKey::Scope(sc));
    }
    None
}

// Definition span for a SymbolKey
fn def_span_for_symbol<'db>(
    db: &'db dyn HirAnalysisDb,
    spanned: &'db dyn SpannedHirDb,
    key: SymbolKey<'db>,
) -> Option<(TopLevelMod<'db>, DynLazySpan<'db>)> {
    match key {
        SymbolKey::Local(func, bkey) => {
            let span = hir_analysis::ty::ty_check::binding_def_span_in_func(db, func, bkey)?;
            let tm = span.top_mod(spanned)?;
            Some((tm, span))
        }
        SymbolKey::Method(fd) => {
            if let Some(span) = fd.scope(db).name_span(db) {
                let tm = span.top_mod(db)?;
                Some((tm, span))
            } else if let Some(item) = fd.scope(db).to_item() {
                let lazy = DynLazySpan::from(item.span());
                let tm = lazy.top_mod(spanned)?;
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
    db: &'db dyn HirAnalysisDb,
    spanned: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    key: SymbolKey<'db>,
) -> Vec<Reference<'db>> {
    match key {
        SymbolKey::Local(func, bkey) => {
            let spans = hir_analysis::ty::ty_check::binding_refs_in_func(db, func, bkey);
            spans
                .into_iter()
                .filter_map(|span| {
                    let tm = span.top_mod(spanned)?;
                    Some(Reference { top_mod: tm, span })
                })
                .collect()
        }
        SymbolKey::Method(fd) => {
            let mut out = Vec::new();
            // include declaration name
            if let Some(span) = fd.scope(db).name_span(db) {
                if let Some(tm) = span.top_mod(db) {
                    out.push(Reference { top_mod: tm, span });
                }
            }
            // method calls by typed identity
            for occ in unified_occurrence_rangemap_for_top_mod(spanned, top_mod).iter() {
                if let OccurrencePayload::MethodName { scope, body, receiver, ident, span: name_span } = &occ.payload {
                    if let Some(func) = find_enclosing_func(spanned, body.scope()) {
                        let (_diags, typed) = check_func_body(db, func).clone();
                        let recv_ty = typed.expr_prop(db, *receiver).ty;
                        let assumptions = PredicateListId::empty_list(db);
                        if let Some(cand) = hir_analysis::name_resolution::find_method_id(
                            db,
                            Canonical::new(db, recv_ty),
                            *ident,
                            *scope,
                            assumptions,
                        ) {
                            if cand == fd {
                                out.push(Reference { top_mod, span: name_span.clone() });
                            }
                        }
                    }
                }
            }
            // UFCS/associated paths: include both
            // - Paths to the same function scope (PathRes::Func -> TyBase::Func)
            // - Paths resolved as methods that match the same FuncDef identity
            let func_scope = fd.scope(db);
            let assumptions = PredicateListId::empty_list(db);
            for occ in unified_occurrence_rangemap_for_top_mod(spanned, top_mod).iter() {
                let (p, s, path_lazy) = match &occ.payload {
                    OccurrencePayload::PathSeg { path, scope, path_lazy, .. } => (*path, *scope, path_lazy.clone()),
                    _ => continue,
                };
                let Ok(res) = resolve_with_policy(db, p, s, assumptions, DomainPreference::Either)
                else {
                    continue;
                };
                let matches_fd = match method_func_def_from_res(&res) {
                    Some(mfd) => mfd == fd,
                    None => false,
                };
                if matches_fd || res.as_scope(db) == Some(func_scope) {
                    let view = hir::path_view::HirPathAdapter::new(spanned, p);
                    // If the whole path resolves to the function scope, anchor on the segment
                    // that resolves to that scope; otherwise, anchor at the tail (method name).
                    let span = if res.as_scope(db) == Some(func_scope) {
                        anchor_for_scope_match(spanned, db, &view, path_lazy.clone(), p, s, func_scope)
                    } else {
                        let anchor = hir::path_anchor::AnchorPicker::pick_unresolved_tail(&view);
                        hir::path_anchor::map_path_anchor_to_dyn_lazy(path_lazy.clone(), anchor)
                    };
                    out.push(Reference { top_mod, span });
                }
            }
            // If this is a trait method, include def-site headers of all implementing methods in impl-trait blocks.
            if let Some(func) = fd.hir_func_def(db) {
                // Determine if the func is defined inside a trait item
                if let Some(parent) = func.scope().parent(spanned) {
                    if let ScopeId::Item(ItemKind::Trait(trait_item)) = parent {
                        let method_name = fd.name(db);
                        let assumptions = PredicateListId::empty_list(db);
                        // Iterate impl-trait blocks in this top module
                        for it in top_mod.all_impl_traits(spanned) {
                            // Resolve the trait of this impl-trait; skip if not the same trait
                            if let Some(tr_ref) = it.trait_ref(spanned).to_opt() {
                                if let hir::hir_def::Partial::Present(p) = tr_ref.path(spanned) {
                                    if let Ok(hir_analysis::name_resolution::PathRes::Trait(tr_inst)) = resolve_with_policy(
                                        db,
                                        p,
                                        it.scope(),
                                        assumptions,
                                        DomainPreference::Type,
                                    ) {
                                        if tr_inst.def(db).trait_(db) != trait_item {
                                            continue;
                                        }
                                    } else {
                                        continue;
                                    }
                                } else {
                                    continue;
                                }
                            } else {
                                continue;
                            }

                            // Find the impl method with the same name
                            for child in it.children_non_nested(spanned) {
                                if let ItemKind::Func(impl_fn) = child {
                                    if impl_fn.name(spanned).to_opt() == Some(method_name) {
                                        let span: DynLazySpan = impl_fn.span().name().into();
                                        if let Some(tm) = span.top_mod(spanned) {
                                            out.push(Reference { top_mod: tm, span });
                                        } else if let Some(sc_name) = impl_fn.scope().name_span(db) {
                                            if let Some(tm) = sc_name.top_mod(db) {
                                                out.push(Reference { top_mod: tm, span: sc_name });
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            out
        }
        SymbolKey::EnumVariant(variant) => {
            let mut out = Vec::new();
            if let Some(def_name) = variant.scope().name_span(db) {
                if let Some(tm) = def_name.top_mod(db) {
                    out.push(Reference {
                        top_mod: tm,
                        span: def_name,
                    });
                }
            }
            let assumptions = PredicateListId::empty_list(db);
            for occ in unified_occurrence_rangemap_for_top_mod(spanned, top_mod).iter() {
                let (p, s, path_lazy) = match &occ.payload {
                    OccurrencePayload::PathSeg { path, scope, path_lazy, .. } => (*path, *scope, path_lazy.clone()),
                    _ => continue,
                };
                let Ok(res) = resolve_with_policy(db, p, s, assumptions, DomainPreference::Either)
                else {
                    continue;
                };
                if let hir_analysis::name_resolution::PathRes::EnumVariant(v2) = res {
                    if v2.variant == variant {
                        let view = hir::path_view::HirPathAdapter::new(spanned, p);
                        let anchor = hir::path_anchor::AnchorPicker::pick_unresolved_tail(&view);
                        let span =
                            hir::path_anchor::map_path_anchor_to_dyn_lazy(path_lazy.clone(), anchor);
                        out.push(Reference { top_mod, span });
                    }
                }
            }
            out
        }
        SymbolKey::Scope(target_sc) => {
            let mut out = Vec::new();
            if let Some(def_name) = target_sc.name_span(db) {
                if let Some(tm) = def_name.top_mod(db) {
                    out.push(Reference {
                        top_mod: tm,
                        span: def_name,
                    });
                }
            }
            let assumptions = PredicateListId::empty_list(db);
            // If the scope is an enum item, do not include variant occurrences
            // when searching for references to the enum itself.
            let is_enum_item = matches!(target_sc, ScopeId::Item(ItemKind::Enum(_)));
            for occ in unified_occurrence_rangemap_for_top_mod(spanned, top_mod).iter() {
                let (p, s, path_lazy) = match &occ.payload {
                    OccurrencePayload::PathSeg { path, scope, path_lazy, .. } => (*path, *scope, path_lazy.clone()),
                    _ => continue,
                };
                let Ok(res) = resolve_with_policy(db, p, s, assumptions, DomainPreference::Either)
                else {
                    continue;
                };
                if is_enum_item {
                    // Skip variant occurrences to keep enum refs identity-clean.
                    if matches!(res, hir_analysis::name_resolution::PathRes::EnumVariant(_)) {
                        continue;
                    }
                }
                // Match either direct scope equality (e.g., PathRes::Func -> function scope)
                // or method/UFCS resolutions whose FuncDef scope matches the target scope.
                let method_matches =
                    method_func_def_from_res(&res).map_or(false, |fd| fd.scope(db) == target_sc);
                if res.as_scope(db) == Some(target_sc) || method_matches {
                    let view = hir::path_view::HirPathAdapter::new(spanned, p);
                    let span = anchor_for_scope_match(spanned, db, &view, path_lazy.clone(), p, s, target_sc);
                    out.push(Reference { top_mod, span });
                }
            }
            out
        }
        SymbolKey::FuncParam(item, idx) => {
            let sc = hir::hir_def::scope_graph::ScopeId::FuncParam(item, idx);
            let mut out = Vec::new();
            if let Some(def_name) = sc.name_span(db) {
                if let Some(tm) = def_name.top_mod(db) {
                    out.push(Reference {
                        top_mod: tm,
                        span: def_name,
                    });
                }
            }
            let assumptions = PredicateListId::empty_list(db);
            for occ in unified_occurrence_rangemap_for_top_mod(spanned, top_mod).iter() {
                let (p, s, path_lazy) = match &occ.payload {
                    OccurrencePayload::PathSeg { path, scope, path_lazy, .. } => (*path, *scope, path_lazy.clone()),
                    _ => continue,
                };
                let Ok(res) = resolve_with_policy(db, p, s, assumptions, DomainPreference::Either)
                else {
                    continue;
                };
                if res.as_scope(db) == Some(sc) {
                    let view = hir::path_view::HirPathAdapter::new(spanned, p);
                    let span = anchor_for_scope_match(spanned, db, &view, path_lazy.clone(), p, s, sc);
                    out.push(Reference { top_mod, span });
                }
            }
            out
        }
    }
}

fn anchor_for_scope_match<'db>(
    spanned: &'db dyn SpannedHirDb,
    db: &'db dyn HirAnalysisDb,
    view: &hir::path_view::HirPathAdapter<'db>,
    lazy_path: hir::span::path::LazyPathSpan<'db>,
    p: PathId<'db>,
    s: ScopeId<'db>,
    target_sc: ScopeId<'db>,
) -> DynLazySpan<'db> {
    let assumptions = PredicateListId::empty_list(db);
    let tail = p.segment_index(spanned);
    for i in 0..=tail {
        let seg_path = p.segment(spanned, i).unwrap_or(p);
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
