use parser::TextSize;

use crate::{
    hir_def::{
        Body, Expr, ExprId, IdentId, Partial, Pat, PatId, PathId, TopLevelMod, UseAlias, UsePathId,
        UsePathSegment,
    },
    span::path::LazyPathSpan,
    span::{DynLazySpan, LazySpan},
    visitor::{prelude::LazyPathSpan as VisitorLazyPathSpan, Visitor, VisitorCtxt},
    SpannedHirDb,
};

// (legacy segment-span projections removed)

// ---------- Unified occurrence rangemap ----------

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub enum OccurrencePayload<'db> {
    PathSeg {
        path: PathId<'db>,
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        seg_idx: usize,
        path_lazy: LazyPathSpan<'db>,
        span: DynLazySpan<'db>,
    },
    UsePathSeg {
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        path: UsePathId<'db>,
        seg_idx: usize,
        span: DynLazySpan<'db>,
    },
    UseAliasName {
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        ident: IdentId<'db>,
        span: DynLazySpan<'db>,
    },
    MethodName {
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        body: Body<'db>,
        ident: IdentId<'db>,
        receiver: ExprId,
        span: DynLazySpan<'db>,
    },
    FieldAccessName {
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        body: Body<'db>,
        ident: IdentId<'db>,
        receiver: ExprId,
        span: DynLazySpan<'db>,
    },
    PatternLabelName {
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        body: Body<'db>,
        ident: IdentId<'db>,
        constructor_path: Option<PathId<'db>>,
        span: DynLazySpan<'db>,
    },
    PathExprSeg {
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        body: Body<'db>,
        expr: ExprId,
        path: PathId<'db>,
        seg_idx: usize,
        span: DynLazySpan<'db>,
    },
    PathPatSeg {
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        body: Body<'db>,
        pat: PatId,
        path: PathId<'db>,
        seg_idx: usize,
        span: DynLazySpan<'db>,
    },
    /// Name token of an item/variant/param header. Allows goto/hover via the unified index.
    ItemHeaderName {
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        span: DynLazySpan<'db>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct OccurrenceRangeEntry<'db> {
    pub start: TextSize,
    pub end: TextSize,
    pub payload: OccurrencePayload<'db>,
}

// (legacy MethodCallEntry removed; semantic-query consumes OccurrencePayload::MethodName directly)

#[salsa::tracked(return_ref)]
pub fn unified_occurrence_rangemap_for_top_mod<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<OccurrenceRangeEntry<'db>> {
    let payloads = collect_unified_occurrences(db, top_mod);
    let mut out: Vec<OccurrenceRangeEntry<'db>> = Vec::new();
    for p in payloads.into_iter() {
        let span = match &p {
            OccurrencePayload::PathSeg { span, .. } => span,
            OccurrencePayload::UsePathSeg { span, .. } => span,
            OccurrencePayload::UseAliasName { span, .. } => span,
            OccurrencePayload::MethodName { span, .. } => span,
            OccurrencePayload::FieldAccessName { span, .. } => span,
            OccurrencePayload::PatternLabelName { span, .. } => span,
            OccurrencePayload::PathExprSeg { span, .. } => span,
            OccurrencePayload::PathPatSeg { span, .. } => span,
            OccurrencePayload::ItemHeaderName { span, .. } => span,
        };
        if let Some(res) = span.clone().resolve(db) {
            out.push(OccurrenceRangeEntry {
                start: res.range.start(),
                end: res.range.end(),
                payload: p,
            });
        }
    }
    out.sort_by(|a, b| match a.start.cmp(&b.start) {
        core::cmp::Ordering::Equal => (a.end - a.start).cmp(&(b.end - b.start)),
        ord => ord,
    });
    out
}

// ---------- Unified collector powering rangemap + path spans ----------

fn collect_unified_occurrences<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<OccurrencePayload<'db>> {
    #[derive(Default)]
    struct Collector<'db> {
        occ: Vec<OccurrencePayload<'db>>,
        suppress_generic_for_path: Option<PathId<'db>>,
    }

    impl<'db, 'ast: 'db> Visitor<'ast> for Collector<'db> {
        fn visit_path(
            &mut self,
            ctxt: &mut VisitorCtxt<'ast, VisitorLazyPathSpan<'ast>>,
            path: PathId<'db>,
        ) {
            // Suppress generic PathSeg occurrences when this path is the same
            // path that is already recorded as a contextual PathExprSeg/PathPatSeg.
            if let Some(p) = self.suppress_generic_for_path {
                if p == path {
                    return;
                }
            }
            if let Some(span) = ctxt.span() {
                let scope = ctxt.scope();
                let tail = path.segment_index(ctxt.db());
                for i in 0..=tail {
                    let seg_span: DynLazySpan<'db> = span.clone().segment(i).ident().into();
                    self.occ.push(OccurrencePayload::PathSeg {
                        path,
                        scope,
                        seg_idx: i,
                        path_lazy: span.clone(),
                        span: seg_span,
                    });
                }
            }
        }
        fn visit_use(
            &mut self,
            ctxt: &mut VisitorCtxt<'ast, crate::span::item::LazyUseSpan<'ast>>,
            use_item: crate::hir_def::Use<'db>,
        ) {
            // Record alias name if present
            if let Some(Partial::Present(UseAlias::Ident(ident))) = use_item.alias(ctxt.db()) {
                if let Some(span) = ctxt.span() {
                    let scope = ctxt.scope();
                    let alias_span: DynLazySpan<'db> = span.alias().name().into();
                    self.occ.push(OccurrencePayload::UseAliasName {
                        scope,
                        ident,
                        span: alias_span,
                    });
                }
            }
            // Traverse use path segments and collect occurrences
            if let Partial::Present(path) = use_item.path(ctxt.db()) {
                if let Some(lazy) = ctxt.span() {
                    let scope = ctxt.scope();
                    let use_path_span = lazy.path();
                    for (i, seg) in path.data(ctxt.db()).iter().enumerate() {
                        if matches!(seg.to_opt(), Some(UsePathSegment::Glob)) {
                            continue;
                        }
                        let seg_span: DynLazySpan<'db> =
                            use_path_span.clone().segment(i).into_atom().into();
                        self.occ.push(OccurrencePayload::UsePathSeg {
                            scope,
                            path,
                            seg_idx: i,
                            span: seg_span,
                        });
                    }
                }
            }
        }
        fn visit_expr(
            &mut self,
            ctxt: &mut VisitorCtxt<'ast, crate::span::expr::LazyExprSpan<'ast>>,
            id: ExprId,
            expr: &Expr<'db>,
        ) {
            match expr {
                Expr::MethodCall(receiver, method_name, _gargs, _args) => {
                    if let Some(name) = method_name.to_opt() {
                        if let Some(span) = ctxt.span() {
                            let scope = ctxt.scope();
                            let body = ctxt.body();
                            let name_span: DynLazySpan<'db> =
                                span.into_method_call_expr().method_name().into();
                            self.occ.push(OccurrencePayload::MethodName {
                                scope,
                                body,
                                ident: name,
                                receiver: *receiver,
                                span: name_span,
                            });
                        }
                    }
                }
                Expr::Field(
                    receiver,
                    Partial::Present(crate::hir_def::FieldIndex::Ident(ident)),
                ) => {
                    if let Some(span) = ctxt.span() {
                        let scope = ctxt.scope();
                        let body = ctxt.body();
                        let name_span: DynLazySpan<'db> = span.into_field_expr().accessor().into();
                        self.occ.push(OccurrencePayload::FieldAccessName {
                            scope,
                            body,
                            ident: *ident,
                            receiver: *receiver,
                            span: name_span,
                        });
                    }
                }
                Expr::Path(Partial::Present(path)) => {
                    if let Some(span) = ctxt.span() {
                        let scope = ctxt.scope();
                        let body = ctxt.body();
                        let tail = path.segment_index(ctxt.db());
                        for i in 0..=tail {
                            let seg_span: DynLazySpan<'db> = span
                                .clone()
                                .into_path_expr()
                                .path()
                                .segment(i)
                                .ident()
                                .into();
                            self.occ.push(OccurrencePayload::PathExprSeg {
                                scope,
                                body,
                                expr: id,
                                path: *path,
                                seg_idx: i,
                                span: seg_span,
                            });
                        }
                        // Avoid emitting generic PathSeg for this path by suppressing
                        // it during the recursive walk of this expression.
                        let prev = self.suppress_generic_for_path;
                        self.suppress_generic_for_path = Some(*path);
                        crate::visitor::walk_expr(self, ctxt, id);
                        self.suppress_generic_for_path = prev;
                        return;
                    }
                }
                _ => {}
            }
            crate::visitor::walk_expr(self, ctxt, id);
        }
        fn visit_pat(
            &mut self,
            ctxt: &mut VisitorCtxt<'ast, crate::span::pat::LazyPatSpan<'ast>>,
            pat: PatId,
            pat_data: &Pat<'db>,
        ) {
            match pat_data {
                Pat::Record(path, fields) => {
                    if let Some(span) = ctxt.span() {
                        let scope = ctxt.scope();
                        let body = ctxt.body();
                        let ctor_path = match path {
                            Partial::Present(p) => Some(*p),
                            _ => None,
                        };
                        for (i, fld) in fields.iter().enumerate() {
                            if let Some(ident) = fld.label(ctxt.db(), body) {
                                let name_span: DynLazySpan<'db> = span
                                    .clone()
                                    .into_record_pat()
                                    .fields()
                                    .field(i)
                                    .name()
                                    .into();
                                self.occ.push(OccurrencePayload::PatternLabelName {
                                    scope,
                                    body,
                                    ident,
                                    constructor_path: ctor_path,
                                    span: name_span,
                                });
                            }
                        }
                    }
                }
                Pat::Path(Partial::Present(path), _is_mut) => {
                    if let Some(span) = ctxt.span() {
                        let scope = ctxt.scope();
                        let body = ctxt.body();
                        let tail = path.segment_index(ctxt.db());
                        for i in 0..=tail {
                            let seg_span: DynLazySpan<'db> = span
                                .clone()
                                .into_path_pat()
                                .path()
                                .segment(i)
                                .ident()
                                .into();
                            self.occ.push(OccurrencePayload::PathPatSeg {
                                scope,
                                body,
                                pat,
                                path: *path,
                                seg_idx: i,
                                span: seg_span,
                            });
                        }
                        // Suppress generic PathSeg emission for this pattern path.
                        let prev = self.suppress_generic_for_path;
                        self.suppress_generic_for_path = Some(*path);
                        crate::visitor::walk_pat(self, ctxt, pat);
                        self.suppress_generic_for_path = prev;
                        return;
                    }
                }
                _ => {}
            }
            crate::visitor::walk_pat(self, ctxt, pat)
        }
    }

    let mut coll = Collector::default();
    let mut ctxt = VisitorCtxt::with_top_mod(db, top_mod);
    coll.visit_top_mod(&mut ctxt, top_mod);
    // Add item/variant/param header name occurrences
    for it in top_mod.all_items(db).iter() {
        if let Some(name) = it.name_span() {
            let sc = crate::hir_def::scope_graph::ScopeId::from_item(*it);
            let name_dyn: DynLazySpan<'db> = name;
            coll.occ.push(OccurrencePayload::ItemHeaderName {
                scope: sc,
                span: name_dyn,
            });
        }
        if let crate::hir_def::ItemKind::Enum(e) = *it {
            let vars = e.variants(db);
            for (idx, vdef) in vars.data(db).iter().enumerate() {
                if vdef.name.to_opt().is_none() {
                    continue;
                }
                let variant = crate::hir_def::EnumVariant::new(e, idx);
                let sc = variant.scope();
                let name_dyn: DynLazySpan<'db> = variant.span().name().into();
                coll.occ.push(OccurrencePayload::ItemHeaderName {
                    scope: sc,
                    span: name_dyn,
                });
            }
        }
        if let crate::hir_def::ItemKind::Func(f) = *it {
            if let Some(params) = f.params(db).to_opt() {
                for (idx, _p) in params.data(db).iter().enumerate() {
                    let sc = crate::hir_def::scope_graph::ScopeId::FuncParam(*it, idx as u16);
                    let name_dyn: DynLazySpan<'db> = f.span().params().param(idx).name().into();
                    coll.occ.push(OccurrencePayload::ItemHeaderName {
                        scope: sc,
                        span: name_dyn,
                    });
                }
            }
        }
    }
    // Prefer contextual occurrences (PathExprSeg/PathPatSeg) over generic PathSeg
    // when both cover the exact same textual span. Build a set of spans covered
    // by contextual occurrences, then drop PathSeg entries that overlap exactly.
    use rustc_hash::FxHashSet;
    let mut contextual_spans: FxHashSet<(parser::TextSize, parser::TextSize)> =
        FxHashSet::default();
    for o in coll.occ.iter() {
        match o {
            OccurrencePayload::PathExprSeg { span, .. }
            | OccurrencePayload::PathPatSeg { span, .. } => {
                if let Some(sp) = span.clone().resolve(db) {
                    contextual_spans.insert((sp.range.start(), sp.range.end()));
                }
            }
            _ => {}
        }
    }

    let mut filtered: Vec<OccurrencePayload<'db>> = Vec::with_capacity(coll.occ.len());
    for o in coll.occ.into_iter() {
        match &o {
            OccurrencePayload::PathSeg { span, .. } => {
                if let Some(sp) = span.clone().resolve(db) {
                    let key = (sp.range.start(), sp.range.end());
                    if contextual_spans.contains(&key) {
                        // Skip generic PathSeg if there is a contextual occurrence for this span
                        continue;
                    }
                }
                filtered.push(o);
            }
            _ => filtered.push(o),
        }
    }

    filtered
}

// (legacy entry structs removed; semantic-query derives hits from OccurrencePayload)

/// Return all occurrences whose resolved range contains the given offset.
/// Note: linear scan; callers should prefer small files or pre-filtered contexts.
pub fn occurrences_at_offset<'db>(
    db: &'db dyn SpannedHirDb,
    top_mod: TopLevelMod<'db>,
    offset: parser::TextSize,
) -> Vec<OccurrencePayload<'db>> {
    // Half-open containment: [start, end)
    unified_occurrence_rangemap_for_top_mod(db, top_mod)
        .iter()
        .filter(|e| e.start <= offset && offset < e.end)
        .map(|e| e.payload.clone())
        .collect()
}

impl<'db> OccurrencePayload<'db> {
    /// Returns true if this occurrence should be included in rename operations.
    /// Filters out language keywords like 'self', 'Self', 'super', etc.
    pub fn rename_allowed(&self, db: &'db dyn crate::SpannedHirDb) -> bool {
        use crate::hir_def::scope_graph::ScopeId;

        match self {
            // Path-based occurrences: check if they resolve to language keywords
            OccurrencePayload::PathSeg { path, seg_idx, .. }
            | OccurrencePayload::PathExprSeg { path, seg_idx, .. }
            | OccurrencePayload::PathPatSeg { path, seg_idx, .. } => {
                Self::check_path_segment_keyword(db, *path, *seg_idx)
            }

            // Direct IdentId occurrences: check for language keywords
            OccurrencePayload::UseAliasName { ident, .. }
            | OccurrencePayload::MethodName { ident, .. }
            | OccurrencePayload::FieldAccessName { ident, .. }
            | OccurrencePayload::PatternLabelName { ident, .. } => {
                !Self::is_language_keyword(db, *ident)
            }

            // Use path segments are always safe to rename
            OccurrencePayload::UsePathSeg { .. } => true,

            // ItemHeaderName: exclude definitions, but allow function parameters (except self)
            OccurrencePayload::ItemHeaderName { scope, .. } => {
                match scope {
                    ScopeId::FuncParam(_, _) => {
                        if let Some(param_name) = scope.name(db) {
                            !param_name.is_self(db)
                        } else {
                            true
                        }
                    }
                    _ => false, // Item definitions should not be renamed
                }
            }
        }
    }

    /// Helper: check if a path segment at the given index is a language keyword
    fn check_path_segment_keyword(
        db: &'db dyn crate::SpannedHirDb,
        path: crate::hir_def::PathId<'db>,
        seg_idx: usize,
    ) -> bool {
        if let Some(seg) = path.segment(db, seg_idx) {
            if let Some(ident) = seg.as_ident(db) {
                return !Self::is_language_keyword(db, ident);
            }
        }
        true // Default to allow if we can't resolve the segment
    }

    /// Helper: check if an IdentId represents a language keyword
    fn is_language_keyword(db: &'db dyn crate::SpannedHirDb, ident: IdentId<'db>) -> bool {
        ident.is_self(db) || ident.is_super(db) || ident.is_self_ty(db)
    }
}
