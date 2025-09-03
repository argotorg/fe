use parser::TextSize;

use crate::{
    hir_def::{
        Body, Expr, ExprId, IdentId, Partial, Pat, PatId, PathId, TopLevelMod,
    },
    SpannedHirDb,
    span::{DynLazySpan, LazySpan},
    span::path::LazyPathSpan,
    visitor::{prelude::LazyPathSpan as VisitorLazyPathSpan, Visitor, VisitorCtxt},
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct OccurrenceRangeEntry<'db> {
    pub start: TextSize,
    pub end: TextSize,
    pub payload: OccurrencePayload<'db>,
}

// Type consumed by semantic-query for method name occurrences
#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct MethodCallEntry<'db> {
    pub scope: crate::hir_def::scope_graph::ScopeId<'db>,
    pub body: Body<'db>,
    pub receiver: ExprId,
    pub ident: IdentId<'db>,
    pub name_span: DynLazySpan<'db>,
}

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
            OccurrencePayload::MethodName { span, .. } => span,
            OccurrencePayload::FieldAccessName { span, .. } => span,
            OccurrencePayload::PatternLabelName { span, .. } => span,
            OccurrencePayload::PathExprSeg { span, .. } => span,
            OccurrencePayload::PathPatSeg { span, .. } => span,
        };
        if let Some(res) = span.clone().resolve(db) {
            out.push(OccurrenceRangeEntry { start: res.range.start(), end: res.range.end(), payload: p });
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
    struct Collector<'db> {
        occ: Vec<OccurrencePayload<'db>>,
    }
    impl<'db> Default for Collector<'db> { fn default() -> Self { Self { occ: Vec::new() } } }

    impl<'db, 'ast: 'db> Visitor<'ast> for Collector<'db> {
        fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'ast, VisitorLazyPathSpan<'ast>>, path: PathId<'db>) {
            if let Some(span) = ctxt.span() {
                let scope = ctxt.scope();
                let tail = path.segment_index(ctxt.db());
                for i in 0..=tail {
                    let seg_span: DynLazySpan<'db> = span.clone().segment(i).ident().into();
                    self.occ.push(OccurrencePayload::PathSeg { path, scope, seg_idx: i, path_lazy: span.clone(), span: seg_span });
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
                            let name_span: DynLazySpan<'db> = span.into_method_call_expr().method_name().into();
                            self.occ.push(OccurrencePayload::MethodName { scope, body, ident: name, receiver: *receiver, span: name_span });
                        }
                    }
                }
                Expr::Field(receiver, field_name) => {
                    if let Partial::Present(crate::hir_def::FieldIndex::Ident(ident)) = field_name {
                        if let Some(span) = ctxt.span() {
                            let scope = ctxt.scope();
                            let body = ctxt.body();
                            let name_span: DynLazySpan<'db> = span.into_field_expr().accessor().into();
                            self.occ.push(OccurrencePayload::FieldAccessName { scope, body, ident: *ident, receiver: *receiver, span: name_span });
                        }
                    }
                }
                Expr::Path(path) => {
                    if let Partial::Present(path) = path {
                        if let Some(span) = ctxt.span() {
                            let scope = ctxt.scope();
                            let body = ctxt.body();
                            let tail = path.segment_index(ctxt.db());
                            for i in 0..=tail {
                                let seg_span: DynLazySpan<'db> = span.clone().into_path_expr().path().segment(i).ident().into();
                                self.occ.push(OccurrencePayload::PathExprSeg { scope, body, expr: id, path: *path, seg_idx: i, span: seg_span });
                            }
                        }
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
                        let ctor_path = match path { Partial::Present(p) => Some(*p), _ => None };
                        for (i, fld) in fields.iter().enumerate() {
                            if let Some(ident) = fld.label(ctxt.db(), body) {
                                let name_span: DynLazySpan<'db> = span.clone().into_record_pat().fields().field(i).name().into();
                                self.occ.push(OccurrencePayload::PatternLabelName { scope, body, ident, constructor_path: ctor_path, span: name_span });
                            }
                        }
                    }
                }
                Pat::Path(path, _is_mut) => {
                    if let Partial::Present(path) = path {
                        if let Some(span) = ctxt.span() {
                            let scope = ctxt.scope();
                            let body = ctxt.body();
                            let tail = path.segment_index(ctxt.db());
                            for i in 0..=tail {
                                let seg_span: DynLazySpan<'db> = span.clone().into_path_pat().path().segment(i).ident().into();
                                self.occ.push(OccurrencePayload::PathPatSeg { scope, body, pat, path: *path, seg_idx: i, span: seg_span });
                            }
                        }
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
    coll.occ
}

// (legacy entry structs removed; semantic-query derives hits from OccurrencePayload)
