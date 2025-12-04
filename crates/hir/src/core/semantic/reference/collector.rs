//! Reference collection infrastructure.
//!
//! This module contains the unified reference collector and salsa queries
//! for caching reference collection results.

use crate::{
    HirDb,
    hir_def::{Body, Enum, Expr, ExprId, Func, Impl, ImplTrait, Struct, Trait, TypeAlias, Use, UsePathId},
    span::lazy_spans::{LazyBodySpan, LazyExprSpan, LazyPathSpan, LazyUsePathSpan},
    visitor::{Visitor, VisitorCtxt, walk_expr, walk_path, walk_use_path},
};

use super::{PathView, FieldAccessView, MethodCallView, UsePathView, ReferenceView, PathId};

/// Single unified collector for all reference types.
pub(super) struct ReferenceCollector<'db> {
    db: &'db dyn HirDb,
    pub refs: Vec<ReferenceView<'db>>,
    /// Current body context (set during body traversal)
    current_body: Option<Body<'db>>,
    /// Current use item context (set during use traversal)
    current_use: Option<Use<'db>>,
    /// Whether to skip body traversal (for signature-only collection)
    skip_body: bool,
}

impl<'db> ReferenceCollector<'db> {
    pub fn new(db: &'db dyn HirDb, skip_body: bool) -> Self {
        Self {
            db,
            refs: Vec::new(),
            current_body: None,
            current_use: None,
            skip_body,
        }
    }

    pub fn for_body(db: &'db dyn HirDb, body: Body<'db>) -> Self {
        Self {
            db,
            refs: Vec::new(),
            current_body: Some(body),
            current_use: None,
            skip_body: false,
        }
    }
}

impl<'db> Visitor<'db> for ReferenceCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        if let Some(span) = ctxt.span() {
            let scope = ctxt.scope();
            self.refs.push(ReferenceView::Path(PathView::new(path, scope, span)));
        }
        walk_path(self, ctxt, path);
    }

    fn visit_body(&mut self, ctxt: &mut VisitorCtxt<'db, LazyBodySpan<'db>>, body: Body<'db>) {
        if self.skip_body {
            return;
        }
        let old = self.current_body.replace(body);
        crate::visitor::walk_body(self, ctxt, body);
        self.current_body = old;
    }

    fn visit_expr(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
        expr: ExprId,
        expr_data: &Expr<'db>,
    ) {
        if let Some(body) = self.current_body {
            match expr_data {
                Expr::Field(_, _) => {
                    if let Some(span) = ctxt.span() {
                        self.refs.push(ReferenceView::FieldAccess(FieldAccessView {
                            body,
                            expr,
                            span: span.into_field_expr(),
                        }));
                    }
                }
                Expr::MethodCall(_, _, _, _) => {
                    if let Some(span) = ctxt.span() {
                        self.refs.push(ReferenceView::MethodCall(MethodCallView {
                            body,
                            expr,
                            span: span.into_method_call_expr(),
                        }));
                    }
                }
                _ => {}
            }
        }
        walk_expr(self, ctxt, expr);
    }

    fn visit_use(&mut self, ctxt: &mut VisitorCtxt<'db, crate::span::item::LazyUseSpan<'db>>, use_: Use<'db>) {
        let old = self.current_use.replace(use_);
        crate::visitor::walk_use(self, ctxt, use_);
        self.current_use = old;
    }

    fn visit_use_path(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyUsePathSpan<'db>>,
        use_path: UsePathId<'db>,
    ) {
        if let (Some(use_item), Some(span)) = (self.current_use, ctxt.span()) {
            let segment_count = use_path.data(self.db).len();
            for idx in 0..segment_count {
                self.refs.push(ReferenceView::UsePath(UsePathView {
                    use_item,
                    segment: idx,
                    span: span.clone(),
                }));
            }
        }
        walk_use_path(self, ctxt, use_path);
    }
}

#[salsa::tracked(return_ref)]
pub fn body_references<'db>(db: &'db dyn HirDb, body: Body<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::for_body(db, body);
    let mut ctxt = VisitorCtxt::with_body(db, body);
    collector.visit_body(&mut ctxt, body);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn func_signature_references<'db>(db: &'db dyn HirDb, func: Func<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db, true);
    let mut ctxt = VisitorCtxt::with_func(db, func);
    collector.visit_func(&mut ctxt, func);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn struct_references<'db>(db: &'db dyn HirDb, struct_: Struct<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db, false);
    let mut ctxt = VisitorCtxt::with_struct(db, struct_);
    collector.visit_struct(&mut ctxt, struct_);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn enum_references<'db>(db: &'db dyn HirDb, enum_: Enum<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db, false);
    let mut ctxt = VisitorCtxt::with_enum(db, enum_);
    collector.visit_enum(&mut ctxt, enum_);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn type_alias_references<'db>(db: &'db dyn HirDb, type_alias: TypeAlias<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db, false);
    let mut ctxt = VisitorCtxt::with_type_alias(db, type_alias);
    collector.visit_type_alias(&mut ctxt, type_alias);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn impl_references<'db>(db: &'db dyn HirDb, impl_: Impl<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db, true);
    let mut ctxt = VisitorCtxt::with_impl(db, impl_);
    collector.visit_impl(&mut ctxt, impl_);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn trait_references<'db>(db: &'db dyn HirDb, trait_: Trait<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db, true);
    let mut ctxt = VisitorCtxt::with_trait(db, trait_);
    collector.visit_trait(&mut ctxt, trait_);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn impl_trait_references<'db>(db: &'db dyn HirDb, impl_trait: ImplTrait<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db, true);
    let mut ctxt = VisitorCtxt::with_impl_trait(db, impl_trait);
    collector.visit_impl_trait(&mut ctxt, impl_trait);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn use_references<'db>(db: &'db dyn HirDb, use_item: Use<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db, false);
    let mut ctxt = VisitorCtxt::with_use(db, use_item);
    collector.visit_use(&mut ctxt, use_item);
    collector.refs
}
