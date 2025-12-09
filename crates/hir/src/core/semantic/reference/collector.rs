//! Reference collection infrastructure.
//!
//! This module contains the unified reference collector and salsa queries
//! for caching reference collection results.

use crate::{
    HirDb,
    hir_def::{
        Body, Enum, Expr, ExprId, Func, Impl, ImplTrait, Pat, PatId, Struct, Trait, TypeAlias, Use,
        UsePathId,
    },
    span::lazy_spans::{LazyBodySpan, LazyExprSpan, LazyPatSpan, LazyPathSpan, LazyUsePathSpan},
    visitor::{Visitor, VisitorCtxt, walk_expr, walk_pat, walk_path, walk_use_path},
};

use super::{
    BodyPathContext, FieldAccessView, MethodCallView, PathId, PathView, ReferenceView, UsePathView,
};

/// Tracks context for path references during body traversal.
#[derive(Clone, Copy)]
enum PathContext {
    /// Path is in an expression
    Expr(ExprId),
    /// Path is in a pattern (could be binding or reference, determined later)
    Pat(PatId),
    /// No body context (signatures, types, etc.)
    None,
}

/// Single unified collector for all reference types.
pub(super) struct ReferenceCollector<'db> {
    db: &'db dyn HirDb,
    pub refs: Vec<ReferenceView<'db>>,
    current_body: Option<Body<'db>>,
    current_use: Option<Use<'db>>,
    /// Tracks the current context when visiting paths inside a body.
    /// Set before walking expressions/patterns that contain paths.
    path_context: PathContext,
    skip_body: bool,
}

impl<'db> ReferenceCollector<'db> {
    pub fn new(db: &'db dyn HirDb, skip_body: bool) -> Self {
        Self {
            db,
            refs: Vec::new(),
            current_body: None,
            current_use: None,
            path_context: PathContext::None,
            skip_body,
        }
    }

    pub fn for_body(db: &'db dyn HirDb, body: Body<'db>) -> Self {
        Self {
            db,
            refs: Vec::new(),
            current_body: Some(body),
            current_use: None,
            path_context: PathContext::None,
            skip_body: false,
        }
    }
}

impl<'db> Visitor<'db> for ReferenceCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        if let Some(span) = ctxt.span() {
            let mut path_view = PathView::new(path, ctxt.scope(), span);

            // Attach body context if we're inside a body expression/pattern
            match self.path_context {
                PathContext::Expr(expr_id) => {
                    path_view = path_view.with_body_ctx(BodyPathContext::Expr(expr_id));
                }
                PathContext::Pat(pat_id) => {
                    // For patterns, we store PatBinding initially.
                    // local_target() will determine if it's actually a binding or reference.
                    path_view = path_view.with_body_ctx(BodyPathContext::PatBinding(pat_id));
                }
                PathContext::None => {}
            }

            self.refs.push(ReferenceView::Path(path_view));
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
                Expr::Path(_) => {
                    // Set context for path expressions before walking
                    let old_ctx = self.path_context;
                    self.path_context = PathContext::Expr(expr);
                    walk_expr(self, ctxt, expr);
                    self.path_context = old_ctx;
                    return;
                }
                _ => {}
            }
        }
        walk_expr(self, ctxt, expr);
    }

    fn visit_pat(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyPatSpan<'db>>,
        pat: PatId,
        pat_data: &Pat<'db>,
    ) {
        // Set context for pattern paths before walking
        if matches!(pat_data, Pat::Path(_, _)) {
            let old_ctx = self.path_context;
            self.path_context = PathContext::Pat(pat);
            walk_pat(self, ctxt, pat);
            self.path_context = old_ctx;
            return;
        }
        walk_pat(self, ctxt, pat);
    }

    fn visit_use(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, crate::span::item::LazyUseSpan<'db>>,
        use_: Use<'db>,
    ) {
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
pub fn func_signature_references<'db>(
    db: &'db dyn HirDb,
    func: Func<'db>,
) -> Vec<ReferenceView<'db>> {
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
pub fn type_alias_references<'db>(
    db: &'db dyn HirDb,
    type_alias: TypeAlias<'db>,
) -> Vec<ReferenceView<'db>> {
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
pub fn impl_trait_references<'db>(
    db: &'db dyn HirDb,
    impl_trait: ImplTrait<'db>,
) -> Vec<ReferenceView<'db>> {
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
