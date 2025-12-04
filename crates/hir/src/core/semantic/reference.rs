//! Reference views for symbolic references in the HIR.
//!
//! This module provides view types for tracking symbolic references (paths,
//! field accesses, method calls, use paths) throughout the HIR. These views
//! enable efficient goto-definition and find-all-references functionality.
//!
//! Each view bundles the minimal information needed to:
//! 1. Resolve the reference to its target definition
//! 2. Locate the reference in source code via its span

use parser::TextSize;

use crate::{
    HirDb, SpannedHirDb,
    analysis::HirAnalysisDb,
    analysis::name_resolution::{resolve_path, resolve_query, PathResErrorKind, EarlyNameQueryId, QueryDirective},
    analysis::ty::{trait_resolution::PredicateListId, ty_check::{check_func_body, RecordLike}},
    hir_def::{Body, Enum, Expr, ExprId, FieldIndex, Func, Impl, ImplTrait, ItemKind, Partial, PathId, Struct, TopLevelMod, Trait, TypeAlias, Use, UsePathId, UsePathSegment},
    hir_def::scope_graph::ScopeId,
    span::{
        DynLazySpan, LazySpan,
        lazy_spans::{
            LazyPathSpan, LazyBodySpan, LazyExprSpan, LazyFieldExprSpan, LazyMethodCallExprSpan, LazyUsePathSpan,
        },
    },
    visitor::{Visitor, VisitorCtxt, walk_expr, walk_path, walk_use_path},
};

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Resolve a path to a scope, handling all error cases uniformly.
///
/// Handles successful resolution, NotFound errors with bucket results,
/// and Ambiguous errors (takes first result).
fn resolve_path_to_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> Option<ScopeId<'db>> {
    let assumptions = PredicateListId::empty_list(db);
    match resolve_path(db, path, scope, assumptions, false) {
        Ok(res) => res.as_scope(db),
        Err(err) => {
            match err.kind {
                PathResErrorKind::NotFound { bucket, .. } => {
                    // The bucket may contain a valid result even on "NotFound"
                    bucket.iter_ok().flat_map(|r| r.scope()).next()
                }
                PathResErrorKind::Ambiguous(vec) => {
                    // Take the first unambiguous result
                    vec.into_iter().flat_map(|r| r.scope()).next()
                }
                _ => None,
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Reference views
// ---------------------------------------------------------------------------

/// The resolved target of a path reference.
///
/// Paths can resolve to either module-level items (represented as scopes)
/// or local bindings (represented as definition spans).
#[derive(Clone, Debug)]
pub enum Target<'db> {
    /// A module-level item (function, struct, enum, etc.)
    Scope(ScopeId<'db>),
    /// A local binding (variable, parameter, effect parameter)
    Span(DynLazySpan<'db>),
}

/// A view of a path reference in the HIR.
///
/// Paths appear in expressions (`Expr::Path`), patterns (`Pat::Path`),
/// types (`TypeKind::Path`), and trait references.
#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct PathView<'db> {
    pub path: PathId<'db>,
    pub scope: ScopeId<'db>,
    pub span: LazyPathSpan<'db>,
}

impl<'db> PathView<'db> {
    /// Create a new PathView.
    pub fn new(path: PathId<'db>, scope: ScopeId<'db>, span: LazyPathSpan<'db>) -> Self {
        Self { path, scope, span }
    }

    /// Resolve this path to its target definition.
    ///
    /// Tries module-level resolution first, then falls back to local binding resolution.
    pub fn target(&self, db: &'db dyn HirAnalysisDb) -> Option<Target<'db>> {
        // Try module-level resolution first
        if let Some(scope) = resolve_path_to_scope(db, self.path, self.scope) {
            return Some(Target::Scope(scope));
        }

        // Fall back to local binding resolution
        self.local_binding_target(db).map(Target::Span)
    }

    /// Resolve this path at a specific cursor position (segment-aware).
    ///
    /// Clicking on `foo` in `foo::Bar` resolves to `foo`, not `Bar`.
    pub fn target_at<DB>(&self, db: &'db DB, cursor: TextSize) -> Option<Target<'db>>
    where
        DB: HirAnalysisDb + SpannedHirDb,
    {
        let last_idx = self.path.segment_index(db);

        // Find which segment the cursor is in
        for idx in 0..=last_idx {
            let Some(seg_span) = self.span.clone().segment(idx).resolve(db) else {
                continue;
            };

            if seg_span.range.contains(cursor) {
                if let Some(seg_path) = self.path.segment(db, idx) {
                    if let Some(scope) = resolve_path_to_scope(db, seg_path, self.scope) {
                        return Some(Target::Scope(scope));
                    }

                    // If this is the last segment, try local binding resolution
                    if idx == last_idx {
                        return self.local_binding_target(db).map(Target::Span);
                    }
                }
                return None;
            }
        }

        // Cursor not in any segment, try full path resolution
        self.target(db)
    }

    /// Resolve to a local binding's definition span (internal helper).
    fn local_binding_target(&self, db: &'db dyn HirAnalysisDb) -> Option<DynLazySpan<'db>> {
        let body = self.scope.body()?;
        let func = body.containing_func(db)?;
        let expr_id = body.find_expr_for_path(db, self.path)?;
        let (_, typed_body) = check_func_body(db, func);
        typed_body.expr_binding_def_span(func, expr_id)
    }

    /// Get the source span of this path reference.
    pub fn span(&self) -> DynLazySpan<'db> {
        self.span.clone().into()
    }
}

/// A view of a field access expression (`expr.field`).
#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct FieldAccessView<'db> {
    pub body: Body<'db>,
    pub expr: ExprId,
    pub span: LazyFieldExprSpan<'db>,
}

impl<'db> FieldAccessView<'db> {
    /// Resolve this field access to its target scope.
    ///
    /// Uses type inference to determine the receiver type and look up
    /// the field definition in the struct.
    pub fn target(&self, db: &'db dyn HirAnalysisDb) -> Option<ScopeId<'db>> {
        // Get the expression data to extract receiver and field name
        let Partial::Present(Expr::Field(receiver, field_index)) = self.expr.data(db, self.body) else {
            return None;
        };
        let Partial::Present(FieldIndex::Ident(field_name)) = field_index else {
            return None; // Tuple field access (e.g., tuple.0) doesn't have a scope
        };

        // Get the containing function for type checking
        let func = self.body.containing_func(db)?;

        // Run type checking to get typed body
        let (_, typed_body) = check_func_body(db, func);

        // Get the type of the receiver expression
        let receiver_ty = typed_body.expr_ty(db, *receiver);
        if receiver_ty.has_invalid(db) {
            return None;
        }

        // Resolve the field scope using RecordLike
        let record_like = RecordLike::from_ty(receiver_ty);
        record_like.record_field_scope(db, *field_name)
    }

    /// Get the source span of this field access.
    pub fn span(&self) -> DynLazySpan<'db> {
        self.span.clone().into()
    }
}

/// A view of a method call expression (`expr.method(...)`).
#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct MethodCallView<'db> {
    pub body: Body<'db>,
    pub expr: ExprId,
    pub span: LazyMethodCallExprSpan<'db>,
}

impl<'db> MethodCallView<'db> {
    /// Resolve this method call to its target scope.
    ///
    /// Uses the typed body's callable information to find the resolved method.
    pub fn target(&self, db: &'db dyn HirAnalysisDb) -> Option<ScopeId<'db>> {
        // Get the containing function for type checking
        let func = self.body.containing_func(db)?;

        // Run type checking to get typed body with callable information
        let (_, typed_body) = check_func_body(db, func);

        // Get the callable for this method call expression
        let callable = typed_body.callable_expr(self.expr)?;

        // Extract the scope from the callable definition
        match callable.callable_def {
            crate::hir_def::CallableDef::Func(method_func) => {
                Some(ScopeId::from_item(ItemKind::Func(method_func)))
            }
            crate::hir_def::CallableDef::VariantCtor(variant) => {
                Some(ScopeId::Variant(variant))
            }
        }
    }

    /// Get the source span of this method call.
    pub fn span(&self) -> DynLazySpan<'db> {
        self.span.clone().into()
    }
}

/// A view of a use path segment.
#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct UsePathView<'db> {
    pub use_item: Use<'db>,
    pub segment: usize,
    pub span: LazyUsePathSpan<'db>,
}

impl<'db> UsePathView<'db> {
    /// Resolve this use path segment to its target scope.
    ///
    /// For a use like `use foo::bar::Baz`, clicking on `bar` (segment 1)
    /// resolves to the `bar` module/item.
    pub fn target(&self, db: &'db dyn HirAnalysisDb) -> Option<ScopeId<'db>> {
        let use_path = self.use_item.path(db).to_opt()?;
        let segments = use_path.data(db);

        // Start from the use's parent scope
        let mut current_scope = self.use_item.scope().parent(db)?;

        // Resolve each segment up to and including self.segment
        for idx in 0..=self.segment {
            let segment = segments.get(idx)?.to_opt()?;
            let ident = match segment {
                UsePathSegment::Ident(id) => id,
                UsePathSegment::Glob => return None, // Can't resolve glob
            };

            // Create a query for this name in the current scope
            let directive = QueryDirective::new();
            let query = EarlyNameQueryId::new(db, ident, current_scope, directive);
            let bucket = resolve_query(db, query);

            // Get the first successful resolution
            let res = bucket.iter_ok().next()?;
            current_scope = res.scope()?;
        }

        Some(current_scope)
    }

    /// Get the source span of this use path segment.
    pub fn span(&self) -> DynLazySpan<'db> {
        self.span.clone().into()
    }
}

/// A unified view of any symbolic reference in the HIR.
///
/// This enum provides a common interface for working with different kinds
/// of references, enabling heterogeneous collections and uniform handling
/// in the language server.
#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::Update)]
pub enum ReferenceView<'db> {
    /// A path reference (expression, pattern, type, or trait ref)
    Path(PathView<'db>),
    /// A field access expression
    FieldAccess(FieldAccessView<'db>),
    /// A method call expression
    MethodCall(MethodCallView<'db>),
    /// A use path segment
    UsePath(UsePathView<'db>),
}

impl<'db> ReferenceView<'db> {
    /// Resolve this reference to its target definition.
    ///
    /// Returns the target as a `Target` which can be either a module-level
    /// item (scope) or a local binding (span).
    pub fn target(&self, db: &'db dyn HirAnalysisDb) -> Option<Target<'db>> {
        match self {
            Self::Path(v) => v.target(db),
            Self::FieldAccess(v) => v.target(db).map(Target::Scope),
            Self::MethodCall(v) => v.target(db).map(Target::Scope),
            Self::UsePath(v) => v.target(db).map(Target::Scope),
        }
    }

    /// Resolve this reference at a specific cursor position.
    ///
    /// For paths, this handles segment-level resolution - clicking on `foo` in
    /// `foo::Bar` resolves to `foo`, not `Bar`.
    pub fn target_at<DB>(&self, db: &'db DB, cursor: TextSize) -> Option<Target<'db>>
    where
        DB: HirAnalysisDb + SpannedHirDb,
    {
        match self {
            Self::Path(v) => v.target_at(db, cursor),
            _ => self.target(db),
        }
    }

    /// Get the source span of this reference.
    pub fn span(&self) -> DynLazySpan<'db> {
        match self {
            Self::Path(v) => v.span(),
            Self::FieldAccess(v) => v.span(),
            Self::MethodCall(v) => v.span(),
            Self::UsePath(v) => v.span(),
        }
    }
}

// ---------------------------------------------------------------------------
// Reference collector
// ---------------------------------------------------------------------------

/// Single unified collector for all reference types.
struct ReferenceCollector<'db> {
    db: &'db dyn HirDb,
    refs: Vec<ReferenceView<'db>>,
    /// Current body context (set during body traversal)
    current_body: Option<Body<'db>>,
    /// Current use item context (set during use traversal)
    current_use: Option<Use<'db>>,
    /// Whether to skip body traversal (for signature-only collection)
    skip_body: bool,
}

impl<'db> ReferenceCollector<'db> {
    fn new(db: &'db dyn HirDb, skip_body: bool) -> Self {
        Self {
            db,
            refs: Vec::new(),
            current_body: None,
            current_use: None,
            skip_body,
        }
    }

    fn for_body(db: &'db dyn HirDb, body: Body<'db>) -> Self {
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

// ---------------------------------------------------------------------------
// Salsa queries for reference collection
// ---------------------------------------------------------------------------

#[salsa::tracked(return_ref)]
fn body_references<'db>(db: &'db dyn HirDb, body: Body<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::for_body(db, body);
    let mut ctxt = VisitorCtxt::with_body(db, body);
    collector.visit_body(&mut ctxt, body);
    collector.refs
}

#[salsa::tracked(return_ref)]
fn func_signature_references<'db>(db: &'db dyn HirDb, func: Func<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db, true);
    let mut ctxt = VisitorCtxt::with_func(db, func);
    collector.visit_func(&mut ctxt, func);
    collector.refs
}

#[salsa::tracked(return_ref)]
fn struct_references<'db>(db: &'db dyn HirDb, struct_: Struct<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db, false);
    let mut ctxt = VisitorCtxt::with_struct(db, struct_);
    collector.visit_struct(&mut ctxt, struct_);
    collector.refs
}

#[salsa::tracked(return_ref)]
fn enum_references<'db>(db: &'db dyn HirDb, enum_: Enum<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db, false);
    let mut ctxt = VisitorCtxt::with_enum(db, enum_);
    collector.visit_enum(&mut ctxt, enum_);
    collector.refs
}

#[salsa::tracked(return_ref)]
fn type_alias_references<'db>(db: &'db dyn HirDb, type_alias: TypeAlias<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db, false);
    let mut ctxt = VisitorCtxt::with_type_alias(db, type_alias);
    collector.visit_type_alias(&mut ctxt, type_alias);
    collector.refs
}

#[salsa::tracked(return_ref)]
fn impl_references<'db>(db: &'db dyn HirDb, impl_: Impl<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db, true);
    let mut ctxt = VisitorCtxt::with_impl(db, impl_);
    collector.visit_impl(&mut ctxt, impl_);
    collector.refs
}

#[salsa::tracked(return_ref)]
fn trait_references<'db>(db: &'db dyn HirDb, trait_: Trait<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db, true);
    let mut ctxt = VisitorCtxt::with_trait(db, trait_);
    collector.visit_trait(&mut ctxt, trait_);
    collector.refs
}

#[salsa::tracked(return_ref)]
fn impl_trait_references<'db>(db: &'db dyn HirDb, impl_trait: ImplTrait<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db, true);
    let mut ctxt = VisitorCtxt::with_impl_trait(db, impl_trait);
    collector.visit_impl_trait(&mut ctxt, impl_trait);
    collector.refs
}

#[salsa::tracked(return_ref)]
fn use_references<'db>(db: &'db dyn HirDb, use_item: Use<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db, false);
    let mut ctxt = VisitorCtxt::with_use(db, use_item);
    collector.visit_use(&mut ctxt, use_item);
    collector.refs
}

// ---------------------------------------------------------------------------
// HasReferences trait
// ---------------------------------------------------------------------------

/// Empty reference slice for types that don't contain references.
static EMPTY_REFS: &[ReferenceView<'static>] = &[];

/// Trait for types that contain symbolic references.
///
/// This provides a unified interface for accessing references in different
/// parts of the HIR (bodies, items, scopes).
pub trait HasReferences<'db> {
    /// Returns all symbolic references within this node.
    fn references(&self, db: &'db dyn HirDb) -> &'db [ReferenceView<'db>];
}

impl<'db> HasReferences<'db> for Body<'db> {
    fn references(&self, db: &'db dyn HirDb) -> &'db [ReferenceView<'db>] {
        body_references(db, *self)
    }
}

impl<'db> HasReferences<'db> for ScopeId<'db> {
    fn references(&self, db: &'db dyn HirDb) -> &'db [ReferenceView<'db>] {
        match self {
            ScopeId::Item(item) => item.references(db),
            ScopeId::Block(body, _) => body.references(db),
            // TODO: Implement references for other scope types (Phase 3)
            ScopeId::GenericParam(_, _) => EMPTY_REFS,
            ScopeId::TraitType(_, _) => EMPTY_REFS,
            ScopeId::TraitConst(_, _) => EMPTY_REFS,
            ScopeId::FuncParam(_, _) => EMPTY_REFS,
            ScopeId::Field(_, _) => EMPTY_REFS,
            ScopeId::Variant(_) => EMPTY_REFS,
        }
    }
}

impl<'db> ScopeId<'db> {
    /// Find the reference at a cursor position within this scope.
    pub fn reference_at(self, db: &'db dyn SpannedHirDb, cursor: TextSize) -> Option<&'db ReferenceView<'db>> {
        self.references(db)
            .iter()
            .find(|r| r.span().resolve(db).is_some_and(|s| s.range.contains(cursor)))
    }
}

impl<'db> HasReferences<'db> for ItemKind<'db> {
    fn references(&self, db: &'db dyn HirDb) -> &'db [ReferenceView<'db>] {
        match self {
            ItemKind::Body(body) => body.references(db),
            ItemKind::Func(func) => func_signature_references(db, *func),
            ItemKind::Struct(struct_) => struct_references(db, *struct_),
            ItemKind::Enum(enum_) => enum_references(db, *enum_),
            ItemKind::TypeAlias(alias) => type_alias_references(db, *alias),
            ItemKind::Impl(impl_) => impl_references(db, *impl_),
            ItemKind::Trait(trait_) => trait_references(db, *trait_),
            ItemKind::ImplTrait(impl_trait) => impl_trait_references(db, *impl_trait),
            ItemKind::Use(use_item) => use_references(db, *use_item),
            ItemKind::Const(c) => c.body(db).to_opt().map_or(EMPTY_REFS, |b| b.references(db)),
            // Modules don't contain references themselves
            ItemKind::TopMod(_) | ItemKind::Mod(_) => EMPTY_REFS,
            // Contract references could be added in the future
            ItemKind::Contract(_) => EMPTY_REFS,
        }
    }
}

// ---------------------------------------------------------------------------
// TopLevelMod reference lookup
// ---------------------------------------------------------------------------

impl<'db> TopLevelMod<'db> {
    /// Find the reference at a cursor position anywhere in this module.
    ///
    /// This is the primary entry point for goto-definition - it finds the
    /// smallest enclosing item containing the cursor and returns the reference
    /// at that position.
    pub fn reference_at(self, db: &'db dyn SpannedHirDb, cursor: TextSize) -> Option<&'db ReferenceView<'db>> {
        // Find the smallest item containing the cursor
        let item = self.find_enclosing_item(db, cursor)?;
        let scope = ScopeId::from_item(item);
        scope.reference_at(db, cursor)
    }

    /// Find the smallest item enclosing the cursor position.
    fn find_enclosing_item(self, db: &'db dyn SpannedHirDb, cursor: TextSize) -> Option<ItemKind<'db>> {
        let items = self.scope_graph(db).items_dfs(db);

        let mut smallest_enclosing_item = None;
        let mut smallest_range_size = None;

        for item in items {
            let lazy_item_span = DynLazySpan::from(item.span());
            let Some(item_span) = lazy_item_span.resolve(db) else {
                continue;
            };

            if item_span.range.contains(cursor) {
                let range_size = item_span.range.end() - item_span.range.start();
                if smallest_range_size.is_none() || range_size < smallest_range_size.unwrap() {
                    smallest_enclosing_item = Some(item);
                    smallest_range_size = Some(range_size);
                }
            }
        }

        smallest_enclosing_item
    }

    /// Find all references to a given scope within this module.
    ///
    /// This is the primary entry point for find-all-references - it collects
    /// all references across the module that resolve to the target scope.
    pub fn references_to(
        self,
        db: &'db dyn HirAnalysisDb,
        target: ScopeId<'db>,
    ) -> Vec<&'db ReferenceView<'db>> {
        let items = self.scope_graph(db).items_dfs(db);
        let mut results = Vec::new();

        for item in items {
            let scope = ScopeId::from_item(item);
            for reference in scope.references(db) {
                // Check if this reference resolves to our target
                if let Some(resolved) = reference.target(db) {
                    let matches = match resolved {
                        Target::Scope(s) => s == target,
                        Target::Span(_) => false, // Local bindings don't match scope targets
                    };
                    if matches {
                        results.push(reference);
                    }
                }
            }
        }

        results
    }
}

