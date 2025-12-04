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
    analysis::name_resolution::{resolve_path, PathResErrorKind},
    analysis::ty::{trait_resolution::PredicateListId, ty_check::{check_func_body, RecordLike}},
    hir_def::{Body, Enum, Expr, ExprId, FieldIndex, Func, Impl, ImplTrait, ItemKind, Partial, PathId, Struct, TopLevelMod, Trait, TypeAlias, Use, UsePathId},
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
pub enum ResolvedPath<'db> {
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
    pub fn target(&self, db: &'db dyn HirAnalysisDb) -> Option<ResolvedPath<'db>> {
        // Try module-level resolution first
        if let Some(scope) = resolve_path_to_scope(db, self.path, self.scope) {
            return Some(ResolvedPath::Scope(scope));
        }

        // Fall back to local binding resolution
        self.local_binding_target(db).map(ResolvedPath::Span)
    }

    /// Resolve this path at a specific cursor position (segment-aware).
    ///
    /// Clicking on `foo` in `foo::Bar` resolves to `foo`, not `Bar`.
    pub fn target_at<DB>(&self, db: &'db DB, cursor: TextSize) -> Option<ResolvedPath<'db>>
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
                        return Some(ResolvedPath::Scope(scope));
                    }

                    // If this is the last segment, try local binding resolution
                    if idx == last_idx {
                        return self.local_binding_target(db).map(ResolvedPath::Span);
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
    pub fn scope_target(&self, db: &'db dyn HirAnalysisDb) -> Option<ScopeId<'db>> {
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
    pub fn scope_target(&self, db: &'db dyn HirAnalysisDb) -> Option<ScopeId<'db>> {
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
    pub fn scope_target(&self, _db: &'db dyn HirAnalysisDb) -> Option<ScopeId<'db>> {
        // TODO: Implement use path resolution
        None
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
    /// Returns the target as a `ResolvedPath` which can be either a module-level
    /// item (scope) or a local binding (span).
    pub fn target(&self, db: &'db dyn HirAnalysisDb) -> Option<ResolvedPath<'db>> {
        match self {
            Self::Path(v) => v.target(db),
            Self::FieldAccess(v) => v.scope_target(db).map(ResolvedPath::Scope),
            Self::MethodCall(v) => v.scope_target(db).map(ResolvedPath::Scope),
            Self::UsePath(v) => v.scope_target(db).map(ResolvedPath::Scope),
        }
    }

    /// Resolve this reference at a specific cursor position.
    ///
    /// For paths, this handles segment-level resolution - clicking on `foo` in
    /// `foo::Bar` resolves to `foo`, not `Bar`.
    pub fn target_at<DB>(&self, db: &'db DB, cursor: TextSize) -> Option<ResolvedPath<'db>>
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
// Body reference collection
// ---------------------------------------------------------------------------

/// Collects all symbolic references within a body using the Visitor pattern.
struct BodyReferenceCollector<'db> {
    db: &'db dyn HirDb,
    body: Body<'db>,
    refs: Vec<ReferenceView<'db>>,
}

impl<'db> BodyReferenceCollector<'db> {
    fn new(db: &'db dyn HirDb, body: Body<'db>) -> Self {
        Self {
            db,
            body,
            refs: Vec::new(),
        }
    }

    fn collect(mut self) -> Vec<ReferenceView<'db>> {
        let mut ctxt = VisitorCtxt::with_body(self.db, self.body);
        self.visit_body(&mut ctxt, self.body);
        self.refs
    }
}

impl<'db> Visitor<'db> for BodyReferenceCollector<'db> {
    fn visit_expr(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
        expr: ExprId,
        expr_data: &Expr<'db>,
    ) {
        // Collect field access and method call references
        match expr_data {
            Expr::Field(_, _) => {
                if let Some(span) = ctxt.span() {
                    let field_span = span.into_field_expr();
                    self.refs.push(ReferenceView::FieldAccess(FieldAccessView {
                        body: self.body,
                        expr,
                        span: field_span,
                    }));
                }
            }
            Expr::MethodCall(_, _, _, _) => {
                if let Some(span) = ctxt.span() {
                    let method_span = span.into_method_call_expr();
                    self.refs.push(ReferenceView::MethodCall(MethodCallView {
                        body: self.body,
                        expr,
                        span: method_span,
                    }));
                }
            }
            _ => {}
        }
        // Continue walking to visit nested expressions and paths
        walk_expr(self, ctxt, expr);
    }

    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        // Collect this path reference
        if let Some(span) = ctxt.span() {
            let scope = ctxt.scope();
            self.refs.push(ReferenceView::Path(PathView::new(path, scope, span)));
        }
        // Continue walking to collect nested paths (e.g., generic arguments)
        walk_path(self, ctxt, path);
    }
}

// Salsa query for caching body references (internal)
#[salsa::tracked(return_ref)]
fn body_references<'db>(db: &'db dyn HirDb, body: Body<'db>) -> Vec<ReferenceView<'db>> {
    BodyReferenceCollector::new(db, body).collect()
}

// ---------------------------------------------------------------------------
// Function signature reference collection
// ---------------------------------------------------------------------------

/// Collects all symbolic references within a function's signature (params, return type,
/// generics, where clause) but NOT the body.
struct FuncSignatureReferenceCollector<'db> {
    db: &'db dyn HirDb,
    func: Func<'db>,
    refs: Vec<ReferenceView<'db>>,
}

impl<'db> FuncSignatureReferenceCollector<'db> {
    fn new(db: &'db dyn HirDb, func: Func<'db>) -> Self {
        Self {
            db,
            func,
            refs: Vec::new(),
        }
    }

    fn collect(mut self) -> Vec<ReferenceView<'db>> {
        let mut ctxt = VisitorCtxt::with_func(self.db, self.func);
        self.visit_func(&mut ctxt, self.func);
        self.refs
    }
}

impl<'db> Visitor<'db> for FuncSignatureReferenceCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        if let Some(span) = ctxt.span() {
            let scope = ctxt.scope();
            self.refs.push(ReferenceView::Path(PathView::new(path, scope, span)));
        }
        walk_path(self, ctxt, path);
    }

    // Override to skip body traversal - body refs are collected separately
    fn visit_body(&mut self, _ctxt: &mut VisitorCtxt<'db, LazyBodySpan<'db>>, _body: Body<'db>) {
        // Do nothing - we only want signature refs
    }
}

// Salsa query for caching func signature references (internal)
#[salsa::tracked(return_ref)]
fn func_signature_references<'db>(db: &'db dyn HirDb, func: Func<'db>) -> Vec<ReferenceView<'db>> {
    FuncSignatureReferenceCollector::new(db, func).collect()
}

// ---------------------------------------------------------------------------
// Struct reference collection
// ---------------------------------------------------------------------------

/// Collects all symbolic references within a struct definition (field types,
/// generic params, where clause).
struct StructReferenceCollector<'db> {
    db: &'db dyn HirDb,
    struct_: Struct<'db>,
    refs: Vec<ReferenceView<'db>>,
}

impl<'db> StructReferenceCollector<'db> {
    fn new(db: &'db dyn HirDb, struct_: Struct<'db>) -> Self {
        Self {
            db,
            struct_,
            refs: Vec::new(),
        }
    }

    fn collect(mut self) -> Vec<ReferenceView<'db>> {
        let mut ctxt = VisitorCtxt::with_struct(self.db, self.struct_);
        self.visit_struct(&mut ctxt, self.struct_);
        self.refs
    }
}

impl<'db> Visitor<'db> for StructReferenceCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        if let Some(span) = ctxt.span() {
            let scope = ctxt.scope();
            self.refs.push(ReferenceView::Path(PathView::new(path, scope, span)));
        }
        walk_path(self, ctxt, path);
    }
}

// Salsa query for caching struct references (internal)
#[salsa::tracked(return_ref)]
fn struct_references<'db>(db: &'db dyn HirDb, struct_: Struct<'db>) -> Vec<ReferenceView<'db>> {
    StructReferenceCollector::new(db, struct_).collect()
}

// ---------------------------------------------------------------------------
// Enum reference collection
// ---------------------------------------------------------------------------

/// Collects all symbolic references within an enum definition (variant types,
/// generic params, where clause).
struct EnumReferenceCollector<'db> {
    db: &'db dyn HirDb,
    enum_: Enum<'db>,
    refs: Vec<ReferenceView<'db>>,
}

impl<'db> EnumReferenceCollector<'db> {
    fn new(db: &'db dyn HirDb, enum_: Enum<'db>) -> Self {
        Self {
            db,
            enum_,
            refs: Vec::new(),
        }
    }

    fn collect(mut self) -> Vec<ReferenceView<'db>> {
        let mut ctxt = VisitorCtxt::with_enum(self.db, self.enum_);
        self.visit_enum(&mut ctxt, self.enum_);
        self.refs
    }
}

impl<'db> Visitor<'db> for EnumReferenceCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        if let Some(span) = ctxt.span() {
            let scope = ctxt.scope();
            self.refs.push(ReferenceView::Path(PathView::new(path, scope, span)));
        }
        walk_path(self, ctxt, path);
    }
}

// Salsa query for caching enum references (internal)
#[salsa::tracked(return_ref)]
fn enum_references<'db>(db: &'db dyn HirDb, enum_: Enum<'db>) -> Vec<ReferenceView<'db>> {
    EnumReferenceCollector::new(db, enum_).collect()
}

// ---------------------------------------------------------------------------
// TypeAlias reference collection
// ---------------------------------------------------------------------------

/// Collects all symbolic references within a type alias definition.
struct TypeAliasReferenceCollector<'db> {
    db: &'db dyn HirDb,
    type_alias: TypeAlias<'db>,
    refs: Vec<ReferenceView<'db>>,
}

impl<'db> TypeAliasReferenceCollector<'db> {
    fn new(db: &'db dyn HirDb, type_alias: TypeAlias<'db>) -> Self {
        Self {
            db,
            type_alias,
            refs: Vec::new(),
        }
    }

    fn collect(mut self) -> Vec<ReferenceView<'db>> {
        let mut ctxt = VisitorCtxt::with_type_alias(self.db, self.type_alias);
        self.visit_type_alias(&mut ctxt, self.type_alias);
        self.refs
    }
}

impl<'db> Visitor<'db> for TypeAliasReferenceCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        if let Some(span) = ctxt.span() {
            let scope = ctxt.scope();
            self.refs.push(ReferenceView::Path(PathView::new(path, scope, span)));
        }
        walk_path(self, ctxt, path);
    }
}

// Salsa query for caching type alias references (internal)
#[salsa::tracked(return_ref)]
fn type_alias_references<'db>(db: &'db dyn HirDb, type_alias: TypeAlias<'db>) -> Vec<ReferenceView<'db>> {
    TypeAliasReferenceCollector::new(db, type_alias).collect()
}

// ---------------------------------------------------------------------------
// Impl reference collection
// ---------------------------------------------------------------------------

/// Collects all symbolic references within an impl block (target type,
/// trait ref, generic params, where clause).
struct ImplReferenceCollector<'db> {
    db: &'db dyn HirDb,
    impl_: Impl<'db>,
    refs: Vec<ReferenceView<'db>>,
}

impl<'db> ImplReferenceCollector<'db> {
    fn new(db: &'db dyn HirDb, impl_: Impl<'db>) -> Self {
        Self {
            db,
            impl_,
            refs: Vec::new(),
        }
    }

    fn collect(mut self) -> Vec<ReferenceView<'db>> {
        let mut ctxt = VisitorCtxt::with_impl(self.db, self.impl_);
        self.visit_impl(&mut ctxt, self.impl_);
        self.refs
    }
}

impl<'db> Visitor<'db> for ImplReferenceCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        if let Some(span) = ctxt.span() {
            let scope = ctxt.scope();
            self.refs.push(ReferenceView::Path(PathView::new(path, scope, span)));
        }
        walk_path(self, ctxt, path);
    }

    // Skip body traversal - body refs are collected separately
    fn visit_body(&mut self, _ctxt: &mut VisitorCtxt<'db, LazyBodySpan<'db>>, _body: Body<'db>) {}
}

// Salsa query for caching impl references (internal)
#[salsa::tracked(return_ref)]
fn impl_references<'db>(db: &'db dyn HirDb, impl_: Impl<'db>) -> Vec<ReferenceView<'db>> {
    ImplReferenceCollector::new(db, impl_).collect()
}

// ---------------------------------------------------------------------------
// Trait reference collection
// ---------------------------------------------------------------------------

/// Collects all symbolic references within a trait definition (super traits,
/// generic params, where clause, associated types).
struct TraitReferenceCollector<'db> {
    db: &'db dyn HirDb,
    trait_: Trait<'db>,
    refs: Vec<ReferenceView<'db>>,
}

impl<'db> TraitReferenceCollector<'db> {
    fn new(db: &'db dyn HirDb, trait_: Trait<'db>) -> Self {
        Self {
            db,
            trait_,
            refs: Vec::new(),
        }
    }

    fn collect(mut self) -> Vec<ReferenceView<'db>> {
        let mut ctxt = VisitorCtxt::with_trait(self.db, self.trait_);
        self.visit_trait(&mut ctxt, self.trait_);
        self.refs
    }
}

impl<'db> Visitor<'db> for TraitReferenceCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        if let Some(span) = ctxt.span() {
            let scope = ctxt.scope();
            self.refs.push(ReferenceView::Path(PathView::new(path, scope, span)));
        }
        walk_path(self, ctxt, path);
    }

    // Skip body traversal - body refs are collected separately
    fn visit_body(&mut self, _ctxt: &mut VisitorCtxt<'db, LazyBodySpan<'db>>, _body: Body<'db>) {}
}

// Salsa query for caching trait references (internal)
#[salsa::tracked(return_ref)]
fn trait_references<'db>(db: &'db dyn HirDb, trait_: Trait<'db>) -> Vec<ReferenceView<'db>> {
    TraitReferenceCollector::new(db, trait_).collect()
}

// ---------------------------------------------------------------------------
// ImplTrait reference collection
// ---------------------------------------------------------------------------

/// Collects all symbolic references within an impl trait block.
struct ImplTraitReferenceCollector<'db> {
    db: &'db dyn HirDb,
    impl_trait: ImplTrait<'db>,
    refs: Vec<ReferenceView<'db>>,
}

impl<'db> ImplTraitReferenceCollector<'db> {
    fn new(db: &'db dyn HirDb, impl_trait: ImplTrait<'db>) -> Self {
        Self {
            db,
            impl_trait,
            refs: Vec::new(),
        }
    }

    fn collect(mut self) -> Vec<ReferenceView<'db>> {
        let mut ctxt = VisitorCtxt::with_impl_trait(self.db, self.impl_trait);
        self.visit_impl_trait(&mut ctxt, self.impl_trait);
        self.refs
    }
}

impl<'db> Visitor<'db> for ImplTraitReferenceCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        if let Some(span) = ctxt.span() {
            let scope = ctxt.scope();
            self.refs.push(ReferenceView::Path(PathView::new(path, scope, span)));
        }
        walk_path(self, ctxt, path);
    }

    // Skip body traversal - body refs are collected separately
    fn visit_body(&mut self, _ctxt: &mut VisitorCtxt<'db, LazyBodySpan<'db>>, _body: Body<'db>) {}
}

// Salsa query for caching impl trait references (internal)
#[salsa::tracked(return_ref)]
fn impl_trait_references<'db>(db: &'db dyn HirDb, impl_trait: ImplTrait<'db>) -> Vec<ReferenceView<'db>> {
    ImplTraitReferenceCollector::new(db, impl_trait).collect()
}

// ---------------------------------------------------------------------------
// Use reference collection
// ---------------------------------------------------------------------------

/// Collects all symbolic references within a use statement.
struct UseReferenceCollector<'db> {
    db: &'db dyn HirDb,
    use_item: Use<'db>,
    refs: Vec<ReferenceView<'db>>,
}

impl<'db> UseReferenceCollector<'db> {
    fn new(db: &'db dyn HirDb, use_item: Use<'db>) -> Self {
        Self {
            db,
            use_item,
            refs: Vec::new(),
        }
    }

    fn collect(mut self) -> Vec<ReferenceView<'db>> {
        let mut ctxt = VisitorCtxt::with_use(self.db, self.use_item);
        self.visit_use(&mut ctxt, self.use_item);
        self.refs
    }
}

impl<'db> Visitor<'db> for UseReferenceCollector<'db> {
    fn visit_use_path(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyUsePathSpan<'db>>,
        use_path: UsePathId<'db>,
    ) {
        // Collect a UsePathView for each segment
        if let Some(span) = ctxt.span() {
            let segment_count = use_path.data(self.db).len();
            for idx in 0..segment_count {
                self.refs.push(ReferenceView::UsePath(UsePathView {
                    use_item: self.use_item,
                    segment: idx,
                    span: span.clone(),
                }));
            }
        }
        // Continue walking for any nested paths
        walk_use_path(self, ctxt, use_path);
    }
}

// Salsa query for caching use references (internal)
#[salsa::tracked(return_ref)]
fn use_references<'db>(db: &'db dyn HirDb, use_item: Use<'db>) -> Vec<ReferenceView<'db>> {
    UseReferenceCollector::new(db, use_item).collect()
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
}

