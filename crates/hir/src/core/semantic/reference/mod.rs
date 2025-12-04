//! Reference views for symbolic references in the HIR.
//!
//! This module provides view types for tracking symbolic references (paths,
//! field accesses, method calls, use paths) throughout the HIR. These views
//! enable efficient goto-definition and find-all-references functionality.
//!
//! Each view bundles the minimal information needed to:
//! 1. Resolve the reference to its target definition
//! 2. Locate the reference in source code via its span

mod collector;
mod has_references;

use parser::TextSize;

use crate::{
    SpannedHirDb,
    analysis::HirAnalysisDb,
    analysis::name_resolution::{
        EarlyNameQueryId, PathResErrorKind, QueryDirective, resolve_path, resolve_query,
    },
    analysis::ty::{
        trait_resolution::PredicateListId,
        ty_check::{RecordLike, check_func_body},
        ty_def::TyId,
    },
    hir_def::scope_graph::ScopeId,
    hir_def::{Body, Expr, ExprId, FieldIndex, ItemKind, Partial, PathId, Use, UsePathSegment},
    span::{
        DynLazySpan, LazySpan,
        lazy_spans::{LazyFieldExprSpan, LazyMethodCallExprSpan, LazyPathSpan, LazyUsePathSpan},
    },
};

pub use has_references::HasReferences;

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

/// The resolved target of a reference.
///
/// References can resolve to either module-level items (scopes) or
/// local bindings (variables, parameters).
#[derive(Clone, Debug)]
pub enum Target<'db> {
    /// A module-level item (function, struct, enum, etc.)
    Scope(ScopeId<'db>),
    /// A local binding - has definition span, inferred type, and context for finding other references
    Local {
        span: DynLazySpan<'db>,
        ty: TyId<'db>,
        /// The containing function (needed to find other references to this local)
        func: crate::hir_def::Func<'db>,
        /// The expression referencing this local (needed to identify the binding)
        expr: ExprId,
    },
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
    /// Tries local binding resolution first (for paths inside function bodies),
    /// then falls back to module-level resolution. This ensures local variables
    /// shadow module-level items with the same name.
    pub fn target(&self, db: &'db dyn HirAnalysisDb) -> Option<Target<'db>> {
        // Try local binding resolution first - locals shadow module-level items
        if let Some(local) = self.local_target(db) {
            return Some(local);
        }

        // Fall back to module-level resolution
        resolve_path_to_scope(db, self.path, self.scope).map(Target::Scope)
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
                // If this is the last segment, try local binding resolution first
                // (locals shadow module-level items)
                if idx == last_idx {
                    if let Some(local) = self.local_target(db) {
                        return Some(local);
                    }
                }

                // Try module-level resolution for this segment
                if let Some(seg_path) = self.path.segment(db, idx) {
                    if let Some(scope) = resolve_path_to_scope(db, seg_path, self.scope) {
                        return Some(Target::Scope(scope));
                    }
                }
                return None;
            }
        }

        // Cursor not in any segment, try full path resolution
        self.target(db)
    }

    /// Resolve to a local binding target (internal helper).
    fn local_target(&self, db: &'db dyn HirAnalysisDb) -> Option<Target<'db>> {
        let body = self.scope.body()?;
        let func = body.containing_func(db)?;
        let expr_id = body.find_expr_for_path(db, self.path)?;
        let (_, typed_body) = check_func_body(db, func);

        let span = typed_body.expr_binding_def_span(func, expr_id)?;
        let ty = typed_body.expr_ty(db, expr_id);

        Some(Target::Local {
            span,
            ty,
            func,
            expr: expr_id,
        })
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
        let Partial::Present(Expr::Field(receiver, field_index)) = self.expr.data(db, self.body)
        else {
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
            crate::hir_def::CallableDef::VariantCtor(variant) => Some(ScopeId::Variant(variant)),
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
