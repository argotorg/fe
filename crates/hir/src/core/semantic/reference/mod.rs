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
    hir_def::HirIngot,
    hir_def::scope_graph::ScopeId,
    hir_def::{Body, Expr, ExprId, FieldIndex, ItemKind, Partial, PathId, Use, UsePathSegment},
    span::{
        DynLazySpan, LazySpan,
        lazy_spans::{LazyFieldExprSpan, LazyMethodCallExprSpan, LazyPathSpan, LazyUsePathSpan},
    },
};

pub use has_references::HasReferences;

/// Resolve a path to all possible scopes, including ambiguous candidates.
///
/// For ambiguous paths that can resolve to multiple items (e.g., a module
/// and a function with the same name), this returns all candidates.
pub(crate) fn resolve_path_to_scopes<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> Vec<ScopeId<'db>> {
    let assumptions = PredicateListId::empty_list(db);
    match resolve_path(db, path, scope, assumptions, false) {
        Ok(res) => res.as_scope(db).into_iter().collect(),
        Err(err) => {
            match err.kind {
                PathResErrorKind::NotFound { bucket, .. } => {
                    // The bucket may contain valid results even on "NotFound"
                    bucket.iter_ok().flat_map(|r| r.scope()).collect()
                }
                PathResErrorKind::Ambiguous(vec) => {
                    // Return all ambiguous candidates
                    vec.into_iter().flat_map(|r| r.scope()).collect()
                }
                _ => vec![],
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

/// The result of resolving a reference to its target(s).
///
/// References typically resolve to a single target, but ambiguous references
/// (e.g., a path that matches both a module and a function) can have multiple
/// candidates.
#[derive(Clone, Debug)]
pub enum TargetResolution<'db> {
    /// No target found
    None,
    /// Resolved to a single unambiguous target
    Single(Target<'db>),
    /// Multiple possible targets (ambiguous reference)
    Ambiguous(Vec<Target<'db>>),
}

impl<'db> TargetResolution<'db> {
    /// Returns true if resolution found at least one target.
    pub fn is_resolved(&self) -> bool {
        !matches!(self, Self::None)
    }

    /// Returns the first/best target, or None if no targets found.
    pub fn first(&self) -> Option<&Target<'db>> {
        match self {
            Self::None => None,
            Self::Single(target) => Some(target),
            Self::Ambiguous(targets) => targets.first(),
        }
    }

    /// Returns all targets as a slice.
    pub fn as_slice(&self) -> &[Target<'db>] {
        match self {
            Self::None => &[],
            Self::Single(target) => std::slice::from_ref(target),
            Self::Ambiguous(targets) => targets,
        }
    }

    /// Consumes self and returns all targets as a Vec.
    pub fn into_vec(self) -> Vec<Target<'db>> {
        match self {
            Self::None => vec![],
            Self::Single(target) => vec![target],
            Self::Ambiguous(targets) => targets,
        }
    }

    /// Returns true if this is an ambiguous resolution.
    pub fn is_ambiguous(&self) -> bool {
        matches!(self, Self::Ambiguous(_))
    }
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

    /// Resolve this path to its target definition(s).
    ///
    /// Returns TargetResolution which can be:
    /// - None: path doesn't resolve
    /// - Single: unambiguous resolution
    /// - Ambiguous: multiple candidates (e.g., module and function with same name)
    ///
    /// Local bindings always take precedence and are never ambiguous.
    pub fn target(&self, db: &'db dyn HirAnalysisDb) -> TargetResolution<'db> {
        // Try local binding resolution first - locals shadow module-level items
        if let Some(local) = self.local_target(db) {
            return TargetResolution::Single(local);
        }

        // Fall back to module-level resolution (may be ambiguous)
        let scopes = resolve_path_to_scopes(db, self.path, self.scope);
        match scopes.len() {
            0 => TargetResolution::None,
            1 => TargetResolution::Single(Target::Scope(scopes.into_iter().next().unwrap())),
            _ => TargetResolution::Ambiguous(scopes.into_iter().map(Target::Scope).collect()),
        }
    }

    /// Resolve this path at a specific cursor position (segment-aware).
    ///
    /// Clicking on `foo` in `foo::Bar` resolves to `foo`, not `Bar`.
    pub fn target_at<DB>(&self, db: &'db DB, cursor: TextSize) -> TargetResolution<'db>
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
                // (locals shadow module-level items and are never ambiguous)
                if idx == last_idx {
                    if let Some(local) = self.local_target(db) {
                        return TargetResolution::Single(local);
                    }
                }

                // Try module-level resolution for this segment (may be ambiguous)
                if let Some(seg_path) = self.path.segment(db, idx) {
                    let scopes = resolve_path_to_scopes(db, seg_path, self.scope);
                    return match scopes.len() {
                        0 => TargetResolution::None,
                        1 => TargetResolution::Single(Target::Scope(
                            scopes.into_iter().next().unwrap(),
                        )),
                        _ => TargetResolution::Ambiguous(
                            scopes.into_iter().map(Target::Scope).collect(),
                        ),
                    };
                }
                return TargetResolution::None;
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

    /// Get the span of the last segment (the actual referenced item).
    ///
    /// For `Foo::Bar`, this returns just "Bar", not the entire path.
    /// Used for rename operations to replace only the referenced item.
    pub fn last_segment_span(&self, db: &'db dyn SpannedHirDb) -> DynLazySpan<'db> {
        let last_idx = self.path.segment_index(db);
        self.span.clone().segment(last_idx).ident().into()
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
    pub fn target(&self, db: &'db dyn HirAnalysisDb) -> TargetResolution<'db> {
        // Get the expression data to extract receiver and field name
        let Partial::Present(Expr::Field(receiver, field_index)) = self.expr.data(db, self.body)
        else {
            return TargetResolution::None;
        };
        let Partial::Present(FieldIndex::Ident(field_name)) = field_index else {
            return TargetResolution::None; // Tuple field access (e.g., tuple.0) doesn't have a scope
        };

        // Get the containing function for type checking
        let Some(func) = self.body.containing_func(db) else {
            return TargetResolution::None;
        };

        // Run type checking to get typed body
        let (_, typed_body) = check_func_body(db, func);

        // Get the type of the receiver expression
        let receiver_ty = typed_body.expr_ty(db, *receiver);
        if receiver_ty.has_invalid(db) {
            return TargetResolution::None;
        }

        // Resolve the field scope using RecordLike
        let record_like = RecordLike::from_ty(receiver_ty);
        match record_like.record_field_scope(db, *field_name) {
            Some(scope) => TargetResolution::Single(Target::Scope(scope)),
            None => TargetResolution::None,
        }
    }

    /// Get the source span of this field access.
    ///
    /// Returns the span of just the field name token, not the entire
    /// field access expression. For `self.storage`, this returns just "storage".
    pub fn span(&self) -> DynLazySpan<'db> {
        self.span.clone().accessor().into()
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
    pub fn target(&self, db: &'db dyn HirAnalysisDb) -> TargetResolution<'db> {
        // Get the containing function for type checking
        let Some(func) = self.body.containing_func(db) else {
            return TargetResolution::None;
        };

        // Run type checking to get typed body with callable information
        let (_, typed_body) = check_func_body(db, func);

        // Get the callable for this method call expression
        let Some(callable) = typed_body.callable_expr(self.expr) else {
            return TargetResolution::None;
        };

        // Extract the scope from the callable definition
        let scope = match callable.callable_def {
            crate::hir_def::CallableDef::Func(method_func) => {
                ScopeId::from_item(ItemKind::Func(method_func))
            }
            crate::hir_def::CallableDef::VariantCtor(variant) => ScopeId::Variant(variant),
        };
        TargetResolution::Single(Target::Scope(scope))
    }

    /// Get the source span of this method call.
    ///
    /// Returns the span of just the method name token, not the entire
    /// method call expression. For `self.get(key)`, this returns just "get".
    pub fn span(&self) -> DynLazySpan<'db> {
        self.span.clone().method_name().into()
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
    pub fn target(&self, db: &'db dyn HirAnalysisDb) -> TargetResolution<'db> {
        let Some(use_path) = self.use_item.path(db).to_opt() else {
            return TargetResolution::None;
        };
        let segments = use_path.data(db);

        // Start from the use's parent scope
        let Some(mut current_scope) = self.use_item.scope().parent(db) else {
            return TargetResolution::None;
        };

        // Resolve each segment up to and including self.segment
        for idx in 0..=self.segment {
            let Some(segment) = segments.get(idx).and_then(|s| s.to_opt()) else {
                return TargetResolution::None;
            };
            let ident = match segment {
                UsePathSegment::Ident(id) => id,
                UsePathSegment::Glob => return TargetResolution::None, // Can't resolve glob
            };

            // Try regular name resolution first
            let directive = QueryDirective::new();
            let query = EarlyNameQueryId::new(db, ident, current_scope, directive);
            let bucket = resolve_query(db, query);

            if let Some(res) = bucket.iter_ok().next() {
                if let Some(scope) = res.scope() {
                    current_scope = scope;
                    continue;
                }
            }

            // If name resolution failed and we're in a TopLevelMod scope,
            // check for child file modules in the module tree
            if let ScopeId::Item(ItemKind::TopMod(top_mod)) = current_scope {
                let module_tree = top_mod.ingot(db).module_tree(db);
                if let Some(child) = module_tree
                    .children(top_mod)
                    .find(|child_mod| child_mod.name(db) == ident)
                {
                    current_scope = ScopeId::Item(ItemKind::TopMod(child));
                    continue;
                }
            }

            // Resolution failed
            return TargetResolution::None;
        }

        TargetResolution::Single(Target::Scope(current_scope))
    }

    /// Get the source span of this use path segment.
    ///
    /// Returns the span of just this segment, not the entire use path.
    /// For `use foo::bar::Baz` with segment=1, this returns just "bar".
    pub fn span(&self) -> DynLazySpan<'db> {
        self.span.clone().segment(self.segment).into()
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
    /// Resolve this reference to its target definition(s).
    ///
    /// Returns TargetResolution which can be None, Single, or Ambiguous.
    pub fn target(&self, db: &'db dyn HirAnalysisDb) -> TargetResolution<'db> {
        match self {
            Self::Path(v) => v.target(db),
            Self::FieldAccess(v) => v.target(db),
            Self::MethodCall(v) => v.target(db),
            Self::UsePath(v) => v.target(db),
        }
    }

    /// Resolve this reference at a specific cursor position (segment-aware).
    ///
    /// For paths, this handles segment-level resolution - clicking on `foo` in
    /// `foo::Bar` resolves to `foo`, not `Bar`.
    pub fn target_at<DB>(&self, db: &'db DB, cursor: TextSize) -> TargetResolution<'db>
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

    /// Get the span to use for rename operations.
    ///
    /// For paths, returns only the last segment (the actual referenced item).
    /// For other references, returns the same as span().
    pub fn rename_span(&self, db: &'db dyn SpannedHirDb) -> DynLazySpan<'db> {
        match self {
            Self::Path(v) => v.last_segment_span(db),
            Self::FieldAccess(v) => v.span(),
            Self::MethodCall(v) => v.span(),
            Self::UsePath(v) => v.span(),
        }
    }
}
