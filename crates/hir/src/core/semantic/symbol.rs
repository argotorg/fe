//! Symbol lookup for LSP features.
//!
//! This module provides a simple interface for finding symbols at cursor positions.
//! Instead of a complex SymbolKind enum, we use ScopeId directly as the universal
//! identity for all symbols, with visitor-based traversal for collecting scopes.

use common::diagnostics::Span;
use common::file::File;
use parser::TextSize;

use crate::analysis::name_resolution::{PathRes, resolve_path};
use crate::analysis::ty::trait_resolution::PredicateListId;
use crate::analysis::HirAnalysisDb;
use crate::hir_def::scope_graph::ScopeId;
use crate::hir_def::{Body, Expr, ExprId, ItemKind, Partial, PathId, TopLevelMod};
use crate::hir_def::{FieldDef, FuncParam, GenericParam, VariantDef};
use crate::lower::map_file_to_mod_impl;
use crate::span::{DynLazySpan, LazySpan};
use crate::visitor::prelude::*;
use crate::visitor::{Visitor, VisitorCtxt, walk_field_def, walk_func_param, walk_generic_param, walk_item, walk_top_mod, walk_variant_def};
use crate::span::path::LazyPathSpan;
use crate::SpannedHirDb;

// =============================================================================
// Reference types - things that reference definitions
// =============================================================================

/// A reference to something within a definition.
/// Unlike ScopeId which represents definitions, Reference represents
/// use-sites: paths, field accesses, method calls, type annotations, etc.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Reference<'db> {
    /// A path expression like `foo::Bar` or just `x`
    Path(PathRefView<'db>),
    /// A field access like `x.field`
    FieldAccess(FieldAccessView<'db>),
    /// A method call like `x.method()`
    MethodCall(MethodCallView<'db>),
}

impl<'db> Reference<'db> {
    /// Returns the span of this reference.
    pub fn span(&self) -> DynLazySpan<'db> {
        match self {
            Self::Path(p) => p.span(),
            Self::FieldAccess(f) => f.span(),
            Self::MethodCall(m) => m.span(),
        }
    }

    /// Returns the containing body.
    pub fn body(&self) -> Body<'db> {
        match self {
            Self::Path(p) => p.body,
            Self::FieldAccess(f) => f.body,
            Self::MethodCall(m) => m.body,
        }
    }

    /// Resolves this reference to target scopes for goto-definition.
    pub fn resolve_to_scopes(&self, db: &'db dyn HirAnalysisDb) -> Vec<ScopeId<'db>> {
        match self {
            Self::Path(p) => p.resolve_to_scopes(db),
            // TODO: Field and method resolution require type information
            Self::FieldAccess(_) => Vec::new(),
            Self::MethodCall(_) => Vec::new(),
        }
    }
}

/// A path reference within a body (e.g., `foo::Bar` in an expression).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PathRefView<'db> {
    pub(in crate::core) body: Body<'db>,
    pub(in crate::core) expr: ExprId,
    pub(in crate::core) path: PathId<'db>,
}

impl<'db> PathRefView<'db> {
    pub fn new(body: Body<'db>, expr: ExprId, path: PathId<'db>) -> Self {
        Self { body, expr, path }
    }

    pub fn span(&self) -> DynLazySpan<'db> {
        self.expr.span(self.body).into_path_expr().path().into()
    }

    /// Returns the LazyPathSpan for this path expression.
    fn lazy_path_span(&self) -> LazyPathSpan<'db> {
        self.expr.span(self.body).into_path_expr().path()
    }

    pub fn path(&self) -> PathId<'db> {
        self.path
    }

    /// Returns a new PathRefView narrowed to the segment containing the cursor.
    /// If the cursor is on a specific segment of a multi-segment path (e.g., `core` in `core::todo`),
    /// returns a PathRefView for just that segment's path.
    pub fn narrow_to_segment_at_cursor(
        &self,
        db: &'db dyn SpannedHirDb,
        cursor: TextSize,
    ) -> Option<PathRefView<'db>> {
        let lazy_span = self.lazy_path_span();
        let last_idx = self.path.segment_index(db);

        for idx in 0..=last_idx {
            let Some(seg_span) = lazy_span.clone().segment(idx).resolve(db) else {
                continue;
            };

            if seg_span.range.contains(cursor) {
                if let Some(seg_path) = self.path.segment(db, idx) {
                    return Some(PathRefView::new(self.body, self.expr, seg_path));
                }
            }
        }

        None
    }

    /// Resolves this path reference to its target.
    pub fn resolve(&self, db: &'db dyn HirAnalysisDb) -> Option<PathRes<'db>> {
        let scope = self.body.scope();
        let assumptions = PredicateListId::empty_list(db);
        resolve_path(db, self.path, scope, assumptions, true).ok()
    }

    /// Resolves this path reference to target scopes for goto-definition.
    pub fn resolve_to_scopes(&self, db: &'db dyn HirAnalysisDb) -> Vec<ScopeId<'db>> {
        self.resolve(db)
            .and_then(|res| res.as_scope(db))
            .into_iter()
            .collect()
    }
}

/// A field access reference like `x.field`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldAccessView<'db> {
    pub(in crate::core) body: Body<'db>,
    pub(in crate::core) expr: ExprId,
}

impl<'db> FieldAccessView<'db> {
    pub fn new(body: Body<'db>, expr: ExprId) -> Self {
        Self { body, expr }
    }

    pub fn span(&self) -> DynLazySpan<'db> {
        self.expr.span(self.body).into_field_expr().accessor().into()
    }
}

/// A method call reference like `x.method()`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodCallView<'db> {
    pub(in crate::core) body: Body<'db>,
    pub(in crate::core) expr: ExprId,
}

impl<'db> MethodCallView<'db> {
    pub fn new(body: Body<'db>, expr: ExprId) -> Self {
        Self { body, expr }
    }

    pub fn span(&self) -> DynLazySpan<'db> {
        self.expr
            .span(self.body)
            .into_method_call_expr()
            .method_name()
            .into()
    }
}

// =============================================================================
// Reference collection
// =============================================================================

/// Collects all references from a body's expressions.
pub fn collect_body_references<'db>(db: &'db dyn crate::HirDb, body: Body<'db>) -> Vec<Reference<'db>> {
    let mut refs = Vec::new();

    for (expr_id, expr_data) in body.exprs(db).iter() {
        let Partial::Present(expr) = expr_data else {
            continue;
        };

        match expr {
            Expr::Path(Partial::Present(path)) => {
                refs.push(Reference::Path(PathRefView::new(body, expr_id, *path)));
            }
            Expr::Field(_, _) => {
                refs.push(Reference::FieldAccess(FieldAccessView::new(body, expr_id)));
            }
            Expr::MethodCall(_, _, _, _) => {
                refs.push(Reference::MethodCall(MethodCallView::new(body, expr_id)));
            }
            Expr::RecordInit(Partial::Present(path), _) => {
                // Record initialization also references a type via path
                refs.push(Reference::Path(PathRefView::new(body, expr_id, *path)));
            }
            _ => {}
        }
    }

    refs
}

// =============================================================================
// Cursor lookup - finding symbols at a position
// =============================================================================

/// What we found at a cursor position.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CursorTarget<'db> {
    /// The cursor is on a definition.
    Definition(ScopeId<'db>),
    /// The cursor is on a reference to something.
    Reference(Reference<'db>),
}

impl<'db> CursorTarget<'db> {
    /// Returns the span of this cursor target.
    pub fn span(&self, db: &'db dyn SpannedHirDb) -> Option<Span> {
        match self {
            Self::Definition(scope) => scope.name_span(db).and_then(|s| s.resolve(db)),
            Self::Reference(r) => r.span().resolve(db),
        }
    }

    /// Resolves this cursor target to scopes for goto-definition.
    ///
    /// For definitions, returns the definition's own scope.
    /// For references, resolves the reference to its target(s).
    pub fn resolve_to_scopes(&self, db: &'db dyn HirAnalysisDb) -> Vec<ScopeId<'db>> {
        match self {
            Self::Definition(scope) => vec![*scope],
            Self::Reference(r) => r.resolve_to_scopes(db),
        }
    }
}

/// Finds the symbol or reference at the given cursor position in a file.
///
/// Returns `None` if the cursor is not on any recognized symbol.
pub fn find_at_cursor<'db>(
    db: &'db dyn SpannedHirDb,
    file: File,
    cursor: TextSize,
) -> Option<CursorTarget<'db>> {
    let top_mod = map_file_to_mod_impl(db, file);

    // Collect all scopes using visitor-based iterator generator
    let all_scopes: Vec<_> = scope_iter(db, top_mod).collect();

    // First, find all scopes that might contain the cursor (on their name span)
    // We look for the deepest (most specific) one
    let mut best_def: Option<(ScopeId<'db>, Span)> = None;

    for scope in &all_scopes {
        if let Some(name_span) = scope.name_span(db) {
            if let Some(resolved) = name_span.resolve(db) {
                if resolved.range.contains(cursor) {
                    // Prefer more specific (smaller) spans
                    let dominated = best_def.as_ref().is_some_and(|(_, prev_span)| {
                        prev_span.range.contains(resolved.range.start())
                            && prev_span.range.contains(resolved.range.end())
                    });
                    if best_def.is_none() || dominated {
                        best_def = Some((*scope, resolved));
                    }
                }
            }
        }
    }

    // Check if cursor is on a reference within a function body
    // We need to check all functions since the cursor might be in an expression
    for scope in &all_scopes {
        if let ScopeId::Item(ItemKind::Func(f)) = scope {
            if let Some(body) = f.body(db) {
                let refs = collect_body_references(db, body);
                for r in refs {
                    if let Some(resolved) = r.span().resolve(db) {
                        if resolved.range.contains(cursor) {
                            // For path references, narrow to the specific segment at cursor
                            if let Reference::Path(path_ref) = &r {
                                if let Some(narrowed) = path_ref.narrow_to_segment_at_cursor(db, cursor) {
                                    return Some(CursorTarget::Reference(Reference::Path(narrowed)));
                                }
                            }
                            return Some(CursorTarget::Reference(r));
                        }
                    }
                }
            }
        }
    }

    // Return the definition if found
    best_def.map(|(scope, _)| CursorTarget::Definition(scope))
}

// =============================================================================
// Iterator generator for scope traversal
// =============================================================================

use std::collections::VecDeque;

/// Iterator generator that uses visitor infrastructure to yield scopes.
/// The visitor populates a queue during traversal, and the Iterator impl
/// drains from that queue.
struct ScopeIterator<'db> {
    queue: VecDeque<ScopeId<'db>>,
}

impl<'db> ScopeIterator<'db> {
    fn new() -> Self {
        Self {
            queue: VecDeque::new(),
        }
    }

    /// Run the visitor on a top-level module to populate the queue.
    fn populate(mut self, db: &'db dyn SpannedHirDb, top_mod: TopLevelMod<'db>) -> Self {
        let mut ctxt = VisitorCtxt::with_top_mod(db, top_mod);
        self.visit_top_mod(&mut ctxt, top_mod);
        self
    }
}

impl<'db> Iterator for ScopeIterator<'db> {
    type Item = ScopeId<'db>;

    fn next(&mut self) -> Option<Self::Item> {
        self.queue.pop_front()
    }
}

impl<'db> Visitor<'db> for ScopeIterator<'db> {
    fn visit_top_mod(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyTopModSpan<'db>>,
        top_mod: TopLevelMod<'db>,
    ) {
        self.queue.push_back(ctxt.scope());
        walk_top_mod(self, ctxt, top_mod);
    }

    fn visit_item(&mut self, ctxt: &mut VisitorCtxt<'db, LazyItemSpan<'db>>, item: ItemKind<'db>) {
        // Don't emit TopMod again - it's already emitted in visit_top_mod
        if !matches!(item, ItemKind::TopMod(_)) {
            self.queue.push_back(ctxt.scope());
        }
        walk_item(self, ctxt, item);
    }

    fn visit_generic_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyGenericParamSpan<'db>>,
        param: &GenericParam<'db>,
    ) {
        self.queue.push_back(ctxt.scope());
        walk_generic_param(self, ctxt, param);
    }

    fn visit_func_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFuncParamSpan<'db>>,
        param: &FuncParam<'db>,
    ) {
        self.queue.push_back(ctxt.scope());
        walk_func_param(self, ctxt, param);
    }

    fn visit_field_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFieldDefSpan<'db>>,
        field: &FieldDef<'db>,
    ) {
        self.queue.push_back(ctxt.scope());
        walk_field_def(self, ctxt, field);
    }

    fn visit_variant_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyVariantDefSpan<'db>>,
        variant: &VariantDef<'db>,
    ) {
        self.queue.push_back(ctxt.scope());
        walk_variant_def(self, ctxt, variant);
    }
}

/// Returns an iterator over all scopes in a top-level module.
fn scope_iter<'db>(db: &'db dyn SpannedHirDb, top_mod: TopLevelMod<'db>) -> impl Iterator<Item = ScopeId<'db>> {
    ScopeIterator::new().populate(db, top_mod)
}
