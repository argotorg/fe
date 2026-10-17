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
pub(crate) mod resolver;

use parser::TextSize;

use crate::{
    SpannedHirDb,
    analysis::HirAnalysisDb,
    analysis::name_resolution::{
        EarlyNameQueryId, PathRes, PathResError, PathResErrorKind, QueryDirective, resolve_path,
        resolve_query, resolve_trait_path_with_observer_and_minter,
    },
    analysis::ty::{
        const_ty::LoweringContext,
        trait_resolution::{PredicateListId, constraint::collect_constraints},
        ty_check::{
            EffectParamSite, LocalBinding, ParamSite, RecordLike, TypedBody,
            check_contract_init_body, check_contract_recv_arm_body, check_func_body,
        },
        ty_def::TyId,
    },
    core::semantic::EffectEnvView,
    hir_def::scope_graph::ScopeId,
    hir_def::{
        Body, Contract, Expr, ExprId, FieldIndex, ItemKind, Partial, PathId, Use, UsePathSegment,
    },
    hir_def::{GenericParamOwner, HirIngot},
    span::{
        DynLazySpan, LazySpan,
        lazy_spans::{LazyFieldExprSpan, LazyMethodCallExprSpan, LazyPathSpan, LazyUsePathSpan},
    },
};

pub use has_references::{HasReferences, MatchedReference};
pub use resolver::{ResolvedScopeTarget, resolved_item_scope_targets};

/// Collect the trait bound assumptions visible at `scope` by walking up the
/// scope chain to the nearest enclosing generic param owner (function, impl,
/// trait, etc.). Returns empty assumptions if no owner is found.
pub(crate) fn enclosing_assumptions<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
) -> PredicateListId<'db> {
    let mut current = scope;
    loop {
        if let Some(owner) = GenericParamOwner::from_item_opt(current.item()) {
            return collect_constraints(db, owner).instantiate_identity();
        }
        match current.parent(db) {
            Some(parent) => current = parent,
            None => return PredicateListId::empty_list(db),
        }
    }
}

/// Get the TypedBody for any Body, regardless of its owner.
///
/// Bodies can belong to functions, contract init blocks, or contract recv arms.
/// This function identifies the owner and calls the appropriate type checker.
pub(crate) fn typed_body_for_body<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
) -> Option<&'db TypedBody<'db>> {
    // Try function body first (most common case)
    if let Some(func) = body.containing_func(db) {
        return Some(&check_func_body(db, func).1);
    }

    // Try contract bodies (init and recv arms)
    let scope_graph = body.scope().scope_graph(db);
    for item in scope_graph.items_dfs(db) {
        if let ItemKind::Contract(contract) = item {
            // Check init body
            if let Some(init) = contract.init(db)
                && init.body(db) == body
            {
                return Some(&check_contract_init_body(db, contract).1);
            }
            // Check recv arm bodies
            for (recv_idx, recv) in contract.recvs(db).data(db).iter().enumerate() {
                for (arm_idx, arm) in recv.arms.data(db).iter().enumerate() {
                    if arm.body == body {
                        return Some(
                            &check_contract_recv_arm_body(
                                db,
                                contract,
                                recv_idx as u32,
                                arm_idx as u32,
                            )
                            .1,
                        );
                    }
                }
            }
        }
    }

    None
}

/// Extract all resolvable scopes from a path resolution result, including
/// ambiguous and partial matches.
fn scopes_from_resolution<'db>(
    db: &'db dyn HirAnalysisDb,
    result: &Result<PathRes<'db>, PathResError<'db>>,
) -> Vec<ScopeId<'db>> {
    match result {
        Ok(res) => res.as_scope(db).into_iter().collect(),
        Err(err) => match &err.kind {
            PathResErrorKind::NotFound { bucket, .. } => {
                bucket.iter_ok().flat_map(|r| r.scope()).collect()
            }
            PathResErrorKind::Ambiguous(vec) => vec.iter().flat_map(|r| r.scope()).collect(),
            _ => vec![],
        },
    }
}

/// Resolve a path to all possible scopes, including ambiguous candidates.
///
/// For ambiguous paths that can resolve to multiple items (e.g., a module
/// and a function with the same name), this returns all candidates.
fn resolve_path_to_scopes<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> Vec<ScopeId<'db>> {
    // Try type-domain resolution first (resolve_tail_as_value=false).
    let result = resolve_path(db, path, scope, PredicateListId::empty_list(db), false);
    let scopes = scopes_from_resolution(db, &result);
    if !scopes.is_empty() {
        return scopes;
    }

    // Retry with value-domain resolution to find methods on type parameters
    // (e.g., `C::method()` where `C: Trait`).
    let assumptions = enclosing_assumptions(db, scope);
    let result = resolve_path(db, path, scope, assumptions, true);
    scopes_from_resolution(db, &result)
}

/// Resolve a path from a scope, with a fallback through the msg module
/// if the scope belongs to a recv arm body.
///
/// In `recv TokenMsg { Mint { to, amount } -> bool { ... } }`, the pattern
/// path `Mint` is stored as a bare identifier resolved from the body scope.
/// Standard resolution fails because `Mint` lives inside the desugared
/// `TokenMsg` module. This helper retries resolution from the msg module
/// scope when the body belongs to a recv arm.
fn resolve_path_with_recv_fallback<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
) -> Vec<ScopeId<'db>> {
    let scopes = resolve_path_to_scopes(db, path, scope);
    if !scopes.is_empty() {
        return scopes;
    }

    // Fallback: if the scope is a recv arm body, resolve from the msg module
    if let Some(body) = scope.body()
        && let Some(msg_scope) = recv_arm_msg_scope(db, body)
    {
        return resolve_path_to_scopes(db, path, msg_scope);
    }

    vec![]
}

/// For a body that belongs to a recv arm with a named msg type, return the
/// msg module's scope for path resolution fallback.
fn recv_arm_msg_scope<'db>(db: &'db dyn HirAnalysisDb, body: Body<'db>) -> Option<ScopeId<'db>> {
    let contract = recv_arm_contract(db, body)?;
    for recv in contract.recvs(db).data(db) {
        for arm in recv.arms.data(db) {
            if arm.body == body {
                let msg_path = recv.msg_path?;
                let assumptions = PredicateListId::empty_list(db);
                if let Ok(PathRes::Mod(scope)) =
                    resolve_path(db, msg_path, contract.scope(), assumptions, false)
                {
                    return Some(scope);
                }
                return None;
            }
        }
    }
    None
}

/// Find the contract that owns a recv arm body.
fn recv_arm_contract<'db>(db: &'db dyn HirAnalysisDb, body: Body<'db>) -> Option<Contract<'db>> {
    let scope_graph = body.scope().scope_graph(db);
    for item in scope_graph.items_dfs(db) {
        if let ItemKind::Contract(contract) = item {
            for recv in contract.recvs(db).data(db) {
                for arm in recv.arms.data(db) {
                    if arm.body == body {
                        return Some(contract);
                    }
                }
            }
        }
    }
    None
}

/// The resolved target of a reference.
///
/// References can resolve to either module-level items (scopes) or
/// local bindings (variables, parameters).
#[derive(Clone, Debug)]
pub enum Target<'db> {
    /// A module-level item (function, struct, enum, etc.)
    Scope(ScopeId<'db>),
    /// A local binding - has definition span, inferred type, and binding info
    Local {
        span: DynLazySpan<'db>,
        ty: TyId<'db>,
        /// The containing body (needed to find other references to this local)
        body: Body<'db>,
        /// The binding itself (param, local variable, or effect param)
        binding: LocalBinding<'db>,
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
    /// Build a `TargetResolution` from a set of resolved scopes.
    pub fn from_scopes(scopes: Vec<ScopeId<'db>>) -> Self {
        match scopes.len() {
            0 => Self::None,
            1 => Self::Single(Target::Scope(scopes.into_iter().next().unwrap())),
            _ => Self::Ambiguous(scopes.into_iter().map(Target::Scope).collect()),
        }
    }

    pub fn first(&self) -> Option<&Target<'db>> {
        match self {
            Self::None => None,
            Self::Single(target) => Some(target),
            Self::Ambiguous(targets) => targets.first(),
        }
    }

    pub fn as_slice(&self) -> &[Target<'db>] {
        match self {
            Self::None => &[],
            Self::Single(target) => std::slice::from_ref(target),
            Self::Ambiguous(targets) => targets,
        }
    }

    pub fn is_ambiguous(&self) -> bool {
        matches!(self, Self::Ambiguous(_))
    }
}

/// Context for where a path appears in a function body.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, salsa::Update)]
pub enum BodyPathContext {
    /// Path in an expression (e.g., `x` in `x + 1` or `foo()`)
    Expr(ExprId),
    /// Path in a pattern that defines a local binding (e.g., `x` in `let x = ...`)
    PatBinding(crate::hir_def::PatId),
    /// Path in a pattern that references an item (e.g., `Red` in `Color::Red => ...`)
    PatReference(crate::hir_def::PatId),
}

/// An unlabeled entry of a contract `init`'s or recv arm's `uses`. Its key
/// names what the compiler resolves the entry to: a field of the contract, or
/// an effect of the contract's own `uses`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct UsesEntry<'db> {
    pub site: EffectParamSite<'db>,
    pub idx: usize,
}

/// A view of a path reference in the HIR.
#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct PathView<'db> {
    pub path: PathId<'db>,
    pub scope: ScopeId<'db>,
    pub span: LazyPathSpan<'db>,
    /// Context when this path is inside a function body.
    /// None for paths in signatures, type annotations, etc.
    pub body_ctx: Option<BodyPathContext>,
    /// Whether the path is written where a trait is expected: a bound, a
    /// super-trait, an implemented trait, or a qualified path's trait.
    pub trait_ref: bool,
    /// The entry whose key the path is, for an unlabeled `uses` entry of a
    /// contract `init` or recv arm.
    pub uses_entry: Option<UsesEntry<'db>>,
}

impl<'db> PathView<'db> {
    pub fn new(
        path: PathId<'db>,
        scope: ScopeId<'db>,
        span: LazyPathSpan<'db>,
        trait_ref: bool,
    ) -> Self {
        Self {
            path,
            scope,
            span,
            body_ctx: None,
            trait_ref,
            uses_entry: None,
        }
    }

    pub fn with_body_ctx(mut self, ctx: BodyPathContext) -> Self {
        self.body_ctx = Some(ctx);
        self
    }

    /// Resolve this path to its target definition(s).
    pub fn target<DB>(&self, db: &'db DB) -> TargetResolution<'db>
    where
        DB: HirAnalysisDb + SpannedHirDb,
    {
        self.segment_target(db, self.path.segment_index(db))
    }

    /// Resolve the path up to segment `idx` to its target definition(s): the
    /// local binding a path names, or the scopes of `segment_targets`.
    pub fn segment_target<DB>(&self, db: &'db DB, idx: usize) -> TargetResolution<'db>
    where
        DB: HirAnalysisDb + SpannedHirDb,
    {
        if idx == self.path.segment_index(db)
            && let Some(local) = self.local_target(db)
        {
            return TargetResolution::Single(local);
        }
        TargetResolution::from_scopes(self.segment_targets(db, idx))
    }

    /// Resolve the path up to segment `idx` to all its possible scopes.
    ///
    /// A path the type checker bound to a local binding names that binding,
    /// so it resolves to the binding's scope if it has one: a function's
    /// parameter, or the contract field a contract's `uses` entry provides.
    /// Resolving it as an item path could find an item of the same name, or
    /// the module for `self`. Likewise, an unlabeled `uses` entry of a
    /// contract `init` or recv arm resolves to the field that provides it, or
    /// to nothing for an effect of the contract's own `uses`, if the compiler
    /// resolves it.
    ///
    /// The last segment of a trait reference is resolved as a trait, under the
    /// enclosing bounds, which its arguments can need (`Sink<T::Item>`).
    pub fn segment_targets(&self, db: &'db dyn HirAnalysisDb, idx: usize) -> Vec<ScopeId<'db>> {
        let Some(seg_path) = self.path.segment(db, idx) else {
            return Vec::new();
        };
        if seg_path == self.path
            && let Some(UsesEntry { site, idx: entry }) = self.uses_entry
            && let Some(binding) = EffectEnvView::new(site).resolved_binding(db, entry)
        {
            return binding
                .provider
                .source
                .contract_field()
                .and_then(|field| field.scope(db))
                .into_iter()
                .collect();
        }
        if seg_path == self.path
            && let Some(binding) = self.binding(db)
        {
            let scope = match binding {
                LocalBinding::Param {
                    site: ParamSite::Func(func),
                    idx: param,
                    ..
                } => Some(ScopeId::FuncParam(func.into(), param as u16)),
                LocalBinding::EffectParam {
                    site, idx: entry, ..
                } => EffectEnvView::new(site)
                    .resolved_binding(db, entry)
                    .and_then(|binding| binding.provider.source.contract_field())
                    .and_then(|field| field.scope(db)),
                LocalBinding::Local { .. } | LocalBinding::Param { .. } => None,
            };
            return scope.into_iter().collect();
        }
        if self.trait_ref && seg_path == self.path {
            let res = resolve_trait_path_with_observer_and_minter(
                db,
                seg_path,
                self.scope,
                enclosing_assumptions(db, self.scope),
                &mut |_, _| {},
                &LoweringContext::new(),
            );
            return scopes_from_resolution(db, &res);
        }
        resolve_path_with_recv_fallback(db, seg_path, self.scope)
    }

    /// Resolve at a specific cursor position (segment-aware: `foo` in `foo::Bar` → foo).
    pub fn target_at<DB>(&self, db: &'db DB, cursor: TextSize) -> TargetResolution<'db>
    where
        DB: HirAnalysisDb + SpannedHirDb,
    {
        let last_idx = self.path.segment_index(db);

        for idx in 0..=last_idx {
            let Some(seg_span) = self.span.clone().segment(idx).resolve(db) else {
                continue;
            };

            if seg_span.range.contains(cursor) {
                return self.segment_target(db, idx);
            }
        }

        self.target(db)
    }

    /// The local binding the type checker bound this path to: a `let` or
    /// pattern binding, a parameter, or an effect.
    fn binding(&self, db: &'db dyn HirAnalysisDb) -> Option<LocalBinding<'db>> {
        let typed_body = typed_body_for_body(db, self.scope.body()?)?;
        match self.body_ctx? {
            BodyPathContext::Expr(expr) => typed_body.expr_binding(expr),
            BodyPathContext::PatBinding(pat) => typed_body.pat_binding(pat),
            BodyPathContext::PatReference(_) => None,
        }
    }

    fn local_target<DB>(&self, db: &'db DB) -> Option<Target<'db>>
    where
        DB: HirAnalysisDb + SpannedHirDb,
    {
        let body_ctx = self.body_ctx?;
        let body = self.scope.body()?;
        let typed_body = typed_body_for_body(db, body)?;

        match body_ctx {
            BodyPathContext::Expr(expr_id) => {
                // Expression reference (e.g., `p` in `p.foo()` or `x + 1`)
                let def_span = typed_body.expr_binding_def_span_in_body(db, body, expr_id)?;
                let ty = typed_body.expr_ty(db, expr_id);
                let binding = typed_body.expr_binding(expr_id)?;

                Some(Target::Local {
                    span: def_span,
                    ty,
                    body,
                    binding,
                })
            }
            BodyPathContext::PatBinding(pat_id) => {
                // Pattern binding definition site (e.g., `x` in `let x = ...`)
                // If the type checker recorded this as a binding, it's a local variable.
                // Otherwise it's something else (enum variant, etc.) - return None.
                let binding = typed_body.pat_binding(pat_id)?;
                let ty = typed_body.pat_ty(db, pat_id);
                let def_span = pat_id.span(body).into();

                Some(Target::Local {
                    span: def_span,
                    ty,
                    body,
                    binding,
                })
            }
            BodyPathContext::PatReference(_) => {
                // Explicit pattern reference (e.g., enum variant in match) - not a local binding
                None
            }
        }
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

        // Get the typed body (works for functions, contract init, recv arms)
        let Some(typed_body) = typed_body_for_body(db, self.body) else {
            return TargetResolution::None;
        };

        // Get the type of the receiver expression.
        let receiver_ty = typed_body.expr_ty(db, *receiver);
        if receiver_ty.has_invalid(db) {
            return TargetResolution::None;
        }
        let receiver_ty = receiver_ty
            .as_capability(db)
            .map_or(receiver_ty, |(_, inner)| inner);

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
        // Get the typed body (works for functions, contract init, recv arms)
        let Some(typed_body) = typed_body_for_body(db, self.body) else {
            return TargetResolution::None;
        };

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

        // Start from the use's scope (the module containing the use statement)
        let mut current_scope = self.use_item.scope();

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

            if let Some(res) = bucket.iter_ok().next()
                && let Some(scope) = res.scope()
            {
                current_scope = scope;
                continue;
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

    /// Resolve a use path segment at a specific cursor position.
    ///
    /// This enables clicking on individual segments within a use statement.
    /// For `use foo::bar::Baz`, clicking on "foo" resolves to foo's definition.
    pub fn target_at<DB>(&self, db: &'db DB, cursor: TextSize) -> TargetResolution<'db>
    where
        DB: HirAnalysisDb + SpannedHirDb,
    {
        // Find which segment the cursor is in
        for idx in 0..=self.segment {
            let Some(seg_span) = self.span.clone().segment(idx).resolve(db) else {
                continue;
            };

            if seg_span.range.contains(cursor) {
                // Create a view for just this segment and resolve it
                let seg_view = UsePathView {
                    use_item: self.use_item,
                    segment: idx,
                    span: self.span.clone(),
                };
                return seg_view.target(db);
            }
        }

        // Cursor not in any segment, use default resolution
        self.target(db)
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
    pub fn target<DB>(&self, db: &'db DB) -> TargetResolution<'db>
    where
        DB: HirAnalysisDb + SpannedHirDb,
    {
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
            Self::UsePath(v) => v.target_at(db, cursor),
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

    /// Check if this reference is a `Self` type path.
    ///
    /// `Self` paths should not be renamed when renaming the type they refer to,
    /// because `Self` is a keyword that contextually refers to the enclosing type.
    /// This includes both explicit `Self` in code and implicit self-parameter types.
    pub fn is_self_ty_path(&self, db: &'db dyn crate::HirDb) -> bool {
        match self {
            Self::Path(v) => v.path.is_self_ty(db),
            _ => false,
        }
    }
}
