//! Reference resolution infrastructure.
//!
//! Pre-resolves symbolic references to their target scopes and caches
//! the results via salsa queries. This enables efficient find-references
//! by filtering pre-resolved targets instead of doing live path resolution
//! on every request.
//!
//! Each item type has a corresponding salsa query that wraps the collector
//! output with scope resolution. The results are shared across multiple
//! find-references calls on different targets within the same module.

use crate::{
    analysis::HirAnalysisDb,
    hir_def::{
        Body, Contract, Enum, Func, Impl, ImplTrait, ItemKind, Struct, Trait, TypeAlias, Use,
        scope_graph::ScopeId,
    },
    span::DynLazySpan,
};

use crate::analysis::ty::ty_check::LocalBinding;

use super::{
    BodyPathContext, PathView, ReferenceView, Target,
    collector::{
        body_references, contract_references, enum_references, func_signature_references,
        impl_references, impl_trait_references, struct_references, trait_references,
        type_alias_references, use_references,
    },
    resolve_path_with_recv_fallback, typed_body_for_body,
};

/// A pre-resolved scope reference: target scope paired with the span
/// of the matched portion. Cached per item via salsa queries.
#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct ResolvedScopeTarget<'db> {
    pub scope: ScopeId<'db>,
    pub span: DynLazySpan<'db>,
    pub is_self_ty: bool,
}

/// Empty resolved targets for item types with no references.
static EMPTY_RESOLVED: &[ResolvedScopeTarget<'static>] = &[];

/// Check if a path reference is a `let`-bound local variable.
///
/// Only `Local` bindings (from `let`) are problematic — they aren't in
/// the scope graph, so `resolve_path` walks up and may find a module-level
/// item with the same name. `Param` and `EffectParam` bindings ARE in the
/// scope graph (parameters and `uses` storage refs), so scope resolution
/// finds the correct target for those.
fn is_let_local_binding<'db>(db: &'db dyn HirAnalysisDb, pv: &PathView<'db>) -> bool {
    let body_ctx = match pv.body_ctx {
        Some(ctx) => ctx,
        None => return false,
    };
    let body = match pv.scope.body() {
        Some(b) => b,
        None => return false,
    };
    let typed_body = match typed_body_for_body(db, body) {
        Some(tb) => tb,
        None => return false,
    };
    match body_ctx {
        BodyPathContext::Expr(expr_id) => {
            matches!(
                typed_body.expr_binding(expr_id),
                Some(LocalBinding::Local { .. })
            )
        }
        BodyPathContext::PatBinding(pat_id) => {
            matches!(
                typed_body.pat_binding(pat_id),
                Some(LocalBinding::Local { .. })
            )
        }
        BodyPathContext::PatReference(_) => false,
    }
}

/// Resolve all references in a slice to their target scopes.
///
/// For path references, resolves each segment independently so that
/// `MyEnum::Variant` produces entries for both `MyEnum` and `Variant`.
/// For field access, method calls, and use paths, resolves via type
/// inference or name resolution.
fn resolve_references<'db>(
    db: &'db dyn HirAnalysisDb,
    refs: &[ReferenceView<'db>],
) -> Vec<ResolvedScopeTarget<'db>> {
    let mut results = Vec::new();
    for reference in refs {
        match reference {
            ReferenceView::Path(pv) => {
                // Skip let-bound locals — they aren't in the scope graph,
                // so resolve_path would walk up and falsely match a
                // module-level item with the same name.
                if is_let_local_binding(db, pv) {
                    continue;
                }
                let last_idx = pv.path.segment_index(db);
                for idx in 0..=last_idx {
                    if let Some(seg_path) = pv.path.segment(db, idx) {
                        let is_self_ty = seg_path.is_self_ty(db);
                        for scope in resolve_path_with_recv_fallback(db, seg_path, pv.scope) {
                            results.push(ResolvedScopeTarget {
                                scope,
                                span: pv.span.clone().segment(idx).ident().into(),
                                is_self_ty,
                            });
                        }
                    }
                }
            }
            ReferenceView::FieldAccess(fv) => {
                let resolution = fv.target(db);
                for t in resolution.as_slice() {
                    if let Target::Scope(scope) = t {
                        results.push(ResolvedScopeTarget {
                            scope: *scope,
                            span: fv.span(),
                            is_self_ty: false,
                        });
                    }
                }
            }
            ReferenceView::MethodCall(mv) => {
                let resolution = mv.target(db);
                for t in resolution.as_slice() {
                    if let Target::Scope(scope) = t {
                        results.push(ResolvedScopeTarget {
                            scope: *scope,
                            span: mv.span(),
                            is_self_ty: false,
                        });
                    }
                }
            }
            ReferenceView::UsePath(uv) => {
                let resolution = uv.target(db);
                for t in resolution.as_slice() {
                    if let Target::Scope(scope) = t {
                        results.push(ResolvedScopeTarget {
                            scope: *scope,
                            span: uv.span(),
                            is_self_ty: false,
                        });
                    }
                }
            }
        }
    }
    results
}

// --- Per-type salsa queries ---
//
// Each wraps the corresponding collector query with scope resolution.
// Salsa caches the result per item, so repeated find-references calls
// on different targets share the cached resolution work.

#[salsa::tracked(return_ref)]
pub fn resolved_body_scope_targets<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
) -> Vec<ResolvedScopeTarget<'db>> {
    resolve_references(db, body_references(db, body))
}

#[salsa::tracked(return_ref)]
pub fn resolved_func_scope_targets<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Vec<ResolvedScopeTarget<'db>> {
    resolve_references(db, func_signature_references(db, func))
}

#[salsa::tracked(return_ref)]
pub fn resolved_struct_scope_targets<'db>(
    db: &'db dyn HirAnalysisDb,
    struct_: Struct<'db>,
) -> Vec<ResolvedScopeTarget<'db>> {
    resolve_references(db, struct_references(db, struct_))
}

#[salsa::tracked(return_ref)]
pub fn resolved_enum_scope_targets<'db>(
    db: &'db dyn HirAnalysisDb,
    enum_: Enum<'db>,
) -> Vec<ResolvedScopeTarget<'db>> {
    resolve_references(db, enum_references(db, enum_))
}

#[salsa::tracked(return_ref)]
pub fn resolved_type_alias_scope_targets<'db>(
    db: &'db dyn HirAnalysisDb,
    alias: TypeAlias<'db>,
) -> Vec<ResolvedScopeTarget<'db>> {
    resolve_references(db, type_alias_references(db, alias))
}

#[salsa::tracked(return_ref)]
pub fn resolved_impl_scope_targets<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_: Impl<'db>,
) -> Vec<ResolvedScopeTarget<'db>> {
    resolve_references(db, impl_references(db, impl_))
}

#[salsa::tracked(return_ref)]
pub fn resolved_trait_scope_targets<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: Trait<'db>,
) -> Vec<ResolvedScopeTarget<'db>> {
    resolve_references(db, trait_references(db, trait_))
}

#[salsa::tracked(return_ref)]
pub fn resolved_impl_trait_scope_targets<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Vec<ResolvedScopeTarget<'db>> {
    resolve_references(db, impl_trait_references(db, impl_trait))
}

#[salsa::tracked(return_ref)]
pub fn resolved_use_scope_targets<'db>(
    db: &'db dyn HirAnalysisDb,
    use_item: Use<'db>,
) -> Vec<ResolvedScopeTarget<'db>> {
    resolve_references(db, use_references(db, use_item))
}

#[salsa::tracked(return_ref)]
pub fn resolved_contract_scope_targets<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> Vec<ResolvedScopeTarget<'db>> {
    resolve_references(db, contract_references(db, contract))
}

/// Dispatch to the appropriate per-type cached query.
///
/// Returns the pre-resolved scope targets for all references in an item.
/// The slice is salsa-cached and shared across multiple find-references calls.
pub fn resolved_item_scope_targets<'db>(
    db: &'db dyn HirAnalysisDb,
    item: ItemKind<'db>,
) -> &'db [ResolvedScopeTarget<'db>] {
    match item {
        ItemKind::Body(body) => resolved_body_scope_targets(db, body),
        ItemKind::Func(func) => resolved_func_scope_targets(db, func),
        ItemKind::Struct(s) => resolved_struct_scope_targets(db, s),
        ItemKind::Enum(e) => resolved_enum_scope_targets(db, e),
        ItemKind::TypeAlias(a) => resolved_type_alias_scope_targets(db, a),
        ItemKind::Impl(i) => resolved_impl_scope_targets(db, i),
        ItemKind::Trait(t) => resolved_trait_scope_targets(db, t),
        ItemKind::ImplTrait(it) => resolved_impl_trait_scope_targets(db, it),
        ItemKind::Use(u) => resolved_use_scope_targets(db, u),
        ItemKind::Contract(c) => resolved_contract_scope_targets(db, c),
        ItemKind::Const(c) => {
            if let Some(body) = c.body(db).to_opt() {
                resolved_body_scope_targets(db, body)
            } else {
                EMPTY_RESOLVED
            }
        }
        ItemKind::TopMod(_) | ItemKind::Mod(_) => EMPTY_RESOLVED,
    }
}
