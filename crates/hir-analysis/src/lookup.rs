use hir::SpannedHirDb;
use hir::hir_def::{TopLevelMod, scope_graph::ScopeId, ItemKind, PathId};
use parser::TextSize;

use crate::{HirAnalysisDb, diagnostics::SpannedHirAnalysisDb};
use crate::name_resolution::{resolve_with_policy, DomainPreference, PathRes};
use crate::ty::{trait_resolution::PredicateListId, func_def::FuncDef};

/// Generic semantic identity at a source offset.
/// This is compiler-facing and independent of any IDE layer types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolIdentity<'db> {
    Scope(hir::hir_def::scope_graph::ScopeId<'db>),
    EnumVariant(hir::hir_def::EnumVariant<'db>),
    FuncParam(hir::hir_def::ItemKind<'db>, u16),
    Method(FuncDef<'db>),
    Local(hir::hir_def::item::Func<'db>, crate::ty::ty_check::BindingKey<'db>),
}

fn enclosing_func<'db>(db: &'db dyn SpannedHirDb, mut scope: ScopeId<'db>) -> Option<hir::hir_def::item::Func<'db>> {
    for _ in 0..16 {
        if let Some(item) = scope.to_item() {
            if let ItemKind::Func(f) = item { return Some(f); }
        }
        scope = scope.parent(db)?;
    }
    None
}

fn map_path_res<'db>(db: &'db dyn HirAnalysisDb, res: PathRes<'db>) -> Option<SymbolIdentity<'db>> {
    match res {
        PathRes::EnumVariant(v) => Some(SymbolIdentity::EnumVariant(v.variant)),
        PathRes::FuncParam(item, idx) => Some(SymbolIdentity::FuncParam(item, idx)),
        PathRes::Method(..) => crate::name_resolution::method_func_def_from_res(&res).map(SymbolIdentity::Method),
        _ => res.as_scope(db).map(SymbolIdentity::Scope),
    }
}

/// Resolve the semantic identity (definition-level target) at a given source offset.
/// Uses half-open span policy in the HIR occurrence index.
pub fn identity_at_offset<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    offset: TextSize,
) -> Option<SymbolIdentity<'db>> {
    use hir::source_index::{occurrences_at_offset, OccurrencePayload as OP};

    // Get the most specific occurrence at this offset and map it to a symbol identity
    let occs = occurrences_at_offset(db, top_mod, offset);
    
    
    // Prefer contextual occurrences (PathExprSeg/PathPatSeg) over generic ones
    let best_occ = occs.iter().min_by_key(|o| match o {
        OP::PathExprSeg{..} | OP::PathPatSeg{..} => 0u8,
        _ => 1u8,
    });
    
    if let Some(occ) = best_occ {
        match occ {
            // Handle local variables first - PathExprSeg in function context
            OP::PathExprSeg { body, expr, scope, path, seg_idx, .. } => {
                if let Some(func) = enclosing_func(db, body.scope()) {
                    if let Some(bkey) = crate::ty::ty_check::expr_binding_key_for_expr(db, func, *expr) {
                        return Some(SymbolIdentity::Local(func, bkey));
                    }
                }
                // Fall back to path resolution for non-local paths
                let seg_path: PathId<'db> = path.segment(db, *seg_idx).unwrap_or(*path);
                if let Ok(res) = resolve_with_policy(db, seg_path, *scope, PredicateListId::empty_list(db), DomainPreference::Either) {
                    if let Some(k) = map_path_res(db, res) { return Some(k); }
                }
            }
            OP::PathPatSeg { scope, path, seg_idx, .. } => {
                let seg_path: PathId<'db> = path.segment(db, *seg_idx).unwrap_or(*path);
                if let Ok(res) = resolve_with_policy(db, seg_path, *scope, PredicateListId::empty_list(db), DomainPreference::Either) {
                    if let Some(k) = map_path_res(db, res) { return Some(k); }
                }
            }
            OP::UseAliasName { scope, ident, .. } => {
                let ing = top_mod.ingot(db);
                let (_d, imports) = crate::name_resolution::resolve_imports(db, ing);
                if let Some(named) = imports.named_resolved.get(scope) {
                    if let Some(bucket) = named.get(ident) {
                        if let Ok(nr) = bucket.pick_any(&[crate::name_resolution::NameDomain::TYPE, crate::name_resolution::NameDomain::VALUE]).as_ref() {
                            if let crate::name_resolution::NameResKind::Scope(sc) = nr.kind { return Some(SymbolIdentity::Scope(sc)); }
                        }
                    }
                }
            }
            OP::PathSeg { scope, path, seg_idx, .. } => {
                let seg_path: PathId<'db> = path.segment(db, *seg_idx).unwrap_or(*path);
                if let Ok(res) = resolve_with_policy(db, seg_path, *scope, PredicateListId::empty_list(db), DomainPreference::Either) {
                    if let Some(k) = map_path_res(db, res) { return Some(k); }
                }
            }
            OP::UsePathSeg { scope, path, seg_idx, .. } => {
                let last = path.segment_len(db) - 1;
                if *seg_idx == last {
                    if let Some(seg) = path.data(db).get(*seg_idx).and_then(|p| p.to_opt()) {
                        if let hir::hir_def::UsePathSegment::Ident(ident) = seg {
                            let ing = top_mod.ingot(db);
                            let (_d, imports) = crate::name_resolution::resolve_imports(db, ing);
                            if let Some(named) = imports.named_resolved.get(scope) {
                                if let Some(bucket) = named.get(&ident) {
                                    if let Ok(nr) = bucket.pick_any(&[crate::name_resolution::NameDomain::TYPE, crate::name_resolution::NameDomain::VALUE]).as_ref() {
                                        if let crate::name_resolution::NameResKind::Scope(sc) = nr.kind { return Some(SymbolIdentity::Scope(sc)); }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            OP::MethodName { scope, receiver, ident, body, .. } => {
                if let Some(func) = enclosing_func(db, body.scope()) {
                    use crate::ty::{ty_check::check_func_body, canonical::Canonical};
                    let (_diags, typed) = check_func_body(db, func).clone();
                    let recv_ty = typed.expr_prop(db, *receiver).ty;
                    let assumptions = PredicateListId::empty_list(db);
                    if let Some(fd) = crate::name_resolution::find_method_id(db, Canonical::new(db, recv_ty), *ident, *scope, assumptions) {
                        return Some(SymbolIdentity::Method(fd));
                    }
                }
            }
            OP::FieldAccessName { body, ident, receiver, .. } => {
                if let Some(func) = enclosing_func(db, body.scope()) {
                    let (_d, typed) = crate::ty::ty_check::check_func_body(db, func).clone();
                    let recv_ty = typed.expr_prop(db, *receiver).ty;
                    if let Some(sc) = crate::ty::ty_check::RecordLike::from_ty(recv_ty).record_field_scope(db, *ident) {
                        return Some(SymbolIdentity::Scope(sc));
                    }
                }
            }
            OP::PatternLabelName { scope, ident, constructor_path, .. } => {
                if let Some(p) = constructor_path {
                    if let Ok(res) = resolve_with_policy(db, *p, *scope, PredicateListId::empty_list(db), DomainPreference::Either) {
                        use crate::name_resolution::PathRes as PR;
                        let sc = match res {
                            PR::EnumVariant(v) => crate::ty::ty_check::RecordLike::from_variant(v).record_field_scope(db, *ident),
                            PR::Ty(ty) => crate::ty::ty_check::RecordLike::from_ty(ty).record_field_scope(db, *ident),
                            PR::TyAlias(_, ty) => crate::ty::ty_check::RecordLike::from_ty(ty).record_field_scope(db, *ident),
                            _ => None,
                        };
                        if let Some(sc) = sc { return Some(SymbolIdentity::Scope(sc)); }
                    }
                }
            }
            OP::ItemHeaderName { scope, .. } => return Some(SymbolIdentity::Scope(*scope)),
        }
    }
    // No module-wide brute force; rely on occurrence presence + indexed local lookup.
    None
}
