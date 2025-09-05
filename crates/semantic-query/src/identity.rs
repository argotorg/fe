use hir::hir_def::{scope_graph::ScopeId, ItemKind, TopLevelMod};
use hir::source_index::OccurrencePayload;

use hir_analysis::diagnostics::SpannedHirAnalysisDb;
use hir_analysis::name_resolution::{resolve_with_policy, DomainPreference, NameDomain, NameResKind, PathRes};
use hir_analysis::ty::{
    func_def::FuncDef,
    trait_resolution::PredicateListId,
    ty_check::{RecordLike, check_func_body, BindingKey},
};

/// Analysis-side identity for a single occurrence. Mirrors `SymbolKey` mapping
/// without pulling semantic-query’s public type into analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OccTarget<'db> {
    Scope(ScopeId<'db>),
    EnumVariant(hir::hir_def::EnumVariant<'db>),
    FuncParam(hir::hir_def::ItemKind<'db>, u16),
    Method(FuncDef<'db>),
    Local(hir::hir_def::item::Func<'db>, BindingKey<'db>),
}

pub fn occurrence_symbol_target<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    occ: &OccurrencePayload<'db>,
) -> Option<OccTarget<'db>> {
    match *occ {
        OccurrencePayload::ItemHeaderName { scope, .. } => {
            match scope {
                ScopeId::Item(ItemKind::Func(f)) => {
                    if let Some(fd) = hir_analysis::ty::func_def::lower_func(db, f) {
                        if fd.is_method(db) { return Some(OccTarget::Method(fd)); }
                    }
                    Some(OccTarget::Scope(scope))
                }
                ScopeId::FuncParam(item, idx) => Some(OccTarget::FuncParam(item, idx)),
                ScopeId::Variant(v) => Some(OccTarget::EnumVariant(v)),
                other => Some(OccTarget::Scope(other)),
            }
        }
        OccurrencePayload::MethodName { scope, body, ident, receiver, .. } => {
            let func = crate::util::enclosing_func(db, body.scope())?;
            crate::util::resolve_method_call(db, func, receiver, ident, scope).map(OccTarget::Method)
        }
        OccurrencePayload::PathExprSeg { scope, body, expr, path, seg_idx, .. } => {
            let func = crate::util::enclosing_func(db, body.scope())?;
            if let Some(bkey) = hir_analysis::ty::ty_check::expr_binding_key_for_expr(db, func, expr) {
                return Some(match bkey {
                    BindingKey::FuncParam(f, idx) => OccTarget::FuncParam(ItemKind::Func(f), idx),
                    other => OccTarget::Local(func, other),
                });
            }
            let seg_path = path.segment(db, seg_idx).unwrap_or(path);
            if let Ok(res) = resolve_with_policy(db, seg_path, scope, PredicateListId::empty_list(db), DomainPreference::Either) {
                return match res {
                    PathRes::Ty(_) | PathRes::Func(_) | PathRes::Const(_) | PathRes::TyAlias(..) | PathRes::Trait(_) | PathRes::Mod(_) =>
                        res.as_scope(db).map(OccTarget::Scope),
                    PathRes::EnumVariant(v) => Some(OccTarget::EnumVariant(v.variant)),
                    PathRes::FuncParam(item, idx) => Some(OccTarget::FuncParam(item, idx)),
                    PathRes::Method(..) => hir_analysis::name_resolution::method_func_def_from_res(&res).map(OccTarget::Method),
                };
            }
            None
        }
        OccurrencePayload::PathPatSeg { body, pat, .. } => {
            let func = crate::util::enclosing_func(db, body.scope())?;
            Some(OccTarget::Local(func, BindingKey::LocalPat(pat)))
        }
        OccurrencePayload::FieldAccessName { body, ident, receiver, .. } => {
            let func = crate::util::enclosing_func(db, body.scope())?;
            let (_diags, typed) = check_func_body(db, func).clone();
            let recv_ty = typed.expr_prop(db, receiver).ty;
            RecordLike::from_ty(recv_ty).record_field_scope(db, ident).map(OccTarget::Scope)
        }
        OccurrencePayload::PatternLabelName { scope, ident, constructor_path, .. } => {
            let Some(p) = constructor_path else { return None };
            let res = resolve_with_policy(db, p, scope, PredicateListId::empty_list(db), DomainPreference::Either).ok()?;
            use hir_analysis::name_resolution::PathRes;
            let target = match res {
                PathRes::EnumVariant(v) => RecordLike::from_variant(v).record_field_scope(db, ident),
                PathRes::Ty(ty) => RecordLike::from_ty(ty).record_field_scope(db, ident),
                PathRes::TyAlias(_, ty) => RecordLike::from_ty(ty).record_field_scope(db, ident),
                _ => None,
            }?;
            Some(OccTarget::Scope(target))
        }
        OccurrencePayload::UseAliasName { scope, ident, .. } => {
            let sc = imported_scope_for_use_alias(db, top_mod, scope, ident)?;
            Some(OccTarget::Scope(sc))
        }
        OccurrencePayload::UsePathSeg { scope, path, seg_idx, .. } => {
            if seg_idx + 1 != path.segment_len(db) { return None; }
            let sc = imported_scope_for_use_path_tail(db, top_mod, scope, path)?;
            Some(OccTarget::Scope(sc))
        }
        OccurrencePayload::PathSeg { scope, path, seg_idx, .. } => {
            let seg_path = path.segment(db, seg_idx).unwrap_or(path);
            let res = resolve_with_policy(db, seg_path, scope, PredicateListId::empty_list(db), DomainPreference::Either).ok()?;
            match res {
                PathRes::Ty(_) | PathRes::Func(_) | PathRes::Const(_) | PathRes::TyAlias(..) | PathRes::Trait(_) | PathRes::Mod(_) =>
                    res.as_scope(db).map(OccTarget::Scope),
                PathRes::EnumVariant(v) => Some(OccTarget::EnumVariant(v.variant)),
                PathRes::FuncParam(item, idx) => Some(OccTarget::FuncParam(item, idx)),
                PathRes::Method(..) => hir_analysis::name_resolution::method_func_def_from_res(&res).map(OccTarget::Method),
            }
        }
    }
}

pub fn imported_scope_for_use_alias<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    scope: ScopeId<'db>,
    ident: hir::hir_def::IdentId<'db>,
) -> Option<ScopeId<'db>> {
    let ing = top_mod.ingot(db);
    let (_diags, imports) = hir_analysis::name_resolution::resolve_imports(db, ing);
    let named = imports.named_resolved.iter().find_map(|(k,v)| if *k == scope { Some(v) } else { None })?;
    let bucket = named.get(&ident)?;
    let nr = bucket.pick_any(&[NameDomain::TYPE, NameDomain::VALUE]).as_ref().ok()?;
    match nr.kind { NameResKind::Scope(sc) => Some(sc), _ => None }
}

pub fn imported_scope_for_use_path_tail<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    scope: ScopeId<'db>,
    path: hir::hir_def::UsePathId<'db>,
) -> Option<ScopeId<'db>> {
    let ident = path.last_ident(db)?;
    imported_scope_for_use_alias(db, top_mod, scope, ident)
}
