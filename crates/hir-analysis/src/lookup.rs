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

/// Resolve the semantic identity for a given occurrence payload.
/// This is the single source of truth for occurrence interpretation.
/// Returns multiple identities for ambiguous cases (e.g., ambiguous imports).
pub fn identity_for_occurrence<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    occ: &hir::source_index::OccurrencePayload<'db>,
) -> Vec<SymbolIdentity<'db>> {
    use hir::source_index::OccurrencePayload as OP;
    
    match *occ {
        OP::ItemHeaderName { scope, .. } => match scope {
            hir::hir_def::scope_graph::ScopeId::Item(ItemKind::Func(f)) => {
                if let Some(fd) = crate::ty::func_def::lower_func(db, f) {
                    if fd.is_method(db) {
                        return vec![SymbolIdentity::Method(fd)];
                    }
                }
                vec![SymbolIdentity::Scope(scope)]
            }
            hir::hir_def::scope_graph::ScopeId::FuncParam(item, idx) => vec![SymbolIdentity::FuncParam(item, idx)],
            hir::hir_def::scope_graph::ScopeId::Variant(v) => vec![SymbolIdentity::EnumVariant(v)],
            other => vec![SymbolIdentity::Scope(other)],
        },
        OP::MethodName { scope, receiver, ident, body, .. } => {
            if let Some(func) = enclosing_func(db, body.scope()) {
                use crate::ty::{ty_check::check_func_body, canonical::Canonical};
                use crate::name_resolution::method_selection::{select_method_candidate, MethodSelectionError};
                
                let (_diags, typed) = check_func_body(db, func).clone();
                let recv_ty = typed.expr_prop(db, receiver).ty;
                let assumptions = PredicateListId::empty_list(db);
                
                match select_method_candidate(db, Canonical::new(db, recv_ty), ident, scope, assumptions) {
                    Ok(cand) => {
                        use crate::name_resolution::method_selection::MethodCandidate;
                        let fd = match cand {
                            MethodCandidate::InherentMethod(fd) => fd,
                            MethodCandidate::TraitMethod(tm) | MethodCandidate::NeedsConfirmation(tm) => tm.method.0,
                        };
                        vec![SymbolIdentity::Method(fd)]
                    }
                    Err(MethodSelectionError::AmbiguousInherentMethod(methods)) => {
                        methods.iter().map(|fd| SymbolIdentity::Method(*fd)).collect()
                    }
                    Err(MethodSelectionError::AmbiguousTraitMethod(traits)) => {
                        traits.iter().filter_map(|trait_def| {
                            trait_def.methods(db).get(&ident)
                                .map(|tm| SymbolIdentity::Method(tm.0))
                        }).collect()
                    }
                    Err(_) => vec![]
                }
            } else {
                vec![]
            }
        }
        OP::PathExprSeg { body, expr, scope, path, seg_idx, .. } => {
            if let Some(func) = enclosing_func(db, body.scope()) {
                if let Some(bkey) = crate::ty::ty_check::expr_binding_key_for_expr(db, func, expr) {
                    return vec![match bkey {
                        crate::ty::ty_check::BindingKey::FuncParam(f, idx) => SymbolIdentity::FuncParam(ItemKind::Func(f), idx),
                        other => SymbolIdentity::Local(func, other),
                    }];
                }
            }
            let seg_path: PathId<'db> = path.segment(db, seg_idx).unwrap_or(path);
            if let Ok(res) = resolve_with_policy(db, seg_path, scope, PredicateListId::empty_list(db), DomainPreference::Either) {
                if let Some(identity) = map_path_res(db, res) {
                    vec![identity]
                } else {
                    vec![]
                }
            } else {
                // This is where the key insight comes: if resolve_with_policy fails,
                // it might be due to ambiguous imports. Let's check for that case.
                find_ambiguous_candidates_for_path_seg(db, top_mod, scope, path, seg_idx)
            }
        }
        OP::PathPatSeg { body, pat, .. } => {
            if let Some(func) = enclosing_func(db, body.scope()) {
                vec![SymbolIdentity::Local(func, crate::ty::ty_check::BindingKey::LocalPat(pat))]
            } else {
                vec![]
            }
        }
        OP::FieldAccessName { body, ident, receiver, .. } => {
            if let Some(func) = enclosing_func(db, body.scope()) {
                let (_d, typed) = crate::ty::ty_check::check_func_body(db, func).clone();
                let recv_ty = typed.expr_prop(db, receiver).ty;
                if let Some(sc) = crate::ty::ty_check::RecordLike::from_ty(recv_ty).record_field_scope(db, ident) {
                    return vec![SymbolIdentity::Scope(sc)];
                }
            }
            vec![]
        }
        OP::PatternLabelName { scope, ident, constructor_path, .. } => {
            if let Some(p) = constructor_path {
                if let Ok(res) = resolve_with_policy(db, p, scope, PredicateListId::empty_list(db), DomainPreference::Either) {
                    use crate::name_resolution::PathRes as PR;
                    let target = match res {
                        PR::EnumVariant(v) => crate::ty::ty_check::RecordLike::from_variant(v).record_field_scope(db, ident),
                        PR::Ty(ty) => crate::ty::ty_check::RecordLike::from_ty(ty).record_field_scope(db, ident),
                        PR::TyAlias(_, ty) => crate::ty::ty_check::RecordLike::from_ty(ty).record_field_scope(db, ident),
                        _ => None,
                    };
                    if let Some(target) = target {
                        return vec![SymbolIdentity::Scope(target)];
                    }
                }
            }
            vec![]
        }
        OP::UseAliasName { scope, ident, .. } => {
            let ing = top_mod.ingot(db);
            let (_d, imports) = crate::name_resolution::resolve_imports(db, ing);
            if let Some(named) = imports.named_resolved.get(&scope) {
                if let Some(bucket) = named.get(&ident) {
                    if let Ok(nr) = bucket.pick_any(&[crate::name_resolution::NameDomain::TYPE, crate::name_resolution::NameDomain::VALUE]).as_ref() {
                        if let crate::name_resolution::NameResKind::Scope(sc) = nr.kind {
                            return vec![SymbolIdentity::Scope(sc)];
                        }
                    }
                }
            }
            vec![]
        }
        OP::UsePathSeg { scope, path, seg_idx, .. } => {
            if seg_idx + 1 != path.segment_len(db) {
                return vec![];
            }
            if let Some(seg) = path.data(db).get(seg_idx).and_then(|p| p.to_opt()) {
                if let hir::hir_def::UsePathSegment::Ident(ident) = seg {
                    let ing = top_mod.ingot(db);
                    let (_d, imports) = crate::name_resolution::resolve_imports(db, ing);
                    if let Some(named) = imports.named_resolved.get(&scope) {
                        if let Some(bucket) = named.get(&ident) {
                            if let Ok(nr) = bucket.pick_any(&[crate::name_resolution::NameDomain::TYPE, crate::name_resolution::NameDomain::VALUE]).as_ref() {
                                if let crate::name_resolution::NameResKind::Scope(sc) = nr.kind {
                                    return vec![SymbolIdentity::Scope(sc)];
                                }
                            }
                        }
                    }
                }
            }
            vec![]
        }
        OP::PathSeg { scope, path, seg_idx, .. } => {
            let seg_path: PathId<'db> = path.segment(db, seg_idx).unwrap_or(path);
            if let Ok(res) = resolve_with_policy(db, seg_path, scope, PredicateListId::empty_list(db), DomainPreference::Either) {
                if let Some(identity) = map_path_res(db, res) {
                    vec![identity]
                } else {
                    vec![]
                }
            } else {
                // For regular PathSeg, also check for ambiguous imports
                find_ambiguous_candidates_for_path_seg(db, top_mod, scope, path, seg_idx)
            }
        }
    }
}

/// Find multiple candidates for ambiguous import cases
fn find_ambiguous_candidates_for_path_seg<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    scope: ScopeId<'db>,
    path: PathId<'db>,
    seg_idx: usize,
) -> Vec<SymbolIdentity<'db>> {
    use crate::name_resolution::NameDomain;
    
    // Get the identifier from the path segment
    let seg_path = path.segment(db, seg_idx).unwrap_or(path);
    let Some(ident) = seg_path.as_ident(db) else {
        return vec![];
    };
    
    // Check imports for this scope - walk up the scope hierarchy to find where imports are resolved
    let ing = top_mod.ingot(db);
    let (_diags, imports) = crate::name_resolution::resolve_imports(db, ing);
    
    // Try current scope first, then walk up the hierarchy
    let mut current_scope = Some(scope);
    let (_import_scope, named) = loop {
        let Some(sc) = current_scope else {
            return vec![];
        };
        
        if let Some(named) = imports.named_resolved.get(&sc) {
            break (sc, named);
        }
        
        // Walk up to parent scope
        current_scope = sc.parent(db);
    };
    let Some(bucket) = named.get(&ident) else {
        return vec![];
    };
    
    let mut candidates = Vec::new();
    
    // Check both TYPE and VALUE domains for multiple resolutions
    for domain in [NameDomain::TYPE, NameDomain::VALUE] {
        match bucket.pick(domain) {
            Ok(name_res) => {
                if let crate::name_resolution::NameResKind::Scope(sc) = name_res.kind {
                    candidates.push(SymbolIdentity::Scope(sc));
                }
            }
            Err(crate::name_resolution::NameResolutionError::Ambiguous(ambiguous_candidates)) => {
                // This is exactly what we want for ambiguous imports!
                for name_res in ambiguous_candidates {
                    if let crate::name_resolution::NameResKind::Scope(sc) = name_res.kind {
                        candidates.push(SymbolIdentity::Scope(sc));
                    }
                }
            }
            Err(_) => {
                // Other errors (like NotFound) are ignored
            }
        }
    }
    
    candidates
}

/// Resolve the semantic identity (definition-level target) at a given source offset.
/// Uses half-open span policy in the HIR occurrence index.
/// Returns the first identity found, or None if no identities are found.
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
        identity_for_occurrence(db, top_mod, occ).into_iter().next()
    } else {
        None
    }
}
