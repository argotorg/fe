//! Contract-specific type checking functions.
//!
//! This module contains functions for checking contract init bodies,
//! recv blocks, and recv arm bodies.
use rustc_hash::{FxHashMap, FxHashSet};

use super::{EffectParamSite, LocalBinding, TypedBody, owner::BodyOwner};

use num_traits::ToPrimitive;

use crate::{
    analysis::{
        HirAnalysisDb,
        name_resolution::{ExpectedPathKind, PathRes, diagnostics::PathResDiag, resolve_path},
        semantic::{SemConstScalar, SemConstValue, eval_body_owner_const},
        ty::{
            adt_def::AdtRef,
            canonical::Canonical,
            corelib::resolve_core_trait,
            diagnostics::{BodyDiag, FuncBodyDiag, TraitConstraintDiag, TyDiagCollection},
            provider::{ProviderAddressSpace, address_space_from_ty},
            trait_def::TraitInstId,
            trait_def::impls_for_ty,
            trait_resolution::{
                GoalSatisfiability, PredicateListId, TraitSolveCx, is_goal_satisfiable,
            },
            ty_check::check_body,
            ty_def::{PrimTy, TyBase, TyData, TyId},
        },
    },
    hir_def::{
        Body, Cond, Contract, Expr, ExprId, FieldParent, IdentId, ItemKind, Mod, Partial, PathId,
        Stmt, Struct, scope_graph::ScopeId,
    },
    semantic::{EffectEnvView, FieldView, ProviderSource},
    span::{DynLazySpan, path::LazyPathSpan},
};
use common::{indexmap::IndexMap, ingot::IngotKind};

#[allow(clippy::enum_variant_names)]
pub enum VariantResError<'db> {
    /// Path doesn't resolve at all.
    NotFound,
    /// Path resolves to a type that doesn't implement MsgVariant.
    NotMsgVariant(TyId<'db>),
    /// Path resolves to a type that implements MsgVariant but is not a variant
    /// of the specified msg module.
    NotVariantOfMsg(TyId<'db>),
}

/// Returns true if a struct implements the core MsgVariant trait.
fn implements_msg_variant<'db>(db: &'db dyn HirAnalysisDb, struct_: Struct<'db>) -> bool {
    let Some(msg_variant_trait) =
        resolve_core_trait(db, struct_.scope(), &["message", "MsgVariant"])
    else {
        return false;
    };

    let adt_def = AdtRef::from(struct_).as_adt(db);
    let ty = TyId::adt(db, adt_def);
    let canonical_ty = Canonical::new(db, ty);
    let ingot = struct_.top_mod(db).ingot(db);

    impls_for_ty(db, ingot, canonical_ty)
        .iter()
        .any(|impl_| impl_.skip_binder().trait_def(db).eq(&msg_variant_trait))
}

fn resolve_sol_abi_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<TyId<'db>> {
    let ingot = scope.ingot(db);
    let std_root = if ingot.kind(db) == IngotKind::Std {
        IdentId::make_ingot(db)
    } else {
        IdentId::new(db, "std".to_string())
    };

    let sol_path = PathId::from_ident(db, std_root)
        .push_ident(db, IdentId::new(db, "abi".to_string()))
        .push_ident(db, IdentId::new(db, "Sol".to_string()));

    match resolve_path(db, sol_path, scope, assumptions, false).ok()? {
        PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => Some(ty),
        _ => None,
    }
}

#[allow(clippy::too_many_arguments)]
fn check_ty_decodable<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    decode_trait: crate::hir_def::Trait<'db>,
    sol_ty: TyId<'db>,
    ty: TyId<'db>,
    span: DynLazySpan<'db>,
    diags: &mut Vec<FuncBodyDiag<'db>>,
) {
    if ty.has_invalid(db) {
        return;
    }

    if ty.is_tuple(db) {
        for elem in ty.field_types(db) {
            check_ty_decodable(
                db,
                solve_cx,
                decode_trait,
                sol_ty,
                elem,
                span.clone(),
                diags,
            );
        }
        return;
    }

    if ty.has_var(db) {
        return;
    }

    let inst = TraitInstId::new(db, decode_trait, vec![ty, sol_ty], IndexMap::new());
    if let GoalSatisfiability::UnSat(_) = is_goal_satisfiable(db, solve_cx, inst) {
        diags.push(
            TyDiagCollection::from(TraitConstraintDiag::TraitBoundNotSat {
                span,
                primary_goal: inst,
                unsat_subgoal: None,
                required_by: None,
            })
            .into(),
        );
    }
}

fn check_recv_variant_param_types_decodable<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    variant: ResolvedRecvVariant<'db>,
    span: DynLazySpan<'db>,
    assumptions: PredicateListId<'db>,
    diags: &mut Vec<FuncBodyDiag<'db>>,
) {
    let Some(sol_ty) = resolve_sol_abi_ty(db, contract.scope(), assumptions) else {
        return;
    };
    let Some(decode_trait) = resolve_core_trait(db, contract.scope(), &["abi", "Decode"]) else {
        return;
    };
    let solve_cx = TraitSolveCx::new(db, contract.scope()).with_assumptions(assumptions);

    for field_ty in variant.ty.field_types(db) {
        check_ty_decodable(
            db,
            solve_cx,
            decode_trait,
            sol_ty,
            field_ty,
            span.clone(),
            diags,
        );
    }
}

/// Returns all variant structs in a msg module (structs that implement MsgVariant).
fn msg_variants<'db>(
    db: &'db dyn HirAnalysisDb,
    msg_mod: Mod<'db>,
) -> impl Iterator<Item = Struct<'db>> + 'db {
    msg_mod
        .children_non_nested(db)
        .filter_map(|item| match item {
            ItemKind::Struct(s) => Some(s),
            _ => None,
        })
        .filter(move |s| implements_msg_variant(db, *s))
}

/// Resolved msg variant in a recv arm.
#[derive(Debug, Clone, Copy)]
pub struct ResolvedRecvVariant<'db> {
    pub variant_struct: Struct<'db>,
    pub ty: TyId<'db>,
}

/// Resolves a variant path within a msg module.
pub fn resolve_variant_in_msg<'db>(
    db: &'db dyn HirAnalysisDb,
    msg_mod: Mod<'db>,
    variant_path: PathId<'db>,
    assumptions: PredicateListId<'db>,
) -> Result<ResolvedRecvVariant<'db>, VariantResError<'db>> {
    let Ok(PathRes::Ty(ty)) = resolve_path(db, variant_path, msg_mod.scope(), assumptions, false)
    else {
        return Err(VariantResError::NotFound);
    };

    if let Some(adt_def) = ty.adt_def(db)
        && let AdtRef::Struct(struct_) = adt_def.adt_ref(db)
        && implements_msg_variant(db, struct_)
    {
        if let Some(parent) = struct_.scope().parent(db)
            && parent == ScopeId::Item(ItemKind::Mod(msg_mod))
        {
            return Ok(ResolvedRecvVariant {
                variant_struct: struct_,
                ty,
            });
        }
        return Err(VariantResError::NotVariantOfMsg(ty));
    }
    // Resolved to a type but it doesn't implement MsgVariant
    Err(VariantResError::NotMsgVariant(ty))
}

/// Resolves a variant path in a bare recv block (no msg module specified).
/// Paths are resolved from the contract's scope.
pub fn resolve_variant_bare<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    variant_path: PathId<'db>,
    assumptions: PredicateListId<'db>,
) -> Result<ResolvedRecvVariant<'db>, VariantResError<'db>> {
    match resolve_path(db, variant_path, contract.scope(), assumptions, false) {
        Ok(PathRes::Ty(ty)) => {
            if let Some(adt_def) = ty.adt_def(db)
                && let AdtRef::Struct(s) = adt_def.adt_ref(db)
                && implements_msg_variant(db, s)
            {
                return Ok(ResolvedRecvVariant {
                    variant_struct: s,
                    ty,
                });
            }
            // Resolved to a type but it doesn't implement MsgVariant
            Err(VariantResError::NotMsgVariant(ty))
        }
        _ => Err(VariantResError::NotFound),
    }
}

#[salsa::tracked(return_ref)]
pub fn check_contract_recv_block<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    recv_idx: u32,
) -> Vec<FuncBodyDiag<'db>> {
    let mut diags = Vec::new();

    let Some(recv) = contract.recvs(db).data(db).get(recv_idx as usize) else {
        return diags;
    };

    let recv_span = contract.span().recv(recv_idx as usize);
    let path_span = recv_span.clone().path();

    // Check if this is a named recv block (recv MsgType { ... }) or bare (recv { ... })
    if let Some(msg_mod) = resolve_recv_msg_mod(
        db,
        contract,
        recv.msg_path,
        path_span.clone(),
        &mut diags,
        true,
    ) {
        // Named recv block - validate against the specific msg module
        check_named_recv_block(db, contract, recv_idx, msg_mod, &mut diags);
    } else if recv.msg_path.is_none() {
        // Bare recv block - no msg module specified
        check_bare_recv_block(db, contract, recv_idx, &mut diags);
    }
    // If msg_path was Some but didn't resolve, diagnostics were already emitted

    diags
}

/// Check a named recv block (recv MsgType { ... }).
/// All variants must be children of the specified msg module.
fn check_named_recv_block<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    recv_idx: u32,
    msg_mod: Mod<'db>,
    diags: &mut Vec<FuncBodyDiag<'db>>,
) {
    let recv = &contract.recvs(db).data(db)[recv_idx as usize];
    let recv_span = contract.span().recv(recv_idx as usize);
    let assumptions = PredicateListId::empty_list(db);

    // Use TyId for duplicate detection to correctly handle generic types
    let mut seen = FxHashMap::<TyId<'db>, DynLazySpan<'db>>::default();
    // Use Struct for exhaustiveness checking (tracks which base structs are covered)
    let mut covered = FxHashSet::<Struct<'db>>::default();
    let mut checked_decode = FxHashSet::<Struct<'db>>::default();

    // Get msg name for diagnostics
    let Some(msg_name) = msg_mod.name(db).to_opt() else {
        return;
    };

    for (arm_idx, arm) in recv.arms.data(db).iter().enumerate() {
        let arm_span = recv_span.clone().arms().arm(arm_idx);
        let pat_span: DynLazySpan<'db> = arm_span.clone().pat().into();

        if arm.is_fallback(db) {
            diags.push(
                BodyDiag::RecvFallbackOnlyInBareBlock {
                    primary: pat_span.clone(),
                }
                .into(),
            );
            if arm.ret_ty.is_some() {
                diags.push(
                    BodyDiag::RecvFallbackReturnTypeNotAllowed {
                        primary: arm_span.ret_ty().into(),
                    }
                    .into(),
                );
            }
            continue;
        }

        let Some(path) = arm.variant_path(db) else {
            continue;
        };

        match resolve_variant_in_msg(db, msg_mod, path, assumptions) {
            Ok(resolved) => {
                let Some(ident) = resolved.variant_struct.name(db).to_opt() else {
                    continue;
                };

                if let Some(first_span) = seen.get(&resolved.ty) {
                    diags.push(
                        BodyDiag::RecvArmDuplicateVariant {
                            primary: pat_span.clone(),
                            first_use: first_span.clone(),
                            variant: ident,
                        }
                        .into(),
                    );
                } else {
                    seen.insert(resolved.ty, pat_span.clone());
                }

                covered.insert(resolved.variant_struct);
                if checked_decode.insert(resolved.variant_struct) {
                    check_recv_variant_param_types_decodable(
                        db,
                        contract,
                        resolved,
                        pat_span.clone(),
                        assumptions,
                        diags,
                    );
                }
            }
            Err(VariantResError::NotVariantOfMsg(ty)) => {
                // Type implements MsgVariant but is not a child of this msg module
                diags.push(
                    BodyDiag::RecvArmNotVariantOfMsg {
                        primary: pat_span,
                        variant_ty: ty,
                        msg_name,
                    }
                    .into(),
                );
            }
            Err(VariantResError::NotMsgVariant(ty)) => {
                // Type doesn't implement MsgVariant
                diags.push(
                    BodyDiag::RecvArmNotMsgVariantTrait {
                        primary: pat_span,
                        given_ty: ty,
                    }
                    .into(),
                );
            }
            Err(VariantResError::NotFound) => {
                // Path doesn't resolve at all - use the generic error
                diags.push(
                    BodyDiag::RecvArmNotMsgVariant {
                        primary: pat_span,
                        msg_name,
                    }
                    .into(),
                );
            }
        }
    }

    // Check for missing variants (exhaustiveness)
    let missing: Vec<_> = msg_variants(db, msg_mod)
        .filter_map(|variant| {
            if !covered.contains(&variant) {
                variant.name(db).to_opt()
            } else {
                None
            }
        })
        .collect();

    if !missing.is_empty() {
        diags.push(
            BodyDiag::RecvMissingMsgVariants {
                primary: recv_span.clone().path().into(),
                variants: missing,
            }
            .into(),
        );
    }
}

/// Check a bare recv block (recv { ... }).
/// Variants can be any type that implements MsgVariant.
fn check_bare_recv_block<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    recv_idx: u32,
    diags: &mut Vec<FuncBodyDiag<'db>>,
) {
    let recv = &contract.recvs(db).data(db)[recv_idx as usize];
    let recv_span = contract.span().recv(recv_idx as usize);
    let assumptions = PredicateListId::empty_list(db);

    // Use TyId as key to correctly handle generic types like GenericMsg<u8> vs GenericMsg<u16>
    let mut seen = FxHashMap::<TyId<'db>, DynLazySpan<'db>>::default();
    let mut checked_decode = FxHashSet::<Struct<'db>>::default();

    for (arm_idx, arm) in recv.arms.data(db).iter().enumerate() {
        let arm_span = recv_span.clone().arms().arm(arm_idx);
        let pat_span: DynLazySpan<'db> = arm_span.clone().pat().into();

        if arm.is_fallback(db) {
            if arm.ret_ty.is_some() {
                diags.push(
                    BodyDiag::RecvFallbackReturnTypeNotAllowed {
                        primary: arm_span.ret_ty().into(),
                    }
                    .into(),
                );
            }
            continue;
        }

        let Some(path) = arm.variant_path(db) else {
            continue;
        };

        match resolve_variant_bare(db, contract, path, assumptions) {
            Ok(resolved) => {
                let Some(ident) = resolved.variant_struct.name(db).to_opt() else {
                    continue;
                };

                if let Some(first_span) = seen.get(&resolved.ty) {
                    diags.push(
                        BodyDiag::RecvArmDuplicateVariant {
                            primary: pat_span.clone(),
                            first_use: first_span.clone(),
                            variant: ident,
                        }
                        .into(),
                    );
                } else {
                    seen.insert(resolved.ty, pat_span.clone());
                }

                if checked_decode.insert(resolved.variant_struct) {
                    check_recv_variant_param_types_decodable(
                        db,
                        contract,
                        resolved,
                        pat_span.clone(),
                        assumptions,
                        diags,
                    );
                }
            }
            Err(VariantResError::NotMsgVariant(ty)) => {
                // Type doesn't implement MsgVariant
                diags.push(
                    BodyDiag::RecvArmNotMsgVariantTrait {
                        primary: pat_span,
                        given_ty: ty,
                    }
                    .into(),
                );
            }
            Err(VariantResError::NotVariantOfMsg(_)) => {
                // This shouldn't happen in bare recv blocks
                unreachable!("NotVariantOfMsg should not occur in bare recv blocks");
            }
            Err(VariantResError::NotFound) => {
                // Path doesn't resolve - this will be caught by name resolution
                // We don't emit a recv-specific error here
            }
        }
    }

    // No exhaustiveness check for bare recv blocks
}

#[salsa::tracked(return_ref)]
pub fn check_contract_recv_blocks<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let mut diags = Vec::new();
    let mut seen_msg_blocks = FxHashMap::<Mod<'db>, (DynLazySpan<'db>, IdentId<'db>)>::default();
    let mut seen_fallback: Option<DynLazySpan<'db>> = None;

    // Track selectors across ALL recv blocks: selector -> (span, variant_name, struct)
    // We store the struct to correctly identify duplicates - comparing by name alone fails
    // when different msg blocks have variants with the same name but different selectors.
    let mut seen_selectors =
        FxHashMap::<u32, (DynLazySpan<'db>, IdentId<'db>, Struct<'db>)>::default();

    // Track handler types across ALL recv blocks for duplicate detection.
    // We use TyId to handle type aliases correctly.
    let mut seen_handlers = FxHashMap::<TyId<'db>, DynLazySpan<'db>>::default();

    let assumptions = PredicateListId::empty_list(db);

    for (idx, recv) in contract.recvs(db).data(db).iter().enumerate() {
        let recv_span = contract.span().recv(idx);
        let path_span = recv_span.clone().path();

        for (arm_idx, arm) in recv.arms.data(db).iter().enumerate() {
            if !arm.is_fallback(db) {
                continue;
            }

            let pat_span: DynLazySpan<'db> = recv_span.clone().arms().arm(arm_idx).pat().into();
            if let Some(first_use) = &seen_fallback {
                diags.push(
                    BodyDiag::RecvDuplicateFallback {
                        primary: pat_span,
                        first_use: first_use.clone(),
                    }
                    .into(),
                );
            } else {
                seen_fallback = Some(pat_span);
            }
        }

        // Check if this is a named recv block
        if let Some(msg_mod) = resolve_recv_msg_mod(
            db,
            contract,
            recv.msg_path,
            path_span.clone(),
            &mut diags,
            false,
        ) {
            let Some(msg_name) = msg_mod.name(db).to_opt() else {
                continue;
            };

            let path_span: DynLazySpan<'db> = path_span.into();
            let is_duplicate_msg_block = seen_msg_blocks.contains_key(&msg_mod);
            if is_duplicate_msg_block {
                if let Some((first_span, first_name)) = seen_msg_blocks.get(&msg_mod) {
                    diags.push(
                        BodyDiag::RecvDuplicateMsgBlock {
                            primary: path_span.clone(),
                            first_use: first_span.clone(),
                            msg_name: *first_name,
                        }
                        .into(),
                    );
                }
                // Skip handler/selector conflict checks for duplicate msg blocks
                continue;
            } else {
                seen_msg_blocks.insert(msg_mod, (path_span.clone(), msg_name));
            }

            // Check for selector and handler conflicts across all msg variants in this recv block
            for variant in msg_variants(db, msg_mod) {
                let Some(variant_name) = variant.name(db).to_opt() else {
                    continue;
                };

                let variant_span: DynLazySpan<'db> = variant.span().name().into();

                // Check selector conflicts
                let variant_ty = TyId::adt(db, AdtRef::from(variant).as_adt(db));
                if let Some(selector) =
                    eval_msg_variant_selector(db, variant_ty, variant.scope(), &mut diags)
                {
                    check_selector_conflict(
                        selector,
                        variant,
                        variant_name,
                        variant_span.clone(),
                        &mut seen_selectors,
                        &mut diags,
                    );
                }

                // Check handler type conflicts
                let adt_def = AdtRef::from(variant).as_adt(db);
                let ty = TyId::adt(db, adt_def);
                check_handler_conflict(ty, variant_span, &mut seen_handlers, &mut diags);
            }
        } else if recv.msg_path.is_none() {
            // Bare recv block - check each arm individually
            for (arm_idx, arm) in recv.arms.data(db).iter().enumerate() {
                let pat_span: DynLazySpan<'db> = recv_span.clone().arms().arm(arm_idx).pat().into();

                if arm.is_fallback(db) {
                    continue;
                }

                let Some(path) = arm.variant_path(db) else {
                    continue;
                };

                if let Ok(resolved) = resolve_variant_bare(db, contract, path, assumptions) {
                    let Some(variant_name) = resolved.variant_struct.name(db).to_opt() else {
                        continue;
                    };

                    // Check selector conflicts
                    if let Some(selector) =
                        eval_msg_variant_selector(db, resolved.ty, contract.scope(), &mut diags)
                    {
                        check_selector_conflict(
                            selector,
                            resolved.variant_struct,
                            variant_name,
                            pat_span.clone(),
                            &mut seen_selectors,
                            &mut diags,
                        );
                    }

                    // Check handler type conflicts
                    check_handler_conflict(resolved.ty, pat_span, &mut seen_handlers, &mut diags);
                }
            }
        }
    }

    diags
}

/// Check for selector conflicts and emit diagnostics if found.
fn check_selector_conflict<'db>(
    selector: u32,
    variant: Struct<'db>,
    variant_name: IdentId<'db>,
    variant_span: DynLazySpan<'db>,
    seen_selectors: &mut FxHashMap<u32, (DynLazySpan<'db>, IdentId<'db>, Struct<'db>)>,
    diags: &mut Vec<FuncBodyDiag<'db>>,
) {
    if let Some((first_span, first_variant, first_struct)) = seen_selectors.get(&selector) {
        // Don't report if it's the same variant (duplicate msg block already reported)
        if *first_struct != variant {
            diags.push(
                BodyDiag::RecvDuplicateSelector {
                    primary: variant_span,
                    first_use: first_span.clone(),
                    selector,
                    first_variant: *first_variant,
                    second_variant: variant_name,
                }
                .into(),
            );
        }
    } else {
        seen_selectors.insert(selector, (variant_span, variant_name, variant));
    }
}

/// Check for handler type conflicts and emit diagnostics if found.
fn check_handler_conflict<'db>(
    ty: TyId<'db>,
    variant_span: DynLazySpan<'db>,
    seen_handlers: &mut FxHashMap<TyId<'db>, DynLazySpan<'db>>,
    diags: &mut Vec<FuncBodyDiag<'db>>,
) {
    if let Some(first_span) = seen_handlers.get(&ty) {
        diags.push(
            BodyDiag::RecvDuplicateHandler {
                primary: variant_span,
                first_use: first_span.clone(),
                handler_ty: ty,
            }
            .into(),
        );
    } else {
        seen_handlers.insert(ty, variant_span);
    }
}

/// Evaluates a msg variant's `SELECTOR` associated const via CTFE.
pub(crate) fn eval_msg_variant_selector<'db>(
    db: &'db dyn HirAnalysisDb,
    variant_ty: TyId<'db>,
    scope: ScopeId<'db>,
    diags: &mut Vec<FuncBodyDiag<'db>>,
) -> Option<u32> {
    let msg_variant_trait = resolve_core_trait(db, scope, &["message", "MsgVariant"])?;

    let canonical_ty = Canonical::new(db, variant_ty);
    let scope_ingot = scope.ingot(db);
    let search_ingots = [
        Some(scope_ingot),
        variant_ty.ingot(db).filter(|&ingot| ingot != scope_ingot),
    ];
    let implementor = search_ingots.into_iter().flatten().find_map(|ingot| {
        impls_for_ty(db, ingot, canonical_ty)
            .iter()
            .find(|impl_| impl_.skip_binder().trait_def(db) == msg_variant_trait)
            .copied()
    })?;
    let impl_ = implementor.skip_binder();

    let selector_name = IdentId::new(db, "SELECTOR".to_string());
    let selector_const = impl_
        .hir_impl_trait(db)
        .hir_consts(db)
        .iter()
        .find(|c| c.name.to_opt() == Some(selector_name))?;

    let body = selector_const.value.to_opt()?;
    if matches!(
        body.expr(db).data(db, body),
        crate::hir_def::Partial::Absent
    ) {
        return None;
    }

    let expected_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U32)));
    let result = super::check_anon_const_body(db, body, expected_ty);
    diags.extend(result.0.clone());
    if !result.0.is_empty() {
        return None;
    }

    match eval_body_owner_const(
        db,
        BodyOwner::AnonConstBody {
            body,
            expected: expected_ty,
        },
        Vec::new(),
    ) {
        Ok(value) => match value.value(db) {
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } => value.to_u32(),
            _ => {
                diags.push(BodyDiag::ConstValueMustBeKnown(body.span().into()).into());
                None
            }
        },
        Err(_) => {
            diags.push(BodyDiag::ConstValueMustBeKnown(body.span().into()).into());
            None
        }
    }
}

#[salsa::tracked(return_ref)]
pub fn check_contract_recv_arm_body<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    recv_idx: u32,
    arm_idx: u32,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    check_body(
        db,
        BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        },
    )
}

#[derive(Debug, Clone, Default)]
struct InitFieldState {
    fields: FxHashSet<u32>,
}

impl InitFieldState {
    fn intersection(lhs: &Self, rhs: &Self) -> Self {
        Self {
            fields: lhs.fields.intersection(&rhs.fields).copied().collect(),
        }
    }
}

#[derive(Debug, Clone, Default)]
struct InitFieldFlow {
    normal: Option<InitFieldState>,
    returns: Vec<InitFieldState>,
}

impl InitFieldFlow {
    fn normal(state: InitFieldState) -> Self {
        Self {
            normal: Some(state),
            returns: Vec::new(),
        }
    }

    fn divergent() -> Self {
        Self {
            normal: None,
            returns: Vec::new(),
        }
    }

    fn with_returns(mut self, returns: Vec<InitFieldState>) -> Self {
        self.returns.extend(returns);
        self
    }
}

fn merge_normal_states(
    states: impl IntoIterator<Item = Option<InitFieldState>>,
) -> Option<InitFieldState> {
    states
        .into_iter()
        .flatten()
        .reduce(|lhs, rhs| InitFieldState::intersection(&lhs, &rhs))
}

struct InitFieldAssignmentCheck<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    body: Body<'db>,
    typed_body: &'a TypedBody<'db>,
    required: FxHashSet<u32>,
}

impl<'a, 'db> InitFieldAssignmentCheck<'a, 'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        contract: Contract<'db>,
        body: Body<'db>,
        typed_body: &'a TypedBody<'db>,
        required: FxHashSet<u32>,
    ) -> Self {
        Self {
            db,
            contract,
            body,
            typed_body,
            required,
        }
    }

    fn successful_exit_states(&self) -> Vec<InitFieldState> {
        let flow = self.analyze_expr(self.body.expr(self.db), InitFieldState::default());
        let mut exits = flow.returns;
        if let Some(normal) = flow.normal {
            exits.push(normal);
        }
        exits
    }

    fn analyze_stmt(&self, stmt: Stmt<'db>, state: InitFieldState) -> InitFieldFlow {
        match stmt {
            Stmt::Let(_, _, Some(init)) => self.analyze_expr(init, state),
            Stmt::Let(_, _, None) => InitFieldFlow::normal(state),
            Stmt::Expr(expr) => self.analyze_expr(expr, state),
            Stmt::Return(expr) => {
                let flow = if let Some(expr) = expr {
                    self.analyze_expr(expr, state)
                } else {
                    InitFieldFlow::normal(state)
                };
                let returns = flow.normal.into_iter().collect();
                InitFieldFlow::divergent().with_returns([flow.returns, returns].concat())
            }
            Stmt::While(cond, body) => {
                let cond_flow = self.analyze_cond(cond, state);
                let mut returns = cond_flow.returns;
                if let Some(cond_state) = cond_flow.normal.clone() {
                    returns.extend(self.analyze_expr(body, cond_state).returns);
                }
                InitFieldFlow {
                    // The loop body may execute zero times, so only condition-side effects are
                    // guaranteed after a normal loop exit.
                    normal: cond_flow.normal,
                    returns,
                }
            }
            Stmt::For(_, iter, body, _) => {
                let iter_flow = self.analyze_expr(iter, state);
                let mut returns = iter_flow.returns;
                if let Some(iter_state) = iter_flow.normal.clone() {
                    returns.extend(self.analyze_expr(body, iter_state).returns);
                }
                InitFieldFlow {
                    // A for loop may execute zero times, so body writes are not definite.
                    normal: iter_flow.normal,
                    returns,
                }
            }
            Stmt::Break | Stmt::Continue => InitFieldFlow::divergent(),
        }
    }

    fn analyze_cond(&self, cond: crate::hir_def::CondId, state: InitFieldState) -> InitFieldFlow {
        let Partial::Present(cond) = cond.data(self.db, self.body) else {
            return InitFieldFlow::normal(state);
        };
        match cond {
            Cond::Expr(expr) | Cond::Let(_, expr) => self.analyze_expr(*expr, state),
            Cond::Bin(lhs, rhs, _) => {
                let lhs_flow = self.analyze_cond(*lhs, state);
                let mut returns = lhs_flow.returns;
                let normal = lhs_flow.normal.and_then(|lhs_state| {
                    let rhs_flow = self.analyze_cond(*rhs, lhs_state.clone());
                    returns.extend(rhs_flow.returns);
                    // RHS of `&&`/`||` is short-circuited, so only writes that happen with and
                    // without RHS evaluation are definite after the condition.
                    merge_normal_states([Some(lhs_state), rhs_flow.normal])
                });
                InitFieldFlow { normal, returns }
            }
        }
    }

    fn analyze_expr(&self, expr: ExprId, state: InitFieldState) -> InitFieldFlow {
        let Partial::Present(expr_data) = expr.data(self.db, self.body) else {
            return InitFieldFlow::normal(state);
        };

        let flow = match expr_data {
            Expr::Lit(_) | Expr::Path(_) => InitFieldFlow::normal(state),
            Expr::Block(stmts) => self.analyze_block(stmts, state),
            Expr::Tuple(elems) | Expr::Array(elems) => self.analyze_exprs(elems, state),
            Expr::ArrayRep(elem, _) | Expr::Un(elem, _) | Expr::Cast(elem, _) => {
                self.analyze_expr(*elem, state)
            }
            Expr::Bin(lhs, rhs, _) => self.analyze_expr_pair(*lhs, *rhs, state),
            Expr::Call(callee, args) => {
                let exprs = std::iter::once(*callee)
                    .chain(args.iter().map(|arg| arg.expr))
                    .collect::<Vec<_>>();
                self.analyze_exprs(&exprs, state)
            }
            Expr::MethodCall(receiver, _, _, args) => {
                let exprs = std::iter::once(*receiver)
                    .chain(args.iter().map(|arg| arg.expr))
                    .collect::<Vec<_>>();
                self.analyze_exprs(&exprs, state)
            }
            Expr::RecordInit(_, fields) => {
                let exprs = fields.iter().map(|field| field.expr).collect::<Vec<_>>();
                self.analyze_exprs(&exprs, state)
            }
            Expr::Field(base, _) => self.analyze_expr(*base, state),
            Expr::If(cond, then_expr, else_expr) => {
                let cond_flow = self.analyze_cond(*cond, state);
                let mut returns = cond_flow.returns;
                let normal = cond_flow.normal.and_then(|cond_state| {
                    let then_flow = self.analyze_expr(*then_expr, cond_state.clone());
                    returns.extend(then_flow.returns);
                    let else_flow = if let Some(else_expr) = else_expr {
                        self.analyze_expr(*else_expr, cond_state)
                    } else {
                        InitFieldFlow::normal(cond_state)
                    };
                    returns.extend(else_flow.returns);
                    merge_normal_states([then_flow.normal, else_flow.normal])
                });
                InitFieldFlow { normal, returns }
            }
            Expr::Match(scrutinee, arms) => {
                let scrutinee_flow = self.analyze_expr(*scrutinee, state);
                let mut returns = scrutinee_flow.returns;
                let normal = scrutinee_flow.normal.and_then(|scrutinee_state| {
                    let Partial::Present(arms) = arms else {
                        return Some(scrutinee_state);
                    };
                    let mut normals = Vec::new();
                    for arm in arms {
                        let arm_flow = self.analyze_expr(arm.body, scrutinee_state.clone());
                        returns.extend(arm_flow.returns);
                        normals.push(arm_flow.normal);
                    }
                    merge_normal_states(normals)
                });
                InitFieldFlow { normal, returns }
            }
            Expr::Assign(lhs, rhs) => {
                let mut flow = self.analyze_expr(*rhs, state);
                if let Some(normal) = &mut flow.normal
                    && let Some(field_idx) = self.assigned_contract_field(*lhs)
                {
                    normal.fields.insert(field_idx);
                }
                flow
            }
            Expr::AugAssign(lhs, rhs, _) => self.analyze_expr_pair(*lhs, *rhs, state),
            Expr::With(bindings, body) => {
                let exprs = bindings
                    .iter()
                    .map(|binding| binding.value)
                    .chain(std::iter::once(*body))
                    .collect::<Vec<_>>();
                self.analyze_exprs(&exprs, state)
            }
        };

        if self.typed_body.expr_ty(self.db, expr).is_never(self.db) {
            InitFieldFlow {
                normal: None,
                returns: flow.returns,
            }
        } else {
            flow
        }
    }

    fn analyze_block(
        &self,
        stmts: &[crate::hir_def::StmtId],
        state: InitFieldState,
    ) -> InitFieldFlow {
        let mut current = Some(state);
        let mut returns = Vec::new();
        for stmt in stmts {
            let Some(state) = current.take() else {
                break;
            };
            let Partial::Present(stmt_data) = stmt.data(self.db, self.body) else {
                current = Some(state);
                continue;
            };
            let flow = self.analyze_stmt(stmt_data.clone(), state);
            returns.extend(flow.returns);
            current = flow.normal;
        }
        InitFieldFlow {
            normal: current,
            returns,
        }
    }

    fn analyze_expr_pair(&self, lhs: ExprId, rhs: ExprId, state: InitFieldState) -> InitFieldFlow {
        let lhs_flow = self.analyze_expr(lhs, state);
        let mut returns = lhs_flow.returns;
        let normal = lhs_flow.normal.and_then(|lhs_state| {
            let rhs_flow = self.analyze_expr(rhs, lhs_state);
            returns.extend(rhs_flow.returns);
            rhs_flow.normal
        });
        InitFieldFlow { normal, returns }
    }

    fn analyze_exprs(&self, exprs: &[ExprId], state: InitFieldState) -> InitFieldFlow {
        let mut current = Some(state);
        let mut returns = Vec::new();
        for expr in exprs {
            let Some(state) = current.take() else {
                break;
            };
            let flow = self.analyze_expr(*expr, state);
            returns.extend(flow.returns);
            current = flow.normal;
        }
        InitFieldFlow {
            normal: current,
            returns,
        }
    }

    fn assigned_contract_field(&self, lhs: ExprId) -> Option<u32> {
        let place = self.typed_body.expr_place(lhs)?;
        if !place.projections.is_empty() {
            return None;
        }
        let crate::analysis::place::PlaceBase::Binding(binding) = place.base;
        let LocalBinding::EffectParam { site, idx, .. } = binding else {
            return None;
        };
        if site
            != (EffectParamSite::ContractInit {
                contract: self.contract,
            })
        {
            return None;
        }
        let provider = EffectEnvView::new(site)
            .resolved_binding(self.db, idx)
            .map(|binding| binding.provider)?;
        let ProviderSource::ContractField {
            contract,
            field_idx,
        } = provider.source
        else {
            return None;
        };
        (contract == self.contract && self.required.contains(&field_idx)).then_some(field_idx)
    }
}

#[salsa::tracked(return_ref)]
pub fn check_contract_immutable_fields_initialized<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let required_fields = contract
        .field_layout(db)
        .values()
        .filter(|field| field.slot_count != 0)
        .filter(|field| {
            !field.declared_ty.has_invalid(db)
                && !field.target_ty.has_invalid(db)
                && !field.address_space.has_invalid(db)
        })
        .filter(|field| {
            FieldView {
                parent: FieldParent::Contract(contract),
                idx: field.index as usize,
            }
            .ty_diags(db)
            .is_empty()
        })
        .filter(|field| {
            address_space_from_ty(db, contract.scope(), field.address_space)
                == Some(ProviderAddressSpace::Code)
        })
        .cloned()
        .collect::<Vec<_>>();
    if required_fields.is_empty() {
        return Vec::new();
    }

    let required = required_fields
        .iter()
        .map(|field| field.index)
        .collect::<FxHashSet<_>>();
    let init = contract.init(db);
    let missing = if init.is_some() {
        let (_, typed_body) = check_contract_init_body(db, contract);
        if let Some(body) = typed_body.body() {
            let exits =
                InitFieldAssignmentCheck::new(db, contract, body, typed_body, required.clone())
                    .successful_exit_states();
            if exits.is_empty() {
                FxHashSet::default()
            } else {
                required
                    .iter()
                    .copied()
                    .filter(|field| !exits.iter().all(|state| state.fields.contains(field)))
                    .collect()
            }
        } else {
            required.clone()
        }
    } else {
        required
    };

    let init_span = init.map(|_| contract.span().init_block().body().into());
    required_fields
        .into_iter()
        .filter(|field| missing.contains(&field.index))
        .map(|field| {
            BodyDiag::ImmutableContractFieldNotInitialized {
                primary: FieldParent::Contract(contract).field_name_span(field.index as usize),
                field: field.name,
                init: init_span.clone(),
            }
            .into()
        })
        .collect()
}

#[salsa::tracked(return_ref)]
pub fn check_contract_init_body<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    check_body(db, BodyOwner::ContractInit { contract })
}

pub(super) fn resolve_recv_msg_mod<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    msg_path: Option<PathId<'db>>,
    span: LazyPathSpan<'db>,
    diags: &mut Vec<FuncBodyDiag<'db>>,
    emit_diag: bool,
) -> Option<Mod<'db>> {
    let msg_path = msg_path?;
    let assumptions = PredicateListId::empty_list(db);

    match resolve_path(db, msg_path, contract.scope(), assumptions, false) {
        Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) => {
            if emit_diag {
                diags.push(
                    BodyDiag::RecvExpectedMsgType {
                        primary: span.clone().into(),
                        given: ty,
                    }
                    .into(),
                );
            }
            None
        }
        Ok(PathRes::Mod(scope)) => {
            // Accept any module as a recv root (both msg-desugared and manually defined)
            if let ScopeId::Item(ItemKind::Mod(mod_)) = scope {
                return Some(mod_);
            }
            unreachable!();
        }
        Ok(other) => {
            let ident = msg_path.ident(db).to_opt()?;
            if emit_diag {
                diags.push(PathResDiag::ExpectedType(span.into(), ident, other.kind_name()).into());
            }
            None
        }
        Err(err) => {
            if emit_diag
                && let Some(diag) = err.into_diag(db, msg_path, span, ExpectedPathKind::Type)
            {
                diags.push(diag.into());
            }
            None
        }
    }
}

/// Gets the Return type from a type's MsgVariant trait implementation.
/// Specifically looks for the MsgVariant trait and returns `None` if no impl is found.
pub(super) fn get_msg_variant_return_type<'db>(
    db: &'db dyn HirAnalysisDb,
    variant_ty: TyId<'db>,
    scope: ScopeId<'db>,
) -> Option<TyId<'db>> {
    let msg_variant_trait = resolve_core_trait(db, scope, &["message", "MsgVariant"])?;

    let canonical_ty = Canonical::new(db, variant_ty);
    let scope_ingot = scope.ingot(db);
    let search_ingots = [
        Some(scope_ingot),
        variant_ty.ingot(db).filter(|&ingot| ingot != scope_ingot),
    ];

    // Find the MsgVariant impl specifically, probing both:
    // - the call-site ingot (for local traits implemented for external types), and
    // - the receiver type's ingot (for external traits implemented in the type's ingot).
    let msg_variant_impl = search_ingots.into_iter().flatten().find_map(|ingot| {
        impls_for_ty(db, ingot, canonical_ty)
            .iter()
            .find(|impl_| impl_.skip_binder().trait_def(db).eq(&msg_variant_trait))
            .copied()
    })?;

    // Get the Return associated type from the impl
    let return_name = IdentId::new(db, "Return".to_string());
    msg_variant_impl.skip_binder().assoc_ty(db, return_name)
}
