//! Contract-specific type checking functions.
//!
//! This module contains functions for checking contract init bodies,
//! recv blocks, and recv arm bodies.
use rustc_hash::{FxHashMap, FxHashSet};

use super::{TypedBody, owner::BodyOwner};

use num_traits::ToPrimitive;

use crate::{
    analysis::{
        HirAnalysisDb,
        name_resolution::{ExpectedPathKind, PathRes, diagnostics::PathResDiag, resolve_path},
        semantic::{CtfeError, SemConstScalar, SemConstValue, eval_body_owner_const},
        ty::{
            adt_def::AdtRef,
            canonical::Canonical,
            corelib::resolve_core_trait,
            diagnostics::{BodyDiag, FuncBodyDiag, TraitConstraintDiag, TyDiagCollection},
            normalize::normalize_ty,
            trait_def::TraitInstId,
            trait_def::{assoc_const_body_for_trait_inst, impls_for_ty},
            trait_resolution::{
                GoalSatisfiability, PredicateListId, TraitSolveCx, is_goal_satisfiable,
            },
            ty_check::check_body,
            ty_def::TyId,
        },
    },
    hir_def::{Contract, IdentId, IntegerId, ItemKind, Mod, PathId, Struct, scope_graph::ScopeId},
    span::{DynLazySpan, path::LazyPathSpan},
};
use common::indexmap::IndexMap;

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

fn implements_msg_variant_for_abi<'db>(
    db: &'db dyn HirAnalysisDb,
    struct_: Struct<'db>,
    abi_ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> bool {
    let Some(msg_variant_trait) = resolve_core_trait(db, scope, &["message", "MsgVariant"]) else {
        return false;
    };

    let adt_def = AdtRef::from(struct_).as_adt(db);
    let ty = TyId::adt(db, adt_def);
    let inst = TraitInstId::new(db, msg_variant_trait, vec![ty, abi_ty], IndexMap::new());
    let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
    !matches!(
        is_goal_satisfiable(db, solve_cx, inst),
        GoalSatisfiability::UnSat(_)
    )
}

fn abi_selector_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    abi_ty: TyId<'db>,
) -> Option<TyId<'db>> {
    let abi_trait = resolve_core_trait(db, scope, &["abi", "Abi"])?;
    let inst = TraitInstId::new(db, abi_trait, vec![abi_ty], IndexMap::new());
    let selector_ident = IdentId::new(db, "Selector".to_string());
    Some(normalize_ty(
        db,
        TyId::assoc_ty(db, inst, selector_ident),
        scope,
        assumptions,
    ))
}

fn eval_abi_selector_size<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    abi_ty: TyId<'db>,
) -> Option<usize> {
    let abi_trait = resolve_core_trait(db, scope, &["abi", "Abi"])?;
    let inst = TraitInstId::new(db, abi_trait, vec![abi_ty], IndexMap::new());
    let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
    let selector_size_name = IdentId::new(db, "SELECTOR_SIZE".to_string());
    let body = assoc_const_body_for_trait_inst(db, solve_cx, inst, selector_size_name)?;
    match eval_body_owner_const(
        db,
        BodyOwner::AnonConstBody {
            body,
            expected: TyId::u256(db),
        },
        Vec::new(),
    ) {
        Ok(value) => match value.value(db) {
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } => value.to_usize(),
            _ => None,
        },
        _ => None,
    }
}

#[allow(clippy::too_many_arguments)]
fn check_ty_decodable<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    decode_trait: crate::hir_def::Trait<'db>,
    abi_ty: TyId<'db>,
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
                abi_ty,
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

    let inst = TraitInstId::new(db, decode_trait, vec![ty, abi_ty], IndexMap::new());
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
    abi_ty: TyId<'db>,
    span: DynLazySpan<'db>,
    assumptions: PredicateListId<'db>,
    diags: &mut Vec<FuncBodyDiag<'db>>,
) {
    let Some(decode_trait) = resolve_core_trait(db, contract.scope(), &["abi", "Decode"]) else {
        return;
    };
    let solve_cx = TraitSolveCx::new(db, contract.scope()).with_assumptions(assumptions);

    for field_ty in variant.ty.field_types(db) {
        check_ty_decodable(
            db,
            solve_cx,
            decode_trait,
            abi_ty,
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

fn msg_variants_for_abi<'db>(
    db: &'db dyn HirAnalysisDb,
    msg_mod: Mod<'db>,
    abi_ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> impl Iterator<Item = Struct<'db>> + 'db {
    msg_variants(db, msg_mod)
        .filter(move |s| implements_msg_variant_for_abi(db, *s, abi_ty, scope, assumptions))
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
    let recv_view = contract
        .recv(db, recv_idx)
        .expect("recv index was validated above");
    let abi_ty = recv_view.abi_ty(db);

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
        check_named_recv_block(db, contract, recv_idx, msg_mod, abi_ty, &mut diags);
    } else if recv.msg_path.is_none() {
        // Bare recv block - no msg module specified
        check_bare_recv_block(db, contract, recv_idx, abi_ty, &mut diags);
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
    abi_ty: TyId<'db>,
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

                if !implements_msg_variant_for_abi(
                    db,
                    resolved.variant_struct,
                    abi_ty,
                    contract.scope(),
                    assumptions,
                ) {
                    diags.push(
                        BodyDiag::RecvArmNotMsgVariantTrait {
                            primary: pat_span,
                            given_ty: resolved.ty,
                        }
                        .into(),
                    );
                    continue;
                }

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
                        abi_ty,
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
    let missing: Vec<_> = msg_variants_for_abi(db, msg_mod, abi_ty, contract.scope(), assumptions)
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
    abi_ty: TyId<'db>,
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

                if !implements_msg_variant_for_abi(
                    db,
                    resolved.variant_struct,
                    abi_ty,
                    contract.scope(),
                    assumptions,
                ) {
                    diags.push(
                        BodyDiag::RecvArmNotMsgVariantTrait {
                            primary: pat_span,
                            given_ty: resolved.ty,
                        }
                        .into(),
                    );
                    continue;
                }

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
                        abi_ty,
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
    let mut seen_msg_blocks =
        FxHashMap::<(Mod<'db>, TyId<'db>), (DynLazySpan<'db>, IdentId<'db>)>::default();
    let mut seen_fallback: Option<DynLazySpan<'db>> = None;

    // Track selectors across ALL recv blocks. Mixed selector sizes are ambiguous when
    // one selector is a prefix of another, so conflicts are checked by bytes rather than
    // by integer value alone.
    let mut seen_selectors = Vec::<SeenSelector<'db>>::new();

    // Track handler types across ALL recv blocks for duplicate detection.
    // We use TyId to handle type aliases correctly.
    let mut seen_handlers = FxHashMap::<TyId<'db>, DynLazySpan<'db>>::default();

    let assumptions = PredicateListId::empty_list(db);

    for (idx, recv) in contract.recvs(db).data(db).iter().enumerate() {
        let recv_span = contract.span().recv(idx);
        let path_span = recv_span.clone().path();
        let recv_view = contract
            .recv(db, idx as u32)
            .expect("recv index came from contract recv list");
        let abi_ty = recv_view.abi_ty(db);
        let selector_size = eval_abi_selector_size(db, contract.scope(), assumptions, abi_ty);

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
            let msg_block_key = (msg_mod, abi_ty);
            let is_duplicate_msg_block = seen_msg_blocks.contains_key(&msg_block_key);
            if is_duplicate_msg_block {
                if let Some((first_span, first_name)) = seen_msg_blocks.get(&msg_block_key) {
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
                seen_msg_blocks.insert(msg_block_key, (path_span.clone(), msg_name));
            }

            // Check for selector and handler conflicts across all msg variants in this recv block
            for variant in msg_variants_for_abi(db, msg_mod, abi_ty, contract.scope(), assumptions)
            {
                let Some(variant_name) = variant.name(db).to_opt() else {
                    continue;
                };

                let variant_span: DynLazySpan<'db> = variant.span().name().into();

                // Check selector conflicts
                let variant_ty = TyId::adt(db, AdtRef::from(variant).as_adt(db));
                if let (Some(selector), Some(selector_size)) = (
                    eval_msg_variant_selector(db, variant_ty, abi_ty, variant.scope(), &mut diags),
                    selector_size,
                ) {
                    check_selector_conflict(
                        db,
                        selector_size,
                        SelectorUse {
                            selector,
                            variant,
                            variant_name,
                            span: variant_span.clone(),
                        },
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
                    if !implements_msg_variant_for_abi(
                        db,
                        resolved.variant_struct,
                        abi_ty,
                        contract.scope(),
                        assumptions,
                    ) {
                        continue;
                    }
                    let Some(variant_name) = resolved.variant_struct.name(db).to_opt() else {
                        continue;
                    };

                    // Check selector conflicts
                    if let (Some(selector), Some(selector_size)) = (
                        eval_msg_variant_selector(
                            db,
                            resolved.ty,
                            abi_ty,
                            contract.scope(),
                            &mut diags,
                        ),
                        selector_size,
                    ) {
                        check_selector_conflict(
                            db,
                            selector_size,
                            SelectorUse {
                                selector,
                                variant: resolved.variant_struct,
                                variant_name,
                                span: pat_span.clone(),
                            },
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

struct SeenSelector<'db> {
    bytes: Vec<u8>,
    text: String,
    span: DynLazySpan<'db>,
    variant_name: IdentId<'db>,
    variant: Struct<'db>,
}

struct SelectorUse<'db> {
    selector: IntegerId<'db>,
    variant: Struct<'db>,
    variant_name: IdentId<'db>,
    span: DynLazySpan<'db>,
}

fn selector_bytes<'db>(
    db: &'db dyn HirAnalysisDb,
    selector: IntegerId<'db>,
    selector_size: usize,
) -> Option<Vec<u8>> {
    let raw = selector.data(db).to_bytes_be();
    if raw.len() > selector_size {
        return None;
    }
    let mut out = vec![0; selector_size.saturating_sub(raw.len())];
    out.extend(raw);
    Some(out)
}

fn selector_hex(bytes: &[u8]) -> String {
    let mut out = String::from("0x");
    for byte in bytes {
        out.push_str(&format!("{byte:02x}"));
    }
    out
}

fn selector_prefixes_overlap(left: &[u8], right: &[u8]) -> bool {
    left.iter().zip(right.iter()).all(|(lhs, rhs)| lhs == rhs)
}

/// Check for selector conflicts and emit diagnostics if found.
fn check_selector_conflict<'db>(
    db: &'db dyn HirAnalysisDb,
    selector_size: usize,
    use_: SelectorUse<'db>,
    seen_selectors: &mut Vec<SeenSelector<'db>>,
    diags: &mut Vec<FuncBodyDiag<'db>>,
) {
    let Some(bytes) = selector_bytes(db, use_.selector, selector_size) else {
        return;
    };
    let selector_text = selector_hex(&bytes);

    for first in seen_selectors.iter() {
        // Don't report if it's the same variant (duplicate msg block already reported)
        if first.variant != use_.variant && selector_prefixes_overlap(&first.bytes, &bytes) {
            let selector_desc = if first.text == selector_text {
                selector_text.clone()
            } else {
                format!("{selector_text} overlaps {}", first.text)
            };
            diags.push(
                BodyDiag::RecvDuplicateSelector {
                    primary: use_.span.clone(),
                    first_use: first.span.clone(),
                    selector: selector_desc,
                    first_variant: first.variant_name,
                    second_variant: use_.variant_name,
                }
                .into(),
            );
            return;
        }
    }

    seen_selectors.push(SeenSelector {
        bytes,
        text: selector_text,
        span: use_.span,
        variant_name: use_.variant_name,
        variant: use_.variant,
    });
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
    abi_ty: TyId<'db>,
    scope: ScopeId<'db>,
    diags: &mut Vec<FuncBodyDiag<'db>>,
) -> Option<IntegerId<'db>> {
    let msg_variant_trait = resolve_core_trait(db, scope, &["message", "MsgVariant"])?;
    let selector_name = IdentId::new(db, "SELECTOR".to_string());
    let inst = TraitInstId::new(
        db,
        msg_variant_trait,
        vec![variant_ty, abi_ty],
        IndexMap::new(),
    );
    let assumptions = PredicateListId::empty_list(db);
    let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
    let body = assoc_const_body_for_trait_inst(db, solve_cx, inst, selector_name)?;
    if matches!(
        body.expr(db).data(db, body),
        crate::hir_def::Partial::Absent
    ) {
        return None;
    }

    let expected_ty = abi_selector_ty(db, scope, assumptions, abi_ty)?;
    let result = super::check_anon_const_body(db, body, expected_ty);
    diags.extend(result.0.clone());
    if !result.0.is_empty() {
        return None;
    }

    let owner = BodyOwner::AnonConstBody {
        body,
        expected: expected_ty,
    };
    match eval_body_owner_const(db, owner, Vec::new()) {
        Ok(value) => match value.value(db) {
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } => value
                .to_biguint()
                .map(|value| IntegerId::new(db, value))
                .or_else(|| {
                    diags.push(BodyDiag::ConstValueMustBeKnown(body.span().into()).into());
                    None
                }),
            _ => {
                diags.push(BodyDiag::ConstValueMustBeKnown(body.span().into()).into());
                None
            }
        },
        Err(CtfeError::NotConstEvaluable { .. }) => {
            diags.push(BodyDiag::ConstValueMustBeKnown(body.span().into()).into());
            None
        }
        Err(err) => {
            let ty = TyId::invalid(db, super::invalid_cause_from_ctfe_error(db, owner, err));
            if let Some(diag) = ty.emit_diag(db, body.span().into()) {
                diags.push(diag.into());
            }
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
