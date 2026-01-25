//! Contract-specific type checking functions.
//!
//! This module contains functions for checking contract init bodies,
//! recv blocks, and recv arm bodies.
use rustc_hash::{FxHashMap, FxHashSet};

use super::{TypedBody, owner::BodyOwner};

use crate::{
    analysis::{
        HirAnalysisDb,
        name_resolution::{ExpectedPathKind, PathRes, diagnostics::PathResDiag, resolve_path},
        ty::{
            adt_def::AdtRef,
            canonical::Canonical,
            corelib::resolve_core_trait,
            diagnostics::{BodyDiag, FuncBodyDiag, TraitConstraintDiag, TyDiagCollection},
            trait_def::TraitInstId,
            trait_def::impls_for_ty,
            trait_resolution::{GoalSatisfiability, PredicateListId, is_goal_satisfiable},
            ty_check::check_body,
            ty_def::TyId,
        },
    },
    hir_def::{Contract, IdentId, ItemKind, Mod, PathId, Struct, scope_graph::ScopeId},
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
    contract_ingot: common::ingot::Ingot<'db>,
    decode_trait: crate::hir_def::Trait<'db>,
    sol_ty: TyId<'db>,
    ty: TyId<'db>,
    span: DynLazySpan<'db>,
    assumptions: PredicateListId<'db>,
    diags: &mut Vec<FuncBodyDiag<'db>>,
) {
    if ty.has_invalid(db) {
        return;
    }

    if ty.is_tuple(db) {
        for elem in ty.field_types(db) {
            check_ty_decodable(
                db,
                contract_ingot,
                decode_trait,
                sol_ty,
                elem,
                span.clone(),
                assumptions,
                diags,
            );
        }
        return;
    }

    if ty.has_var(db) {
        return;
    }

    let inst = TraitInstId::new(db, decode_trait, vec![ty, sol_ty], IndexMap::new());
    let canonical_inst = Canonical::new(db, inst);

    if let GoalSatisfiability::UnSat(_) =
        is_goal_satisfiable(db, contract_ingot, canonical_inst, assumptions)
    {
        diags.push(
            TyDiagCollection::from(TraitConstraintDiag::TraitBoundNotSat {
                span,
                primary_goal: inst,
                unsat_subgoal: None,
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
    let contract_ingot = contract.top_mod(db).ingot(db);

    let Some(sol_ty) = resolve_sol_abi_ty(db, contract.scope(), assumptions) else {
        return;
    };
    let Some(decode_trait) = resolve_core_trait(db, contract.scope(), &["abi", "Decode"]) else {
        return;
    };

    for field_ty in variant.ty.field_types(db) {
        check_ty_decodable(
            db,
            contract_ingot,
            decode_trait,
            sol_ty,
            field_ty,
            span.clone(),
            assumptions,
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
        let pat_span: DynLazySpan<'db> = recv_span.clone().arms().arm(arm_idx).pat().into();

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
        let pat_span: DynLazySpan<'db> = recv_span.clone().arms().arm(arm_idx).pat().into();

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
                if let Some(selector) = eval_msg_variant_selector(db, variant, &mut diags) {
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

                let Some(path) = arm.variant_path(db) else {
                    continue;
                };

                if let Ok(resolved) = resolve_variant_bare(db, contract, path, assumptions) {
                    let Some(variant_name) = resolved.variant_struct.name(db).to_opt() else {
                        continue;
                    };

                    // Check selector conflicts
                    if let Some(selector) =
                        eval_msg_variant_selector(db, resolved.variant_struct, &mut diags)
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

/// Gets the selector value from a msg variant struct by evaluating its `SELECTOR`
/// associated const via CTFE.
///
/// Used for cross-recv-block duplicate detection.
pub(crate) fn eval_msg_variant_selector<'db>(
    db: &'db dyn HirAnalysisDb,
    struct_: Struct<'db>,
    diags: &mut Vec<FuncBodyDiag<'db>>,
) -> Option<u32> {
    use crate::analysis::ty::{
        const_ty::{ConstTyData, EvaluatedConstTy},
        ctfe::{CtfeConfig, CtfeInterpreter},
        ty_def::{PrimTy, TyBase, TyData},
    };
    use num_traits::ToPrimitive;

    // Find the MsgVariant trait
    let msg_variant_trait = resolve_core_trait(db, struct_.scope(), &["message", "MsgVariant"])?;

    // Get the impl for this struct
    let adt_def = AdtRef::from(struct_).as_adt(db);
    let ty = TyId::adt(db, adt_def);
    let canonical_ty = Canonical::new(db, ty);
    let ingot = struct_.top_mod(db).ingot(db);

    let impl_ = impls_for_ty(db, ingot, canonical_ty)
        .iter()
        .find(|impl_| impl_.skip_binder().trait_def(db).eq(&msg_variant_trait))?
        .skip_binder();

    // Get the SELECTOR const from the impl
    let selector_name = IdentId::new(db, "SELECTOR".to_string());
    let hir_impl = impl_.hir_impl_trait(db);

    let selector_const = hir_impl
        .hir_consts(db)
        .iter()
        .find(|c| c.name.to_opt() == Some(selector_name))?;

    // Evaluate the const body to a concrete `u32` via CTFE.
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

    let mut interp = CtfeInterpreter::new(db, CtfeConfig::default());

    match interp.eval_const_body(body, result.1.clone()) {
        Ok(const_ty) => match const_ty.data(db) {
            ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int_id), _) => int_id.data(db).to_u32(),
            _ => {
                diags.push(BodyDiag::ConstValueMustBeKnown(body.span().into()).into());
                None
            }
        },
        Err(cause) => {
            let ty = TyId::invalid(db, cause);
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
    let ingot = scope.ingot(db);

    // Find the MsgVariant impl specifically
    let msg_variant_impl = impls_for_ty(db, ingot, canonical_ty)
        .iter()
        .find(|impl_| impl_.skip_binder().trait_def(db).eq(&msg_variant_trait))?;

    // Get the Return associated type from the impl
    let return_name = IdentId::new(db, "Return".to_string());
    msg_variant_impl.skip_binder().assoc_ty(db, return_name)
}
