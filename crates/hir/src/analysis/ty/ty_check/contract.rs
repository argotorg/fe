//! Contract-specific type checking functions.
//!
//! This module contains functions for checking contract init bodies,
//! recv blocks, and recv arm bodies.

use crate::analysis::name_resolution::Tier;
use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, SubDiagnostic,
};
use rustc_hash::{FxHashMap, FxHashSet};

use super::{TypedBody, owner::BodyOwner};

use num_traits::ToPrimitive;
use ruint::aliases::U256;

use crate::{
    analysis::{
        HirAnalysisDb,
        diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb},
        name_resolution::{
            ExpectedPathKind, PathRes, PathResError, Viewer, diagnostics::PathResDiag,
            resolve_path_with_observer_and_minter,
        },
        semantic::{
            ConstUsePolicy, EvalOutcome, GenericSubst, SemConstScalar, SemConstValue,
            contract_init_assigned_fields, diagnostics::foreign_ingot_note, eval_body_owner_const,
        },
        ty::{
            adt_def::AdtRef,
            const_ty::LoweringContext,
            corelib::resolve_core_trait,
            diagnostics::{
                BodyDiag, FuncBodyDiag, TraitConstraintDiag, TyDiagCollection, TyLowerDiag,
            },
            provider::AddressSpace,
            trait_def::TraitInstId,
            trait_resolution::{
                GoalSatisfiability, PredicateListId, TraitSolveCx, is_goal_satisfiable,
            },
            ty_check::{
                check_anon_const_body, check_body, const_body_ctfe_diags,
                const_requirements::constrained_applications,
            },
            ty_def::{PrimTy, TyBase, TyData, TyId},
            ty_error::{emit_invalid_ty_error, path_generic_arg_diags},
        },
    },
    core::semantic::{
        RecvArmView, RecvView, ValidatedFieldLayoutPlan, contract_entry, msg_variant_impl,
        resolve_sol_abi_ty,
    },
    hir_def::{
        Body, Contract, ContractRecvArm, FieldParent, IdentId, ItemKind, Mod, Partial, Pat, PathId,
        PathKind, Struct, scope_graph::ScopeId,
    },
    semantic::FieldView,
    span::{DynLazySpan, LazySpan, pat::LazyPatSpan, path::LazyPathSpan},
};

/// Returns true if `ty` implements the core MsgVariant trait for the ABI of
/// the recv blocks.
fn implements_msg_variant<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
) -> bool {
    let Some(abi) = resolve_sol_abi_ty(db, scope, PredicateListId::empty_list(db)) else {
        return false;
    };
    msg_variant_impl(db, scope, ty, abi).is_some()
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

    let inst = TraitInstId::new_simple(db, decode_trait, vec![ty, sol_ty]);
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

/// Requires `Decode<Sol>` of the fields that `arm` decodes in full, which its
/// reading decides: a field read in place with `ref` or bound to no name is
/// not decoded.
fn check_recv_variant_param_types_decodable<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    arm: RecvArmView<'db>,
    variant_ty: TyId<'db>,
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
    let solve_cx = TraitSolveCx::new(db, contract.scope(), Tier::All).with_assumptions(assumptions);

    let field_tys = variant_ty.field_types(db);
    for &index in &arm.reading(db).decoded {
        let Some(&field_ty) = field_tys.get(index as usize) else {
            continue;
        };
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
        .filter(move |s| {
            implements_msg_variant(db, s.scope(), TyId::adt(db, AdtRef::from(*s).as_adt(db)))
        })
}

/// The msg a recv block handles.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub enum RecvMsg<'db> {
    /// `recv { .. }`, whose arms handle any msg variant.
    Any,
    /// `recv M { .. }`, whose arms handle the variants of the msg module `M`.
    Module(Mod<'db>),
    /// `recv P { .. }` whose path doesn't name a module.
    Invalid,
}

/// A recv block's msg, resolved from its path once for every use of it.
#[derive(Debug, Clone, PartialEq, Eq, salsa::SalsaValue)]
pub struct RecvMsgResolution<'db> {
    pub msg: RecvMsg<'db>,
    /// What resolving the path reports.
    pub diags: Vec<FuncBodyDiag<'db>>,
}

/// Resolves the msg path of `contract`'s recv block `recv_idx` where it's
/// written.
#[salsa::tracked]
pub fn recv_msg<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    recv_idx: u32,
) -> RecvMsgResolution<'db> {
    let Some(recv) = contract.recvs(db).data(db).get(recv_idx as usize) else {
        return RecvMsgResolution {
            msg: RecvMsg::Invalid,
            diags: Vec::new(),
        };
    };
    let Some(msg_path) = recv.msg_path else {
        return RecvMsgResolution {
            msg: RecvMsg::Any,
            diags: Vec::new(),
        };
    };
    let span = contract.span().recv(recv_idx as usize).path();
    let (res, _, invisible) = resolve_recv_path(db, contract, msg_path, 0, span.clone());
    let mut diags: Vec<_> = invisible.into_iter().collect();
    let msg = match res {
        // Accept any module as a recv root (both msg-desugared and manually defined).
        Ok(PathRes::Mod(ScopeId::Item(ItemKind::Mod(msg_mod)))) => RecvMsg::Module(msg_mod),
        Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) => {
            diags.push(
                BodyDiag::RecvExpectedMsgType {
                    primary: span.into(),
                    given: ty,
                }
                .into(),
            );
            RecvMsg::Invalid
        }
        // Other module-like scopes (e.g. top-level file modules) are reported
        // instead of accepted.
        Ok(other) => {
            if let Some(given) = msg_path.ident(db).to_opt() {
                let given_kind = match other {
                    PathRes::Mod(ScopeId::Item(ItemKind::TopMod(_))) => "file module",
                    _ => other.kind_name(),
                };
                diags.push(
                    BodyDiag::RecvExpectedMsgModule {
                        primary: span.into(),
                        given,
                        given_kind,
                    }
                    .into(),
                );
            }
            RecvMsg::Invalid
        }
        Err(err) => {
            diags.extend(
                err.into_diag(db, msg_path, span, ExpectedPathKind::Type)
                    .map(Into::into),
            );
            RecvMsg::Invalid
        }
    };
    RecvMsgResolution { msg, diags }
}

/// The msg variant a recv arm handles.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct RecvArmVariant<'db> {
    pub variant_struct: Struct<'db>,
    pub ty: TyId<'db>,
}

/// The path that a recv arm's variant path is resolved as, from the
/// contract's scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct RecvArmLookup<'db> {
    /// The arm's path or, for a single segment in a named recv block, that
    /// segment under the msg's path, so that `V` in `recv M { .. }` is `M::V`.
    pub path: PathId<'db>,
    /// The number of segments of `path` before the arm's first.
    pub offset: usize,
}

impl<'db> RecvArmLookup<'db> {
    /// The lookup's prefix that ends at the arm path's segment `idx`.
    pub fn segment(self, db: &'db dyn HirAnalysisDb, idx: usize) -> Option<PathId<'db>> {
        self.path.segment(db, self.offset + idx)
    }
}

/// A recv arm's variant path, resolved once for every use of it.
#[derive(Debug, Clone, Default, PartialEq, Eq, salsa::SalsaValue)]
pub struct RecvArmResolution<'db> {
    /// The path resolved for the arm, unless the arm names no variant or its
    /// block's msg doesn't resolve.
    pub lookup: Option<RecvArmLookup<'db>>,
    /// The msg variant the arm handles, if its path names one.
    pub variant: Option<RecvArmVariant<'db>>,
    /// The applications with const conditions that the path passes through,
    /// whose requirements are checked with the arm's body.
    pub applications: Vec<TyId<'db>>,
    /// What resolving the path reports.
    pub diags: Vec<FuncBodyDiag<'db>>,
}

/// Resolves the variant path of arm `arm_idx` of `contract`'s recv block
/// `recv_idx` as a type written in the arm, which must be a msg variant, and
/// one of the block's msg's in a named block.
#[salsa::tracked]
pub fn recv_arm_resolution<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    recv_idx: u32,
    arm_idx: u32,
) -> RecvArmResolution<'db> {
    let mut resolution = RecvArmResolution::default();
    let Some(recv) = contract.recvs(db).data(db).get(recv_idx as usize) else {
        return resolution;
    };
    // A named block's msg module, with the name its diagnostics give it.
    let named = match recv_msg(db, contract, recv_idx).msg {
        RecvMsg::Any => None,
        RecvMsg::Module(msg_mod) => match msg_mod.name(db).to_opt() {
            Some(msg_name) => Some((msg_mod, msg_name)),
            None => return resolution,
        },
        RecvMsg::Invalid => return resolution,
    };
    let Some((arm, path)) = recv
        .arms
        .data(db)
        .get(arm_idx as usize)
        .and_then(|arm| Some((arm, arm.variant_path(db)?)))
    else {
        return resolution;
    };
    let lookup = match (recv.msg_path, path.kind(db)) {
        (Some(msg_path), kind @ PathKind::Ident { .. })
            if named.is_some() && path.parent(db).is_none() =>
        {
            RecvArmLookup {
                path: msg_path.push(db, kind),
                offset: msg_path.len(db),
            }
        }
        _ => RecvArmLookup { path, offset: 0 },
    };
    resolution.lookup = Some(lookup);

    let arm_span = contract
        .span()
        .recv(recv_idx as usize)
        .arms()
        .arm(arm_idx as usize);
    let pat_span: DynLazySpan<'db> = arm_span.clone().pat().into();
    let path_span = recv_arm_path_span(db, arm, arm_span.pat());
    let (res, applications, invisible) =
        resolve_recv_path(db, contract, lookup.path, lookup.offset, path_span.clone());
    resolution.applications = applications;
    let diags = &mut resolution.diags;
    diags.extend(invisible);

    let ty = match (res, named) {
        (Ok(PathRes::Ty(ty)), _) => ty,
        // A named block reports any path that names no type as naming no
        // variant of its msg.
        (_, Some((_, msg_name))) => {
            diags.push(
                BodyDiag::RecvArmNotMsgVariant {
                    primary: pat_span,
                    msg_name,
                }
                .into(),
            );
            return resolution;
        }
        (Err(err), None) => {
            diags.extend(
                err.into_diag(db, path, path_span, ExpectedPathKind::Type)
                    .map(Into::into),
            );
            return resolution;
        }
        (Ok(res), None) => {
            if let Some(ident) = path.ident(db).to_opt() {
                let span = path_span.segment(path.segment_index(db));
                diags.push(PathResDiag::ExpectedType(span.into(), ident, res.kind_name()).into());
            }
            return resolution;
        }
    };

    // The arm's generic arguments are types written in the arm. An invalid
    // type that none of them reports is reported for the whole path, as a
    // written type is.
    let arg_diags = path_generic_arg_diags(
        db,
        path,
        path_span.clone(),
        Viewer::written_in(db, contract.scope()),
        PredicateListId::empty_list(db),
        false,
    );
    let args_reported = !arg_diags.is_empty();
    diags.extend(arg_diags.into_iter().map(Into::into));
    if ty.has_invalid(db) {
        if !args_reported {
            diags.extend(emit_invalid_ty_error(db, ty, path_span.into()).map(Into::into));
        }
        return resolution;
    }
    if !ty.is_star_kind(db) {
        diags.push(TyDiagCollection::from(TyLowerDiag::ExpectedStarKind(path_span.into())).into());
        return resolution;
    }

    let Some(variant_struct) = ty
        .adt_def(db)
        .and_then(|adt_def| match adt_def.adt_ref(db) {
            AdtRef::Struct(variant_struct) if implements_msg_variant(db, contract.scope(), ty) => {
                Some(variant_struct)
            }
            _ => None,
        })
    else {
        diags.push(
            BodyDiag::RecvArmNotMsgVariantTrait {
                primary: pat_span,
                given_ty: ty,
            }
            .into(),
        );
        return resolution;
    };
    if let Some((msg_mod, msg_name)) = named
        && variant_struct.scope().parent(db) != Some(ScopeId::Item(ItemKind::Mod(msg_mod)))
    {
        diags.push(
            BodyDiag::RecvArmNotVariantOfMsg {
                primary: pat_span,
                variant_ty: ty,
                msg_name,
            }
            .into(),
        );
        return resolution;
    }
    resolution.variant = Some(RecvArmVariant { variant_struct, ty });
    resolution
}

/// Resolves `path`, a path of `contract`'s recv blocks whose segments from
/// `offset` on are written at `path_span`, from the contract's scope. Returns
/// the resolution, the applications with const conditions that it passes
/// through, and the report of the first written segment that isn't visible
/// from the contract.
fn resolve_recv_path<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    path: PathId<'db>,
    offset: usize,
    path_span: LazyPathSpan<'db>,
) -> (
    Result<PathRes<'db>, PathResError<'db>>,
    Vec<TyId<'db>>,
    Option<FuncBodyDiag<'db>>,
) {
    let scope = contract.scope();
    let viewer = Viewer::written_in(db, scope);
    let mut invisible = None;
    let mut applications = Vec::new();
    let mut observe_segment =
        |segment: PathId<'db>, reso: &PathRes<'db>, quoted_visibility: Option<bool>| {
            applications.extend(constrained_applications(db, reso));
            if invisible.is_none()
                && segment.segment_index(db) >= offset
                && !quoted_visibility.unwrap_or_else(|| reso.is_visible_to(db, viewer))
            {
                invisible = Some((segment, reso.name_span(db)));
            }
        };
    let res = resolve_path_with_observer_and_minter(
        db,
        path,
        scope,
        PredicateListId::empty_list(db),
        false,
        &mut observe_segment,
        &LoweringContext::new(),
    );
    let invisible = invisible.and_then(|(segment, defined_at)| {
        let ident = segment.ident(db).to_opt()?;
        let span = path_span
            .segment(segment.segment_index(db) - offset)
            .ident();
        Some(PathResDiag::Invisible(span.into(), ident, defined_at).into())
    });
    (res, applications, invisible)
}

/// The span of the variant path of `arm`, whose pattern is at `pat_span`.
fn recv_arm_path_span<'db>(
    db: &'db dyn HirAnalysisDb,
    arm: &ContractRecvArm<'db>,
    pat_span: LazyPatSpan<'db>,
) -> LazyPathSpan<'db> {
    match arm.pat.data(db, arm.body) {
        Partial::Present(Pat::Record(..)) => pat_span.into_record_pat().path(),
        Partial::Present(Pat::PathTuple(..)) => pat_span.into_path_tuple_pat().path(),
        _ => pat_span.into_path_pat().path(),
    }
}

/// Checks `contract`'s recv block `recv_idx`: its msg and arm paths, the
/// variants its arms handle, and, for a named block, that they handle each
/// of its msg's variants.
#[salsa::tracked]
pub fn check_contract_recv_block<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    recv_idx: u32,
) -> Vec<FuncBodyDiag<'db>> {
    let msg_resolution = recv_msg(db, contract, recv_idx);
    let mut diags = msg_resolution.diags.clone();
    let msg = msg_resolution.msg;
    let Some(recv) = contract.recvs(db).data(db).get(recv_idx as usize) else {
        return diags;
    };
    // The msg's own diagnostics cover a block whose msg doesn't resolve.
    if msg == RecvMsg::Invalid {
        return diags;
    }

    let recv_span = contract.span().recv(recv_idx as usize);
    let assumptions = PredicateListId::empty_list(db);

    // Use TyId for duplicate detection to correctly handle generic types
    let mut seen = FxHashMap::<TyId<'db>, DynLazySpan<'db>>::default();
    // Use Struct for exhaustiveness checking (tracks which base structs are covered)
    let mut covered = FxHashSet::<Struct<'db>>::default();
    let mut checked_decode = FxHashSet::<Struct<'db>>::default();

    for (arm_idx, arm) in recv.arms.data(db).iter().enumerate() {
        let arm_span = recv_span.clone().arms().arm(arm_idx);
        let pat_span: DynLazySpan<'db> = arm_span.clone().pat().into();

        if arm.is_fallback(db) {
            if matches!(msg, RecvMsg::Module(_)) {
                diags.push(
                    BodyDiag::RecvFallbackOnlyInBareBlock {
                        primary: pat_span.clone(),
                    }
                    .into(),
                );
            }
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

        let resolution = recv_arm_resolution(db, contract, recv_idx, arm_idx as u32);
        diags.extend(resolution.diags.iter().cloned());
        let Some(variant) = resolution.variant else {
            continue;
        };
        let Some(ident) = variant.variant_struct.name(db).to_opt() else {
            continue;
        };

        if let Some(first_span) = seen.get(&variant.ty) {
            diags.push(
                BodyDiag::RecvArmDuplicateVariant {
                    primary: pat_span.clone(),
                    first_use: first_span.clone(),
                    variant: ident,
                }
                .into(),
            );
        } else {
            seen.insert(variant.ty, pat_span.clone());
        }

        covered.insert(variant.variant_struct);
        if checked_decode.insert(variant.variant_struct) {
            check_recv_variant_param_types_decodable(
                db,
                contract,
                RecvArmView::new(db, RecvView::new(db, contract, recv_idx), arm_idx as u32),
                variant.ty,
                pat_span,
                assumptions,
                &mut diags,
            );
        }
    }

    // Check for missing variants (exhaustiveness)
    if let RecvMsg::Module(msg_mod) = msg {
        let missing: Vec<_> = msg_variants(db, msg_mod)
            .filter(|variant| !covered.contains(variant))
            .filter_map(|variant| variant.name(db).to_opt())
            .collect();
        if !missing.is_empty() {
            diags.push(
                BodyDiag::RecvMissingMsgVariants {
                    primary: recv_span.path().into(),
                    variants: missing,
                }
                .into(),
            );
        }
    }

    diags
}

#[salsa::tracked]
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

    let abi = resolve_sol_abi_ty(db, contract.scope(), PredicateListId::empty_list(db));

    for (idx, recv) in contract.recvs(db).data(db).iter().enumerate() {
        let recv_span = contract.span().recv(idx);

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

        let named = match recv_msg(db, contract, idx as u32).msg {
            RecvMsg::Module(msg_mod) => {
                let Some(msg_name) = msg_mod.name(db).to_opt() else {
                    continue;
                };
                let path_span: DynLazySpan<'db> = recv_span.clone().path().into();
                if let Some((first_span, first_name)) = seen_msg_blocks.get(&msg_mod) {
                    diags.push(
                        BodyDiag::RecvDuplicateMsgBlock {
                            primary: path_span,
                            first_use: first_span.clone(),
                            msg_name: *first_name,
                        }
                        .into(),
                    );
                    // Skip handler/selector conflict checks for duplicate msg blocks
                    continue;
                }
                seen_msg_blocks.insert(msg_mod, (path_span, msg_name));
                true
            }
            RecvMsg::Any => false,
            RecvMsg::Invalid => continue,
        };

        // Check for selector and handler conflicts across the variants the
        // block's arms handle. The block's own check reports an arm that
        // handles a variant an earlier arm of the block handles.
        let mut handled = FxHashSet::<TyId<'db>>::default();
        for arm_idx in 0..recv.arms.data(db).len() {
            let Some(variant) =
                recv_arm_resolution(db, contract, idx as u32, arm_idx as u32).variant
            else {
                continue;
            };
            let Some(variant_name) = variant.variant_struct.name(db).to_opt() else {
                continue;
            };
            if !handled.insert(variant.ty) {
                continue;
            }
            // A named block's variants are reported where its msg declares
            // them, and a bare block's where its arms name them.
            let span: DynLazySpan<'db> = if named {
                variant.variant_struct.span().name().into()
            } else {
                recv_span.clone().arms().arm(arm_idx).pat().into()
            };

            // Check selector conflicts, among the selectors of the ABI that
            // the blocks answer to
            if let Some(selector) = abi.and_then(|abi| {
                eval_msg_variant_selector(db, variant.ty, abi, contract.scope(), &mut diags)
            }) {
                check_selector_conflict(
                    selector,
                    variant.variant_struct,
                    variant_name,
                    span.clone(),
                    &mut seen_selectors,
                    &mut diags,
                );
            }

            // Check handler type conflicts
            check_handler_conflict(variant.ty, span, &mut seen_handlers, &mut diags);
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
    abi: TyId<'db>,
    scope: ScopeId<'db>,
    diags: &mut Vec<FuncBodyDiag<'db>>,
) -> Option<u32> {
    let impl_ = msg_variant_impl(db, scope, variant_ty, abi)?;

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
        GenericSubst::none(db),
    ) {
        EvalOutcome::Ready(value) => {
            if let SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } = value.value(db)
            {
                value.to_u32()
            } else {
                diags.push(BodyDiag::ConstValueMustBeKnown(body.span().into()).into());
                None
            }
        }
        EvalOutcome::Blocked(_) | EvalOutcome::Failed(_) => {
            diags.push(BodyDiag::ConstValueMustBeKnown(body.span().into()).into());
            None
        }
    }
}

#[salsa::tracked]
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

/// The root slot a contract field's `#[slot(e)]` names: `e` evaluated as a
/// `u256`, or the diagnostics explaining why it is not one.
pub(crate) fn contract_field_slot<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
) -> Result<U256, Vec<FuncBodyDiag<'db>>> {
    let u256 = TyId::u256(db);
    let diags = &check_anon_const_body(db, body, u256).0;
    if !diags.is_empty() {
        return Err(diags.clone());
    }
    let diags = const_body_ctfe_diags(db, body, u256, ConstUsePolicy::RequireValue);
    if !diags.is_empty() {
        return Err(diags);
    }
    let owner = BodyOwner::AnonConstBody {
        body,
        expected: u256,
    };
    if let EvalOutcome::Ready(value) = eval_body_owner_const(db, owner, GenericSubst::none(db))
        && let SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } = value.value(db)
        && let Some(value) = value.to_biguint()
        && let Some(slot) = U256::try_from_be_slice(&value.to_bytes_be())
    {
        Ok(slot)
    } else {
        Err(vec![FuncBodyDiag::Ty(
            TyLowerDiag::ContractFieldSlotNotConst {
                span: body.span().into(),
            }
            .into(),
        )])
    }
}

/// Reports the `#[slot(e)]` arguments of `contract`'s fields that are not
/// constant `u256`s.
#[salsa::tracked]
pub fn check_contract_field_slots<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    FieldParent::Contract(contract)
        .fields(db)
        .filter_map(|field| field.slot(db))
        .flat_map(|body| contract_field_slot(db, body).err().unwrap_or_default())
        .collect()
}

#[salsa::tracked]
pub fn check_contract_field_address_spaces<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    valid_contract_fields(db, contract)
        .filter(|field| {
            matches!(
                field.address_space,
                AddressSpace::Memory | AddressSpace::Calldata
            )
        })
        .map(|field| {
            BodyDiag::UnsupportedContractFieldAddressSpace {
                primary: FieldView {
                    parent: FieldParent::Contract(contract),
                    idx: field.field.index as usize,
                }
                .ty_span(),
                field: field.name,
                space: field.address_space,
            }
            .into()
        })
        .collect()
}

fn valid_contract_fields<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> impl Iterator<Item = &'db ValidatedFieldLayoutPlan<'db>> + 'db {
    contract
        .storage_layout(db)
        .valid_fields()
        .filter(|field| !field.declared.has_invalid(db))
        .filter(move |field| {
            FieldView {
                parent: FieldParent::Contract(contract),
                idx: field.field.index as usize,
            }
            .ty_diags(db)
            .is_empty()
        })
}

#[salsa::tracked]
pub fn check_contract_immutable_fields_initialized<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let required_fields = valid_contract_fields(db, contract)
        .filter(|field| field.slot_count != 0 && field.address_space == AddressSpace::Code)
        .cloned()
        .collect::<Vec<_>>();
    if required_fields.is_empty() {
        return Vec::new();
    }

    let required = required_fields
        .iter()
        .map(|field| field.field.index)
        .collect::<FxHashSet<_>>();
    let init = contract.init(db);
    let missing = if init.is_some() {
        let (init_diags, typed_body) = check_contract_init_body(db, contract);
        if !init_diags.is_empty() {
            // Type errors in `init` already block compilation; don't pile
            // missing-field diagnostics on top of a body we can't analyze.
            FxHashSet::default()
        } else if typed_body.body().is_some() {
            match contract_init_assigned_fields(db, contract) {
                Ok(Some(assigned)) => required
                    .iter()
                    .copied()
                    .filter(|field| !assigned.contains(field))
                    .collect(),
                // No normal exit is reachable, so `init` can never complete
                // and the contract can never be deployed with unset fields.
                Ok(None) => FxHashSet::default(),
                // Admission diagnostics are emitted by the semantic analysis
                // pass; do not duplicate them as missing-field errors.
                Err(_) => FxHashSet::default(),
            }
        } else {
            required
        }
    } else {
        required
    };

    let init_span = init.map(|_| contract.span().init_block().body().into());
    required_fields
        .into_iter()
        .filter_map(|field| {
            missing.contains(&field.field.index).then(|| {
                BodyDiag::ImmutableContractFieldNotInitialized {
                    primary: FieldParent::Contract(contract)
                        .field_name_span(field.field.index as usize),
                    field: field.name,
                    init: init_span.clone(),
                }
                .into()
            })
        })
        .collect()
}

#[salsa::tracked]
pub fn check_contract_init_body<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    check_body(db, BodyOwner::ContractInit { contract })
}

/// Gets the Return type from a type's `MsgVariant<abi>` implementation.
/// Returns `None` if the type has none for `abi`.
pub(super) fn get_msg_variant_return_type<'db>(
    db: &'db dyn HirAnalysisDb,
    variant_ty: TyId<'db>,
    abi: TyId<'db>,
    scope: ScopeId<'db>,
) -> Option<TyId<'db>> {
    let msg_variant_impl = msg_variant_impl(db, scope, variant_ty, abi)?;

    // Get the Return associated type from the impl
    let return_name = IdentId::new(db, "Return".to_string());
    msg_variant_impl.assoc_ty(db, return_name)
}

/// A step of the entry code of a contract that cannot be generated. The entry
/// plan holds the instances, and the lowering of the contract reads the same
/// plan, so every step that lowering would fail is reported here, also where
/// a check of the message or of an implementation reports the cause as well.
pub(crate) struct ContractEntryFailure<'db> {
    contract_name: String,
    span: DynLazySpan<'db>,
    message: String,
    /// Another place that the failure is about, with what it is.
    secondary: Option<(DynLazySpan<'db>, &'static str)>,
}

impl DiagnosticVoucher for ContractEntryFailure<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let primary = self.span.resolve(db);
        let mut labels = vec![SubDiagnostic::new(
            LabelStyle::Primary,
            self.message.clone(),
            primary.clone(),
        )];
        if let Some((span, what)) = &self.secondary {
            let span = span.resolve(db);
            let note = foreign_ingot_note(db, primary.as_ref(), span.as_ref());
            labels.push(SubDiagnostic::new(
                LabelStyle::Secondary,
                format!("{what}{note}"),
                span,
            ));
        }
        CompleteDiagnostic::new(
            Severity::Error,
            format!(
                "the entry code of contract `{}` cannot be generated",
                self.contract_name
            ),
            labels,
            Vec::new(),
            GlobalErrorCode::new(DiagnosticPass::SemanticAccess, 24),
        )
    }
}

/// The steps of the entry code of `contract` that have no instance to run.
pub(crate) fn contract_entry_failures<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> Vec<ContractEntryFailure<'db>> {
    let contract_name = contract
        .name(db)
        .to_opt()
        .map_or("<unnamed>", |name| name.data(db))
        .to_string();
    let plan = contract_entry(db, contract);
    let span = contract.span();
    let mut failures = Vec::new();
    let mut fail = |span, message, secondary| {
        failures.push(ContractEntryFailure {
            contract_name: contract_name.clone(),
            span,
            message,
            secondary,
        })
    };
    if let Err(error) = &plan.init {
        let params = span.clone().init_block().params();
        for (arg, text) in error.parts() {
            match arg {
                Some(idx) => fail(
                    params.clone().param(idx).into(),
                    format!("the dispatcher cannot decode this `init` argument: {text}"),
                    None,
                ),
                None => fail(
                    params.clone().into(),
                    format!("the dispatcher cannot decode the arguments of `init`: {text}"),
                    None,
                ),
            }
        }
    }
    for arm in &plan.arms {
        let arm_span = span
            .clone()
            .recv(arm.recv_idx as usize)
            .arms()
            .arm(arm.arm_idx as usize);
        if let Err(error) = &arm.input {
            for (_, text) in error.parts() {
                fail(
                    arm_span.clone().pat().into(),
                    format!("the dispatcher cannot read the message of this arm: {text}"),
                    None,
                );
            }
        }
        if let Err(error) = &arm.ret {
            let declared = RecvView::new(db, contract, arm.recv_idx)
                .arms(db)
                .find(|view| view.arm_idx(db) == arm.arm_idx)
                .and_then(|view| view.return_decl_span(db));
            let pat_span: DynLazySpan<'db> = arm_span.clone().pat().into();
            for (_, text) in error.parts() {
                let message = format!("the dispatcher cannot encode this return type: {text}");
                match &declared {
                    // A declaration in the contract's own ingot is where the
                    // error is, with the arm as the secondary label. In
                    // another ingot, the arm stays primary.
                    Some(declared)
                        if declared.top_mod(db).map(|top_mod| top_mod.ingot(db))
                            == Some(contract.top_mod(db).ingot(db)) =>
                    {
                        fail(
                            declared.clone(),
                            message,
                            Some((pat_span.clone(), "returned by this arm")),
                        )
                    }
                    Some(declared) => {
                        let written = contract.recvs(db).data(db)[arm.recv_idx as usize]
                            .arms
                            .data(db)[arm.arm_idx as usize]
                            .ret_ty
                            .is_some();
                        let primary = if written {
                            arm_span.clone().ret_ty().into()
                        } else {
                            pat_span.clone()
                        };
                        fail(
                            primary,
                            message,
                            Some((declared.clone(), "the `Return` type is declared here")),
                        )
                    }
                    None => fail(pat_span.clone(), message, None),
                }
            }
        }
    }
    failures
}
