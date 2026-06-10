mod callable;
mod contract;
mod effect_env;
pub(crate) mod env;
mod expr;
mod owner;
mod pat;
mod path;
mod stmt;

pub(crate) use self::contract::eval_msg_variant_selector;
pub use self::contract::{
    ResolvedRecvVariant, VariantResError, check_contract_immutable_fields_initialized,
    check_contract_init_body, check_contract_recv_arm_body, check_contract_recv_block,
    check_contract_recv_blocks, resolve_variant_bare, resolve_variant_in_msg,
};
pub use self::path::RecordLike;
use crate::analysis::name_resolution::ResolvedVariant;
pub use crate::analysis::ty::ProviderAddressSpace;
use crate::analysis::ty::binder::Binder;
use crate::analysis::ty::corelib::resolve_lib_type_path;
use crate::analysis::ty::fold::TyFoldable;
use crate::analysis::ty::provider::{ProviderKind, provider_semantics};
use crate::analysis::ty::visitor::TyVisitable;
use crate::hir_def::CallableDef;
use crate::{
    hir_def::{
        BinOp, Body, Const, Contract, ContractRecvArm, Expr, ExprId, Func, GenericParam,
        GenericParamOwner, ItemKind, LitKind, ManualContractRootAttr, Partial, Pat, PatId, PathId,
        StaticAssert, StaticAssertComparison, Stmt, StmtId, StringId, TypeBound, TypeId as HirTyId,
        WhereClauseOwner,
    },
    span::{
        DynLazySpan, expr::LazyExprSpan, pat::LazyPatSpan, path::LazyPathSpan, types::LazyTySpan,
    },
    visitor::{Visitor, VisitorCtxt, walk_expr, walk_pat},
};
use callable::{CallGenericArgUnifyError, unify_explicit_call_generic_args};
pub use callable::{Callable, EffectProviderProvenance, EffectProviderSpecialization};
use common::indexmap::IndexMap;
use cranelift_entity::{PrimaryMap, SecondaryMap, entity_impl, packed_option::PackedOption};
use ena::unify::InPlace;
use env::TyCheckEnv;
pub use env::{
    EffectParamSite, ExprProp, LocalBinding, ParamSite, PatBindingMode, PathReadSemantics,
};
pub(super) use expr::TraitOps;
pub use owner::BodyOwner;
pub use owner::EffectParamOwner;
pub use stmt::ForLoopSeq;

use rustc_hash::FxHashSet;
use salsa::Update;

use crate::analysis::place::{Place, PlaceBase};

use super::{
    assoc_const::AssocConstUse,
    canonical::Canonical,
    diagnostics::{
        BodyDiag, CallConstraintDiagInfo, FuncBodyDiag, StaticAssertComparisonValues,
        TraitConstraintDiag, TyDiagCollection, TyLowerDiag,
    },
    effects::{EffectKeyKind, ResolvedEffectKey, resolve_effect_key},
    trait_def::TraitInstId,
    trait_resolution::{
        CanonicalGoalQuery, GoalSatisfiability, PredicateListId, TraitSolveCx,
        is_goal_query_satisfiable, is_goal_satisfiable,
    },
    ty_contains_const_hole,
    ty_def::{
        BorrowKind, CapabilityKind, InvalidCause, Kind, MAX_INLINE_STRING_BYTES, StringFallback,
        TyId, TyVarSort,
    },
    ty_lower::{lower_hir_ty, resolve_callable_input_effect_key},
    unify::{InferenceKey, Snapshot, UnificationError, UnificationTable},
};
use crate::analysis::semantic::SemanticCodeRegionRef;
use crate::analysis::semantic::{SemConstId, SemConstScalar, SemConstValue, eval_body_owner_const};
use crate::analysis::ty::ty_def::{TyBase, TyData};
use crate::analysis::ty::{
    const_ty::{ConstTyData, invalid_cause_from_ctfe_error},
    effect_handle_metadata,
    fold::AssocTySubst,
    normalize::normalize_ty,
    pattern_ir::{PatternAnalysisStatus, PatternStore, ValidatedPatId},
    pattern_types::{
        PatternDestructureMode, apply_pattern_borrow_mode, destructure_pattern_source,
    },
    ty_error::collect_ty_lower_errors,
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{
        PathRes, PathResError, diagnostics::PathResDiag, resolve_path_with_observer,
    },
    ty::{
        ty_def::{TyFlags, inference_keys},
        visitor::collect_flags,
    },
};

#[salsa::tracked(return_ref)]
pub fn check_func_body<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    check_body(db, BodyOwner::Func(func))
}

#[salsa::tracked(return_ref)]
pub fn check_const_body<'db>(
    db: &'db dyn HirAnalysisDb,
    const_: Const<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    check_body(db, BodyOwner::Const(const_))
}

#[salsa::tracked(return_ref)]
pub fn check_anon_const_body<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expected: TyId<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    check_body(db, BodyOwner::AnonConstBody { body, expected })
}

#[salsa::tracked(return_ref)]
pub fn check_static_assert<'db>(
    db: &'db dyn HirAnalysisDb,
    assert_: StaticAssert<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let condition = assert_.condition(db);
    let expected = TyId::bool(db);
    let owner = BodyOwner::AnonConstBody {
        body: condition,
        expected,
    };
    let (body_diags, typed_body) = check_anon_const_body(db, condition, expected);
    let ignorable_body_diags = static_assert_ignorable_type_diags(db, body_diags);
    if !body_diags.is_empty() && !ignorable_body_diags {
        return body_diags.clone();
    }

    match eval_body_owner_const(db, owner, Vec::new()) {
        Ok(value) => match static_assert_bool_value(db, value) {
            Some(true) => {}
            Some(false) => {
                let mut diags = body_diags.clone();
                let comparison = if body_diags.is_empty() {
                    assert_.comparison(db).and_then(|comparison| {
                        static_assert_comparison_values(db, condition, typed_body, comparison)
                    })
                } else {
                    None
                };
                diags.push(
                    BodyDiag::StaticAssertFailed {
                        primary: condition.span().into(),
                        comparison,
                    }
                    .into(),
                );
                return diags;
            }
            None => {
                return vec![BodyDiag::ConstValueMustBeKnown(condition.span().into()).into()];
            }
        },
        Err(crate::analysis::semantic::CtfeError::NotConstEvaluable { .. }) => {
            return vec![BodyDiag::ConstValueMustBeKnown(condition.span().into()).into()];
        }
        Err(err) => {
            let ty = TyId::invalid(db, invalid_cause_from_ctfe_error(db, owner, err));
            if let Some(diag) = ty.emit_diag(db, condition.span().into()) {
                return vec![diag.into()];
            }
        }
    }

    Vec::new()
}

fn static_assert_bool_value<'db>(
    db: &'db dyn HirAnalysisDb,
    value: SemConstId<'db>,
) -> Option<bool> {
    let SemConstValue::Scalar {
        value: SemConstScalar::Bool(value),
        ..
    } = value.value(db)
    else {
        return None;
    };
    Some(value)
}

fn static_assert_ignorable_type_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    diags: &[FuncBodyDiag<'db>],
) -> bool {
    !diags.is_empty()
        && diags.iter().all(|diag| {
            matches!(
                diag,
                FuncBodyDiag::Body(BodyDiag::TypeAnnotationNeeded { ty, .. })
                    if ty.is_integral_var(db)
            )
        })
}

fn static_assert_comparison_values<'db>(
    db: &'db dyn HirAnalysisDb,
    condition: Body<'db>,
    typed_body: &TypedBody<'db>,
    comparison: StaticAssertComparison<'db>,
) -> Option<StaticAssertComparisonValues> {
    let Partial::Present(Expr::Bin(lhs_expr, rhs_expr, BinOp::Comp(op))) =
        condition.expr(db).data(db, condition)
    else {
        return None;
    };
    if *op != comparison.op {
        return None;
    }

    let lhs = eval_static_assert_comparison_operand(
        db,
        comparison.lhs,
        typed_body.expr_ty(db, *lhs_expr),
    )?;
    let rhs = eval_static_assert_comparison_operand(
        db,
        comparison.rhs,
        typed_body.expr_ty(db, *rhs_expr),
    )?;

    Some(StaticAssertComparisonValues {
        op: comparison.op,
        lhs: lhs.pretty_print(db),
        rhs: rhs.pretty_print(db),
    })
}

fn eval_static_assert_comparison_operand<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expected: TyId<'db>,
) -> Option<SemConstId<'db>> {
    if expected.has_invalid(db) {
        return None;
    }
    let owner = BodyOwner::AnonConstBody { body, expected };
    let body_diags = &check_anon_const_body(db, body, expected).0;
    if !body_diags.is_empty() && !static_assert_ignorable_type_diags(db, body_diags) {
        return None;
    }
    eval_body_owner_const(db, owner, Vec::new()).ok()
}

pub(super) fn check_body<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    let Ok(mut checker) = TyChecker::new(db, owner) else {
        return (
            Vec::new(),
            match owner {
                BodyOwner::Func(func) => typed_body_for_bodyless_func(db, func),
                BodyOwner::Const(_)
                | BodyOwner::AnonConstBody { .. }
                | BodyOwner::ContractInit { .. }
                | BodyOwner::ContractRecvArm { .. } => TypedBody::empty(db),
            },
        );
    };

    checker.run();
    let (mut diags, typed_body) = checker.finish();
    if let BodyOwner::Func(func) = owner
        && func.is_const(db)
        && !func.is_extern(db)
    {
        diags.extend(crate::analysis::ty::const_check::check_const_fn_body(
            db,
            func,
            &typed_body,
        ));
    }

    if let BodyOwner::Const(const_) = owner
        && diags.is_empty()
        && let Some(body) = const_.body(db).to_opt()
        && !const_.ty(db).has_invalid(db)
    {
        let const_owner = BodyOwner::AnonConstBody {
            body,
            expected: const_.ty(db),
        };
        match eval_body_owner_const(db, const_owner, Vec::new()) {
            Ok(value) => {
                if matches!(value.value(db), SemConstValue::TypeLevel { .. }) {
                    diags.push(BodyDiag::ConstValueMustBeKnown(body.span().into()).into());
                }
            }
            Err(crate::analysis::semantic::CtfeError::NotConstEvaluable { .. }) => {
                diags.push(BodyDiag::ConstValueMustBeKnown(body.span().into()).into());
            }
            Err(err) => {
                let ty = TyId::invalid(db, invalid_cause_from_ctfe_error(db, const_owner, err));
                if let Some(diag) = ty.emit_diag(db, body.span().into()) {
                    diags.push(diag.into());
                }
            }
        }
    }

    (diags, typed_body)
}

fn typed_body_for_bodyless_func<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> TypedBody<'db> {
    let assumptions = crate::semantic::func_body_assumptions(db, func).extend_all_bounds(db);
    let mut result_ty = func.return_ty(db);
    if !result_ty.is_star_kind(db) || ty_contains_const_hole(db, result_ty) {
        result_ty = TyId::invalid(db, InvalidCause::Other);
    }
    let param_bindings = func
        .params(db)
        .enumerate()
        .map(|(idx, view)| {
            let mut ty = *func
                .arg_tys(db)
                .get(idx)
                .map(|binder| binder.skip_binder())
                .unwrap_or(&TyId::invalid(db, InvalidCause::ParseError));
            if !ty.is_star_kind(db) || (!view.is_self_param(db) && ty_contains_const_hole(db, ty)) {
                ty = TyId::invalid(db, InvalidCause::Other);
            }
            LocalBinding::Param {
                site: ParamSite::Func(func),
                idx,
                mode: view.mode(db),
                ty,
                is_mut: view.is_mut(db),
            }
        })
        .collect();
    TypedBody {
        body: None,
        result_ty,
        assumptions,
        pat_ty: SecondaryMap::new(),
        expr_ty: SecondaryMap::new(),
        implicit_moves: FxHashSet::default(),
        const_refs: SecondaryMap::new(),
        value_path_refs: SecondaryMap::new(),
        semantic_expr_lowering: SecondaryMap::new(),
        record_init_lowering: SecondaryMap::new(),
        resolved_field_index: SecondaryMap::new(),
        call_effect_args: SecondaryMap::new(),
        return_borrow_provider: None,
        param_bindings,
        pat_bindings: SecondaryMap::new(),
        pat_binding_modes: SecondaryMap::new(),
        pattern_store: PatternStore::default(),
        pattern_status: SecondaryMap::with_default(PatternAnalysisStatus::Invalid),
        for_loop_seq: SecondaryMap::new(),
        expr_place: SecondaryMap::new(),
        expr_places: PrimaryMap::new(),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BindingInterfaceShape<'db> {
    OrdinaryValue,
    ProviderValue {
        kind: ProviderKind,
        value_ty: TyId<'db>,
    },
    PlaceCarrier {
        value_ty: TyId<'db>,
    },
    DirectCarrier {
        target_ty: TyId<'db>,
    },
}

pub struct TyChecker<'db> {
    pub(crate) db: &'db dyn HirAnalysisDb,
    pub(crate) env: TyCheckEnv<'db>,
    pub(crate) table: UnificationTable<'db>,
    expected: TyId<'db>,
    effect_provider_keys: FxHashSet<InferenceKey<'db>>,
    first_return_borrow_provider: Option<(DynLazySpan<'db>, ProviderAddressSpace)>,
    diags: Vec<FuncBodyDiag<'db>>,
}

pub(crate) struct TyCheckerSnapshot<'db> {
    table: Snapshot<InPlace<InferenceKey<'db>>>,
    deferred_len: usize,
}

enum TraitObligationOutcome<'db> {
    Discharged,
    Progressed,
    Requeue(env::TraitObligation<'db>),
}

enum CallConstraintBoundOwner<'db> {
    GenericParam(GenericParamOwner<'db>, usize, usize),
    WherePredicate(WhereClauseOwner<'db>, usize, usize),
}

impl<'db> TyChecker<'db> {
    fn string_literal_fallback(&self) -> StringFallback {
        StringFallback::Fixed
    }

    fn new(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> Result<Self, ()> {
        let env = TyCheckEnv::new(db, owner)?;
        let expected = env.compute_expected_return();
        let mut checker = Self::new_internal(db, env, expected);
        checker.seed_effect_witnesses();
        Ok(checker)
    }

    fn run(&mut self) {
        self.check_effect_param_keys_resolve();
        self.check_own_param_types();

        if let BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } = self.env.owner()
        {
            let recv_span = self.env.owner().recv_span().unwrap();
            let arm_span = self.env.owner().recv_arm_span().unwrap();
            let arm = contract
                .recv_arm(self.db, recv_idx as usize, arm_idx as usize)
                .expect("recv arm exists");
            let msg_path = contract
                .recvs(self.db)
                .data(self.db)
                .get(recv_idx as usize)
                .and_then(|r| r.msg_path);
            let (pat_ty, ret_ty) =
                self.resolve_recv_arm_types(contract, msg_path, arm, recv_span.path(), arm_span);
            self.expected = ret_ty;
            self.check_pat(arm.pat, pat_ty);
            self.seed_pat_bindings(arm.pat);
            self.env.flush_pending_bindings();
        }

        let root_expr = self.env.body().expr(self.db);
        self.check_expr(root_expr, self.expected);
        self.record_implicit_move_for_owned_expr(root_expr, self.expected);
    }

    fn check_own_param_types(&mut self) {
        match self.env.owner() {
            BodyOwner::Func(_) => {}
            BodyOwner::ContractInit { contract } => {
                let Some(init) = contract.init(self.db) else {
                    return;
                };
                let scope = self.env.scope();
                let assumptions = self.env.assumptions();
                for (idx, param) in init.params(self.db).data(self.db).iter().enumerate() {
                    let Some(hir_ty) = param.ty.to_opt() else {
                        continue;
                    };
                    let ty = lower_hir_ty(self.db, hir_ty, scope, assumptions);

                    if ty_contains_const_hole(self.db, ty) {
                        self.push_diag(TyDiagCollection::from(
                            TyLowerDiag::ConstHoleInValuePosition {
                                span: contract.span().init_block().params().param(idx).ty().into(),
                                ty,
                            },
                        ));
                        continue;
                    }

                    if param.mode != crate::hir_def::params::FuncParamMode::Own {
                        continue;
                    }

                    if ty.as_borrow(self.db).is_some() {
                        self.push_diag(BodyDiag::OwnParamCannotBeBorrow {
                            primary: contract.span().init_block().params().param(idx).ty().into(),
                            ty,
                        });
                    }
                }
            }
            BodyOwner::Const(_)
            | BodyOwner::AnonConstBody { .. }
            | BodyOwner::ContractRecvArm { .. } => {}
        }
    }

    fn check_effect_param_keys_resolve(&mut self) {
        match self.env.owner() {
            BodyOwner::Func(func) => self.check_free_func_effect_list(func, func.effects(self.db)),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => {}
            owner @ BodyOwner::ContractInit { contract } => {
                self.check_contract_scoped_effect_list(owner, contract, owner.effects(self.db));
            }
            owner @ BodyOwner::ContractRecvArm { contract, .. } => {
                self.check_contract_scoped_effect_list(owner, contract, owner.effects(self.db));
            }
        }
    }

    fn check_free_func_effect_list(
        &mut self,
        func: Func<'db>,
        effects: crate::hir_def::EffectParamListId<'db>,
    ) {
        for (idx, effect) in effects.data(self.db).iter().enumerate() {
            let Some(key_path) = effect
                .key_path
                .to_opt()
                .filter(|path| path.ident(self.db).is_present())
            else {
                continue;
            };

            if !matches!(
                resolve_callable_input_effect_key(
                    self.db,
                    func,
                    idx,
                    key_path,
                    self.env.assumptions(),
                ),
                ResolvedEffectKey::Type(_) | ResolvedEffectKey::Trait(_)
            ) {
                self.push_diag(BodyDiag::InvalidEffectKey {
                    owner: EffectParamOwner::Func(func),
                    key: key_path,
                    idx,
                });
            }
        }
    }

    fn check_contract_scoped_effect_list(
        &mut self,
        owner: BodyOwner<'db>,
        contract: Contract<'db>,
        effects: crate::hir_def::EffectParamListId<'db>,
    ) {
        let (owner, site) = match owner {
            BodyOwner::Func(func) => (EffectParamOwner::Func(func), EffectParamSite::Func(func)),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => unreachable!(),
            BodyOwner::ContractInit { contract } => (
                EffectParamOwner::ContractInit { contract },
                EffectParamSite::ContractInit { contract },
            ),
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => (
                EffectParamOwner::ContractRecvArm {
                    contract,
                    recv_idx,
                    arm_idx,
                },
                EffectParamSite::ContractRecvArm {
                    contract,
                    recv_idx,
                    arm_idx,
                },
            ),
        };
        let view = crate::core::semantic::EffectEnvView::new(site);

        let assumptions = PredicateListId::empty_list(self.db);
        let root_effect_ty = crate::analysis::ty::registered_root_providers(
            self.db,
            EffectParamSite::Contract(contract),
        )
        .first()
        .map(|registration| registration.provider_ty);

        for (idx, effect) in effects.data(self.db).iter().enumerate() {
            let Some(key_path) = effect
                .key_path
                .to_opt()
                .filter(|path| path.ident(self.db).is_present())
            else {
                continue;
            };

            // Labeled effects are always type/trait keyed: `name: Type`.
            if effect.name.is_some() {
                match resolve_effect_key(self.db, key_path, contract.scope(), assumptions) {
                    ResolvedEffectKey::Trait(schema) => {
                        let Some(root_effect_ty) = root_effect_ty else {
                            continue;
                        };
                        let trait_req = super::instantiate_trait_self(
                            self.db,
                            schema.into_trait_inst(self.db),
                            root_effect_ty,
                        );
                        if matches!(
                            is_goal_satisfiable(
                                self.db,
                                TraitSolveCx::new(self.db, contract.scope()),
                                trait_req
                            ),
                            GoalSatisfiability::UnSat(_) | GoalSatisfiability::ContainsInvalid
                        ) {
                            self.push_diag(BodyDiag::ContractRootEffectTraitNotImplemented {
                                owner,
                                idx,
                                root_ty: root_effect_ty,
                                trait_req,
                            });
                        }
                    }
                    ResolvedEffectKey::Type(schema) => {
                        let given =
                            normalize_ty(self.db, schema.carrier, contract.scope(), assumptions);
                        if !given.is_zero_sized(self.db) {
                            self.push_diag(BodyDiag::ContractRootEffectTypeNotZeroSized {
                                owner,
                                key: key_path,
                                idx,
                                given,
                            });
                        }
                    }
                    ResolvedEffectKey::Invalid | ResolvedEffectKey::Other => {
                        self.push_diag(BodyDiag::InvalidEffectKey {
                            owner,
                            key: key_path,
                            idx,
                        });
                    }
                }
                continue;
            }

            match view.resolved_binding(self.db, idx) {
                Some(binding)
                    if matches!(
                        binding.provider.source,
                        crate::core::semantic::ProviderSource::ContractField { .. }
                            | crate::core::semantic::ProviderSource::RootProvider { .. }
                    ) =>
                {
                    // Contract fields are immutable unless declared `mut`. The
                    // one exception — assigning immutable code-backed fields in
                    // `init` — is already reflected in the provider's
                    // site-specific mutability.
                    if binding.requirement.is_mut
                        && !binding.provider.is_mut
                        && let crate::core::semantic::ProviderSource::ContractField {
                            contract: field_contract,
                            field_idx,
                        } = binding.provider.source
                    {
                        self.push_diag(BodyDiag::ImmutableContractFieldMutBinding {
                            primary: owner.effect_param_path_span(self.db, idx),
                            field: binding.requirement.binding_name,
                            field_span: crate::hir_def::FieldParent::Contract(field_contract)
                                .field_name_span(field_idx as usize),
                        });
                    }
                }
                _ => {
                    self.push_diag(BodyDiag::InvalidEffectKey {
                        owner,
                        key: key_path,
                        idx,
                    });
                }
            }
        }
    }
    fn finish(self) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
        TyCheckerFinalizer::new(self).finish()
    }

    pub(crate) fn snapshot_state(&mut self) -> TyCheckerSnapshot<'db> {
        TyCheckerSnapshot {
            table: self.table.snapshot(),
            deferred_len: self.env.deferred_len(),
        }
    }

    pub(crate) fn rollback_state(&mut self, snapshot: TyCheckerSnapshot<'db>) {
        self.table.rollback_to(snapshot.table);
        self.env.truncate_deferred_tasks(snapshot.deferred_len);
    }

    fn commit_state(&mut self, snapshot: TyCheckerSnapshot<'db>) {
        self.table.commit(snapshot.table);
    }

    fn concrete_borrow_provider_for_effect_handle_ty(
        &self,
        ty: TyId<'db>,
    ) -> Option<ProviderAddressSpace> {
        let semantics = provider_semantics(self.db, self.env.scope(), self.env.assumptions(), ty);
        // Root-object defaults are binding-site semantics, not type-implied address spaces.
        (!matches!(semantics.kind, ProviderKind::RootObject)).then_some(semantics.address_space)?
    }

    fn concrete_borrow_provider_for_effect_field(
        &self,
        site: EffectParamSite<'db>,
        idx: usize,
    ) -> Option<ProviderAddressSpace> {
        self.env
            .resolved_provider_binding(site, idx)?
            .semantics
            .address_space
    }

    fn concrete_borrow_provider_for_binding(
        &self,
        binding: LocalBinding<'db>,
    ) -> Option<ProviderAddressSpace> {
        let binding_ty = self.env.lookup_binding_ty(&binding);
        match binding {
            LocalBinding::Local { pat, .. } => self.env.local_borrow_provider(pat),
            LocalBinding::EffectParam {
                site, provider_idx, ..
            } => self
                .env
                .provider_binding(site, provider_idx)
                .and_then(|provider| provider.semantics.address_space),
            LocalBinding::Param {
                site: ParamSite::EffectField(site),
                idx,
                ..
            } => self.concrete_borrow_provider_for_effect_field(site, idx),
            _ => None,
        }
        .or_else(|| {
            matches!(
                binding,
                LocalBinding::Local { .. } | LocalBinding::Param { .. }
            )
            .then(|| {
                binding_ty
                    .as_capability(self.db)
                    .map(|_| ProviderAddressSpace::Memory)
            })?
        })
    }

    fn concrete_borrow_provider_for_place(
        &self,
        place: &Place<'db>,
    ) -> Option<ProviderAddressSpace> {
        let PlaceBase::Binding(binding) = place.base;
        let binding_ty = normalize_ty(
            self.db,
            self.env.lookup_binding_ty(&binding),
            self.env.scope(),
            self.env.assumptions(),
        );
        let place_result_ty = normalize_ty(
            self.db,
            place
                .projections
                .last()
                .map(|projection| projection.result_ty())
                .unwrap_or(binding_ty),
            self.env.scope(),
            self.env.assumptions(),
        );
        if binding_ty.as_capability(self.db).is_some() {
            return self.concrete_borrow_provider_for_binding(binding);
        }
        let binding_provider = match binding {
            LocalBinding::EffectParam { .. } => self.concrete_borrow_provider_for_binding(binding),
            LocalBinding::Param {
                site: ParamSite::EffectField(..),
                ..
            } => self.concrete_borrow_provider_for_binding(binding),
            LocalBinding::Local { .. } | LocalBinding::Param { .. } => None,
        };
        binding_provider
            .or_else(|| self.concrete_borrow_provider_for_effect_handle_ty(place_result_ty))
            .or_else(|| {
                matches!(
                    binding,
                    LocalBinding::Local { .. } | LocalBinding::Param { .. }
                )
                .then_some(ProviderAddressSpace::Memory)
            })
    }

    fn merge_concrete_borrow_providers(
        &mut self,
        previous_span: DynLazySpan<'db>,
        previous: Option<ProviderAddressSpace>,
        current_span: DynLazySpan<'db>,
        current: Option<ProviderAddressSpace>,
    ) -> Option<ProviderAddressSpace> {
        if let (Some(previous), Some(current)) = (previous, current)
            && previous != current
        {
            self.push_diag(BodyDiag::IncompatibleBorrowProviders {
                primary: current_span,
                previous: previous_span,
                previous_provider: previous,
                current_provider: current,
            });
        }

        previous
            .zip(current)
            .and_then(|(previous, current)| (previous == current).then_some(previous))
    }

    fn has_dead_inference_keys<T>(&self, value: &T) -> bool
    where
        T: TyVisitable<'db>,
    {
        inference_keys(self.db, value)
            .into_iter()
            .any(|key| key.0 as usize >= self.table.len())
    }

    pub(super) fn normalize_trait_goal(&mut self, goal: TraitInstId<'db>) -> TraitInstId<'db> {
        let db = self.db;
        let scope = self.env.scope();
        let assumptions = self.env.assumptions();
        let goal = goal.fold_with(db, &mut self.table);
        let args: Vec<_> = goal
            .args(db)
            .iter()
            .copied()
            .map(|ty| normalize_ty(db, ty, scope, assumptions))
            .collect();
        let assoc_type_bindings: IndexMap<_, _> = goal
            .assoc_type_bindings(db)
            .iter()
            .map(|(&name, &ty)| (name, normalize_ty(db, ty, scope, assumptions)))
            .collect();
        TraitInstId::new(db, goal.def(db), args, assoc_type_bindings)
    }

    fn trait_goal_is_concrete_for_diagnostics(&self, goal: TraitInstId<'db>) -> bool {
        !collect_flags(self.db, goal).intersects(TyFlags::HAS_VAR | TyFlags::HAS_INVALID)
    }

    fn dedup_equivalent_trait_insts(&self, insts: Vec<TraitInstId<'db>>) -> Vec<TraitInstId<'db>> {
        let db = self.db;
        let mut seen = FxHashSet::default();
        let mut unique = Vec::new();
        for inst in insts {
            if seen.insert(Canonical::new(db, inst)) {
                unique.push(inst);
            }
        }
        unique
    }

    fn dedup_equivalent_pending_method_candidates(
        &self,
        candidates: Vec<env::PendingMethodCandidate<'db>>,
    ) -> Vec<env::PendingMethodCandidate<'db>> {
        let db = self.db;
        let mut seen = FxHashSet::default();
        let mut unique = Vec::new();
        for candidate in candidates {
            if seen.insert(Canonical::new(db, candidate.inst)) {
                unique.push(candidate);
            }
        }
        unique
    }

    fn call_constraint_diag_info(
        &self,
        callable_def: CallableDef<'db>,
        constraint_idx: usize,
    ) -> Option<CallConstraintDiagInfo<'db>> {
        Some(CallConstraintDiagInfo {
            callable_def,
            bound_span: self.call_constraint_bound_span(callable_def, constraint_idx)?,
        })
    }

    fn call_constraint_bound_span(
        &self,
        callable_def: CallableDef<'db>,
        constraint_idx: usize,
    ) -> Option<DynLazySpan<'db>> {
        let db = self.db;
        match callable_def {
            CallableDef::Func(func) => {
                let owner = GenericParamOwner::Func(func);
                let func_constraint_count = self.call_constraint_source_count(owner);
                if let Some(bound) = self.call_constraint_bound_in_owner(owner, constraint_idx) {
                    return Some(self.call_constraint_owner_bound_span(bound));
                }

                let parent_owner = match func.scope().parent_item(db) {
                    Some(ItemKind::Trait(trait_)) => Some(GenericParamOwner::Trait(trait_)),
                    Some(ItemKind::Impl(impl_)) => Some(GenericParamOwner::Impl(impl_)),
                    Some(ItemKind::ImplTrait(impl_trait)) => {
                        Some(GenericParamOwner::ImplTrait(impl_trait))
                    }
                    _ => None,
                }?;
                let parent_idx = constraint_idx.checked_sub(func_constraint_count)?;
                self.call_constraint_bound_in_owner(parent_owner, parent_idx)
                    .map(|bound| self.call_constraint_owner_bound_span(bound))
            }
            CallableDef::VariantCtor(variant) => self
                .call_constraint_bound_in_owner(
                    GenericParamOwner::Enum(variant.enum_),
                    constraint_idx,
                )
                .map(|bound| self.call_constraint_owner_bound_span(bound)),
        }
    }

    fn call_constraint_source_count(&self, owner: GenericParamOwner<'db>) -> usize {
        let db = self.db;
        let param_bounds = owner
            .params(db)
            .filter_map(|view| match view.param {
                GenericParam::Type(param) => Some(
                    param
                        .bounds
                        .iter()
                        .filter(|bound| matches!(bound, TypeBound::Trait(_)))
                        .count(),
                ),
                GenericParam::Const(_) => None,
            })
            .sum::<usize>();

        let where_bounds = owner
            .where_clause_owner()
            .map(|where_owner| {
                where_owner
                    .where_clause(db)
                    .data(db)
                    .iter()
                    .filter(|pred| {
                        pred.ty.to_opt().is_some()
                            && !(pred.ty.to_opt().is_some_and(|ty| ty.is_self_ty(db))
                                && matches!(owner, GenericParamOwner::Trait(_)))
                    })
                    .map(|pred| {
                        pred.bounds
                            .iter()
                            .filter(|bound| matches!(bound, TypeBound::Trait(_)))
                            .count()
                    })
                    .sum::<usize>()
            })
            .unwrap_or(0);

        param_bounds + where_bounds
    }

    fn call_constraint_bound_in_owner(
        &self,
        owner: GenericParamOwner<'db>,
        mut constraint_idx: usize,
    ) -> Option<CallConstraintBoundOwner<'db>> {
        let db = self.db;
        for (param_idx, view) in owner.params(db).enumerate() {
            let GenericParam::Type(param) = view.param else {
                continue;
            };
            for (bound_idx, bound) in param.bounds.iter().enumerate() {
                if matches!(bound, TypeBound::Trait(_)) {
                    if constraint_idx == 0 {
                        return Some(CallConstraintBoundOwner::GenericParam(
                            owner, param_idx, bound_idx,
                        ));
                    }
                    constraint_idx -= 1;
                }
            }
        }

        let where_owner = owner.where_clause_owner()?;
        for (pred_idx, pred) in where_owner.where_clause(db).data(db).iter().enumerate() {
            if pred.ty.to_opt().is_none()
                || pred.ty.to_opt().is_some_and(|ty| ty.is_self_ty(db))
                    && matches!(owner, GenericParamOwner::Trait(_))
            {
                continue;
            }

            for (bound_idx, bound) in pred.bounds.iter().enumerate() {
                if matches!(bound, TypeBound::Trait(_)) {
                    if constraint_idx == 0 {
                        return Some(CallConstraintBoundOwner::WherePredicate(
                            where_owner,
                            pred_idx,
                            bound_idx,
                        ));
                    }
                    constraint_idx -= 1;
                }
            }
        }

        None
    }

    fn call_constraint_owner_bound_span(
        &self,
        bound: CallConstraintBoundOwner<'db>,
    ) -> DynLazySpan<'db> {
        match bound {
            CallConstraintBoundOwner::GenericParam(owner, param_idx, bound_idx) => match owner {
                GenericParamOwner::Func(func) => func
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::Struct(struct_) => struct_
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::Enum(enum_) => enum_
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::TypeAlias(type_alias) => type_alias
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::Impl(impl_) => impl_
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::Trait(trait_) => trait_
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::ImplTrait(impl_trait) => impl_trait
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
            },
            CallConstraintBoundOwner::WherePredicate(owner, pred_idx, bound_idx) => match owner {
                WhereClauseOwner::Func(func) => func
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                WhereClauseOwner::Struct(struct_) => struct_
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                WhereClauseOwner::Enum(enum_) => enum_
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                WhereClauseOwner::Impl(impl_) => impl_
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                WhereClauseOwner::Trait(trait_) => trait_
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                WhereClauseOwner::ImplTrait(impl_trait) => impl_trait
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
            },
        }
    }

    fn process_trait_obligation(
        &mut self,
        mut obligation: env::TraitObligation<'db>,
        final_pass: bool,
    ) -> TraitObligationOutcome<'db> {
        let db = self.db;
        let scope = self.env.scope();
        let assumptions = self.env.assumptions();

        if self.has_dead_inference_keys(&obligation.goal) {
            return TraitObligationOutcome::Discharged;
        }

        obligation.goal = self.normalize_trait_goal(obligation.goal);
        let goal = obligation.goal;
        let flags = collect_flags(db, goal);
        if flags.contains(TyFlags::HAS_INVALID) || self.has_dead_inference_keys(&goal) {
            return TraitObligationOutcome::Discharged;
        }

        let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
        let query = CanonicalGoalQuery::new(db, goal, assumptions);
        match is_goal_query_satisfiable(db, solve_cx, &query) {
            GoalSatisfiability::Satisfied(solution) => {
                let solved = query.extract_solution(&mut self.table, solution).inst;
                if self.has_dead_inference_keys(&solved) {
                    return TraitObligationOutcome::Discharged;
                }
                self.table.unify(goal, solved).unwrap();
                if self.normalize_trait_goal(goal) != goal {
                    TraitObligationOutcome::Progressed
                } else {
                    TraitObligationOutcome::Discharged
                }
            }
            GoalSatisfiability::NeedsConfirmation(ambiguous) => {
                let mut candidates: Vec<_> = ambiguous
                    .iter()
                    .map(|solution| query.extract_solution(&mut self.table, *solution).inst)
                    .collect();
                candidates.retain(|candidate| !self.has_dead_inference_keys(candidate));
                let candidates = self.dedup_equivalent_trait_insts(candidates);

                if let [solution] = candidates.as_slice() {
                    if self.table.unify(goal, *solution).is_ok()
                        && self.normalize_trait_goal(goal) != goal
                    {
                        TraitObligationOutcome::Progressed
                    } else {
                        TraitObligationOutcome::Discharged
                    }
                } else {
                    if final_pass && self.trait_goal_is_concrete_for_diagnostics(goal) {
                        let required_by = match obligation.origin {
                            env::TraitObligationOrigin::CallConstraint {
                                callable_def,
                                constraint_idx,
                                ..
                            } => self.call_constraint_diag_info(callable_def, constraint_idx),
                            env::TraitObligationOrigin::GenericConfirmation => None,
                        };
                        self.push_diag(BodyDiag::AmbiguousTraitInst {
                            primary: obligation.span.clone(),
                            cands: candidates.into_iter().collect(),
                            required_by,
                        });
                        return TraitObligationOutcome::Discharged;
                    }

                    if final_pass {
                        TraitObligationOutcome::Discharged
                    } else {
                        TraitObligationOutcome::Requeue(obligation)
                    }
                }
            }
            GoalSatisfiability::UnSat(subgoal) => {
                if final_pass && self.trait_goal_is_concrete_for_diagnostics(goal) {
                    let required_by = match obligation.origin {
                        env::TraitObligationOrigin::CallConstraint {
                            callable_def,
                            constraint_idx,
                            ..
                        } => self.call_constraint_diag_info(callable_def, constraint_idx),
                        env::TraitObligationOrigin::GenericConfirmation => None,
                    };
                    let unsat = subgoal.map(|goal| query.extract_subgoal(&mut self.table, goal));
                    self.push_diag(TyDiagCollection::from(
                        TraitConstraintDiag::TraitBoundNotSat {
                            span: obligation.span.clone(),
                            primary_goal: goal,
                            unsat_subgoal: unsat,
                            required_by,
                        },
                    ));
                    TraitObligationOutcome::Discharged
                } else if final_pass {
                    TraitObligationOutcome::Discharged
                } else {
                    TraitObligationOutcome::Requeue(obligation)
                }
            }
            GoalSatisfiability::ContainsInvalid => TraitObligationOutcome::Discharged,
        }
    }

    fn resolve_deferred(&mut self) {
        let db = self.db;
        let body = self.env.body();
        let scope = self.env.scope();
        let assumptions = self.env.assumptions();

        let is_viable = |this: &mut Self,
                         pending: &env::PendingMethod<'db>,
                         expr_ty: TyId<'db>,
                         receiver: ExprId,
                         generic_args: crate::hir_def::GenericArgListId<'db>,
                         call_args: &[crate::hir_def::CallArg<'db>],
                         candidate: env::PendingMethodCandidate<'db>| {
            let snap = this.snapshot_state();

            let result = (|| {
                let inst = candidate.inst;
                let recv_ty = {
                    let mut prober = env::Prober::new(&mut this.table, scope);
                    pending.recv_ty.fold_with(db, &mut prober)
                };

                let inst_self = this.table.instantiate_to_term(inst.self_ty(db));
                let recv_ty = if this.table.unify(inst_self, recv_ty).is_ok() {
                    recv_ty
                } else {
                    let recv_inner = recv_ty.as_capability(db).map(|(_, inner)| inner)?;
                    this.table.unify(inst_self, recv_inner).ok()?;
                    recv_inner
                };

                let func_ty = try_instantiate_trait_method(
                    db,
                    candidate.method,
                    &mut this.table,
                    recv_ty,
                    inst,
                )
                .ok()?;
                let func_ty = this.table.instantiate_to_term(func_ty);
                let mut callable =
                    Callable::new(db, func_ty, receiver.span(body).into(), Some(inst)).ok()?;

                let expected_arity = callable.callable_def.arg_tys(db).len();
                let given_arity = call_args.len() + 1;
                if expected_arity != given_arity {
                    return None;
                }

                match unify_explicit_call_generic_args(
                    &mut callable,
                    this,
                    generic_args,
                    |this, _, given, current| this.table.unify(given, *current).is_ok(),
                ) {
                    Ok(()) => {}
                    Err(CallGenericArgUnifyError::ArityMismatch { .. })
                    | Err(CallGenericArgUnifyError::UnificationFailed) => return None,
                }

                let receiver_prop = this.env.typed_expr(receiver)?;
                let mut all_arg_tys = Vec::with_capacity(call_args.len() + 1);
                all_arg_tys.push(receiver_prop.ty);
                for arg in call_args.iter() {
                    let prop = this.env.typed_expr(arg.expr)?;
                    all_arg_tys.push(prop.ty);
                }

                for (&given, expected) in all_arg_tys
                    .iter()
                    .zip(callable.callable_def.arg_tys(db).iter())
                {
                    let mut expected = expected.instantiate(db, callable.generic_args());
                    if let Some(inst) = callable.trait_inst() {
                        let mut subst = AssocTySubst::new(inst);
                        expected = expected.fold_with(db, &mut subst);
                    }
                    let expected = normalize_ty(
                        db,
                        expected.fold_with(db, &mut this.table),
                        scope,
                        assumptions,
                    );
                    let given =
                        normalize_ty(db, given.fold_with(db, &mut this.table), scope, assumptions);
                    let given = this
                        .try_coerce_capability_to_expected(given, expected)
                        .unwrap_or(given);
                    this.table.unify(given, expected).ok()?;
                }

                let ret_ty = normalize_ty(
                    db,
                    callable.ret_ty(db).fold_with(db, &mut this.table),
                    scope,
                    assumptions,
                );
                this.table.unify(expr_ty, ret_ty).ok()?;

                Some(())
            })()
            .is_some();
            this.rollback_state(snap);
            result
        };

        // Fixed-point pass over deferred tasks.
        let mut progressed = true;
        while progressed {
            progressed = false;
            let tasks = self.env.take_deferred_tasks();
            for task in tasks {
                match task {
                    env::DeferredTask::Obligation(obligation) => {
                        match self.process_trait_obligation(obligation, false) {
                            TraitObligationOutcome::Discharged => {}
                            TraitObligationOutcome::Progressed => progressed = true,
                            TraitObligationOutcome::Requeue(obligation) => {
                                self.env.register_trait_obligation(obligation);
                            }
                        }
                    }
                    env::DeferredTask::Method(pending) => {
                        let (receiver, generic_args, call_args) = match pending.expr.data(db, body)
                        {
                            Partial::Present(Expr::MethodCall(receiver, _, generic_args, args)) => {
                                (*receiver, *generic_args, args.as_slice())
                            }
                            _ => continue,
                        };

                        let Some(expr_prop) = self.env.typed_expr(pending.expr) else {
                            continue;
                        };
                        let expr_ty = {
                            let mut prober = env::Prober::new(&mut self.table, scope);
                            expr_prop.ty.fold_with(db, &mut prober)
                        };
                        if expr_ty.has_invalid(db) {
                            self.env.register_pending_method(pending);
                            continue;
                        }

                        let viable: Vec<_> = pending
                            .candidates
                            .iter()
                            .copied()
                            .filter(|&candidate| {
                                is_viable(
                                    self,
                                    &pending,
                                    expr_ty,
                                    receiver,
                                    generic_args,
                                    call_args,
                                    candidate,
                                )
                            })
                            .collect();
                        let viable = self.dedup_equivalent_pending_method_candidates(viable);

                        if let [candidate] = viable.as_slice() {
                            if self.env.callable_expr(pending.expr).is_none() {
                                let call_span = pending.expr.span(body).into_method_call_expr();
                                let inst = candidate.inst;

                                let receiver_prop = self
                                    .env
                                    .typed_expr(receiver)
                                    .unwrap_or_else(|| ExprProp::invalid(db));
                                let recv_ty = {
                                    let mut prober = env::Prober::new(&mut self.table, scope);
                                    pending.recv_ty.fold_with(db, &mut prober)
                                };

                                let func_ty = self.instantiate_trait_method_to_term(
                                    candidate.method,
                                    recv_ty,
                                    inst,
                                );

                                let mut callable = match Callable::new(
                                    db,
                                    func_ty,
                                    receiver.span(body).into(),
                                    Some(inst),
                                ) {
                                    Ok(callable) => callable,
                                    Err(diag) => {
                                        self.push_diag(diag);
                                        progressed = true;
                                        continue;
                                    }
                                };

                                if candidate.needs_confirmation {
                                    self.env.register_trait_obligation(env::TraitObligation {
                                        goal: inst,
                                        origin: env::TraitObligationOrigin::GenericConfirmation,
                                        span: call_span.clone().into(),
                                    });
                                }

                                if !callable.unify_generic_args(
                                    self,
                                    generic_args,
                                    call_span.clone().generic_args(),
                                ) {
                                    progressed = true;
                                    continue;
                                }

                                callable.check_args(
                                    self,
                                    call_args,
                                    call_span.clone().args(),
                                    Some((receiver, receiver_prop)),
                                    true,
                                );

                                self.check_callable_effects(pending.expr, &mut callable);

                                let ret_ty = self.normalize_ty(callable.ret_ty(db));
                                self.table.unify(expr_prop.ty, ret_ty).unwrap();

                                callable.enqueue_constraints(
                                    self,
                                    pending.expr,
                                    call_span.method_name().into(),
                                );
                                if let Some(kind) =
                                    self.code_region_method_kind(recv_ty, pending.method_name)
                                    && call_args.len() == 1
                                    && self
                                        .env
                                        .typed_expr(call_args[0].expr)
                                        .map(|prop| {
                                            ty_may_be_code_region_token(
                                                db,
                                                normalize_ty(db, prop.ty, scope, assumptions),
                                            )
                                        })
                                        .unwrap_or(false)
                                {
                                    self.env.register_code_region_intrinsic(
                                        pending.expr,
                                        callable,
                                        call_args[0].expr,
                                        kind,
                                    );
                                } else {
                                    self.env.register_semantic_call(pending.expr, callable);
                                }
                            }

                            progressed = true;
                        } else {
                            self.env.register_pending_method(pending);
                        }
                    }
                    env::DeferredTask::PrimitiveOp(pending) => {
                        match self.resolve_pending_primitive_op(&pending) {
                            expr::PendingPrimitiveOpResolution::Pending => {
                                self.env.register_pending_primitive_op(pending);
                            }
                            expr::PendingPrimitiveOpResolution::Resolved => {
                                progressed = true;
                            }
                            expr::PendingPrimitiveOpResolution::Done => {}
                        }
                    }
                }
            }
        }

        // Emit diagnostics for remaining tasks.
        for task in self.env.take_deferred_tasks() {
            match task {
                env::DeferredTask::Obligation(obligation) => {
                    let _ = self.process_trait_obligation(obligation, true);
                }
                env::DeferredTask::Method(pending) => {
                    let Some(expr_prop) = self.env.typed_expr(pending.expr) else {
                        continue;
                    };
                    let expr_ty = {
                        let mut prober = env::Prober::new(&mut self.table, scope);
                        expr_prop.ty.fold_with(db, &mut prober)
                    };
                    if expr_ty.has_invalid(db) {
                        continue;
                    }

                    let (receiver, generic_args, call_args) = match pending.expr.data(db, body) {
                        Partial::Present(Expr::MethodCall(receiver, _, generic_args, args)) => {
                            (*receiver, *generic_args, args.as_slice())
                        }
                        _ => continue,
                    };

                    let viable: Vec<_> = pending
                        .candidates
                        .iter()
                        .copied()
                        .filter(|&candidate| {
                            is_viable(
                                self,
                                &pending,
                                expr_ty,
                                receiver,
                                generic_args,
                                call_args,
                                candidate,
                            )
                        })
                        .collect();
                    let viable = self.dedup_equivalent_pending_method_candidates(viable);
                    if viable.len() > 1 {
                        self.push_diag(BodyDiag::AmbiguousTrait {
                            primary: pending.span.clone(),
                            method_name: pending.method_name,
                            traits: viable.into_iter().map(|candidate| candidate.inst).collect(),
                        });
                    }
                }
                env::DeferredTask::PrimitiveOp(pending) => {
                    let _ = self.resolve_pending_primitive_op(&pending);
                }
            }
        }
    }

    fn new_internal(db: &'db dyn HirAnalysisDb, env: TyCheckEnv<'db>, expected: TyId<'db>) -> Self {
        let table = UnificationTable::new(db);
        Self {
            db,
            env,
            table,
            expected,
            effect_provider_keys: FxHashSet::default(),
            first_return_borrow_provider: None,
            diags: Vec::new(),
        }
    }

    /// Resolves the pattern type and return type for a recv arm.
    /// Returns (pattern_type, return_type).
    fn resolve_recv_arm_types(
        &mut self,
        contract: Contract<'db>,
        msg_path: Option<PathId<'db>>,
        arm: ContractRecvArm<'db>,
        path_span: LazyPathSpan<'db>,
        arm_span: crate::span::item::LazyRecvArmSpan<'db>,
    ) -> (TyId<'db>, TyId<'db>) {
        let invalid_ty = TyId::invalid(self.db, InvalidCause::Other);

        if arm.is_fallback(self.db) {
            return (TyId::unit(self.db), TyId::unit(self.db));
        }

        // Get variant path from arm pattern
        let Some(variant_path) = arm.variant_path(self.db) else {
            return (invalid_ty, invalid_ty);
        };

        let assumptions = self.env.assumptions();

        // Resolve based on whether this is a named or bare recv block
        let resolved = if let Some(msg_mod) = contract::resolve_recv_msg_mod(
            self.db,
            contract,
            msg_path,
            path_span,
            &mut self.diags,
            false,
        ) {
            // Named recv block - resolve within the msg module
            match contract::resolve_variant_in_msg(self.db, msg_mod, variant_path, assumptions) {
                Ok(resolved) => resolved,
                _ => {
                    // Return invalid types to suppress spurious type mismatch errors
                    // when the pattern doesn't resolve to a valid msg variant
                    return (invalid_ty, invalid_ty);
                }
            }
        } else if msg_path.is_none() {
            // Bare recv block - resolve from contract scope
            match contract::resolve_variant_bare(self.db, contract, variant_path, assumptions) {
                Ok(resolved) => resolved,
                _ => {
                    // Return invalid types to suppress spurious type mismatch errors
                    return (invalid_ty, invalid_ty);
                }
            }
        } else {
            // msg_path was Some but didn't resolve - diagnostics already emitted
            return (invalid_ty, invalid_ty);
        };

        let pat_ty = resolved.ty;

        // Get annotated return type from the arm
        let arm_ret_span = arm_span.clone().ret_ty();
        let annotated = arm
            .ret_ty
            .map(|hir_ty| self.lower_ty(hir_ty, arm_ret_span.clone(), true));
        let variant_ret = contract::get_msg_variant_return_type(self.db, pat_ty, self.env.scope());

        let ret_ty = match (variant_ret, annotated) {
            (Some(var_ty), Some(annot_ty)) => {
                self.equate_ty(annot_ty, var_ty, arm_ret_span.into());
                var_ty
            }
            (Some(var_ty), None) => {
                // Only require annotation if return type is not unit
                if var_ty != TyId::unit(self.db) {
                    self.push_diag(BodyDiag::RecvArmRetTypeMissing {
                        primary: arm_span.pat().into(),
                        expected: var_ty,
                    });
                }
                var_ty
            }
            (None, Some(annot_ty)) => annot_ty,
            (None, None) => TyId::unit(self.db),
        };

        (pat_ty, ret_ty)
    }

    fn push_diag(&mut self, diag: impl Into<FuncBodyDiag<'db>>) {
        self.diags.push(diag.into())
    }

    fn body(&self) -> Body<'db> {
        self.env.body()
    }

    fn ty_is_copy(&self, ty: TyId<'db>) -> bool {
        crate::analysis::ty::ty_is_copy(self.db, self.env.scope(), ty, self.env.assumptions())
    }

    fn copy_inner_from_borrow(&self, ty: TyId<'db>) -> Option<TyId<'db>> {
        let (_, inner) = ty.as_capability(self.db)?;
        self.ty_is_copy(inner).then_some(inner)
    }

    fn ty_unifies(&mut self, lhs: TyId<'db>, rhs: TyId<'db>) -> bool {
        let snapshot = self.snapshot_state();
        let unifies = self.table.unify(lhs, rhs).is_ok();
        self.rollback_state(snapshot);
        unifies
    }

    fn binding_interface_shape(
        &mut self,
        binding: LocalBinding<'db>,
    ) -> BindingInterfaceShape<'db> {
        let ty = self.normalize_ty(self.env.lookup_binding_ty(&binding));
        if let Some((_, value_ty)) = ty.as_capability(self.db) {
            return BindingInterfaceShape::PlaceCarrier {
                value_ty: self.normalize_ty(value_ty),
            };
        }
        if let Some(metadata) =
            effect_handle_metadata(self.db, self.env.scope(), self.env.assumptions(), ty)
        {
            return BindingInterfaceShape::DirectCarrier {
                target_ty: self.normalize_ty(metadata.target_ty),
            };
        }

        let provider = match binding {
            LocalBinding::EffectParam {
                site, provider_idx, ..
            } => self.env.provider_binding(site, provider_idx),
            LocalBinding::Param {
                site: ParamSite::EffectField(site),
                idx,
                ..
            } => self.env.resolved_provider_binding(site, idx),
            LocalBinding::Local { .. } | LocalBinding::Param { .. } => None,
        };
        provider.map_or(BindingInterfaceShape::OrdinaryValue, |provider| {
            BindingInterfaceShape::ProviderValue {
                kind: provider.semantics.kind,
                value_ty: ty,
            }
        })
    }

    fn binding_path_read_semantics(
        &mut self,
        binding: LocalBinding<'db>,
        expr_ty: TyId<'db>,
    ) -> PathReadSemantics {
        let expr_ty = self.normalize_ty(expr_ty);
        let binding_ty = self.normalize_ty(self.env.lookup_binding_ty(&binding));
        if binding_ty == expr_ty {
            return PathReadSemantics::ReuseLocal;
        }

        match self.binding_interface_shape(binding) {
            BindingInterfaceShape::OrdinaryValue => PathReadSemantics::MaterializeValue,
            BindingInterfaceShape::ProviderValue { .. }
            | BindingInterfaceShape::DirectCarrier { .. } => PathReadSemantics::ForwardInterface,
            BindingInterfaceShape::PlaceCarrier { .. } => expr_ty
                .as_capability(self.db)
                .map_or(PathReadSemantics::MaterializeValue, |_| {
                    PathReadSemantics::ForwardInterface
                }),
        }
    }

    /// Contextual capability coercion:
    /// - `mut T -> ref T`
    /// - `mut/ref/view T -> view T`
    /// - `mut/ref/view T -> T` when `T: Copy`
    /// - `T -> view T`
    fn try_coerce_capability_to_expected(
        &mut self,
        actual: TyId<'db>,
        expected: TyId<'db>,
    ) -> Option<TyId<'db>> {
        self.try_coerce_capability_expr_to_expected(None, actual, expected)
    }

    fn try_coerce_capability_for_expr_to_expected(
        &mut self,
        expr: ExprId,
        actual: TyId<'db>,
        expected: TyId<'db>,
    ) -> Option<TyId<'db>> {
        self.try_coerce_capability_expr_to_expected(Some(expr), actual, expected)
    }

    fn try_coerce_capability_expr_to_expected(
        &mut self,
        expr: Option<ExprId>,
        actual: TyId<'db>,
        expected: TyId<'db>,
    ) -> Option<TyId<'db>> {
        if expected.is_ty_var(self.db) {
            let actual = self.normalize_ty(actual);
            if let Some((CapabilityKind::View, inner)) = actual.as_capability(self.db)
                && self.ty_is_copy(inner)
            {
                return Some(inner);
            }
            return None;
        }

        let actual = self.normalize_ty(actual);
        let expected = self.normalize_ty(expected);
        if actual.has_invalid(self.db) || expected.has_invalid(self.db) {
            return None;
        }

        let actual_cap = actual.as_capability(self.db);
        let expected_cap = expected.as_capability(self.db);

        match (actual_cap, expected_cap) {
            (Some((given_kind, given_inner)), Some((required_kind, required_inner))) => {
                if given_kind.rank() < required_kind.rank() {
                    return None;
                }
                if !self.ty_unifies(given_inner, required_inner) {
                    return None;
                }
                let coerced = match required_kind {
                    CapabilityKind::Mut => TyId::borrow_mut_of(self.db, given_inner),
                    CapabilityKind::Ref => TyId::borrow_ref_of(self.db, given_inner),
                    CapabilityKind::View => TyId::view_of(self.db, given_inner),
                };
                Some(coerced)
            }
            (Some((given_kind, given_inner)), None) => {
                if !self.ty_unifies(given_inner, expected) {
                    return None;
                }

                if self.ty_is_copy(given_inner)
                    || (matches!(given_kind, CapabilityKind::View)
                        && expr.is_some_and(|expr| self.expr_can_move_from_place(expr)))
                {
                    return Some(given_inner);
                }

                None
            }
            (None, Some((CapabilityKind::View, required_inner))) => {
                if !self.ty_unifies(actual, required_inner) {
                    return None;
                }
                Some(TyId::view_of(self.db, actual))
            }
            (None, Some((CapabilityKind::Ref | CapabilityKind::Mut, _))) | (None, None) => None,
        }
    }

    fn expr_can_move_from_place(&self, expr: ExprId) -> bool {
        let Partial::Present(expr_data) = expr.data(self.db, self.body()) else {
            return false;
        };

        match expr_data {
            Expr::Path(_) => true,
            Expr::Field(lhs, _) => self.expr_can_move_from_place(*lhs),
            Expr::Bin(lhs, _, crate::hir_def::expr::BinOp::Index) => {
                self.expr_can_move_from_place(*lhs)
            }
            _ => false,
        }
    }

    /// In "owned" contexts, non-`Copy` values are implicitly moved from places.
    ///
    /// `Copy` values may be duplicated implicitly.
    fn record_implicit_move_for_owned_expr(&mut self, expr: ExprId, ty: TyId<'db>) {
        self.record_implicit_move_for_owned_expr_inner(expr, Some(ty));
    }

    fn record_implicit_move_for_owned_expr_inner(
        &mut self,
        expr: ExprId,
        expected_ty: Option<TyId<'db>>,
    ) {
        let db = self.db;
        let body = self.body();
        let Partial::Present(expr_data) = expr.data(db, body) else {
            return;
        };

        match expr_data {
            Expr::Block(stmts) => {
                let Some(last) = stmts.last() else {
                    return;
                };
                let Partial::Present(stmt) = last.data(db, body) else {
                    return;
                };
                let crate::hir_def::Stmt::Expr(tail) = stmt else {
                    return;
                };
                self.record_implicit_move_for_owned_expr_inner(*tail, expected_ty);
            }
            Expr::With(_, body_expr) | Expr::Cast(body_expr, _) | Expr::If(_, body_expr, None) => {
                self.record_implicit_move_for_owned_expr_inner(*body_expr, expected_ty);
            }
            Expr::If(_, then_expr, Some(else_expr)) => {
                self.record_implicit_move_for_owned_expr_inner(*then_expr, expected_ty);
                self.record_implicit_move_for_owned_expr_inner(*else_expr, expected_ty);
            }
            Expr::Match(_, arms) => {
                let Partial::Present(arms) = arms else {
                    return;
                };
                for arm in arms {
                    self.record_implicit_move_for_owned_expr_inner(arm.body, expected_ty);
                }
            }
            _ => {
                if self.env.expr_place(expr).is_none() {
                    return;
                }

                let Some(prop) = self.env.typed_expr(expr) else {
                    return;
                };
                let expr_ty = prop.ty.fold_with(self.db, &mut self.table);
                let expr_ty = self.normalize_ty(expr_ty);
                if expr_ty.has_invalid(self.db)
                    || expr_ty == TyId::unit(self.db)
                    || expr_ty.is_never(self.db)
                {
                    return;
                }

                let expected_ty = expected_ty
                    .map(|ty| self.normalize_ty(ty))
                    .filter(|ty| {
                        !ty.has_invalid(self.db)
                            && *ty != TyId::unit(self.db)
                            && !ty.is_never(self.db)
                    })
                    .unwrap_or(expr_ty);

                if self.ty_is_copy(expected_ty) {
                    return;
                }

                self.env.record_implicit_move(expr);
            }
        }
    }

    fn lit_ty(&mut self, lit: &LitKind<'db>) -> TyId<'db> {
        match lit {
            LitKind::Bool(_) => TyId::bool(self.db),
            LitKind::Int(_) => self.table.new_var(TyVarSort::Integral, &Kind::Star),
            LitKind::String(s) => {
                let len_bytes = s.len_bytes(self.db);
                if len_bytes > MAX_INLINE_STRING_BYTES {
                    return TyId::invalid(
                        self.db,
                        InvalidCause::StringTooLarge {
                            max: MAX_INLINE_STRING_BYTES,
                            given: len_bytes,
                        },
                    );
                }
                self.table.new_var(
                    TyVarSort::String {
                        min_len: len_bytes,
                        fallback: self.string_literal_fallback(),
                    },
                    &Kind::Star,
                )
            }
        }
    }

    pub(crate) fn string_literal_ty(
        &mut self,
        string_id: StringId<'db>,
        expected: TyId<'db>,
    ) -> TyId<'db> {
        if self.string_literal_should_use_byte_array(expected) {
            return self.string_literal_byte_array_ty(string_id.len_bytes(self.db));
        }
        if expected.is_core_dyn_string(self.db) {
            return expected;
        }

        let len_bytes = string_id.len_bytes(self.db);
        self.table.new_var(
            TyVarSort::String {
                min_len: len_bytes,
                fallback: self.string_literal_fallback(),
            },
            &Kind::Star,
        )
    }

    pub(crate) fn string_literal_byte_array_ty(&self, len_bytes: usize) -> TyId<'db> {
        let u8_ty = TyId::new(
            self.db,
            TyData::TyBase(TyBase::Prim(crate::analysis::ty::ty_def::PrimTy::U8)),
        );
        TyId::array_with_len(self.db, u8_ty, len_bytes)
    }

    pub(crate) fn string_literal_should_use_byte_array(&self, expected: TyId<'db>) -> bool {
        let expected = normalize_ty(self.db, expected, self.env.scope(), self.env.assumptions());
        let (base, args) = expected.decompose_ty_app(self.db);
        matches!(
            base.data(self.db),
            TyData::TyBase(TyBase::Prim(crate::analysis::ty::ty_def::PrimTy::Array))
        ) && args.len() == 2
            && matches!(
                args[0].data(self.db),
                TyData::TyBase(TyBase::Prim(crate::analysis::ty::ty_def::PrimTy::U8))
            )
    }

    fn lower_ty(
        &mut self,
        hir_ty: HirTyId<'db>,
        span: LazyTySpan<'db>,
        star_kind_required: bool,
    ) -> TyId<'db> {
        let ty = lower_hir_ty(self.db, hir_ty, self.env.scope(), self.env.assumptions());

        // If lowering failed, try to produce precise diagnostics (e.g., path resolution errors)
        if ty.has_invalid(self.db) {
            let diags = collect_ty_lower_errors(
                self.db,
                self.env.scope(),
                hir_ty,
                span.clone(),
                self.env.assumptions(),
            );
            if !diags.is_empty() {
                for d in diags {
                    self.push_diag(d);
                }
                // Avoid cascading kind errors for already-invalid types
                return TyId::invalid(self.db, InvalidCause::Other);
            }
        }

        if let Some(diag) = ty.emit_diag(self.db, span.clone().into()) {
            self.push_diag(diag)
        }

        if ty_contains_const_hole(self.db, ty) {
            self.push_diag(TyDiagCollection::from(
                TyLowerDiag::ConstHoleInValuePosition {
                    span: span.clone().into(),
                    ty,
                },
            ));
            return TyId::invalid(self.db, InvalidCause::Other);
        }

        if star_kind_required && ty.is_star_kind(self.db) {
            ty
        } else {
            let diag: TyDiagCollection = TyLowerDiag::ExpectedStarKind(span.into()).into();
            self.push_diag(diag);
            TyId::invalid(self.db, InvalidCause::Other)
        }
    }

    /// Returns the fresh type variable for pattern and expr type checking. The
    /// kind of the type variable is `*`, and the sort is `General`.
    fn fresh_ty(&mut self) -> TyId<'db> {
        self.table.new_var(TyVarSort::General, &Kind::Star)
    }

    fn fresh_tys_n(&mut self, n: usize) -> Vec<TyId<'db>> {
        (0..n).map(|_| self.fresh_ty()).collect()
    }

    fn capability_fallback_candidates(&self, ty: TyId<'db>) -> Vec<TyId<'db>> {
        let mut candidates = vec![ty];
        if let TyData::TyVar(var) = ty.base_ty(self.db).data(self.db)
            && matches!(var.sort, TyVarSort::String { .. })
            && let Some(text_ty) =
                resolve_lib_type_path(self.db, self.env.scope(), "core::abi::DynString")
        {
            candidates.push(text_ty);
            candidates.push(TyId::view_of(self.db, text_ty));
        }
        if let Some((cap, inner)) = ty.as_capability(self.db) {
            if matches!(cap, CapabilityKind::Mut) {
                candidates.push(TyId::borrow_ref_of(self.db, inner));
            }
            if !matches!(cap, CapabilityKind::View) {
                candidates.push(TyId::view_of(self.db, inner));
            }
            candidates.push(inner);
        } else {
            candidates.push(TyId::view_of(self.db, ty));
        }
        candidates.dedup();
        candidates
    }

    fn pattern_binds_any(&self, pat: PatId) -> bool {
        let Partial::Present(pat_data) = pat.data(self.db, self.body()) else {
            return false;
        };
        match pat_data {
            Pat::WildCard | Pat::Rest | Pat::Lit(_) => false,
            Pat::Path(..) => self
                .env
                .pat_binding(pat)
                .is_some_and(|binding| matches!(binding, LocalBinding::Local { .. })),
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                pats.iter().any(|pat| self.pattern_binds_any(*pat))
            }
            Pat::Record(_, fields) => fields.iter().any(|field| self.pattern_binds_any(field.pat)),
            Pat::Or(lhs, rhs) => self.pattern_binds_any(*lhs) || self.pattern_binds_any(*rhs),
        }
    }

    fn destructure_source_mode(&self, ty: TyId<'db>) -> (TyId<'db>, PatternDestructureMode) {
        destructure_pattern_source(self.db, ty)
    }

    fn retype_pattern_bindings_for_borrow(&mut self, pat: PatId, kind: BorrowKind) {
        let Partial::Present(pat_data) = pat.data(self.db, self.body()) else {
            return;
        };
        match pat_data {
            Pat::Path(..) => {
                let Some(binding) = self.env.pat_binding(pat) else {
                    return;
                };
                if !matches!(binding, LocalBinding::Local { .. }) {
                    return;
                }
                self.env.set_pat_binding_mode(pat, PatBindingMode::ByBorrow);
                let inner = self.env.lookup_binding_ty(&binding);
                if inner.has_invalid(self.db) || inner.as_capability(self.db).is_some() {
                    return;
                }
                let borrow_ty =
                    apply_pattern_borrow_mode(self.db, PatternDestructureMode::Borrow(kind), inner);
                self.env.type_pat(pat, borrow_ty);
            }
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                for pat in pats {
                    self.retype_pattern_bindings_for_borrow(*pat, kind);
                }
            }
            Pat::Record(_, fields) => {
                for field in fields {
                    self.retype_pattern_bindings_for_borrow(field.pat, kind);
                }
            }
            Pat::Or(lhs, rhs) => {
                self.retype_pattern_bindings_for_borrow(*lhs, kind);
                self.retype_pattern_bindings_for_borrow(*rhs, kind);
            }
            Pat::WildCard | Pat::Rest | Pat::Lit(..) => {}
        }
    }

    /// Ensure all binding patterns are registered in the current scope.
    fn seed_pat_bindings(&mut self, pat: PatId) {
        let Partial::Present(pat_data) = pat.data(self.db, self.env.body()) else {
            return;
        };

        match pat_data {
            Pat::Path(path, is_mut) => {
                let Partial::Present(path) = path else {
                    return;
                };
                let Some(LocalBinding::Local { .. }) = self.env.pat_binding(pat) else {
                    return;
                };
                if let Some(ident) = path.as_ident(self.db) {
                    let current = self.env.current_block_idx();
                    if self.env.get_block(current).lookup_var(ident).is_none() {
                        let binding = LocalBinding::local(pat, *is_mut);
                        self.env.register_pending_binding(ident, binding);
                    }
                }
            }
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                for &p in pats {
                    self.seed_pat_bindings(p);
                }
            }
            Pat::Record(_, fields) => {
                for field in fields {
                    self.seed_pat_bindings(field.pat);
                }
            }
            Pat::Or(lhs, rhs) => {
                self.seed_pat_bindings(*lhs);
                self.seed_pat_bindings(*rhs);
            }
            Pat::WildCard | Pat::Rest | Pat::Lit(..) => {}
        }
    }

    fn unify_ty<T>(&mut self, t: T, actual: TyId<'db>, expected: TyId<'db>) -> TyId<'db>
    where
        T: Into<Typeable<'db>>,
    {
        let t = t.into();
        let span = t.clone().span(self.env.body());
        let actual = self.equate_ty(actual, expected, span);

        match t {
            Typeable::Expr(expr, mut typed_expr) => {
                typed_expr.ty = actual;
                if typed_expr.borrow_provider.is_none() {
                    typed_expr.borrow_provider =
                        self.concrete_borrow_provider_for_effect_handle_ty(actual);
                }
                typed_expr.path_read_semantics = typed_expr
                    .binding
                    .map(|binding| self.binding_path_read_semantics(binding, actual));
                self.env.type_expr(expr, typed_expr)
            }
            Typeable::Pat(pat) => self.env.type_pat(pat, actual),
        }

        actual
    }

    fn equate_ty(
        &mut self,
        actual: TyId<'db>,
        expected: TyId<'db>,
        span: DynLazySpan<'db>,
    ) -> TyId<'db> {
        // FIXME: This is a temporary workaround, this should be removed when we
        // implement subtyping.
        if expected.is_never(self.db) && !actual.is_never(self.db) {
            if !actual.has_invalid(self.db) && !expected.has_invalid(self.db) {
                let diag = BodyDiag::TypeMismatch {
                    span,
                    expected,
                    given: actual,
                };
                self.push_diag(diag);
            }
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        // Resolve associated types before unification
        let actual = actual.fold_with(self.db, &mut self.table);
        let expected = expected.fold_with(self.db, &mut self.table);
        let actual = self.normalize_ty(actual);
        let expected = self.normalize_ty(expected);
        match self.table.unify(actual, expected) {
            Ok(()) => {
                // FIXME: This is a temporary workaround, this should be removed when we
                // implement subtyping.
                let actual = actual.fold_with(self.db, &mut self.table);
                if actual.is_never(self.db) {
                    expected
                } else {
                    actual
                }
            }

            Err(UnificationError::TypeMismatch) => {
                let actual = actual.fold_with(self.db, &mut self.table);
                let expected = expected.fold_with(self.db, &mut self.table);
                if !actual.has_invalid(self.db) && !expected.has_invalid(self.db) {
                    self.push_diag(BodyDiag::TypeMismatch {
                        span,
                        expected,
                        given: actual,
                    });
                }
                TyId::invalid(self.db, InvalidCause::Other)
            }

            Err(UnificationError::OccursCheckFailed) => {
                self.push_diag(BodyDiag::InfiniteOccurrence(span));

                TyId::invalid(self.db, InvalidCause::Other)
            }
        }
    }

    fn resolve_path(
        &mut self,
        path: PathId<'db>,
        resolve_tail_as_value: bool,
        span: LazyPathSpan<'db>,
    ) -> Result<PathRes<'db>, PathResError<'db>> {
        let scope = self.env.scope();
        let mut invisible = None;
        let mut check_visibility = |path: PathId<'db>, reso: &PathRes<'db>| {
            if invisible.is_some() {
                return;
            }
            if !reso.is_visible_from(self.db, scope) {
                invisible = Some((path, reso.name_span(self.db)));
            }
        };

        let res = match resolve_path_with_observer(
            self.db,
            path,
            scope,
            self.env.assumptions(),
            resolve_tail_as_value,
            &mut check_visibility,
        ) {
            Ok(r) => Ok(r.map_over_ty(|ty| self.instantiate_to_term(ty))),
            Err(err) => Err(err),
        };

        if let Some((path, deriv_span)) = invisible {
            let span = span.clone().segment(path.segment_index(self.db)).ident();
            let ident = path.ident(self.db);
            let diag = PathResDiag::Invisible(span.into(), ident.unwrap(), deriv_span);
            self.diags.push(diag.into());
        }

        res
    }

    fn instantiate_to_term(&mut self, ty: TyId<'db>) -> TyId<'db> {
        let base = ty.base_ty(self.db);
        let TyData::TyBase(TyBase::Func(def)) = base.data(self.db) else {
            return self.table.instantiate_to_term(ty);
        };
        self.instantiate_callable_to_term(ty, *def)
    }

    fn instantiate_trait_method_to_term(
        &mut self,
        method: Func<'db>,
        receiver_ty: TyId<'db>,
        inst: TraitInstId<'db>,
    ) -> TyId<'db> {
        let ty = instantiate_trait_method(self.db, method, &mut self.table, receiver_ty, inst);
        self.instantiate_callable_to_term(ty, method.as_callable(self.db).unwrap())
    }

    fn instantiate_trait_assoc_fn_to_term(
        &mut self,
        method: CallableDef<'db>,
        inst: TraitInstId<'db>,
    ) -> TyId<'db> {
        let ty = instantiate_trait_assoc_fn(self.db, method, inst);
        self.instantiate_callable_to_term(ty, method)
    }

    fn instantiate_inherent_method_to_term(
        &mut self,
        method: CallableDef<'db>,
        receiver_ty: TyId<'db>,
    ) -> TyId<'db> {
        let ty = TyId::func(self.db, method);
        let ty = self.instantiate_callable_to_term(ty, method);

        // Bind the impl-level params by unifying the candidate's probe key
        // (self-param type for methods, the impl self type for path-called
        // associated functions) against the receiver, mirroring the
        // method-table probe that selected this candidate. Copying the
        // receiver's generic args into the params positionally mis-binds
        // impls whose self type mentions params through projections:
        // `impl<T: Bound> Box<T::Item>` called on `Box<U::Item>` must bind
        // `T := U`, not `T := U::Item`.
        let probe_key_ty = if let Some(self_param) = method.receiver_ty(self.db) {
            let self_param = self_param.instantiate(self.db, ty.generic_args(self.db));
            Some(
                self_param
                    .as_capability(self.db)
                    .map(|(_, inner)| inner)
                    .unwrap_or(self_param),
            )
        } else if let CallableDef::Func(func) = method
            && let Some(impl_) = func.containing_impl(self.db)
            && let Some(impl_ty) = impl_.admissible_inherent_impl_ty(self.db)
        {
            Some(Binder::bind(impl_ty).instantiate(self.db, ty.generic_args(self.db)))
        } else {
            None
        };
        if let Some(key_ty) = probe_key_ty {
            let snapshot = self.table.snapshot();
            if self.table.unify(key_ty, receiver_ty).is_err() {
                self.table.rollback_to(snapshot);
            }
        }
        ty
    }

    fn instantiate_callable_to_term(
        &mut self,
        mut ty: TyId<'db>,
        callable: CallableDef<'db>,
    ) -> TyId<'db> {
        if ty.has_invalid(self.db) {
            return ty;
        }

        while let Some(prop) = ty.applicable_ty(self.db) {
            let (_, args) = ty.decompose_ty_app(self.db);
            let param_index = args.len();
            let param_ty = callable.params(self.db).get(param_index).copied();
            let arg = self.table.new_var_for(prop);
            if let Some(param_ty) = param_ty
                && (matches!(param_ty.data(self.db), TyData::TyParam(p) if p.is_effect_provider())
                    || matches!(
                        param_ty.data(self.db),
                        TyData::ConstTy(const_ty)
                            if matches!(
                                const_ty.data(self.db),
                                ConstTyData::TyParam(param, _) if param.is_implicit()
                            )
                    ))
            {
                self.effect_provider_keys
                    .extend(inference_keys(self.db, &arg));
            }
            ty = TyId::app(self.db, ty, arg);
        }

        ty
    }

    /// Resolve associated type to concrete type if possible
    fn normalize_ty(&mut self, ty: TyId<'db>) -> TyId<'db> {
        normalize_ty(
            self.db,
            ty.fold_with(self.db, &mut self.table),
            self.env.scope(),
            self.env.assumptions(),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum EffectArg<'db> {
    Place(Place<'db>),
    Value(ExprId),
    Binding(LocalBinding<'db>),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum EffectPassMode {
    /// The provided effect is already a place; pass it directly.
    ByPlace,
    /// The provided effect is an rvalue; materialize it into a block-scoped temp place.
    ByTempPlace,
    ByValue,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ResolvedEffectArg<'db> {
    pub param_idx: usize,
    pub binding_idx: u32,
    pub key: PathId<'db>,
    pub arg: EffectArg<'db>,
    pub pass_mode: EffectPassMode,
    pub required_mut: bool,
    pub key_kind: EffectKeyKind,
    pub instantiated_key_ty: Option<TyId<'db>>,
    pub provider_target_ty: Option<TyId<'db>>,
    pub provider: Option<ProviderAddressSpace>,
}

/// Resolved reference for a `const`-valued path expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ConstRef<'db> {
    Const(Const<'db>),
    TraitConst(AssocConstUse<'db>),
}

impl<'db> TyVisitable<'db> for ConstRef<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        match self {
            ConstRef::Const(_) => {}
            ConstRef::TraitConst(assoc) => assoc.visit_with(visitor),
        }
    }
}

impl<'db> TyFoldable<'db> for ConstRef<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        match self {
            ConstRef::Const(const_def) => ConstRef::Const(const_def),
            ConstRef::TraitConst(assoc) => ConstRef::TraitConst(assoc.fold_with(db, folder)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
struct ExprPlaceId(u32);
entity_impl!(ExprPlaceId);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedBody<'db> {
    body: Option<Body<'db>>,
    result_ty: TyId<'db>,
    assumptions: PredicateListId<'db>,
    pat_ty: SecondaryMap<PatId, Option<TyId<'db>>>,
    expr_ty: SecondaryMap<ExprId, Option<ExprProp<'db>>>,
    implicit_moves: FxHashSet<ExprId>,
    const_refs: SecondaryMap<ExprId, Option<ConstRef<'db>>>,
    value_path_refs: SecondaryMap<ExprId, Option<ValuePathRef<'db>>>,
    semantic_expr_lowering: SecondaryMap<ExprId, Option<SemanticExprLowering<'db>>>,
    record_init_lowering: SecondaryMap<ExprId, Option<RecordInitLowering<'db>>>,
    resolved_field_index: SecondaryMap<ExprId, Option<u16>>,
    call_effect_args: SecondaryMap<ExprId, Option<Vec<ResolvedEffectArg<'db>>>>,
    return_borrow_provider: Option<ProviderAddressSpace>,
    /// Bindings for function parameters (indexed by param position)
    param_bindings: Vec<LocalBinding<'db>>,
    /// Bindings for local variables (keyed by the pattern that introduces them)
    pat_bindings: SecondaryMap<PatId, Option<LocalBinding<'db>>>,
    /// Binding capture mode for local variables (keyed by the pattern that introduces them)
    pat_binding_modes: SecondaryMap<PatId, Option<PatBindingMode>>,
    pattern_store: PatternStore<'db>,
    pattern_status: SecondaryMap<PatId, PatternAnalysisStatus>,
    /// Resolved Seq trait methods for for-loops
    for_loop_seq: SecondaryMap<StmtId, Option<ForLoopSeq<'db>>>,
    expr_place: SecondaryMap<ExprId, PackedOption<ExprPlaceId>>,
    expr_places: PrimaryMap<ExprPlaceId, Place<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindingSource {
    pub init_expr: ExprId,
    pub field_path: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReturnProvenance {
    Fresh,
    ForwardedParams(Vec<usize>),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Update)]
pub enum ValuePathRef<'db> {
    UnitVariant(ResolvedVariant<'db>),
    TypeConst(TyId<'db>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum CodeRegionIntrinsicKind {
    Offset,
    Len,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ConstIntrinsicKind {
    SizeOf,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub enum SemanticExprLowering<'db> {
    Call {
        callable: Callable<'db>,
    },
    CodeRegionIntrinsic {
        callable: Callable<'db>,
        region_arg: ExprId,
        kind: CodeRegionIntrinsicKind,
    },
    ConstIntrinsic {
        callable: Callable<'db>,
        kind: ConstIntrinsicKind,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum RecordInitLowering<'db> {
    Struct,
    EnumVariant(ResolvedVariant<'db>),
}

impl<'db> TyVisitable<'db> for ValuePathRef<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        match self {
            Self::UnitVariant(variant) => variant.ty.visit_with(visitor),
            Self::TypeConst(ty) => ty.visit_with(visitor),
        }
    }
}

impl<'db> TyFoldable<'db> for ValuePathRef<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        match self {
            Self::UnitVariant(variant) => Self::UnitVariant(ResolvedVariant {
                ty: variant.ty.fold_with(db, folder),
                ..variant
            }),
            Self::TypeConst(ty) => Self::TypeConst(ty.fold_with(db, folder)),
        }
    }
}

impl<'db> TyVisitable<'db> for SemanticExprLowering<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        match self {
            Self::Call { callable }
            | Self::CodeRegionIntrinsic { callable, .. }
            | Self::ConstIntrinsic { callable, .. } => callable.visit_with(visitor),
        }
    }
}

impl<'db> TyFoldable<'db> for SemanticExprLowering<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        match self {
            Self::Call { callable } => Self::Call {
                callable: callable.fold_with(db, folder),
            },
            Self::CodeRegionIntrinsic {
                callable,
                region_arg,
                kind,
            } => Self::CodeRegionIntrinsic {
                callable: callable.fold_with(db, folder),
                region_arg,
                kind,
            },
            Self::ConstIntrinsic { callable, kind } => Self::ConstIntrinsic {
                callable: callable.fold_with(db, folder),
                kind,
            },
        }
    }
}

impl<'db> TyVisitable<'db> for RecordInitLowering<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        if let Self::EnumVariant(variant) = self {
            variant.ty.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for RecordInitLowering<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        match self {
            Self::Struct => Self::Struct,
            Self::EnumVariant(variant) => Self::EnumVariant(ResolvedVariant {
                ty: variant.ty.fold_with(db, folder),
                ..variant
            }),
        }
    }
}

impl<'db> TyVisitable<'db> for TypedBody<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        self.assumptions.visit_with(visitor);
        self.result_ty.visit_with(visitor);
        for ty in self.pat_ty.values().flatten() {
            ty.visit_with(visitor);
        }
        for prop in self.expr_ty.values().flatten() {
            prop.visit_with(visitor);
        }
        for cref in self.const_refs.values().flatten() {
            cref.visit_with(visitor);
        }
        for value_path in self.value_path_refs.values().flatten() {
            value_path.visit_with(visitor);
        }
        for lowering in self.semantic_expr_lowering.values().flatten() {
            lowering.visit_with(visitor);
        }
        for lowering in self.record_init_lowering.values().flatten() {
            lowering.visit_with(visitor);
        }
        for place in self.expr_places.values() {
            place.visit_with(visitor);
        }
        self.pattern_store.visit_with(visitor);
        for seq in self.for_loop_seq.values().flatten() {
            seq.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for TypedBody<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        let mut this = self;
        this.result_ty = this.result_ty.fold_with(db, folder);
        this.assumptions = this.assumptions.fold_with(db, folder);
        this.pat_ty
            .values_mut()
            .flatten()
            .for_each(|ty| *ty = ty.fold_with(db, folder));
        this.expr_ty
            .values_mut()
            .flatten()
            .for_each(|prop| *prop = prop.clone().fold_with(db, folder));
        this.const_refs
            .values_mut()
            .flatten()
            .for_each(|cref| *cref = (*cref).fold_with(db, folder));
        this.value_path_refs
            .values_mut()
            .flatten()
            .for_each(|path| *path = (*path).fold_with(db, folder));
        this.semantic_expr_lowering
            .values_mut()
            .flatten()
            .for_each(|lowering| *lowering = lowering.clone().fold_with(db, folder));
        this.record_init_lowering
            .values_mut()
            .flatten()
            .for_each(|lowering| *lowering = (*lowering).fold_with(db, folder));
        for args in this.call_effect_args.values_mut().flatten() {
            for arg in args {
                *arg = arg.clone().fold_with(db, folder);
            }
        }
        this.param_bindings
            .iter_mut()
            .for_each(|binding| *binding = binding.fold_with(db, folder));
        this.pat_bindings
            .values_mut()
            .flatten()
            .for_each(|binding| *binding = binding.fold_with(db, folder));
        this.pattern_store = this.pattern_store.fold_with(db, folder);
        this.for_loop_seq
            .values_mut()
            .flatten()
            .for_each(|seq| *seq = seq.clone().fold_with(db, folder));
        this.expr_places
            .values_mut()
            .for_each(|place| *place = place.clone().fold_with(db, folder));
        this
    }
}

unsafe impl<'db> Update for TypedBody<'db> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old_value = unsafe { &mut *old_pointer };
        if *old_value == new_value {
            false
        } else {
            *old_value = new_value;
            true
        }
    }
}

impl<'db> TypedBody<'db> {
    pub fn body(&self) -> Option<Body<'db>> {
        self.body
    }

    pub fn result_ty(&self) -> TyId<'db> {
        self.result_ty
    }

    pub(crate) fn has_smir_lowering_blocker(&self, db: &'db dyn HirAnalysisDb) -> bool {
        let Some(body) = self.body else {
            return false;
        };

        body.exprs(db)
            .iter()
            .any(|(expr, expr_data)| self.expr_has_smir_lowering_blocker(db, expr, expr_data))
            || body
                .stmts(db)
                .iter()
                .any(|(stmt, stmt_data)| self.stmt_has_smir_lowering_blocker(stmt, stmt_data))
            || body
                .conds(db)
                .iter()
                .any(|(_, cond_data)| matches!(cond_data, Partial::Absent))
    }

    fn expr_has_smir_lowering_blocker(
        &self,
        db: &'db dyn HirAnalysisDb,
        expr: ExprId,
        expr_data: &Partial<Expr<'db>>,
    ) -> bool {
        let Partial::Present(expr_data) = expr_data else {
            return true;
        };
        let expr_ty = self.expr_ty(db, expr);

        match expr_data {
            Expr::Path(_) => {
                expr_ty.has_invalid(db)
                    && self.expr_binding(expr).is_none()
                    && self.expr_const_ref(expr).is_none()
                    && self.expr_code_region_ref(db, expr).is_none()
                    && self.value_path_ref(expr).is_none()
            }
            Expr::Call(..) | Expr::MethodCall(..) => {
                expr_ty.has_invalid(db) && self.semantic_expr_lowering(expr).is_none()
            }
            Expr::Assert(_) => expr_ty.has_invalid(db),
            Expr::RecordInit(..) => {
                expr_ty.has_invalid(db) && self.record_init_lowering(expr).is_none()
            }
            Expr::Un(inner, crate::hir_def::expr::UnOp::Mut | crate::hir_def::expr::UnOp::Ref) => {
                expr_ty.has_invalid(db) && self.expr_place(*inner).is_none()
            }
            Expr::Assign(dst, _) => self.expr_place(*dst).is_none(),
            Expr::AugAssign(dst, _, _) => {
                self.semantic_expr_lowering(expr).is_none() && self.expr_place(*dst).is_none()
            }
            Expr::Match(_, Partial::Absent) => true,
            Expr::Match(_, Partial::Present(arms)) => arms
                .iter()
                .any(|arm| !self.pattern_status(arm.pat).is_ready()),
            _ => false,
        }
    }

    fn stmt_has_smir_lowering_blocker(&self, stmt: StmtId, stmt_data: &Partial<Stmt<'db>>) -> bool {
        match stmt_data {
            Partial::Present(Stmt::For(..)) => self.for_loop_seq(stmt).is_none(),
            Partial::Present(_) => false,
            Partial::Absent => true,
        }
    }

    pub fn assumptions(&self) -> PredicateListId<'db> {
        self.assumptions
    }

    pub fn expr_ty(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> TyId<'db> {
        self.expr_prop(db, expr).ty
    }

    pub fn expr_prop(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> ExprProp<'db> {
        self.expr_ty
            .get(expr)
            .cloned()
            .flatten()
            .unwrap_or_else(|| ExprProp::invalid(db))
    }

    pub fn is_implicit_move(&self, expr: ExprId) -> bool {
        self.implicit_moves.contains(&expr)
    }

    pub fn expr_const_ref(&self, expr: ExprId) -> Option<ConstRef<'db>> {
        self.const_refs[expr]
    }

    pub fn value_path_ref(&self, expr: ExprId) -> Option<ValuePathRef<'db>> {
        self.value_path_refs[expr]
    }

    pub fn expr_code_region_ref(
        &self,
        db: &'db dyn HirAnalysisDb,
        expr: ExprId,
    ) -> Option<SemanticCodeRegionRef<'db>> {
        self.expr_binding(expr)
            .and_then(|binding| manual_contract_root_ref_from_ty(db, self.binding_ty(db, binding)))
            .or_else(|| manual_contract_root_ref_from_ty(db, self.expr_ty(db, expr)))
    }

    pub fn semantic_expr_lowering(&self, expr: ExprId) -> Option<&SemanticExprLowering<'db>> {
        self.semantic_expr_lowering[expr].as_ref()
    }

    pub fn record_init_lowering(&self, expr: ExprId) -> Option<RecordInitLowering<'db>> {
        self.record_init_lowering[expr]
    }

    pub fn resolved_field_index(&self, expr: ExprId) -> Option<u16> {
        self.resolved_field_index[expr]
    }

    // Final typed pattern/binding view. This can intentionally differ from
    // validated-pattern match types when destructuring borrowed carriers.
    pub fn pat_ty(&self, db: &'db dyn HirAnalysisDb, pat: PatId) -> TyId<'db> {
        self.pat_ty
            .get(pat)
            .copied()
            .flatten()
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
    }

    pub fn callable_expr(&self, expr: ExprId) -> Option<&Callable<'db>> {
        match self.semantic_expr_lowering(expr)? {
            SemanticExprLowering::Call { callable }
            | SemanticExprLowering::CodeRegionIntrinsic { callable, .. }
            | SemanticExprLowering::ConstIntrinsic { callable, .. } => Some(callable),
        }
    }

    pub fn code_region_ref(
        &self,
        db: &'db dyn HirAnalysisDb,
        expr: ExprId,
    ) -> Option<SemanticCodeRegionRef<'db>> {
        match self.semantic_expr_lowering(expr)? {
            SemanticExprLowering::CodeRegionIntrinsic { region_arg, .. } => {
                self.expr_code_region_ref(db, *region_arg)
            }
            SemanticExprLowering::Call { .. } | SemanticExprLowering::ConstIntrinsic { .. } => None,
        }
    }

    pub fn call_effect_args(&self, call_expr: ExprId) -> Option<&[ResolvedEffectArg<'db>]> {
        self.call_effect_args[call_expr].as_deref()
    }

    pub fn return_borrow_provider(&self) -> Option<ProviderAddressSpace> {
        self.return_borrow_provider
    }

    /// Get the binding for a function parameter by index.
    pub fn param_binding(&self, idx: usize) -> Option<LocalBinding<'db>> {
        self.param_bindings.get(idx).copied()
    }

    /// Get the binding for a local variable by its pattern.
    pub fn pat_binding(&self, pat: PatId) -> Option<LocalBinding<'db>> {
        self.pat_bindings[pat]
    }

    /// Get how this local binding is captured by its source pattern destructuring.
    pub fn pat_binding_mode(&self, pat: PatId) -> Option<PatBindingMode> {
        self.pat_binding_modes[pat]
    }

    pub fn binding_ty(&self, db: &'db dyn HirAnalysisDb, binding: LocalBinding<'db>) -> TyId<'db> {
        match binding {
            LocalBinding::Local { pat, .. } => self.pat_ty(db, pat),
            LocalBinding::Param { ty, .. } => ty,
            LocalBinding::EffectParam { site, idx, .. } => {
                crate::core::semantic::EffectEnvView::new(site)
                    .visible_effect_binding_ty(db, idx)
                    .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
            }
        }
    }

    pub fn path_expr_read_semantics(&self, expr: ExprId) -> Option<PathReadSemantics> {
        self.expr_ty[expr]
            .as_ref()
            .and_then(|prop| prop.path_read_semantics)
    }

    pub fn path_expr_reuses_local(&self, expr: ExprId) -> bool {
        matches!(
            self.path_expr_read_semantics(expr),
            Some(PathReadSemantics::ReuseLocal)
        )
    }

    pub fn pattern_store(&self) -> &PatternStore<'db> {
        &self.pattern_store
    }

    pub fn pattern_status(&self, pat: PatId) -> PatternAnalysisStatus {
        self.pattern_status[pat]
    }

    pub fn pattern_root(&self, pat: PatId) -> Option<ValidatedPatId> {
        self.pattern_status(pat).ready_root()
    }

    /// Get the resolved Seq methods for a for-loop statement.
    pub fn for_loop_seq(&self, stmt: StmtId) -> Option<&ForLoopSeq<'db>> {
        self.for_loop_seq[stmt].as_ref()
    }

    pub fn binding_source(
        &self,
        db: &'db dyn HirAnalysisDb,
        binding: LocalBinding<'db>,
    ) -> Option<BindingSource> {
        let LocalBinding::Local { pat, .. } = binding else {
            return None;
        };
        let body = self.body()?;

        for (_, stmt) in body.stmts(db).iter() {
            if let Partial::Present(crate::hir_def::Stmt::Let(stmt_pat, _, Some(init_expr))) = stmt
                && let Some(field_path) = self.binding_field_path_in_pat(db, body, *stmt_pat, pat)
            {
                return Some(BindingSource {
                    init_expr: *init_expr,
                    field_path,
                });
            }
        }

        None
    }

    fn binding_field_path_in_pat(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        pat: PatId,
        binding_pat: PatId,
    ) -> Option<Vec<usize>> {
        if pat == binding_pat {
            return Some(Vec::new());
        }

        match pat.data(db, body) {
            Partial::Present(Pat::Tuple(pats)) | Partial::Present(Pat::PathTuple(_, pats)) => {
                pats.iter().enumerate().find_map(|(field_idx, pat)| {
                    self.binding_field_path_in_pat(db, body, *pat, binding_pat)
                        .map(|suffix| {
                            let mut path = Vec::with_capacity(suffix.len() + 1);
                            path.push(field_idx);
                            path.extend(suffix);
                            path
                        })
                })
            }
            Partial::Present(Pat::Record(_, fields)) => {
                let owner_ty = self.pat_ty(db, pat);
                fields.iter().find_map(|field| {
                    let label = field.label(db, body)?;
                    let field_idx = RecordLike::Type(owner_ty).record_field_idx(db, label)?;
                    self.binding_field_path_in_pat(db, body, field.pat, binding_pat)
                        .map(|suffix| {
                            let mut path = Vec::with_capacity(suffix.len() + 1);
                            path.push(field_idx);
                            path.extend(suffix);
                            path
                        })
                })
            }
            Partial::Present(Pat::Or(lhs, rhs)) => {
                let lhs = self.binding_field_path_in_pat(db, body, *lhs, binding_pat);
                let rhs = self.binding_field_path_in_pat(db, body, *rhs, binding_pat);
                match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) if lhs == rhs => Some(lhs),
                    (Some(path), None) | (None, Some(path)) => Some(path),
                    _ => None,
                }
            }
            Partial::Present(Pat::WildCard | Pat::Rest | Pat::Lit(_) | Pat::Path(_, _))
            | Partial::Absent => None,
        }
    }

    pub fn return_provenance(&self, db: &'db dyn HirAnalysisDb) -> ReturnProvenance {
        let Some(body) = self.body() else {
            return ReturnProvenance::Unknown;
        };
        let Some(func) = body.containing_func(db) else {
            return ReturnProvenance::Unknown;
        };
        let mut seen = FxHashSet::default();
        self.return_provenance_for_func(db, func, &mut seen)
    }

    fn return_provenance_for_func(
        &self,
        db: &'db dyn HirAnalysisDb,
        func: Func<'db>,
        seen: &mut FxHashSet<Func<'db>>,
    ) -> ReturnProvenance {
        if !seen.insert(func) {
            return ReturnProvenance::Unknown;
        }

        let (diags, typed_body) = check_func_body(db, func);
        if !diags.is_empty() {
            seen.remove(&func);
            return ReturnProvenance::Unknown;
        }
        let Some(body) = typed_body.body() else {
            seen.remove(&func);
            return ReturnProvenance::Unknown;
        };
        let Some(func_body) = func.body(db) else {
            seen.remove(&func);
            return ReturnProvenance::Unknown;
        };
        let root_expr = func_body.expr(db);
        let mut out = FxHashSet::default();
        let mut saw_non_param = false;
        typed_body.collect_explicit_return_param_sources_in_expr(
            db,
            body,
            root_expr,
            &mut out,
            &mut saw_non_param,
            seen,
        );
        typed_body.collect_implicit_return_param_sources_from_expr(
            db,
            body,
            root_expr,
            &mut out,
            &mut saw_non_param,
            seen,
        );
        if !saw_non_param && !out.is_empty() {
            let mut indices = out.into_iter().collect::<Vec<_>>();
            indices.sort_unstable();
            if !indices.is_empty() {
                seen.remove(&func);
                return ReturnProvenance::ForwardedParams(indices);
            }
        }

        seen.remove(&func);
        ReturnProvenance::Fresh
    }

    fn forwarded_return_param_sources_from_expr(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: ExprId,
        seen: &mut FxHashSet<Func<'db>>,
        visited_locals: &mut FxHashSet<PatId>,
    ) -> Option<Vec<usize>> {
        let Partial::Present(expr_data) = expr.data(db, body) else {
            return None;
        };

        match expr_data {
            Expr::Block(stmts) => {
                let tail = stmts.last()?;
                match tail.data(db, body) {
                    Partial::Present(crate::hir_def::Stmt::Expr(tail_expr)) => self
                        .forwarded_return_param_sources_from_expr(
                            db,
                            body,
                            *tail_expr,
                            seen,
                            visited_locals,
                        ),
                    Partial::Present(crate::hir_def::Stmt::Return(Some(return_expr))) => self
                        .forwarded_return_param_sources_from_expr(
                            db,
                            body,
                            *return_expr,
                            seen,
                            visited_locals,
                        ),
                    _ => None,
                }
            }
            Expr::If(_, then_expr, else_expr) => merge_forwarded_param_sets(
                self.forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    *then_expr,
                    seen,
                    visited_locals,
                ),
                else_expr.and_then(|else_expr| {
                    self.forwarded_return_param_sources_from_expr(
                        db,
                        body,
                        else_expr,
                        seen,
                        visited_locals,
                    )
                }),
            ),
            Expr::Match(_, arms) => {
                let Partial::Present(arms) = arms else {
                    return None;
                };
                let mut merged = FxHashSet::default();
                for arm in arms {
                    for idx in self.forwarded_return_param_sources_from_expr(
                        db,
                        body,
                        arm.body,
                        seen,
                        visited_locals,
                    )? {
                        merged.insert(idx);
                    }
                }
                let mut out = merged.into_iter().collect::<Vec<_>>();
                out.sort_unstable();
                Some(out)
            }
            Expr::With(_, with_body) => self.forwarded_return_param_sources_from_expr(
                db,
                body,
                *with_body,
                seen,
                visited_locals,
            ),
            Expr::RecordInit(_, fields)
                if self.expr_ty(db, expr).field_types(db).len() == 1 && fields.len() == 1 =>
            {
                self.forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    fields[0].expr,
                    seen,
                    visited_locals,
                )
            }
            Expr::Tuple(items)
                if self.expr_ty(db, expr).field_types(db).len() == 1 && items.len() == 1 =>
            {
                self.forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    items[0],
                    seen,
                    visited_locals,
                )
            }
            Expr::Path(_) => match self.expr_binding(expr)? {
                LocalBinding::Param { idx, .. } => {
                    self.path_expr_reuses_local(expr).then_some(vec![idx])
                }
                binding @ LocalBinding::Local { pat, .. } => {
                    if !visited_locals.insert(pat) {
                        return None;
                    }
                    if !self.path_expr_reuses_local(expr) {
                        visited_locals.remove(&pat);
                        return None;
                    }
                    let sources = self
                        .binding_source(db, binding)
                        .and_then(|source| source.field_path.is_empty().then_some(source.init_expr))
                        .and_then(|init_expr| {
                            self.forwarded_return_param_sources_from_expr(
                                db,
                                body,
                                init_expr,
                                seen,
                                visited_locals,
                            )
                        });
                    visited_locals.remove(&pat);
                    sources
                }
                LocalBinding::EffectParam { .. } => None,
            },
            Expr::Call(_, args) => {
                let callable = self.callable_expr(expr)?;
                let CallableDef::Func(func) = callable.callable_def else {
                    return None;
                };
                let callee_sources =
                    self.forwarded_return_param_sources_from_callable(db, func, seen)?;
                let mut merged = FxHashSet::default();
                for idx in callee_sources {
                    let arg = args.get(idx)?;
                    for source in self.forwarded_return_param_sources_from_expr(
                        db,
                        body,
                        arg.expr,
                        seen,
                        visited_locals,
                    )? {
                        merged.insert(source);
                    }
                }
                let mut out = merged.into_iter().collect::<Vec<_>>();
                out.sort_unstable();
                Some(out)
            }
            Expr::MethodCall(receiver, _, _, args) => {
                let callable = self.callable_expr(expr)?;
                let CallableDef::Func(func) = callable.callable_def else {
                    return None;
                };
                let callee_sources =
                    self.forwarded_return_param_sources_from_callable(db, func, seen)?;
                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.push(*receiver);
                call_args.extend(args.iter().map(|arg| arg.expr));
                let mut merged = FxHashSet::default();
                for idx in callee_sources {
                    let arg_expr = *call_args.get(idx)?;
                    for source in self.forwarded_return_param_sources_from_expr(
                        db,
                        body,
                        arg_expr,
                        seen,
                        visited_locals,
                    )? {
                        merged.insert(source);
                    }
                }
                let mut out = merged.into_iter().collect::<Vec<_>>();
                out.sort_unstable();
                Some(out)
            }
            _ => None,
        }
    }

    fn forwarded_return_param_sources_from_callable(
        &self,
        db: &'db dyn HirAnalysisDb,
        func: Func<'db>,
        seen: &mut FxHashSet<Func<'db>>,
    ) -> Option<Vec<usize>> {
        if !seen.insert(func) {
            return None;
        }

        let (diags, typed_body) = check_func_body(db, func);
        if !diags.is_empty() {
            seen.remove(&func);
            return None;
        }
        let body = typed_body.body()?;
        let root_expr = func.body(db)?.expr(db);
        let sources = typed_body.forwarded_return_param_sources_from_expr(
            db,
            body,
            root_expr,
            seen,
            &mut FxHashSet::default(),
        );
        seen.remove(&func);
        sources
    }

    fn collect_explicit_return_param_sources_in_stmt(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        stmt: StmtId,
        out: &mut FxHashSet<usize>,
        saw_non_param: &mut bool,
        seen: &mut FxHashSet<Func<'db>>,
    ) {
        let Partial::Present(stmt_data) = stmt.data(db, body) else {
            return;
        };

        match stmt_data {
            crate::hir_def::Stmt::Let(_, _, Some(init)) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *init,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            crate::hir_def::Stmt::For(_, iter, loop_body, _) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *iter,
                    out,
                    saw_non_param,
                    seen,
                );
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *loop_body,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            crate::hir_def::Stmt::While(cond, loop_body) => {
                self.collect_explicit_return_param_sources_in_cond(
                    db,
                    body,
                    *cond,
                    out,
                    saw_non_param,
                    seen,
                );
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *loop_body,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            crate::hir_def::Stmt::Return(Some(expr)) => {
                let mut visited_locals = FxHashSet::default();
                if let Some(indices) = self.forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    *expr,
                    seen,
                    &mut visited_locals,
                ) {
                    out.extend(indices);
                } else {
                    *saw_non_param = true;
                }
            }
            crate::hir_def::Stmt::Expr(expr) => self.collect_explicit_return_param_sources_in_expr(
                db,
                body,
                *expr,
                out,
                saw_non_param,
                seen,
            ),
            crate::hir_def::Stmt::Let(_, _, None)
            | crate::hir_def::Stmt::Return(None)
            | crate::hir_def::Stmt::Continue
            | crate::hir_def::Stmt::Break => {}
        }
    }

    fn collect_explicit_return_param_sources_in_expr(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: ExprId,
        out: &mut FxHashSet<usize>,
        saw_non_param: &mut bool,
        seen: &mut FxHashSet<Func<'db>>,
    ) {
        let Partial::Present(expr_data) = expr.data(db, body) else {
            return;
        };

        match expr_data {
            Expr::Block(stmts) => {
                for stmt in stmts {
                    self.collect_explicit_return_param_sources_in_stmt(
                        db,
                        body,
                        *stmt,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::Bin(lhs, rhs, _) | Expr::Assign(lhs, rhs) | Expr::AugAssign(lhs, rhs, _) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *lhs,
                    out,
                    saw_non_param,
                    seen,
                );
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *rhs,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            Expr::Un(inner, _) | Expr::Cast(inner, _) | Expr::Field(inner, _) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *inner,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            Expr::Call(callee, args) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *callee,
                    out,
                    saw_non_param,
                    seen,
                );
                for arg in args {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        arg.expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::Assert(args) => {
                for arg in args {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        arg.expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::MethodCall(receiver, _, _, args) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *receiver,
                    out,
                    saw_non_param,
                    seen,
                );
                for arg in args {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        arg.expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::RecordInit(_, fields) => {
                for field in fields {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        field.expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::Tuple(items) | Expr::Array(items) => {
                for item in items {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        *item,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::ArrayRep(value, _) => self.collect_explicit_return_param_sources_in_expr(
                db,
                body,
                *value,
                out,
                saw_non_param,
                seen,
            ),
            Expr::If(cond, then_expr, else_expr) => {
                self.collect_explicit_return_param_sources_in_cond(
                    db,
                    body,
                    *cond,
                    out,
                    saw_non_param,
                    seen,
                );
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *then_expr,
                    out,
                    saw_non_param,
                    seen,
                );
                if let Some(else_expr) = else_expr {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        *else_expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::Match(scrutinee, arms) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *scrutinee,
                    out,
                    saw_non_param,
                    seen,
                );
                if let Partial::Present(arms) = arms {
                    for arm in arms {
                        self.collect_explicit_return_param_sources_in_expr(
                            db,
                            body,
                            arm.body,
                            out,
                            saw_non_param,
                            seen,
                        );
                    }
                }
            }
            Expr::With(bindings, with_body) => {
                for binding in bindings {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        binding.value,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *with_body,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            Expr::Lit(_) | Expr::Path(_) => {}
        }
    }

    fn collect_implicit_return_param_sources_from_expr(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: ExprId,
        out: &mut FxHashSet<usize>,
        saw_non_param: &mut bool,
        seen: &mut FxHashSet<Func<'db>>,
    ) {
        let Partial::Present(expr_data) = expr.data(db, body) else {
            return;
        };

        match expr_data {
            Expr::Block(stmts) => {
                if let Some(last_stmt) = stmts.last()
                    && let Partial::Present(crate::hir_def::Stmt::Expr(tail_expr)) =
                        last_stmt.data(db, body)
                {
                    self.collect_implicit_return_param_sources_from_expr(
                        db,
                        body,
                        *tail_expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::If(_, then_expr, else_expr) => {
                self.collect_implicit_return_param_sources_from_expr(
                    db,
                    body,
                    *then_expr,
                    out,
                    saw_non_param,
                    seen,
                );
                if let Some(else_expr) = else_expr {
                    self.collect_implicit_return_param_sources_from_expr(
                        db,
                        body,
                        *else_expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::Match(_, arms) => {
                if let Partial::Present(arms) = arms {
                    for arm in arms {
                        self.collect_implicit_return_param_sources_from_expr(
                            db,
                            body,
                            arm.body,
                            out,
                            saw_non_param,
                            seen,
                        );
                    }
                }
            }
            Expr::With(_, with_body) => self.collect_implicit_return_param_sources_from_expr(
                db,
                body,
                *with_body,
                out,
                saw_non_param,
                seen,
            ),
            _ => {
                let mut visited_locals = FxHashSet::default();
                let Some(indices) = self.forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    expr,
                    seen,
                    &mut visited_locals,
                ) else {
                    *saw_non_param = true;
                    return;
                };
                out.extend(indices);
            }
        }
    }

    fn collect_explicit_return_param_sources_in_cond(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        cond: crate::hir_def::CondId,
        out: &mut FxHashSet<usize>,
        saw_non_param: &mut bool,
        seen: &mut FxHashSet<Func<'db>>,
    ) {
        let Partial::Present(cond_data) = cond.data(db, body) else {
            return;
        };

        match cond_data {
            crate::hir_def::Cond::Expr(expr) => self.collect_explicit_return_param_sources_in_expr(
                db,
                body,
                *expr,
                out,
                saw_non_param,
                seen,
            ),
            crate::hir_def::Cond::Let(_, value) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *value,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            crate::hir_def::Cond::Bin(lhs, rhs, _) => {
                self.collect_explicit_return_param_sources_in_cond(
                    db,
                    body,
                    *lhs,
                    out,
                    saw_non_param,
                    seen,
                );
                self.collect_explicit_return_param_sources_in_cond(
                    db,
                    body,
                    *rhs,
                    out,
                    saw_non_param,
                    seen,
                );
            }
        }
    }

    /// Get the definition span for an expression that references a local binding.
    ///
    /// Returns `Some(span)` if the expression references a local variable, parameter,
    /// or effect parameter. Returns `None` if the expression doesn't have a binding
    /// or if no body is available.
    ///
    /// This is used by the language server for goto-definition on local variables.
    pub fn expr_binding_def_span(&self, func: Func<'db>, expr: ExprId) -> Option<DynLazySpan<'db>> {
        let body = self.body?;
        let binding = self.expr_binding(expr)?;
        Some(binding.def_span_with(body, func))
    }

    /// Like `expr_binding_def_span` but takes a `Body` directly.
    ///
    /// Use this when the body may not belong to a function (e.g., contract bodies).
    pub fn expr_binding_def_span_in_body(
        &self,
        body: Body<'db>,
        expr: ExprId,
    ) -> Option<DynLazySpan<'db>> {
        let binding = self.expr_binding(expr)?;
        Some(binding.def_span_in_body(body))
    }

    /// Get the binding kind for an expression that references a local binding.
    ///
    /// Returns the identity of the binding (param index, pattern id, or effect param ident).
    pub fn expr_binding(&self, expr: ExprId) -> Option<LocalBinding<'db>> {
        self.expr_ty[expr].as_ref().and_then(|prop| prop.binding)
    }

    /// Returns a place representation for `expr` if it denotes an assignable location.
    pub fn expr_place(&self, expr: ExprId) -> Option<&Place<'db>> {
        self.expr_place[expr]
            .expand()
            .and_then(|place_id| self.expr_places.get(place_id))
    }

    /// Find all expressions that reference the same local binding as the given expression.
    ///
    /// Returns a list of ExprIds that share the same local binding (variable, parameter,
    /// or effect parameter). Returns an empty list if the expression doesn't have a binding.
    ///
    /// This is used by the language server for find-all-references and rename on local variables.
    pub fn local_references(&self, expr: ExprId) -> Vec<ExprId> {
        let Some(binding) = self.expr_ty[expr].as_ref().and_then(|prop| prop.binding) else {
            return vec![];
        };

        self.expr_ty
            .iter()
            .filter_map(|(id, prop)| {
                if prop.as_ref().and_then(|prop| prop.binding) == Some(binding) {
                    Some(id)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Find all expressions that reference a binding by its kind.
    ///
    /// This is the general method for finding all references to any kind of binding
    /// (param, local, or effect param).
    pub fn references_by_binding(&self, binding: LocalBinding<'db>) -> Vec<ExprId> {
        self.expr_ty
            .iter()
            .filter_map(|(id, prop)| {
                if prop.as_ref().and_then(|prop| prop.binding) == Some(binding) {
                    Some(id)
                } else {
                    None
                }
            })
            .collect()
    }

    fn empty(db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            body: None,
            result_ty: TyId::unit(db),
            assumptions: PredicateListId::empty_list(db),
            pat_ty: SecondaryMap::new(),
            expr_ty: SecondaryMap::new(),
            implicit_moves: FxHashSet::default(),
            const_refs: SecondaryMap::new(),
            value_path_refs: SecondaryMap::new(),
            semantic_expr_lowering: SecondaryMap::new(),
            record_init_lowering: SecondaryMap::new(),
            resolved_field_index: SecondaryMap::new(),
            call_effect_args: SecondaryMap::new(),
            return_borrow_provider: None,
            param_bindings: Vec::new(),
            pat_bindings: SecondaryMap::new(),
            pat_binding_modes: SecondaryMap::new(),
            pattern_store: PatternStore::default(),
            pattern_status: SecondaryMap::with_default(PatternAnalysisStatus::Invalid),
            for_loop_seq: SecondaryMap::new(),
            expr_place: SecondaryMap::new(),
            expr_places: PrimaryMap::new(),
        }
    }
}

pub(super) fn manual_contract_root_ref_from_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<SemanticCodeRegionRef<'db>> {
    let ty = strip_code_region_token_wrapper(db, ty);
    let TyData::TyBase(TyBase::Func(CallableDef::Func(func))) = ty.base_ty(db).data(db) else {
        return None;
    };
    match func.manual_contract_root_attr(db)? {
        ManualContractRootAttr::Init { .. } | ManualContractRootAttr::Runtime { .. } => {
            Some(SemanticCodeRegionRef::ManualContractRoot { func: *func })
        }
        ManualContractRootAttr::Error(_) => None,
    }
}

/// Returns true if the trait-const goal's args contain generic params that
/// are not in scope at `scope`. Such params come from the selected impl's or
/// trait's own binder and are existentials awaiting inference (e.g.
/// `GenericForwardStruct<7>::B` selecting through
/// `impl<const N> GenericForward<N> for GenericForwardStruct<N>`); checking
/// the goal with them treated as rigid universals produces false
/// "unsatisfied bound" errors.
pub(super) fn trait_const_goal_has_foreign_params<'db>(
    db: &'db dyn HirAnalysisDb,
    inst: TraitInstId<'db>,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
) -> bool {
    use crate::analysis::ty::visitor::{TyVisitable, TyVisitor, walk_ty};

    fn is_enclosing_item<'db>(
        db: &'db dyn HirAnalysisDb,
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        owner: crate::hir_def::scope_graph::ScopeId<'db>,
    ) -> bool {
        let owner_item = owner.item();
        let mut item = Some(scope.item());
        while let Some(current) = item {
            if current == owner_item {
                return true;
            }
            item = current.scope().parent_item(db);
        }
        false
    }

    struct ForeignParamFinder<'db> {
        db: &'db dyn HirAnalysisDb,
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        found: bool,
    }

    impl<'db> TyVisitor<'db> for ForeignParamFinder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.found {
                return;
            }
            let param = match ty.data(self.db) {
                TyData::TyParam(param) => Some(param),
                TyData::ConstTy(const_ty) => match const_ty.data(self.db) {
                    crate::analysis::ty::const_ty::ConstTyData::TyParam(param, _) => Some(param),
                    _ => None,
                },
                _ => None,
            };
            if let Some(param) = param
                && !is_enclosing_item(self.db, self.scope, param.owner)
            {
                self.found = true;
                return;
            }
            walk_ty(self, ty);
        }
    }

    let mut finder = ForeignParamFinder {
        db,
        scope,
        found: false,
    };
    for &arg in inst.args(db) {
        arg.visit_with(&mut finder);
        if finder.found {
            return true;
        }
    }
    false
}

pub(super) fn ty_may_be_code_region_token<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    let ty = strip_code_region_token_wrapper(db, ty);
    manual_contract_root_ref_from_ty(db, ty).is_some()
        || matches!(
            ty.base_ty(db).data(db),
            TyData::TyParam(_) | TyData::TyVar(_)
        )
}

fn strip_code_region_token_wrapper<'db>(
    db: &'db dyn HirAnalysisDb,
    mut ty: TyId<'db>,
) -> TyId<'db> {
    while let Some((_, inner)) = ty.as_capability(db) {
        ty = inner;
    }
    ty
}

fn merge_forwarded_param_sets(
    lhs: Option<Vec<usize>>,
    rhs: Option<Vec<usize>>,
) -> Option<Vec<usize>> {
    let mut merged = FxHashSet::default();
    for idx in lhs?.into_iter().chain(rhs?) {
        merged.insert(idx);
    }
    let mut out = merged.into_iter().collect::<Vec<_>>();
    out.sort_unstable();
    Some(out)
}

#[derive(Clone, PartialEq, Eq, derive_more::From)]
enum Typeable<'db> {
    Expr(ExprId, ExprProp<'db>),
    Pat(PatId),
}

impl Typeable<'_> {
    fn span(self, body: Body) -> DynLazySpan {
        match self {
            Self::Expr(expr, ..) => expr.span(body).into(),
            Self::Pat(pat) => pat.span(body).into(),
        }
    }
}

fn try_instantiate_trait_method<'db>(
    db: &'db dyn HirAnalysisDb,
    method: Func<'db>,
    table: &mut UnificationTable<'db>,
    receiver_ty: TyId<'db>,
    inst: TraitInstId<'db>,
) -> Result<TyId<'db>, UnificationError> {
    let ty = TyId::foldl(
        db,
        TyId::func(db, method.as_callable(db).unwrap()),
        inst.args(db),
    );

    let inst_self = table.instantiate_to_term(inst.self_ty(db));
    table.unify(inst_self, receiver_ty)?;

    // Apply associated type substitutions from the trait instance
    use crate::analysis::ty::fold::{AssocTySubst, TyFoldable};
    let mut subst = AssocTySubst::new(inst);
    Ok(ty.fold_with(db, &mut subst))
}

fn instantiate_trait_method<'db>(
    db: &'db dyn HirAnalysisDb,
    method: Func<'db>,
    table: &mut UnificationTable<'db>,
    receiver_ty: TyId<'db>,
    inst: TraitInstId<'db>,
) -> TyId<'db> {
    try_instantiate_trait_method(db, method, table, receiver_ty, inst)
        .expect("selected trait method candidate receiver should unify")
}

/// Instantiate a trait-associated function type (no receiver), e.g. `T::make`.
fn instantiate_trait_assoc_fn<'db>(
    db: &'db dyn HirAnalysisDb,
    method: CallableDef<'db>,
    inst: TraitInstId<'db>,
) -> TyId<'db> {
    let ty = TyId::foldl(db, TyId::func(db, method), inst.args(db));

    // Apply associated type substitutions from the trait instance
    use crate::analysis::ty::fold::{AssocTySubst, TyFoldable};
    let mut subst = AssocTySubst::new(inst);
    ty.fold_with(db, &mut subst)
}

struct TyCheckerFinalizer<'db> {
    db: &'db dyn HirAnalysisDb,
    body: TypedBody<'db>,
    assumptions: PredicateListId<'db>,
    ty_vars: FxHashSet<InferenceKey<'db>>,
    effect_provider_keys: FxHashSet<InferenceKey<'db>>,
    direct_call_callees: FxHashSet<ExprId>,
    diags: Vec<FuncBodyDiag<'db>>,
}

impl<'db> Visitor<'db> for TyCheckerFinalizer<'db> {
    fn visit_pat(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyPatSpan<'db>>,
        pat: PatId,
        _: &Pat<'db>,
    ) {
        let ty = self.body.pat_ty(self.db, pat);
        let span = ctxt.span().unwrap();
        self.check_unknown(ty, span.clone().into());

        walk_pat(self, ctxt, pat)
    }

    fn visit_expr(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
        expr: ExprId,
        expr_data: &Expr<'db>,
    ) {
        // Skip the check if the expr is block.
        if !matches!(expr_data, Expr::Block(..)) {
            let prop = self.body.expr_prop(self.db, expr);
            let span = ctxt.span().unwrap();
            self.check_unknown(prop.ty, span.clone().into());
            let is_direct_call_callee =
                matches!(expr_data, Expr::Path(..)) && self.direct_call_callees.contains(&expr);
            if prop.binding.is_none() && !is_direct_call_callee {
                self.check_wf(prop.ty, span.into());
            }
        }

        // We need this additional check for method call because the callable type is
        // not tied to the expression type. Well-formedness is not re-checked
        // here: the callable's declared constraints were already registered as
        // call-constraint obligations (with a `required by this bound` note),
        // so a WF check would report every unsatisfied bound a second time —
        // the same reason direct-call callees are excluded above.
        if let Expr::MethodCall(..) = expr_data
            && let Some(callable) = self.body.callable_expr(expr)
        {
            let callable_ty = callable.ty(self.db);
            let span = ctxt.span().unwrap().into_method_call_expr().method_name();
            self.check_unknown(callable_ty, span.into());
        }

        walk_expr(self, ctxt, expr);
    }

    fn visit_item(
        &mut self,
        _: &mut VisitorCtxt<'db, crate::core::visitor::prelude::LazyItemSpan<'db>>,
        _: crate::core::hir_def::ItemKind<'db>,
    ) {
    }
}

impl<'db> TyCheckerFinalizer<'db> {
    fn new(mut checker: TyChecker<'db>) -> Self {
        let assumptions = checker.env.assumptions();
        checker.resolve_deferred();
        let mut body = checker.env.finish(&mut checker.table);
        body.return_borrow_provider = checker
            .first_return_borrow_provider
            .map(|(_, provider)| provider);
        let direct_call_callees = body.body.map_or_else(FxHashSet::default, |body_id| {
            body_id
                .exprs(checker.db)
                .iter()
                .filter_map(|(_expr, expr_data)| match expr_data {
                    Partial::Present(Expr::Call(callee, ..)) => Some(*callee),
                    _ => None,
                })
                .collect()
        });

        Self {
            db: checker.db,
            body,
            assumptions,
            ty_vars: FxHashSet::default(),
            effect_provider_keys: checker.effect_provider_keys,
            direct_call_callees,
            diags: checker.diags,
        }
    }

    fn finish(mut self) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
        self.check_unknown_types();
        (self.diags, self.body)
    }

    fn check_unknown_types(&mut self) {
        if let Some(body) = self.body.body {
            let mut ctxt = VisitorCtxt::with_body(self.db, body);
            self.visit_body(&mut ctxt, body);
        }
    }

    fn check_unknown(&mut self, ty: TyId<'db>, span: DynLazySpan<'db>) {
        let flags = ty.flags(self.db);
        if flags.contains(TyFlags::HAS_INVALID) || !flags.contains(TyFlags::HAS_VAR) {
            return;
        }

        let keys = inference_keys(self.db, &ty);
        if !keys.is_empty()
            && keys
                .iter()
                .all(|key| self.effect_provider_keys.contains(key))
        {
            return;
        }

        let mut skip_diag = false;
        for key in keys {
            // If at least one of the inference keys are already seen, we will skip emitting
            // diagnostics.
            skip_diag |= !self.ty_vars.insert(key);
        }

        if !skip_diag {
            let diag = BodyDiag::TypeAnnotationNeeded { span, ty };
            self.diags.push(diag.into())
        }
    }

    fn check_wf(&mut self, ty: TyId<'db>, span: DynLazySpan<'db>) {
        let flags = ty.flags(self.db);
        if flags.contains(TyFlags::HAS_INVALID) || flags.contains(TyFlags::HAS_VAR) {
            return;
        }

        let solve_cx = TraitSolveCx::new(self.db, self.body.body.unwrap().scope());
        if let Some(diag) = ty.emit_wf_diag(self.db, solve_cx, self.assumptions, span) {
            self.diags.push(diag.into());
        }
    }
}
