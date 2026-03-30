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
    ResolvedRecvVariant, VariantResError, check_contract_init_body, check_contract_recv_arm_body,
    check_contract_recv_block, check_contract_recv_blocks, resolve_variant_bare,
    resolve_variant_in_msg,
};
pub use self::path::RecordLike;
use crate::analysis::name_resolution::resolve_path;
use crate::analysis::ty::corelib::resolve_lib_type_path;
use crate::analysis::ty::fold::TyFoldable;
use crate::analysis::ty::visitor::TyVisitable;
use crate::hir_def::CallableDef;
use crate::{
    hir_def::{
        Body, Const, Contract, ContractRecvArm, Expr, ExprId, Func, GenericParam,
        GenericParamOwner, ItemKind, LitKind, Partial, Pat, PatId, PathId, StmtId, TypeBound,
        TypeId as HirTyId, WhereClauseOwner,
    },
    span::{
        DynLazySpan, expr::LazyExprSpan, pat::LazyPatSpan, path::LazyPathSpan, types::LazyTySpan,
    },
    visitor::{Visitor, VisitorCtxt, walk_expr, walk_pat},
};
pub use callable::Callable;
use callable::{CallGenericArgUnifyError, unify_explicit_call_generic_args};
use common::indexmap::IndexMap;
use ena::unify::InPlace;
use env::TyCheckEnv;
pub use env::{
    ConcreteBorrowProvider, EffectParamSite, ExprProp, LocalBinding, ParamSite, PatBindingMode,
};
pub(super) use expr::TraitOps;
pub use owner::BodyOwner;
pub use owner::EffectParamOwner;
pub use stmt::ForLoopSeq;

use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;

use crate::analysis::place::{Place, PlaceBase};

use super::{
    assoc_const::AssocConstUse,
    canonical::Canonical,
    diagnostics::{
        BodyDiag, CallConstraintDiagInfo, FuncBodyDiag, TraitConstraintDiag, TyDiagCollection,
        TyLowerDiag,
    },
    effects::{EffectKeyKind, resolve_normalized_type_effect_key},
    trait_def::TraitInstId,
    trait_resolution::{
        CanonicalGoalQuery, GoalSatisfiability, PredicateListId, TraitSolveCx,
        is_goal_query_satisfiable, is_goal_satisfiable,
    },
    ty_contains_const_hole,
    ty_def::{BorrowKind, CapabilityKind, InvalidCause, Kind, TyId, TyVarSort},
    ty_lower::lower_hir_ty,
    unify::{InferenceKey, Snapshot, UnificationError, UnificationTable},
};
use crate::analysis::ty::ty_def::{TyBase, TyData};
use crate::analysis::ty::{
    const_ty::ConstTyData,
    ctfe::{CtfeConfig, CtfeInterpreter},
    fold::AssocTySubst,
    normalize::normalize_ty,
    pattern_ir::{PatternAnalysisStatus, PatternStore, ValidatedPatId},
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

pub(super) fn check_body<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    let Ok(mut checker) = TyChecker::new(db, owner) else {
        return (Vec::new(), TypedBody::empty(db));
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
        let mut interp = CtfeInterpreter::new(db, CtfeConfig::default());
        match interp.eval_const_body(body, typed_body.clone()) {
            Ok(const_ty) => {
                if !matches!(const_ty.data(db), ConstTyData::Evaluated(..)) {
                    diags.push(BodyDiag::ConstValueMustBeKnown(body.span().into()).into());
                }
            }
            Err(cause) => {
                let ty = TyId::invalid(db, cause);
                if let Some(diag) = ty.emit_diag(db, body.span().into()) {
                    diags.push(diag.into());
                }
            }
        }
    }

    (diags, typed_body)
}

pub struct TyChecker<'db> {
    pub(crate) db: &'db dyn HirAnalysisDb,
    pub(crate) env: TyCheckEnv<'db>,
    pub(crate) table: UnificationTable<'db>,
    expected: TyId<'db>,
    effect_provider_keys: FxHashSet<InferenceKey<'db>>,
    first_return_borrow_provider: Option<(DynLazySpan<'db>, ConcreteBorrowProvider)>,
    diags: Vec<FuncBodyDiag<'db>>,
}

pub(crate) struct TyCheckerSnapshot<'db> {
    table: Snapshot<InPlace<InferenceKey<'db>>>,
    deferred_len: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DestructureSourceMode {
    Owned,
    Borrow(BorrowKind),
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

            if resolve_path(
                self.db,
                key_path,
                func.scope(),
                self.env.assumptions(),
                false,
            )
            .is_err()
            {
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
        let owner = match owner {
            BodyOwner::Func(func) => EffectParamOwner::Func(func),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => unreachable!(),
            BodyOwner::ContractInit { contract } => EffectParamOwner::ContractInit { contract },
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => EffectParamOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            },
        };
        let contract_effect_names: FxHashSet<_> = contract
            .effects(self.db)
            .data(self.db)
            .iter()
            .filter_map(|e| e.name)
            .collect();
        let contract_field_names: FxHashSet<_> = crate::hir_def::FieldParent::Contract(contract)
            .fields(self.db)
            .filter_map(|f| f.name(self.db))
            .collect();

        let assumptions = PredicateListId::empty_list(self.db);
        let root_effect_ty =
            super::resolve_default_root_effect_ty(self.db, contract.scope(), assumptions);

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
                let resolved = resolve_path(
                    self.db,
                    key_path,
                    contract.scope(),
                    self.env.assumptions(),
                    false,
                );
                match resolved {
                    Ok(PathRes::Trait(trait_inst)) => {
                        let Some(root_effect_ty) = root_effect_ty else {
                            continue;
                        };
                        let trait_req =
                            super::instantiate_trait_self(self.db, trait_inst, root_effect_ty);
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
                    Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) => {
                        let given = resolve_normalized_type_effect_key(
                            self.db,
                            key_path,
                            contract.scope(),
                            assumptions,
                        )
                        .map(|ty| normalize_ty(self.db, ty, contract.scope(), assumptions))
                        .unwrap_or_else(|| {
                            normalize_ty(self.db, ty, contract.scope(), assumptions)
                        });
                        if !given.is_zero_sized(self.db) {
                            self.push_diag(BodyDiag::ContractRootEffectTypeNotZeroSized {
                                owner,
                                key: key_path,
                                idx,
                                given,
                            });
                        }
                    }
                    Ok(_) | Err(_) => self.push_diag(BodyDiag::InvalidEffectKey {
                        owner,
                        key: key_path,
                        idx,
                    }),
                }
                continue;
            }

            // Unlabeled contract-scoped effects refer to a contract field name or an
            // existing named contract effect (e.g. `ctx`).
            let Some(ident) = key_path.ident(self.db).to_opt() else {
                self.push_diag(BodyDiag::InvalidEffectKey {
                    owner,
                    key: key_path,
                    idx,
                });
                continue;
            };

            if key_path.len(self.db) != 1
                || (!contract_effect_names.contains(&ident)
                    && !contract_field_names.contains(&ident))
            {
                self.push_diag(BodyDiag::InvalidEffectKey {
                    owner,
                    key: key_path,
                    idx,
                });
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

    fn concrete_borrow_provider_from_address_space(
        &self,
        address_space: TyId<'db>,
    ) -> Option<ConcreteBorrowProvider> {
        let scope = self.env.scope();
        let memory = resolve_lib_type_path(self.db, scope, "core::effect_ref::Memory")?;
        let storage = resolve_lib_type_path(self.db, scope, "core::effect_ref::Storage")?;
        let transient =
            resolve_lib_type_path(self.db, scope, "core::effect_ref::TransientStorage")?;
        let calldata = resolve_lib_type_path(self.db, scope, "core::effect_ref::Calldata")?;

        if address_space == memory {
            Some(ConcreteBorrowProvider::Memory)
        } else if address_space == storage {
            Some(ConcreteBorrowProvider::Storage)
        } else if address_space == transient {
            Some(ConcreteBorrowProvider::TransientStorage)
        } else if address_space == calldata {
            Some(ConcreteBorrowProvider::Calldata)
        } else {
            None
        }
    }

    fn concrete_borrow_provider_for_effect_field(
        &self,
        site: EffectParamSite<'db>,
        idx: usize,
    ) -> Option<ConcreteBorrowProvider> {
        let contract = match site {
            EffectParamSite::Func(_) => return None,
            EffectParamSite::Contract(contract)
            | EffectParamSite::ContractInit { contract }
            | EffectParamSite::ContractRecvArm { contract, .. } => contract,
        };
        let name = self.env.semantic_effect_binding(site, idx)?.binding_name;
        let field = contract.field_layout(self.db).get(&name)?;
        self.concrete_borrow_provider_from_address_space(field.address_space)
    }

    fn concrete_borrow_provider_for_contract_name(
        &self,
        site: EffectParamSite<'db>,
        name: crate::hir_def::IdentId<'db>,
    ) -> Option<ConcreteBorrowProvider> {
        let contract = match site {
            EffectParamSite::Func(_) => return None,
            EffectParamSite::Contract(contract)
            | EffectParamSite::ContractInit { contract }
            | EffectParamSite::ContractRecvArm { contract, .. } => contract,
        };
        if let Some(field) = contract.field_layout(self.db).get(&name) {
            return self.concrete_borrow_provider_from_address_space(field.address_space);
        }
        let root_effect_ty = super::resolve_default_root_effect_ty(
            self.db,
            contract.scope(),
            self.env.base_assumptions(),
        )?;
        self.concrete_borrow_provider_from_address_space(root_effect_ty)
    }

    fn concrete_borrow_provider_for_binding(
        &self,
        binding: LocalBinding<'db>,
    ) -> Option<ConcreteBorrowProvider> {
        match binding {
            LocalBinding::Local { pat, .. } => self.env.local_borrow_provider(pat),
            LocalBinding::EffectParam { site, .. } => self
                .concrete_borrow_provider_for_contract_name(site, binding.binding_name(&self.env)),
            LocalBinding::Param {
                site: ParamSite::EffectField(site),
                idx,
                ..
            } => self
                .concrete_borrow_provider_for_effect_field(site, idx)
                .or_else(|| {
                    self.concrete_borrow_provider_for_contract_name(
                        site,
                        binding.binding_name(&self.env),
                    )
                }),
            _ => None,
        }
    }

    fn concrete_borrow_provider_for_place(
        &self,
        place: &Place<'db>,
    ) -> Option<ConcreteBorrowProvider> {
        let PlaceBase::Binding(binding) = place.base;
        if self
            .env
            .lookup_binding_ty(&binding)
            .as_capability(self.db)
            .is_some()
        {
            return self.concrete_borrow_provider_for_binding(binding);
        }

        match binding {
            LocalBinding::EffectParam { .. } => self.concrete_borrow_provider_for_binding(binding),
            LocalBinding::Param {
                site: ParamSite::EffectField(..),
                ..
            } => self.concrete_borrow_provider_for_binding(binding),
            LocalBinding::Local { .. } | LocalBinding::Param { .. } => {
                Some(ConcreteBorrowProvider::Memory)
            }
        }
    }

    fn merge_concrete_borrow_providers(
        &mut self,
        previous_span: DynLazySpan<'db>,
        previous: Option<ConcreteBorrowProvider>,
        current_span: DynLazySpan<'db>,
        current: Option<ConcreteBorrowProvider>,
    ) -> Option<ConcreteBorrowProvider> {
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
                // Canonical trait solving can succeed even when the extracted
                // solution becomes cyclic in the current inference table.
                match self.table.unify(goal, solved) {
                    Ok(()) => {
                        if self.normalize_trait_goal(goal) != goal {
                            TraitObligationOutcome::Progressed
                        } else {
                            TraitObligationOutcome::Discharged
                        }
                    }
                    Err(UnificationError::OccursCheckFailed) => {
                        if !final_pass {
                            TraitObligationOutcome::Requeue(obligation)
                        } else {
                            self.push_diag(BodyDiag::InfiniteOccurrence(obligation.span));
                            TraitObligationOutcome::Discharged
                        }
                    }
                    Err(UnificationError::TypeMismatch) => {
                        if !final_pass {
                            TraitObligationOutcome::Requeue(obligation)
                        } else {
                            let required_by = match obligation.origin {
                                env::TraitObligationOrigin::CallConstraint {
                                    callable_def,
                                    constraint_idx,
                                    ..
                                } => self.call_constraint_diag_info(callable_def, constraint_idx),
                                env::TraitObligationOrigin::GenericConfirmation => None,
                            };
                            self.push_diag(TyDiagCollection::from(
                                TraitConstraintDiag::TraitBoundNotSat {
                                    span: obligation.span,
                                    primary_goal: goal,
                                    unsat_subgoal: None,
                                    required_by,
                                },
                            ));
                            TraitObligationOutcome::Discharged
                        }
                    }
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
                         inst: TraitInstId<'db>| {
            let snap = this.snapshot_state();

            let result = (|| {
                let recv_ty = {
                    let mut prober = env::Prober::new(&mut this.table);
                    pending.recv_ty.fold_with(db, &mut prober)
                };

                let inst_self = this.table.instantiate_to_term(inst.self_ty(db));
                if this.table.unify(inst_self, recv_ty).is_err() {
                    let recv_inner = recv_ty.as_capability(db).map(|(_, inner)| inner)?;
                    this.table.unify(inst_self, recv_inner).ok()?;
                }

                let trait_method = *inst.def(db).method_defs(db).get(&pending.method_name)?;
                let func_ty =
                    instantiate_trait_method(db, trait_method, &mut this.table, recv_ty, inst);
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
                            let mut prober = env::Prober::new(&mut self.table);
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
                            .filter(|&inst| {
                                is_viable(
                                    self,
                                    &pending,
                                    expr_ty,
                                    receiver,
                                    generic_args,
                                    call_args,
                                    inst,
                                )
                            })
                            .collect();
                        let viable = self.dedup_equivalent_trait_insts(viable);

                        if let [inst] = viable.as_slice() {
                            if self.env.callable_expr(pending.expr).is_none() {
                                let call_span = pending.expr.span(body).into_method_call_expr();

                                let receiver_prop = self
                                    .env
                                    .typed_expr(receiver)
                                    .unwrap_or_else(|| ExprProp::invalid(db));
                                let recv_ty = {
                                    let mut prober = env::Prober::new(&mut self.table);
                                    pending.recv_ty.fold_with(db, &mut prober)
                                };

                                let trait_method = *inst
                                    .def(db)
                                    .method_defs(db)
                                    .get(&pending.method_name)
                                    .unwrap();
                                let func_ty = self.instantiate_trait_method_to_term(
                                    trait_method,
                                    recv_ty,
                                    *inst,
                                );

                                let mut callable = match Callable::new(
                                    db,
                                    func_ty,
                                    receiver.span(body).into(),
                                    Some(*inst),
                                ) {
                                    Ok(callable) => callable,
                                    Err(diag) => {
                                        self.push_diag(diag);
                                        progressed = true;
                                        continue;
                                    }
                                };

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
                                self.env.register_callable(pending.expr, callable);
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
                        let mut prober = env::Prober::new(&mut self.table);
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
                        .filter(|&inst| {
                            is_viable(
                                self,
                                &pending,
                                expr_ty,
                                receiver,
                                generic_args,
                                call_args,
                                inst,
                            )
                        })
                        .collect();
                    let viable = self.dedup_equivalent_trait_insts(viable);
                    if viable.len() > 1 {
                        self.push_diag(BodyDiag::AmbiguousTrait {
                            primary: pending.span.clone(),
                            method_name: pending.method_name,
                            traits: viable.into_iter().collect(),
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
                self.table
                    .new_var(TyVarSort::String(len_bytes), &Kind::Star)
            }
        }
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

    fn destructure_source_mode(&self, ty: TyId<'db>) -> (TyId<'db>, DestructureSourceMode) {
        if let Some((kind, inner)) = ty.as_capability(self.db) {
            let borrow_kind = match kind {
                CapabilityKind::Mut => BorrowKind::Mut,
                CapabilityKind::Ref | CapabilityKind::View => BorrowKind::Ref,
            };
            (inner, DestructureSourceMode::Borrow(borrow_kind))
        } else {
            (ty, DestructureSourceMode::Owned)
        }
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
                let borrow_ty = match kind {
                    BorrowKind::Mut => TyId::borrow_mut_of(self.db, inner),
                    BorrowKind::Ref => TyId::borrow_ref_of(self.db, inner),
                };
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
            let diag = BodyDiag::TypeMismatch {
                span,
                expected,
                given: actual,
            };
            self.push_diag(diag);
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
                self.push_diag(BodyDiag::TypeMismatch {
                    span,
                    expected,
                    given: actual,
                });
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
        self.instantiate_to_term(ty)
    }

    fn instantiate_trait_assoc_fn_to_term(
        &mut self,
        method: CallableDef<'db>,
        inst: TraitInstId<'db>,
    ) -> TyId<'db> {
        let ty = instantiate_trait_assoc_fn(self.db, method, inst);
        self.instantiate_to_term(ty)
    }

    fn instantiate_inherent_method_to_term(
        &mut self,
        method: CallableDef<'db>,
        receiver_ty: TyId<'db>,
    ) -> TyId<'db> {
        let mut ty = TyId::func(self.db, method);
        for &arg in receiver_ty.generic_args(self.db) {
            if ty.applicable_ty(self.db).is_none() {
                break;
            }
            ty = TyId::app(self.db, ty, arg);
        }
        self.instantiate_to_term(ty)
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
    pub key: PathId<'db>,
    pub arg: EffectArg<'db>,
    pub pass_mode: EffectPassMode,
    pub key_kind: EffectKeyKind,
    pub instantiated_target_ty: Option<TyId<'db>>,
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

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct TypedBody<'db> {
    body: Option<Body<'db>>,
    assumptions: PredicateListId<'db>,
    pat_ty: FxHashMap<PatId, TyId<'db>>,
    expr_ty: FxHashMap<ExprId, ExprProp<'db>>,
    implicit_moves: FxHashSet<ExprId>,
    const_refs: FxHashMap<ExprId, ConstRef<'db>>,
    callables: FxHashMap<ExprId, Callable<'db>>,
    call_effect_args: FxHashMap<ExprId, Vec<ResolvedEffectArg<'db>>>,
    /// Bindings for function parameters (indexed by param position)
    param_bindings: Vec<LocalBinding<'db>>,
    /// Bindings for local variables (keyed by the pattern that introduces them)
    pat_bindings: FxHashMap<PatId, LocalBinding<'db>>,
    /// Binding capture mode for local variables (keyed by the pattern that introduces them)
    pat_binding_modes: FxHashMap<PatId, PatBindingMode>,
    pattern_store: PatternStore<'db>,
    pattern_status: FxHashMap<PatId, PatternAnalysisStatus>,
    /// Resolved Seq trait methods for for-loops
    for_loop_seq: FxHashMap<StmtId, ForLoopSeq<'db>>,
}

impl<'db> TyVisitable<'db> for TypedBody<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        self.assumptions.visit_with(visitor);
        for ty in self.pat_ty.values() {
            ty.visit_with(visitor);
        }
        for prop in self.expr_ty.values() {
            prop.visit_with(visitor);
        }
        for cref in self.const_refs.values() {
            cref.visit_with(visitor);
        }
        for callable in self.callables.values() {
            callable.visit_with(visitor);
        }
        self.pattern_store.visit_with(visitor);
        for seq in self.for_loop_seq.values() {
            seq.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for TypedBody<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        let assumptions = self.assumptions.fold_with(db, folder);
        let pat_ty = self
            .pat_ty
            .into_iter()
            .map(|(pat, ty)| (pat, ty.fold_with(db, folder)))
            .collect();
        let expr_ty = self
            .expr_ty
            .into_iter()
            .map(|(expr, prop)| (expr, prop.fold_with(db, folder)))
            .collect();
        let const_refs = self
            .const_refs
            .into_iter()
            .map(|(expr, cref)| (expr, cref.fold_with(db, folder)))
            .collect();
        let callables = self
            .callables
            .into_iter()
            .map(|(expr, callable)| (expr, callable.fold_with(db, folder)))
            .collect();
        let call_effect_args = self
            .call_effect_args
            .into_iter()
            .map(|(expr, args)| {
                (
                    expr,
                    args.into_iter()
                        .map(|arg| arg.fold_with(db, folder))
                        .collect(),
                )
            })
            .collect();
        let param_bindings = self
            .param_bindings
            .into_iter()
            .map(|binding| binding.fold_with(db, folder))
            .collect();
        let pat_bindings = self
            .pat_bindings
            .into_iter()
            .map(|(pat, binding)| (pat, binding.fold_with(db, folder)))
            .collect();
        let pattern_store = self.pattern_store.fold_with(db, folder);
        let for_loop_seq = self
            .for_loop_seq
            .into_iter()
            .map(|(stmt, seq)| (stmt, seq.fold_with(db, folder)))
            .collect();

        Self {
            body: self.body,
            assumptions,
            pat_ty,
            expr_ty,
            implicit_moves: self.implicit_moves,
            const_refs,
            callables,
            pat_binding_modes: self.pat_binding_modes,
            call_effect_args,
            param_bindings,
            pat_bindings,
            for_loop_seq,
            pattern_store,
            pattern_status: self.pattern_status,
        }
    }
}

impl<'db> TypedBody<'db> {
    pub fn body(&self) -> Option<Body<'db>> {
        self.body
    }

    pub fn assumptions(&self) -> PredicateListId<'db> {
        self.assumptions
    }

    pub fn expr_ty(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> TyId<'db> {
        self.expr_prop(db, expr).ty
    }

    pub fn expr_prop(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> ExprProp<'db> {
        self.expr_ty
            .get(&expr)
            .cloned()
            .unwrap_or_else(|| ExprProp::invalid(db))
    }

    pub fn is_implicit_move(&self, expr: ExprId) -> bool {
        self.implicit_moves.contains(&expr)
    }

    pub fn expr_const_ref(&self, expr: ExprId) -> Option<ConstRef<'db>> {
        self.const_refs.get(&expr).copied()
    }

    pub fn pat_ty(&self, db: &'db dyn HirAnalysisDb, pat: PatId) -> TyId<'db> {
        self.pat_ty
            .get(&pat)
            .copied()
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
    }

    pub fn callable_expr(&self, expr: ExprId) -> Option<&Callable<'db>> {
        self.callables.get(&expr)
    }

    pub fn call_effect_args(&self, call_expr: ExprId) -> Option<&[ResolvedEffectArg<'db>]> {
        self.call_effect_args.get(&call_expr).map(|v| v.as_slice())
    }

    /// Get the binding for a function parameter by index.
    pub fn param_binding(&self, idx: usize) -> Option<LocalBinding<'db>> {
        self.param_bindings.get(idx).copied()
    }

    /// Get the binding for a local variable by its pattern.
    pub fn pat_binding(&self, pat: PatId) -> Option<LocalBinding<'db>> {
        self.pat_bindings.get(&pat).copied()
    }

    /// Get how this local binding is captured by its source pattern destructuring.
    pub fn pat_binding_mode(&self, pat: PatId) -> Option<PatBindingMode> {
        self.pat_binding_modes.get(&pat).copied()
    }

    pub fn pattern_store(&self) -> &PatternStore<'db> {
        &self.pattern_store
    }

    pub fn pattern_status(&self, pat: PatId) -> PatternAnalysisStatus {
        self.pattern_status
            .get(&pat)
            .copied()
            .unwrap_or(PatternAnalysisStatus::Invalid)
    }

    pub fn pattern_root(&self, pat: PatId) -> Option<ValidatedPatId> {
        self.pattern_status(pat).ready_root()
    }

    /// Get the resolved Seq methods for a for-loop statement.
    pub fn for_loop_seq(&self, stmt: StmtId) -> Option<&ForLoopSeq<'db>> {
        self.for_loop_seq.get(&stmt)
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
        self.expr_ty.get(&expr)?.binding
    }

    /// Returns a place representation for `expr` if it denotes an assignable location.
    pub fn expr_place(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> Option<Place<'db>> {
        Place::from_expr(db, self, expr)
    }

    /// Find all expressions that reference the same local binding as the given expression.
    ///
    /// Returns a list of ExprIds that share the same local binding (variable, parameter,
    /// or effect parameter). Returns an empty list if the expression doesn't have a binding.
    ///
    /// This is used by the language server for find-all-references and rename on local variables.
    pub fn local_references(&self, expr: ExprId) -> Vec<ExprId> {
        let Some(binding) = self.expr_ty.get(&expr).and_then(|p| p.binding) else {
            return vec![];
        };

        self.expr_ty
            .iter()
            .filter_map(|(id, p)| {
                if p.binding == Some(binding) {
                    Some(*id)
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
            .filter_map(|(id, p)| {
                if p.binding == Some(binding) {
                    Some(*id)
                } else {
                    None
                }
            })
            .collect()
    }

    fn empty(db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            body: None,
            assumptions: PredicateListId::empty_list(db),
            pat_ty: FxHashMap::default(),
            expr_ty: FxHashMap::default(),
            implicit_moves: FxHashSet::default(),
            const_refs: FxHashMap::default(),
            callables: FxHashMap::default(),
            call_effect_args: FxHashMap::default(),
            param_bindings: Vec::new(),
            pat_bindings: FxHashMap::default(),
            pat_binding_modes: FxHashMap::default(),
            pattern_store: PatternStore::default(),
            pattern_status: FxHashMap::default(),
            for_loop_seq: FxHashMap::default(),
        }
    }
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

fn instantiate_trait_method<'db>(
    db: &'db dyn HirAnalysisDb,
    method: Func<'db>,
    table: &mut UnificationTable<'db>,
    receiver_ty: TyId<'db>,
    inst: TraitInstId<'db>,
) -> TyId<'db> {
    let ty = TyId::foldl(
        db,
        TyId::func(db, method.as_callable(db).unwrap()),
        inst.args(db),
    );

    let inst_self = table.instantiate_to_term(inst.self_ty(db));
    table.unify(inst_self, receiver_ty).unwrap();

    // Apply associated type substitutions from the trait instance
    use crate::analysis::ty::fold::{AssocTySubst, TyFoldable};
    let mut subst = AssocTySubst::new(inst);
    ty.fold_with(db, &mut subst)
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
        // not tied to the expression type.
        if let Expr::MethodCall(..) = expr_data
            && let Some(callable) = self.body.callable_expr(expr)
        {
            let callable_ty = callable.ty(self.db);
            let span = ctxt.span().unwrap().into_method_call_expr().method_name();
            self.check_unknown(callable_ty, span.clone().into());
            self.check_wf(callable_ty, span.into())
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
        let body = checker.env.finish(&mut checker.table);
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
