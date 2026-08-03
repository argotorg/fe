use std::collections::hash_map::Entry;

use cranelift_entity::EntityRef;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            CallSiteId, PlaceProvenance, SBlockId, SemanticBody, SemanticCalleeRef,
            SemanticLocalRole, ValueProvenance, VariantIndex,
            borrowck::{
                BorrowDiagnosticId, NExpr, NSStmtKind, NSTerminatorKind, SemanticBorrowDiagKind,
                SemanticBorrowDiagnostic, SemanticBorrowDiagnosticLabel,
                SemanticBorrowDiagnosticSpan,
                normalize_provisional_semantic_body_for_never_return_analysis,
                normalize_semantic_body, normalized_cfg_successors,
            },
            effect_param_site,
            lower::{BindingRoleMode, lower_to_smir, lower_to_smir_with_call_sites},
            verify_semantic_body,
        },
        ty::{
            CallableLayoutBundleInput, CallableLayoutBundleSignature, LayoutBundleInterface,
            adt_def::{AdtDef, AdtRef, instantiate_adt_field_shape},
            binder::Binder,
            const_ty::ConstTyData,
            corelib::{RuntimeBuiltinFuncKind, runtime_builtin_func_kind},
            effects::place_effect_provider_param_index_map,
            fold::TyFoldable,
            instantiate_trait_self,
            normalize::normalize_ty,
            provider::{
                ProviderAddressSpace, ProviderKind, ProviderLayoutEvidence, ProviderTransport,
                RootProviderRegistration, RootProviderSiteKind, provider_semantics,
                provider_semantics_for_specialized_call,
            },
            trait_def::{
                ImplementorId, TraitInstId, complete_resolved_trait_method_args,
                impls_for_trait_def,
            },
            trait_resolution::{
                GoalSatisfiability, PredicateListId, TraitSolveCx, is_goal_satisfiable,
            },
            ty_check::{
                BodyOwner, Callable, EffectParamSite, EffectProviderProvenance,
                EffectProviderSpecialization, LocalBinding, ParamSite, ResolvedEffectArg,
                SemanticExprLowering, TypedBody, TypedCallableBody,
            },
            ty_def::{BorrowKind, CapabilityKind, TyData, TyFlags, TyId},
            ty_lower::{
                closure_layout_bundle_signature, layout_bundle_schema_for_semantic_value,
                specialized_callable_layout_bundle_signature_with_normalizer,
            },
            unify::UnificationTable,
            visitor::{TyVisitable, TyVisitor, collect_flags, walk_ty},
        },
    },
    hir_def::{
        ArithBinOp, BinOp, CallableDef, CompBinOp, Expr, ExprId, Func, HirIngot, IdentId, ItemKind,
        Partial, PathId, Stmt, StmtId, TypeKind, UnOp, scope_graph::ScopeId,
    },
    semantic::{
        AssignedLayoutBindingEnv, EffectEnvView, EffectRequirement, EffectRequirementKey,
        LayoutViewKind, ProviderBinding, ProviderSource, ResolvedEffectBinding,
    },
    span::{expr::LazyExprSpan, item::LazyItemSpan, stmt::LazyStmtSpan},
    visitor::{Visitor, VisitorCtxt, walk_expr, walk_stmt},
};
use common::{indexmap::IndexMap, ingot::Ingot};
use indexmap::IndexSet;
use salsa::Update;
use thin_vec::ThinVec;

use super::{
    EffectProviderSubst, GenericSubst, ImplEnv, instantiate_typed_body,
    provisional_semantic_callee_key, semantic_callee_key_with_effect_providers,
    typed_body_template,
};

#[salsa::interned]
#[derive(Debug)]
pub struct SemanticInstanceKey<'db> {
    pub owner: BodyOwner<'db>,
    pub subst: GenericSubst<'db>,
    pub effect_providers: EffectProviderSubst<'db>,
    pub impl_env: ImplEnv<'db>,
}

/// A source call whose recursive specialization would create an unbounded
/// family of semantic instances.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct NonRegularRecursiveCallSite<'db> {
    pub owner: BodyOwner<'db>,
    pub call_site: CallSiteId,
    pub callee: BodyOwner<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
struct NonRegularRecursiveCallGraph<'db> {
    calls: Vec<NonRegularRecursiveCallSite<'db>>,
    blocked_owners: Vec<BodyOwner<'db>>,
    component_diagnostic_calls: Vec<(BodyOwner<'db>, NonRegularRecursiveCallSite<'db>)>,
}

/// Whether a semantic instance is safe to analyze as a finalized
/// specialization.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticInstanceCompleteness {
    /// Every generic argument, provider, and implementation witness is
    /// independent of caller-local type parameters.
    Complete,
    /// The canonical owner instance intentionally retains its own formal
    /// parameters and must be analyzed parametrically.
    Parametric,
    /// A non-identity specialization still contains caller-local parameters or
    /// is missing finalized effect providers.
    Partial,
}

impl<'db> SemanticInstanceKey<'db> {
    pub fn typed_body(self, db: &'db dyn HirAnalysisDb) -> &'db TypedBody<'db> {
        instantiated_typed_body(db, self)
    }

    pub fn instantiate_typed_body(self, db: &'db dyn HirAnalysisDb) -> TypedBody<'db> {
        self.typed_body(db).clone()
    }

    pub fn callable_body(self, db: &'db dyn HirAnalysisDb) -> TypedCallableBody<'db> {
        TypedCallableBody::new(self.owner(db), self.typed_body(db))
    }

    pub fn layout_bundle_signature(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> CallableLayoutBundleSignature<'db> {
        semantic_layout_bundle_signature(db, self).clone()
    }

    /// Classifies the instance boundary used by semantic analyses.
    ///
    /// Partial instances arise naturally while traversing a parametric call
    /// graph. They are valid keys, but final normalization must not treat their
    /// caller-local parameters as fully resolved provider roots.
    pub fn completeness(self, db: &'db dyn HirAnalysisDb) -> SemanticInstanceCompleteness {
        let unresolved = TyFlags::HAS_PARAM | TyFlags::HAS_VAR;
        let generic_args_are_concrete =
            !collect_flags(db, self.subst(db).generic_args(db).as_slice()).intersects(unresolved);
        let providers = self.effect_providers(db).providers(db);
        let providers_are_concrete =
            !collect_flags(db, providers.as_slice()).intersects(unresolved);
        let impl_env = self.impl_env(db);
        let impl_env_is_concrete = !collect_flags(db, impl_env.assumptions(db))
            .intersects(unresolved)
            && !collect_flags(db, impl_env.witnesses(db).as_slice()).intersects(unresolved);
        let owner_types_are_concrete = match self.owner(db) {
            BodyOwner::AnonConstBody { expected, .. } => !expected.flags(db).intersects(unresolved),
            BodyOwner::Closure { ty, .. } => {
                !TyId::closure(db, ty).flags(db).intersects(unresolved)
            }
            BodyOwner::Func(_)
            | BodyOwner::Const(_)
            | BodyOwner::ContractInit { .. }
            | BodyOwner::ContractRecvArm { .. } => true,
        };
        let has_all_effect_providers = match self.owner(db) {
            BodyOwner::Func(func) => {
                let view = EffectEnvView::new(EffectParamSite::Func(func));
                let requirements = view.requirements(db);
                let resolutions = view.resolutions(db);
                requirements.is_empty()
                    || (!providers.is_empty()
                        && requirements.iter().all(|requirement| {
                            resolutions
                                .iter()
                                .find(|resolution| {
                                    resolution.requirement_idx == requirement.binding_idx
                                })
                                .is_some_and(|resolution| {
                                    providers.iter().any(|specialization| {
                                        specialization.provider.provider_idx
                                            == resolution.provider_idx
                                    })
                                })
                        }))
            }
            BodyOwner::Const(_)
            | BodyOwner::AnonConstBody { .. }
            | BodyOwner::ContractInit { .. }
            | BodyOwner::ContractRecvArm { .. }
            | BodyOwner::Closure { .. } => true,
        };

        if generic_args_are_concrete
            && providers_are_concrete
            && impl_env_is_concrete
            && owner_types_are_concrete
            && has_all_effect_providers
        {
            SemanticInstanceCompleteness::Complete
        } else if self == identity_semantic_instance_key(db, self.owner(db)) {
            SemanticInstanceCompleteness::Parametric
        } else {
            SemanticInstanceCompleteness::Partial
        }
    }
}

#[salsa::tracked(return_ref)]
pub fn semantic_layout_bundle_signature<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> CallableLayoutBundleSignature<'db> {
    let instance = SemanticInstance::new(db, key);
    match key.owner(db) {
        BodyOwner::Func(func) => {
            let args = key.subst(db).generic_args(db);
            specialized_callable_layout_bundle_signature_with_normalizer(db, func, args, |ty| {
                instance.normalized_ty(db, ty)
            })
        }
        owner => {
            let Ok(normalized) = normalize_semantic_body(db, instance) else {
                return CallableLayoutBundleSignature::default();
            };
            let Some(body) = owner.body(db) else {
                return CallableLayoutBundleSignature::default();
            };
            let inputs = normalized
                .entry_locals
                .iter()
                .filter_map(|local| {
                    let local_data = normalized.local(*local)?;
                    let origin = local_data.source?.callable_input_origin(db)?;
                    let ty = local_data.ty;
                    let schema = layout_bundle_schema_for_semantic_value(
                        db,
                        body,
                        local.index() as u32,
                        ty,
                        local.index() as u32,
                        ty,
                    );
                    (!schema.components.is_empty()).then(|| CallableLayoutBundleInput {
                        origin,
                        interface: LayoutBundleInterface::inferred(schema),
                    })
                })
                .collect();
            if let BodyOwner::Closure { def, .. } = owner {
                closure_layout_bundle_signature(db, def, inputs, instance.normalized_result_ty(db))
            } else {
                CallableLayoutBundleSignature {
                    inputs,
                    ..CallableLayoutBundleSignature::default()
                }
            }
        }
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct SemanticInstance<'db> {
    pub key: SemanticInstanceKey<'db>,
}

#[derive(Debug, Clone)]
pub struct SemanticEffectEnvInstantiationError<'db> {
    pub owner: BodyOwner<'db>,
    pub owner_scope: ScopeId<'db>,
    pub offending_ty: TyId<'db>,
    pub param_idx: usize,
    pub args_len: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Update)]
pub struct ReceiverLoweringPlan<'db> {
    pub borrowed_ty: TyId<'db>,
    pub receiver_ty: TyId<'db>,
    pub kind: BorrowKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct CallSiteLowering<'db> {
    pub callee: Option<SemanticCalleeRef<'db>>,
    pub receiver: Option<ReceiverLoweringPlan<'db>>,
    pub effect_args: Box<[ResolvedEffectArg<'db>]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct ForLoopCallSites<'db> {
    pub len: CallSiteLowering<'db>,
    pub get: CallSiteLowering<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub(crate) struct CallSiteProviderRefinement {
    pub call_site: CallSiteId,
    pub binding_idx: u32,
    pub provider_idx: Option<u32>,
    pub address_space: ProviderAddressSpace,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
struct CallSiteFinalizationData<'db> {
    call_sites: Vec<Option<CallSiteLowering<'db>>>,
    for_loop_call_sites: Vec<Option<ForLoopCallSites<'db>>>,
    diagnostic: Option<crate::analysis::semantic::BorrowDiagnosticId<'db>>,
}

#[derive(Debug, Clone)]
pub enum RootSemanticInstanceError<'db> {
    UnsupportedGenericParam {
        owner: BodyOwner<'db>,
        owner_scope: ScopeId<'db>,
        offending_ty: TyId<'db>,
        param_idx: usize,
    },
    MissingRootProvider {
        owner: BodyOwner<'db>,
    },
    UnclosedEffectEnv(SemanticEffectEnvInstantiationError<'db>),
}

type InstantiatedEffectEnvData<'db> = (
    crate::analysis::ty::ty_check::EffectParamSite<'db>,
    Vec<EffectRequirement<'db>>,
    Vec<ProviderBinding<'db>>,
    Vec<ResolvedEffectBinding>,
    Vec<crate::analysis::ty::trait_def::TraitInstId<'db>>,
    PredicateListId<'db>,
);

#[salsa::tracked]
#[derive(Debug)]
pub struct InstantiatedEffectEnv<'db> {
    pub site: crate::analysis::ty::ty_check::EffectParamSite<'db>,
    #[return_ref]
    pub requirements: Vec<EffectRequirement<'db>>,
    #[return_ref]
    pub providers: Vec<ProviderBinding<'db>>,
    #[return_ref]
    pub resolutions: Vec<ResolvedEffectBinding>,
    #[return_ref]
    pub forwarded_witnesses: Vec<crate::analysis::ty::trait_def::TraitInstId<'db>>,
    pub assumptions: PredicateListId<'db>,
}

#[salsa::tracked]
pub fn instantiated_effect_env<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Option<InstantiatedEffectEnv<'db>> {
    let (site, requirements, providers, resolutions, forwarded_witnesses, assumptions) =
        instantiate_effect_env_data_for_key(db, instance.key(db)).unwrap_or_else(|err| {
            panic!(
                "failed to instantiate effect env for {:?}: owner_scope={:?} param_idx={} args_len={} offending_ty={}",
                err.owner,
                err.owner_scope,
                err.param_idx,
                err.args_len,
                err.offending_ty.pretty_print(db),
            )
        })?;
    Some(InstantiatedEffectEnv::new(
        db,
        site,
        requirements,
        providers,
        resolutions,
        forwarded_witnesses,
        assumptions,
    ))
}

#[salsa::tracked(return_ref)]
pub fn instantiated_typed_body<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> TypedBody<'db> {
    instantiate_typed_body(db, typed_body_template(db, key.owner(db)), key.subst(db))
}

fn receiver_lowering_plan<'db>(
    db: &'db dyn HirAnalysisDb,
    expr_data: &Expr<'db>,
    callee_is_receiver: bool,
    callable: &crate::analysis::ty::ty_check::Callable<'db>,
    typed_body: &TypedBody<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<ReceiverLoweringPlan<'db>> {
    let receiver = call_like_receiver_expr(expr_data, callee_is_receiver)?;
    let borrowed_ty = callable.arg_ty(db, 0)?;
    let borrowed_ty = normalize_ty(db, borrowed_ty, scope, assumptions);
    let receiver_ty = normalize_ty(db, typed_body.expr_ty(db, receiver), scope, assumptions);
    let (kind, _) = borrowed_ty.as_capability(db)?;
    if !matches!(kind, CapabilityKind::Mut | CapabilityKind::Ref)
        || receiver_ty.as_capability(db).is_some()
    {
        return None;
    }
    Some(ReceiverLoweringPlan {
        borrowed_ty,
        receiver_ty,
        kind: match kind {
            CapabilityKind::Mut => BorrowKind::Mut,
            CapabilityKind::Ref => BorrowKind::Ref,
            CapabilityKind::View => unreachable!(),
        },
    })
}

fn call_like_receiver_expr<'db>(expr_data: &Expr<'db>, callee_is_receiver: bool) -> Option<ExprId> {
    match expr_data {
        Expr::MethodCall(receiver, ..)
        | Expr::Un(receiver, ..)
        | Expr::Bin(receiver, ..)
        | Expr::AugAssign(receiver, ..) => Some(*receiver),
        Expr::Call(callee, ..) => callee_is_receiver.then_some(*callee),
        Expr::Assert(..)
        | Expr::Lit(..)
        | Expr::Path(..)
        | Expr::Tuple(..)
        | Expr::Array(..)
        | Expr::ArrayRep(..)
        | Expr::RecordInit(..)
        | Expr::Field(..)
        | Expr::Cast(..)
        | Expr::Assign(..)
        | Expr::Closure { .. }
        | Expr::Block(..)
        | Expr::If(..)
        | Expr::Match(..)
        | Expr::With(..) => None,
    }
}

#[salsa::tracked(return_ref)]
fn provisional_call_sites<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Vec<Option<CallSiteLowering<'db>>> {
    let typed_body = instance.key(db).typed_body(db);
    let Some(body) = typed_body.body() else {
        return Vec::new();
    };
    let assumptions = semantic_instance_base_assumptions_for_key(db, instance.key(db));
    let scope = body.scope();
    let mut sites = vec![None; body.exprs(db).len()];

    for (expr, expr_data) in body.exprs(db).iter() {
        let Partial::Present(expr_data) = expr_data else {
            continue;
        };
        let Some(SemanticExprLowering::Call {
            callable,
            callee_is_receiver,
        }) = typed_body.semantic_expr_lowering(expr)
        else {
            continue;
        };
        sites[expr.index()] = Some(CallSiteLowering {
            callee: provisional_semantic_callee_key(db, instance.key(db), callable, assumptions)
                .map(|key| SemanticCalleeRef { key }),
            receiver: receiver_lowering_plan(
                db,
                expr_data,
                *callee_is_receiver,
                callable,
                typed_body,
                scope,
                assumptions,
            ),
            effect_args: typed_body
                .call_effect_args(expr)
                .unwrap_or(&[])
                .to_vec()
                .into_boxed_slice(),
        });
    }

    sites
}

#[salsa::tracked(return_ref)]
fn provisional_for_loop_call_sites<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Vec<Option<ForLoopCallSites<'db>>> {
    let typed_body = instance.key(db).typed_body(db);
    let Some(body) = typed_body.body() else {
        return Vec::new();
    };
    let assumptions = semantic_instance_base_assumptions_for_key(db, instance.key(db));
    let mut sites = vec![None; body.stmts(db).len()];
    for (stmt, _) in body.stmts(db).iter() {
        let Some(seq) = typed_body.for_loop_seq(stmt) else {
            continue;
        };
        sites[stmt.index()] = Some(ForLoopCallSites {
            len: CallSiteLowering {
                callee: provisional_semantic_callee_key(
                    db,
                    instance.key(db),
                    &seq.len_callable,
                    assumptions,
                )
                .map(|key| SemanticCalleeRef { key }),
                receiver: None,
                effect_args: seq.len_effect_args.clone().into_boxed_slice(),
            },
            get: CallSiteLowering {
                callee: provisional_semantic_callee_key(
                    db,
                    instance.key(db),
                    &seq.get_callable,
                    assumptions,
                )
                .map(|key| SemanticCalleeRef { key }),
                receiver: None,
                effect_args: seq.get_effect_args.clone().into_boxed_slice(),
            },
        });
    }
    sites
}

#[salsa::tracked(
    return_ref,
    cycle_fn=final_call_site_data_cycle_recover,
    cycle_initial=final_call_site_data_cycle_initial
)]
fn final_call_site_data<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> CallSiteFinalizationData<'db> {
    let mut call_sites = provisional_call_sites(db, instance).clone();
    let mut for_loop_call_sites = provisional_for_loop_call_sites(db, instance).clone();
    let typed_body = instance.key(db).typed_body(db);
    let Some(body) = typed_body.body() else {
        return CallSiteFinalizationData {
            call_sites,
            for_loop_call_sites,
            diagnostic: None,
        };
    };
    let refinements = if call_sites_have_effect_args(&call_sites, &for_loop_call_sites) {
        match crate::analysis::semantic::borrowck::provisional_call_site_provider_refinements(
            db, instance,
        ) {
            Ok(refinements) => refinements,
            Err(diag) => {
                return CallSiteFinalizationData {
                    call_sites,
                    for_loop_call_sites,
                    diagnostic: Some(crate::analysis::semantic::BorrowDiagnosticId::new(db, diag)),
                };
            }
        }
    } else {
        Vec::new()
    };
    let mut by_site = FxHashMap::<CallSiteId, Vec<CallSiteProviderRefinement>>::default();
    for refinement in refinements {
        by_site
            .entry(refinement.call_site)
            .or_default()
            .push(refinement);
    }

    for (expr, _) in body.exprs(db).iter() {
        let Some(site) = call_sites.get_mut(expr.index()).and_then(Option::as_mut) else {
            continue;
        };
        let Some(SemanticExprLowering::Call { callable, .. }) =
            typed_body.semantic_expr_lowering(expr)
        else {
            continue;
        };
        finalize_call_site(
            db,
            instance,
            callable,
            site,
            by_site.get(&CallSiteId::Expr(expr)).map(Vec::as_slice),
        );
    }

    for (stmt, _) in body.stmts(db).iter() {
        let Some(sites) = for_loop_call_sites
            .get_mut(stmt.index())
            .and_then(Option::as_mut)
        else {
            continue;
        };
        let Some(seq) = typed_body.for_loop_seq(stmt) else {
            continue;
        };
        finalize_call_site(
            db,
            instance,
            &seq.len_callable,
            &mut sites.len,
            by_site
                .get(&CallSiteId::ForLoopLen(stmt))
                .map(Vec::as_slice),
        );
        finalize_call_site(
            db,
            instance,
            &seq.get_callable,
            &mut sites.get,
            by_site
                .get(&CallSiteId::ForLoopGet(stmt))
                .map(Vec::as_slice),
        );
    }

    CallSiteFinalizationData {
        call_sites,
        for_loop_call_sites,
        diagnostic: None,
    }
}

fn final_call_site_data_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> CallSiteFinalizationData<'db> {
    CallSiteFinalizationData {
        call_sites: provisional_call_sites(db, instance).clone(),
        for_loop_call_sites: provisional_for_loop_call_sites(db, instance).clone(),
        diagnostic: None,
    }
}

fn final_call_site_data_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &CallSiteFinalizationData<'db>,
    _count: u32,
    _instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<CallSiteFinalizationData<'db>> {
    salsa::CycleRecoveryAction::Iterate
}

fn call_sites_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Vec<Option<CallSiteLowering<'db>>> {
    provisional_call_sites(db, instance).clone()
}

fn call_sites_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Vec<Option<CallSiteLowering<'db>>>,
    _count: u32,
    _instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<Vec<Option<CallSiteLowering<'db>>>> {
    salsa::CycleRecoveryAction::Iterate
}

fn for_loop_call_sites_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Vec<Option<ForLoopCallSites<'db>>> {
    provisional_for_loop_call_sites(db, instance).clone()
}

fn for_loop_call_sites_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Vec<Option<ForLoopCallSites<'db>>>,
    _count: u32,
    _instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<Vec<Option<ForLoopCallSites<'db>>>> {
    salsa::CycleRecoveryAction::Iterate
}

fn call_sites_have_effect_args<'db>(
    call_sites: &[Option<CallSiteLowering<'db>>],
    for_loop_call_sites: &[Option<ForLoopCallSites<'db>>],
) -> bool {
    call_sites
        .iter()
        .flatten()
        .any(|site| !site.effect_args.is_empty())
        || for_loop_call_sites
            .iter()
            .flatten()
            .any(|sites| !sites.len.effect_args.is_empty() || !sites.get.effect_args.is_empty())
}

fn finalize_call_site<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    callable: &crate::analysis::ty::ty_check::Callable<'db>,
    site: &mut CallSiteLowering<'db>,
    refinements: Option<&[CallSiteProviderRefinement]>,
) {
    let mut effect_providers = callable.effect_providers().to_vec();
    if let Some(refinements) = refinements {
        for refinement in refinements {
            for arg in &mut site.effect_args {
                if arg.binding_idx == refinement.binding_idx {
                    arg.provider = Some(refinement.address_space);
                }
            }
            if let Some(provider_idx) = refinement.provider_idx
                && let Some(specialization) = effect_providers
                    .iter_mut()
                    .find(|provider| provider.provider.provider_idx == provider_idx)
            {
                specialize_provider_address_space(
                    db,
                    instance,
                    specialization,
                    refinement.address_space,
                );
            }
        }
    }
    site.callee = semantic_callee_key_with_effect_providers(
        db,
        instance.key(db),
        callable,
        &effect_providers,
    )
    .map(|key| SemanticCalleeRef { key });
}

fn specialize_provider_address_space<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    specialization: &mut EffectProviderSpecialization<'db>,
    address_space: ProviderAddressSpace,
) {
    let provider = &specialization.provider;
    let semantics = provider_semantics_for_specialized_call(
        db,
        instance.key(db).owner(db).scope(),
        instance.assumptions(db),
        provider.provider_ty,
        provider.semantics.target_ty,
        Some(address_space),
        provider.semantics.transport,
    );
    specialization.provider.semantics = semantics;
}

#[salsa::tracked]
impl<'db> SemanticInstance<'db> {
    #[salsa::tracked]
    pub fn assumptions(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        instantiated_effect_env(db, self)
            .map(|env| env.assumptions(db))
            .unwrap_or_else(|| semantic_instance_base_assumptions_for_key(db, self.key(db)))
    }

    #[salsa::tracked(
        return_ref,
        cycle_fn=call_sites_cycle_recover,
        cycle_initial=call_sites_cycle_initial
    )]
    pub fn call_sites(self, db: &'db dyn HirAnalysisDb) -> Vec<Option<CallSiteLowering<'db>>> {
        final_call_site_data(db, self).call_sites.clone()
    }

    #[salsa::tracked(
        return_ref,
        cycle_fn=for_loop_call_sites_cycle_recover,
        cycle_initial=for_loop_call_sites_cycle_initial
    )]
    pub fn for_loop_call_sites(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<Option<ForLoopCallSites<'db>>> {
        final_call_site_data(db, self).for_loop_call_sites.clone()
    }

    #[salsa::tracked]
    pub fn call_site_finalization_diagnostic(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<crate::analysis::semantic::BorrowDiagnosticId<'db>> {
        final_call_site_data(db, self).diagnostic
    }

    #[salsa::tracked(return_ref)]
    pub(crate) fn provisional_body(self, db: &'db dyn HirAnalysisDb) -> SemanticBody<'db> {
        let key = self.key(db);
        let typed_body = key.typed_body(db);
        let call_sites = provisional_call_sites(db, self);
        let for_loop_call_sites = provisional_for_loop_call_sites(db, self);
        let body = lower_to_smir_with_call_sites(
            db,
            self,
            key.owner(db),
            typed_body,
            call_sites,
            for_loop_call_sites,
            BindingRoleMode::Provisional,
        );
        verify_semantic_body(&body).expect("invalid provisional semantic MIR");
        body
    }

    #[salsa::tracked]
    pub fn binding_role(
        self,
        db: &'db dyn HirAnalysisDb,
        binding: LocalBinding<'db>,
    ) -> SemanticLocalRole<'db> {
        classify_binding_role(
            db,
            self,
            self.binding_ty(db, binding),
            self.assumptions(db),
            resolved_provider_binding_for_instance_effect(db, self, binding),
        )
    }

    pub(crate) fn provisional_binding_role(
        self,
        db: &'db dyn HirAnalysisDb,
        binding: LocalBinding<'db>,
    ) -> SemanticLocalRole<'db> {
        classify_binding_role(
            db,
            self,
            self.provisional_binding_ty(db, binding),
            semantic_instance_base_assumptions_for_key(db, self.key(db)),
            provisional_provider_binding_for_instance_effect(db, self, binding),
        )
    }

    pub(crate) fn provisional_binding_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        binding: LocalBinding<'db>,
    ) -> TyId<'db> {
        match binding {
            LocalBinding::EffectParam { site, idx, .. } => EffectEnvView::new(site)
                .requirements(db)
                .into_iter()
                .find(|requirement| requirement.binding_idx as usize == idx)
                .and_then(|requirement| requirement.key.binding_ty(db))
                .and_then(|ty| instantiate_normalized_ty(db, self.key(db), ty).ok())
                .unwrap_or_else(|| {
                    TyId::invalid(db, crate::analysis::ty::ty_def::InvalidCause::Other)
                }),
            LocalBinding::Local { .. } | LocalBinding::Param { .. } => {
                self.key(db).typed_body(db).binding_ty(db, binding)
            }
        }
    }

    #[salsa::tracked]
    pub fn binding_ty(self, db: &'db dyn HirAnalysisDb, binding: LocalBinding<'db>) -> TyId<'db> {
        match binding {
            LocalBinding::EffectParam {
                idx, provider_idx, ..
            } => effect_binding_ty_from_env(
                db,
                instantiated_effect_env(db, self),
                idx,
                Some(provider_idx),
            ),
            LocalBinding::Param {
                site: ParamSite::EffectField(_),
                idx,
                ..
            } => effect_binding_ty_from_env(db, instantiated_effect_env(db, self), idx, None),
            LocalBinding::Local { .. } | LocalBinding::Param { .. } => {
                self.key(db).typed_body(db).binding_ty(db, binding)
            }
        }
    }

    #[salsa::tracked]
    pub fn normalized_ty(self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        normalize_ty(db, ty, self.normalization_scope(db), self.assumptions(db))
    }

    #[salsa::tracked(return_ref)]
    pub fn normalized_field_types(
        self,
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
    ) -> ThinVec<TyId<'db>> {
        let ty = self.normalized_ty(db, ty);
        if ty.is_tuple(db) {
            let (_, elems) = ty.decompose_ty_app(db);
            return elems
                .iter()
                .map(|elem| self.normalized_ty(db, *elem))
                .collect();
        }

        if let Some(adt_def) = ty.adt_def(db)
            && matches!(adt_def.adt_ref(db), AdtRef::Struct(_))
            && let Some(fields) = adt_def.fields(db).first()
        {
            return normalize_adt_field_types(
                db,
                self,
                adt_def,
                0,
                ty.generic_args(db),
                fields.num_types(),
            );
        }

        ThinVec::new()
    }

    #[salsa::tracked(return_ref)]
    pub fn normalized_enum_variant_field_tys(
        self,
        db: &'db dyn HirAnalysisDb,
        enum_ty: TyId<'db>,
        variant: VariantIndex,
    ) -> ThinVec<TyId<'db>> {
        let enum_ty = self.normalized_ty(db, enum_ty);
        let variant_idx = usize::from(variant.0);
        if let Some(adt_def) = enum_ty.adt_def(db)
            && matches!(adt_def.adt_ref(db), AdtRef::Enum(_))
            && let Some(fields) = adt_def.fields(db).get(variant_idx)
        {
            return normalize_adt_field_types(
                db,
                self,
                adt_def,
                variant_idx,
                enum_ty.generic_args(db),
                fields.num_types(),
            );
        }

        ThinVec::new()
    }

    #[salsa::tracked]
    pub fn normalized_binding_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        binding: LocalBinding<'db>,
    ) -> TyId<'db> {
        self.normalized_ty(db, self.binding_ty(db, binding))
    }

    #[salsa::tracked]
    pub fn normalized_result_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.normalized_ty(db, self.key(db).callable_body(db).result_ty(db))
    }

    #[salsa::tracked]
    pub fn known_never_returns(self, db: &'db dyn HirAnalysisDb) -> bool {
        let root_key = self.key(db);
        let mut instances = vec![self];
        let mut node_by_key = FxHashMap::default();
        node_by_key.insert(root_key, 0);
        let mut nodes = vec![None];
        let mut pending_nodes = vec![0];

        // Materialize the finite reachable call graph without consulting this
        // query recursively. Calls after another call remain present in this
        // provisional view, so a callee that later proves able to return cannot
        // reveal a call-graph edge that was omitted here. Non-regular recursive
        // source components are rejected before node normalization, so every
        // remaining recursive substitution ranges over a finite set of formal
        // permutations, duplications, and caller-independent constants.
        while let Some(node_idx) = pending_nodes.pop() {
            if nodes[node_idx].is_some() {
                continue;
            }
            let instance = instances[node_idx];
            let analysis = analyze_never_return_node(db, instance);
            if !matches!(&analysis.node, NeverReturnNode::Body { .. }) {
                nodes[node_idx] = Some(analysis.node);
                continue;
            }
            for callee_key in analysis.callees {
                let callee_idx = nodes.len();
                let Entry::Vacant(entry) = node_by_key.entry(callee_key) else {
                    continue;
                };
                entry.insert(callee_idx);
                let callee = SemanticInstance::new(db, callee_key);
                instances.push(callee);
                nodes.push(None);
                pending_nodes.push(callee_idx);
            }
            nodes[node_idx] = Some(analysis.node);
        }

        let nodes = nodes
            .into_iter()
            .map(|node| node.expect("every discovered never-return node must be analyzed"))
            .collect::<Vec<_>>();
        let mut never_returns = nodes
            .iter()
            .map(|node| !matches!(node, NeverReturnNode::ConservativeMayReturn))
            .collect::<Vec<_>>();

        // `never_returns` starts at the top element. Repeatedly removing nodes
        // with an executable normal-return path computes the greatest fixed
        // point, which is the desired interpretation for pure and mutual
        // recursion: an infinite call chain has no finite normal return.
        loop {
            let mut changed = false;
            for (node_idx, node) in nodes.iter().enumerate() {
                if !never_returns[node_idx] {
                    continue;
                }
                let holds = match node {
                    NeverReturnNode::Intrinsic => true,
                    NeverReturnNode::ConservativeMayReturn => false,
                    NeverReturnNode::Body { body, successors } => {
                        never_return_body_holds(body, successors, &node_by_key, &never_returns)
                    }
                };
                if !holds {
                    never_returns[node_idx] = false;
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }

        never_returns[node_by_key[&root_key]]
    }

    #[salsa::tracked(return_ref)]
    pub fn body(self, db: &'db dyn HirAnalysisDb) -> SemanticBody<'db> {
        lower_semantic_body(db, self)
    }

    #[salsa::tracked(return_ref)]
    pub fn callees(self, db: &'db dyn HirAnalysisDb) -> Vec<SemanticCalleeRef<'db>> {
        collect_semantic_callees(db, self)
    }
}

enum NeverReturnNode<'db> {
    Intrinsic,
    ConservativeMayReturn,
    Body {
        body: crate::analysis::semantic::borrowck::NormalizedSemanticBody<'db>,
        successors: Vec<Vec<SBlockId>>,
    },
}

struct NeverReturnNodeAnalysis<'db> {
    node: NeverReturnNode<'db>,
    callees: Vec<SemanticInstanceKey<'db>>,
}

fn analyze_never_return_node<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> NeverReturnNodeAnalysis<'db> {
    if instance.is_intrinsically_never_returning(db) {
        return NeverReturnNodeAnalysis {
            node: NeverReturnNode::Intrinsic,
            callees: Vec::new(),
        };
    }
    if semantic_instance_is_in_non_regular_recursive_component(db, instance) {
        return NeverReturnNodeAnalysis {
            node: NeverReturnNode::ConservativeMayReturn,
            callees: Vec::new(),
        };
    }
    if instance
        .key(db)
        .typed_body(db)
        .has_smir_lowering_blocker(db)
    {
        return NeverReturnNodeAnalysis {
            node: NeverReturnNode::ConservativeMayReturn,
            callees: Vec::new(),
        };
    }
    let Ok(body) = normalize_provisional_semantic_body_for_never_return_analysis(db, instance)
    else {
        return NeverReturnNodeAnalysis {
            node: NeverReturnNode::ConservativeMayReturn,
            callees: Vec::new(),
        };
    };
    if body.blocks.is_empty() {
        return NeverReturnNodeAnalysis {
            node: NeverReturnNode::ConservativeMayReturn,
            callees: Vec::new(),
        };
    }

    let successors = normalized_cfg_successors(db, &body);
    let mut callees = Vec::new();
    let mut pending_blocks = vec![SBlockId::new(0)];
    let mut visited_blocks = FxHashSet::default();
    while let Some(block_id) = pending_blocks.pop() {
        if !visited_blocks.insert(block_id) {
            continue;
        }
        let Some(block) = body.block(block_id) else {
            return NeverReturnNodeAnalysis {
                node: NeverReturnNode::ConservativeMayReturn,
                callees: Vec::new(),
            };
        };
        let mut intrinsically_terminated = false;
        for stmt in &block.stmts {
            let NSStmtKind::Assign {
                expr: NExpr::Call { callee, .. },
                ..
            } = &stmt.kind
            else {
                continue;
            };
            callees.push(callee.key);
            if SemanticInstance::new(db, callee.key).is_intrinsically_never_returning(db) {
                intrinsically_terminated = true;
                break;
            }
        }
        if intrinsically_terminated {
            continue;
        }
        let Some(block_successors) = successors.get(block_id.index()) else {
            return NeverReturnNodeAnalysis {
                node: NeverReturnNode::ConservativeMayReturn,
                callees: Vec::new(),
            };
        };
        pending_blocks.extend(block_successors.iter().copied());
    }
    NeverReturnNodeAnalysis {
        node: NeverReturnNode::Body { body, successors },
        callees,
    }
}

pub fn same_syntactic_callable_owner(lhs: BodyOwner<'_>, rhs: BodyOwner<'_>) -> bool {
    match (lhs, rhs) {
        (
            BodyOwner::Closure {
                def: lhs_def,
                receiver_mode: lhs_mode,
                ..
            },
            BodyOwner::Closure {
                def: rhs_def,
                receiver_mode: rhs_mode,
                ..
            },
        ) => lhs_def == rhs_def && lhs_mode == rhs_mode,
        _ => lhs == rhs,
    }
}

fn never_return_body_holds<'db>(
    body: &crate::analysis::semantic::borrowck::NormalizedSemanticBody<'db>,
    successors: &[Vec<SBlockId>],
    node_by_key: &FxHashMap<SemanticInstanceKey<'db>, usize>,
    never_returns: &[bool],
) -> bool {
    let mut pending = vec![SBlockId::new(0)];
    let mut visited = FxHashSet::default();
    while let Some(block_id) = pending.pop() {
        if !visited.insert(block_id) {
            continue;
        }
        let Some(block) = body.block(block_id) else {
            return false;
        };
        let mut terminated_in_stmt = false;
        for stmt in &block.stmts {
            let NSStmtKind::Assign {
                expr: NExpr::Call { callee, .. },
                ..
            } = &stmt.kind
            else {
                continue;
            };
            if node_by_key
                .get(&callee.key)
                .is_some_and(|callee_idx| never_returns[*callee_idx])
            {
                terminated_in_stmt = true;
                break;
            }
        }
        if terminated_in_stmt {
            continue;
        }
        if matches!(block.terminator.kind, NSTerminatorKind::Return(_)) {
            return false;
        }
        let Some(block_successors) = successors.get(block_id.index()) else {
            return false;
        };
        pending.extend(block_successors.iter().copied());
    }
    true
}

impl<'db> SemanticInstance<'db> {
    fn normalization_scope(self, db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        self.key(db).owner(db).scope()
    }

    fn is_intrinsically_never_returning(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.is_nonreturning_builtin(db) || self.normalized_result_ty(db).is_never(db)
    }

    fn is_nonreturning_builtin(self, db: &'db dyn HirAnalysisDb) -> bool {
        let BodyOwner::Func(func) = self.key(db).owner(db) else {
            return false;
        };
        matches!(
            runtime_builtin_func_kind(db, func),
            Some(
                RuntimeBuiltinFuncKind::ReturnData
                    | RuntimeBuiltinFuncKind::Revert
                    | RuntimeBuiltinFuncKind::SelfDestruct
                    | RuntimeBuiltinFuncKind::Stop
                    | RuntimeBuiltinFuncKind::Panic
                    | RuntimeBuiltinFuncKind::PanicWithValue
                    | RuntimeBuiltinFuncKind::Todo
            )
        )
    }
}

fn normalize_adt_field_types<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    adt_def: AdtDef<'db>,
    variant_idx: usize,
    args: &[TyId<'db>],
    field_count: usize,
) -> ThinVec<TyId<'db>> {
    (0..field_count)
        .map(|field_idx| {
            let field_ty = instantiate_adt_field_shape(db, adt_def, variant_idx, field_idx, args);
            instance.normalized_ty(db, field_ty)
        })
        .collect()
}

#[salsa::tracked]
pub fn resolved_provider_binding_for_instance_effect<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    binding: LocalBinding<'db>,
) -> Option<ProviderBinding<'db>> {
    let env = instantiated_effect_env(db, instance)?;
    let (binding_idx, provider_idx) = match binding {
        LocalBinding::EffectParam {
            idx, provider_idx, ..
        } => (idx, Some(provider_idx)),
        LocalBinding::Param {
            site: ParamSite::EffectField(_),
            idx,
            ..
        } => (idx, None),
        LocalBinding::Local { .. } | LocalBinding::Param { .. } => return None,
    };
    provider_idx
        .and_then(|provider_idx| {
            env.providers(db)
                .iter()
                .find(|provider| provider.provider_idx == provider_idx)
                .cloned()
        })
        .or_else(|| {
            instantiated_resolved_binding(env, db, binding_idx).map(|binding| binding.provider)
        })
}

pub(crate) fn resolved_effect_binding_ty_for_instance_effect<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    binding: LocalBinding<'db>,
) -> Option<TyId<'db>> {
    let env = instantiated_effect_env(db, instance)?;
    let (binding_idx, provider_idx) = match binding {
        LocalBinding::EffectParam {
            idx, provider_idx, ..
        } => (idx, Some(provider_idx)),
        LocalBinding::Param {
            site: ParamSite::EffectField(_),
            idx,
            ..
        } => (idx, None),
        LocalBinding::Local { .. } | LocalBinding::Param { .. } => return None,
    };
    Some(effect_binding_ty_from_env(
        db,
        Some(env),
        binding_idx,
        provider_idx,
    ))
}

pub(crate) fn provisional_provider_binding_for_instance_effect<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    binding: LocalBinding<'db>,
) -> Option<ProviderBinding<'db>> {
    let key = instance.key(db);
    let provider_from_subst = |provider_idx| {
        if key.completeness(db) == SemanticInstanceCompleteness::Partial {
            return None;
        }
        key.effect_providers(db)
            .providers(db)
            .iter()
            .find(|provider| provider.provider.provider_idx == provider_idx)
            .map(|provider| provider.provider.clone())
    };
    match binding {
        LocalBinding::EffectParam {
            site,
            idx,
            provider_idx,
            is_mut,
            ..
        } => provider_from_subst(provider_idx).or_else(|| {
            provisional_provider_binding_for_effect(db, key, site, idx as u32, provider_idx, is_mut)
        }),
        LocalBinding::Param {
            site: ParamSite::EffectField(effect_site),
            idx,
            ..
        } => {
            let requirement = EffectEnvView::new(effect_site)
                .requirements(db)
                .into_iter()
                .find(|requirement| requirement.binding_idx as usize == idx)?;
            let provider_idx =
                provisional_provider_idx_for_requirement(db, effect_site, requirement.binding_idx)?;
            provider_from_subst(provider_idx).or_else(|| {
                provisional_provider_binding_for_effect(
                    db,
                    key,
                    effect_site,
                    requirement.binding_idx,
                    provider_idx,
                    requirement.is_mut,
                )
            })
        }
        LocalBinding::Local { .. } | LocalBinding::Param { .. } => None,
    }
}

pub(crate) fn provisional_provider_idx_for_requirement<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
    requirement_idx: u32,
) -> Option<u32> {
    match site {
        EffectParamSite::Func(func) => {
            let explicit_provider_count = place_effect_provider_param_index_map(db, func)
                .iter()
                .filter(|param_idx| param_idx.is_some())
                .count() as u32;
            place_effect_provider_param_index_map(db, func)
                .get(requirement_idx as usize)
                .and_then(|param_idx| param_idx.map(|_| requirement_idx))
                .or(Some(explicit_provider_count))
        }
        EffectParamSite::Contract(contract)
        | EffectParamSite::ContractInit { contract }
        | EffectParamSite::ContractRecvArm { contract, .. } => {
            let field_provider_idx = contract
                .storage_layout(db)
                .values()
                .enumerate()
                .map(|(provider_idx, field)| (field.field.index, provider_idx as u32))
                .collect::<IndexMap<_, _>>();
            let fields = contract.fields(db);
            let requirement = EffectEnvView::new(site)
                .requirements(db)
                .into_iter()
                .find(|requirement| requirement.binding_idx == requirement_idx)?;
            if requirement.binding_path.len(db) == 1
                && let Some(name) = requirement.binding_path.ident(db).to_opt()
                && let Some(field) = fields.get(&name)
                && let Some(provider_idx) = field_provider_idx.get(&field.index).copied()
            {
                return Some(provider_idx);
            }
            Some(field_provider_idx.len() as u32)
        }
    }
}

fn provisional_provider_binding_for_effect<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    site: EffectParamSite<'db>,
    requirement_idx: u32,
    provider_idx: u32,
    is_mut: bool,
) -> Option<ProviderBinding<'db>> {
    match site {
        EffectParamSite::Func(func) => {
            let provider_map = place_effect_provider_param_index_map(db, func);
            if provider_map
                .get(requirement_idx as usize)
                .is_some_and(Option::is_some)
                && provider_idx == requirement_idx
            {
                let provider_param_idx = provider_map[requirement_idx as usize]?;
                let provider_ty = *CallableDef::Func(func).params(db).get(provider_param_idx)?;
                let assumptions = semantic_instance_base_assumptions_for_key(db, key);
                return Some(ProviderBinding {
                    provider_idx,
                    provider_ty,
                    is_mut,
                    source: ProviderSource::UsesParam {
                        site,
                        requirement_idx,
                    },
                    semantics: provider_semantics(db, func.scope(), assumptions, provider_ty),
                    layout_env: None,
                });
            }
            provisional_root_provider_binding(db, key, site, requirement_idx, provider_idx, is_mut)
        }
        EffectParamSite::Contract(contract)
        | EffectParamSite::ContractInit { contract }
        | EffectParamSite::ContractRecvArm { contract, .. } => {
            if let Some((_, field)) = contract
                .storage_layout(db)
                .values()
                .enumerate()
                .find(|(idx, _)| *idx as u32 == provider_idx)
            {
                let provider_ty = field.target_effect_binding_ty(db).ok()?;
                return Some(ProviderBinding {
                    provider_idx,
                    provider_ty,
                    is_mut: true,
                    source: ProviderSource::ContractField { field: field.field },
                    semantics: crate::analysis::ty::provider::ProviderSemantics {
                        provider_ty,
                        kind: if provider_ty.is_struct(db)
                            || provider_ty.is_array(db)
                            || provider_ty.is_tuple(db)
                            || provider_ty.as_enum(db).is_some()
                        {
                            ProviderKind::Handle
                        } else {
                            ProviderKind::RawAddress
                        },
                        address_space: Some(field.address_space),
                        target_ty: Some(provider_ty),
                        transport: ProviderTransport::ByValue,
                        evidence: ProviderLayoutEvidence::ContractField,
                    },
                    layout_env: Some(AssignedLayoutBindingEnv {
                        field: field.field,
                        view: LayoutViewKind::Target,
                    }),
                });
            }
            provisional_root_provider_binding(db, key, site, requirement_idx, provider_idx, is_mut)
        }
    }
}

fn provisional_root_provider_binding<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    site: EffectParamSite<'db>,
    requirement_idx: u32,
    provider_idx: u32,
    is_mut: bool,
) -> Option<ProviderBinding<'db>> {
    let requirement = EffectEnvView::new(site)
        .requirements(db)
        .into_iter()
        .find(|requirement| requirement.binding_idx == requirement_idx)?;
    let provider_ty = requirement
        .key
        .binding_ty(db)
        .and_then(|ty| instantiate_normalized_ty(db, key, ty).ok())?;
    let site_kind = match site {
        EffectParamSite::Func(_) => RootProviderSiteKind::Func,
        EffectParamSite::Contract(_) => RootProviderSiteKind::Contract,
        EffectParamSite::ContractInit { .. } => RootProviderSiteKind::ContractInit,
        EffectParamSite::ContractRecvArm { .. } => RootProviderSiteKind::ContractRecvArm,
    };
    let registration = RootProviderRegistration {
        idx: provider_idx,
        site_kind,
        provider_ty,
    };
    let assumptions = semantic_instance_base_assumptions_for_key(db, key);
    Some(ProviderBinding {
        provider_idx,
        provider_ty,
        is_mut,
        source: ProviderSource::RootProvider { site, registration },
        semantics: provider_semantics(db, key.owner(db).scope(), assumptions, provider_ty),
        layout_env: None,
    })
}

fn effect_binding_ty_from_env<'db>(
    db: &'db dyn HirAnalysisDb,
    env: Option<InstantiatedEffectEnv<'db>>,
    idx: usize,
    provider_idx: Option<u32>,
) -> TyId<'db> {
    let Some(env) = env else {
        return TyId::invalid(db, crate::analysis::ty::ty_def::InvalidCause::Other);
    };
    let requirement = env
        .requirements(db)
        .iter()
        .find(|requirement| requirement.binding_idx as usize == idx)
        .cloned();
    let provider = provider_idx
        .and_then(|provider_idx| {
            env.providers(db)
                .iter()
                .find(|provider| provider.provider_idx == provider_idx)
                .cloned()
        })
        .or_else(|| instantiated_resolved_binding(env, db, idx).map(|binding| binding.provider));
    match requirement.as_ref().map(|requirement| &requirement.key) {
        Some(crate::core::semantic::EffectRequirementKey::Trait(_)) => provider
            .map(|binding| binding.provider_ty)
            .or_else(|| requirement.and_then(|requirement| requirement.key.binding_ty(db))),
        Some(
            crate::core::semantic::EffectRequirementKey::Type(_)
            | crate::core::semantic::EffectRequirementKey::Other,
        ) => requirement
            .and_then(|requirement| requirement.key.binding_ty(db))
            .or_else(|| provider.map(|binding| binding.provider_ty)),
        None => None,
    }
    .unwrap_or_else(|| TyId::invalid(db, crate::analysis::ty::ty_def::InvalidCause::Other))
}

fn instantiated_resolved_binding<'db>(
    env: InstantiatedEffectEnv<'db>,
    db: &'db dyn HirAnalysisDb,
    idx: usize,
) -> Option<crate::core::semantic::ResolvedEffectBindingInfo<'db>> {
    let requirement = env
        .requirements(db)
        .iter()
        .find(|requirement| requirement.binding_idx as usize == idx)
        .cloned()?;
    let provider_idx = env
        .resolutions(db)
        .iter()
        .find(|resolution| resolution.requirement_idx as usize == idx)?
        .provider_idx;
    let provider = env
        .providers(db)
        .iter()
        .find(|provider| provider.provider_idx == provider_idx)
        .cloned()?;
    Some(crate::core::semantic::ResolvedEffectBindingInfo {
        requirement,
        provider,
    })
}

fn requirement_provider_target_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    requirement: &EffectRequirement<'db>,
) -> Option<TyId<'db>> {
    let target_ty = requirement.key.binding_ty(db)?;
    let semantics = provider_semantics(db, scope, assumptions, target_ty);
    match semantics.evidence {
        ProviderLayoutEvidence::ResolvedHandle(_) => semantics.target_ty,
        ProviderLayoutEvidence::InvalidHandle(_) => None,
        ProviderLayoutEvidence::Capability
        | ProviderLayoutEvidence::NotHandle
        | ProviderLayoutEvidence::ContractField => Some(target_ty),
    }
}

fn specialized_root_provider_target_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    requirement: &EffectRequirement<'db>,
    root_provider: &ProviderBinding<'db>,
) -> Option<TyId<'db>> {
    match requirement.key {
        EffectRequirementKey::Trait(_) => Some(root_provider.provider_ty),
        EffectRequirementKey::Type(_) | EffectRequirementKey::Other => {
            requirement_provider_target_ty(db, scope, assumptions, requirement)
                .or(root_provider.semantics.target_ty)
        }
    }
}

pub fn root_semantic_instance_key<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Result<SemanticInstanceKey<'db>, RootSemanticInstanceError<'db>> {
    let generic_args = root_owner_generic_args(db, owner)?;
    let effect_providers = root_owner_effect_providers(db, owner);
    let key = SemanticInstanceKey::new(
        db,
        owner,
        GenericSubst::new(db, generic_args),
        EffectProviderSubst::new(db, effect_providers),
        ImplEnv::empty(db, owner.scope()),
    );
    validate_instantiated_effect_env_key(db, key)
        .map_err(RootSemanticInstanceError::UnclosedEffectEnv)?;
    Ok(key)
}

pub fn identity_semantic_instance_key<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> SemanticInstanceKey<'db> {
    SemanticInstanceKey::new(
        db,
        owner,
        GenericSubst::new(db, owner_identity_generic_args(db, owner)),
        EffectProviderSubst::empty(db),
        ImplEnv::empty(db, owner.scope()),
    )
}

#[salsa::tracked]
pub fn get_or_build_semantic_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> SemanticInstance<'db> {
    SemanticInstance::new(db, key)
}

fn lower_semantic_body<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBody<'db> {
    let key = instance.key(db);
    let typed_body = key.typed_body(db);
    let body = lower_to_smir(db, instance, key.owner(db), typed_body);
    verify_semantic_body(&body).expect("invalid semantic MIR");
    body
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum SyntacticBodyOwner<'db> {
    Func(crate::hir_def::Func<'db>),
    Const(crate::hir_def::Const<'db>),
    AnonConst(crate::hir_def::Body<'db>),
    ContractInit(crate::hir_def::Contract<'db>),
    ContractRecvArm {
        contract: crate::hir_def::Contract<'db>,
        recv_idx: u32,
        arm_idx: u32,
    },
    Closure {
        def: crate::hir_def::ClosureDef<'db>,
        receiver_mode: crate::analysis::ty::ty_check::ClosureReceiverMode,
    },
}

impl<'db> From<BodyOwner<'db>> for SyntacticBodyOwner<'db> {
    fn from(owner: BodyOwner<'db>) -> Self {
        match owner {
            BodyOwner::Func(func) => Self::Func(func),
            BodyOwner::Const(const_) => Self::Const(const_),
            BodyOwner::AnonConstBody { body, .. } => Self::AnonConst(body),
            BodyOwner::ContractInit { contract } => Self::ContractInit(contract),
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => Self::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            },
            BodyOwner::Closure {
                def, receiver_mode, ..
            } => Self::Closure { def, receiver_mode },
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct SourceCallGraphEdge<'db> {
    call_site: Option<CallSiteId>,
    callee_key: SemanticInstanceKey<'db>,
    target: usize,
    flow: SourceCallGraphEdgeFlow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
enum SourceCallGraphEdgeFlow {
    /// Relate each target coordinate to the caller coordinates that occur in
    /// its actual specialization.
    Classify,
    /// An unresolved blanket implementation deconstructs the dispatched
    /// aggregate. Carry the aggregate itself, but do not mistake extracting an
    /// implementation parameter for a size-preserving edge.
    TraitAggregateCarry { impl_param_count: usize },
    /// Projection-heavy dispatch could not expose a structural relation.
    /// Conservatively treat every possible coordinate relation as growth.
    UnknownGrowth,
    /// A parent owns a nested closure syntactically. This is a topology edge,
    /// not a specialization step.
    ClosureContainment,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Update)]
struct SourceCallGraphEdgeSeed<'db> {
    call_site: Option<CallSiteId>,
    callee_key: SemanticInstanceKey<'db>,
    flow: SourceCallGraphEdgeFlow,
}

#[derive(Debug)]
struct SourceCallGraphNode<'db> {
    key: SemanticInstanceKey<'db>,
    edges: Vec<SourceCallGraphEdge<'db>>,
}

fn source_graph_template_owner<'db>(
    db: &'db dyn HirAnalysisDb,
    actual_owner: BodyOwner<'db>,
) -> BodyOwner<'db> {
    if let BodyOwner::Closure {
        def, receiver_mode, ..
    } = actual_owner
    {
        let Some(parent_owner) = BodyOwner::from_body(db, def.body) else {
            return actual_owner;
        };
        let template = typed_body_template(db, parent_owner);
        let Some(template_ty) = template
            .body
            .closure_info(def.expr)
            .filter(|info| info.def == def)
            .map(|info| info.ty)
        else {
            return actual_owner;
        };
        BodyOwner::Closure {
            ty: template_ty,
            def,
            receiver_mode,
        }
    } else {
        actual_owner
    }
}

fn source_graph_key_with_context<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    caller_key: Option<SemanticInstanceKey<'db>>,
    template_key: Option<SemanticInstanceKey<'db>>,
) -> SemanticInstanceKey<'db> {
    let owner = source_graph_template_owner(db, key.owner(db));
    let identity_args = owner_identity_generic_args(db, owner);
    let actual_args = key.subst(db).generic_args(db);
    let unresolved = TyFlags::HAS_PARAM | TyFlags::HAS_VAR | TyFlags::HAS_PROJECTION;
    let template_args = template_key.map(|template| template.subst(db).generic_args(db));
    let has_caller_context = caller_key.is_some();
    let caller_ground_args = caller_key
        .map(|caller| {
            semantic_instance_primary_state_tys(db, caller)
                .into_iter()
                .filter(|ty| !ty.flags(db).intersects(unresolved))
                .collect::<FxHashSet<_>>()
        })
        .unwrap_or_default();
    // Ground arguments can change trait candidate selection. Retain literals
    // already ground in the source template and unchanged ground coordinates
    // carried by the caller. A value made ground only by specializing a
    // caller-dependent expression is widened to the callee's formal
    // coordinate. The exact edge still records that specialization for
    // size-change analysis, while graph construction cannot unroll
    // `f<3, D> -> f<4, D> -> f<5, D> -> ...`.
    let generic_args = if actual_args.len() == identity_args.len() {
        actual_args
            .iter()
            .copied()
            .enumerate()
            .zip(identity_args.iter().copied())
            .map(|((idx, actual), identity)| {
                let source_literal = template_args
                    .and_then(|args| args.get(idx))
                    .is_some_and(|template| !template.flags(db).intersects(unresolved));
                if !actual.flags(db).intersects(unresolved)
                    && (!has_caller_context
                        || source_literal
                        || caller_ground_args.contains(&actual))
                {
                    actual
                } else {
                    identity
                }
            })
            .collect()
    } else {
        identity_args
    };
    let owner = if let BodyOwner::Closure {
        ty,
        def,
        receiver_mode,
    } = owner
    {
        let ty = Binder::bind(TyId::closure(db, ty))
            .instantiate(db, &generic_args)
            .as_closure(db)
            .unwrap_or(ty);
        BodyOwner::Closure {
            ty,
            def,
            receiver_mode,
        }
    } else {
        owner
    };
    SemanticInstanceKey::new(
        db,
        owner,
        GenericSubst::new(db, generic_args),
        EffectProviderSubst::empty(db),
        ImplEnv::empty(db, owner.scope()),
    )
}

fn source_graph_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> SemanticInstanceKey<'db> {
    source_graph_key_with_context(db, key, None, None)
}

fn source_graph_identity_key<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> SemanticInstanceKey<'db> {
    let owner = source_graph_template_owner(db, owner);
    identity_semantic_instance_key(db, owner)
}

#[salsa::tracked(return_ref)]
fn source_call_graph_edges<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    dispatch_ingot: Ingot<'db>,
) -> Vec<SourceCallGraphEdgeSeed<'db>> {
    let instance = SemanticInstance::new(db, key);
    let typed_body = key.typed_body(db);
    let Some(body) = typed_body.body() else {
        return Vec::new();
    };
    let Some(root) = key.owner(db).root_expr(db) else {
        return Vec::new();
    };
    let Partial::Present(root_data) = root.data(db, body) else {
        return Vec::new();
    };
    struct OwnerNodeCollector {
        exprs: FxHashSet<ExprId>,
        stmts: FxHashSet<StmtId>,
    }
    impl<'db> Visitor<'db> for OwnerNodeCollector {
        fn visit_expr(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
            expr: ExprId,
            expr_data: &Expr<'db>,
        ) {
            self.exprs.insert(expr);
            if !matches!(expr_data, Expr::Closure { .. }) {
                walk_expr(self, ctxt, expr);
            }
        }

        fn visit_stmt(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, LazyStmtSpan<'db>>,
            stmt: StmtId,
            _stmt_data: &Stmt<'db>,
        ) {
            self.stmts.insert(stmt);
            walk_stmt(self, ctxt, stmt);
        }

        fn visit_item(
            &mut self,
            _ctxt: &mut VisitorCtxt<'db, LazyItemSpan<'db>>,
            _item: ItemKind<'db>,
        ) {
        }
    }
    let mut owner_nodes = OwnerNodeCollector {
        exprs: FxHashSet::default(),
        stmts: FxHashSet::default(),
    };
    let mut visitor_ctxt = VisitorCtxt::with_expr(db, body.scope(), body, root);
    owner_nodes.visit_expr(&mut visitor_ctxt, root, root_data);

    let call_sites = provisional_call_sites(db, instance);
    let for_loop_call_sites = provisional_for_loop_call_sites(db, instance);
    let mut edges = Vec::new();
    for (expr, _) in body.exprs(db).iter() {
        if !owner_nodes.exprs.contains(&expr) {
            continue;
        }
        let Some(callee_key) = call_sites
            .get(expr.index())
            .and_then(Option::as_ref)
            .and_then(|site| site.callee)
            .map(|callee| callee.key)
        else {
            continue;
        };
        edges.push(SourceCallGraphEdgeSeed {
            call_site: Some(CallSiteId::Expr(expr)),
            callee_key,
            flow: SourceCallGraphEdgeFlow::Classify,
        });
        if let Some(SemanticExprLowering::Call { callable, .. }) =
            typed_body.semantic_expr_lowering(expr)
        {
            edges.extend(
                source_trait_dispatch_target_keys(db, dispatch_ingot, callable, callee_key)
                    .into_iter()
                    .map(|(target, flow)| SourceCallGraphEdgeSeed {
                        call_site: Some(CallSiteId::Expr(expr)),
                        callee_key: target,
                        flow,
                    }),
            );
        }
    }
    for (stmt, _) in body.stmts(db).iter() {
        if !owner_nodes.stmts.contains(&stmt) {
            continue;
        }
        let Some(sites) = for_loop_call_sites
            .get(stmt.index())
            .and_then(Option::as_ref)
        else {
            continue;
        };
        if let Some(callee) = sites.len.callee {
            edges.push(SourceCallGraphEdgeSeed {
                call_site: Some(CallSiteId::ForLoopLen(stmt)),
                callee_key: callee.key,
                flow: SourceCallGraphEdgeFlow::Classify,
            });
            if let Some(seq) = typed_body.for_loop_seq(stmt) {
                edges.extend(
                    source_trait_dispatch_target_keys(
                        db,
                        dispatch_ingot,
                        &seq.len_callable,
                        callee.key,
                    )
                    .into_iter()
                    .map(|(target, flow)| SourceCallGraphEdgeSeed {
                        call_site: Some(CallSiteId::ForLoopLen(stmt)),
                        callee_key: target,
                        flow,
                    }),
                );
            }
        }
        if let Some(callee) = sites.get.callee {
            edges.push(SourceCallGraphEdgeSeed {
                call_site: Some(CallSiteId::ForLoopGet(stmt)),
                callee_key: callee.key,
                flow: SourceCallGraphEdgeFlow::Classify,
            });
            if let Some(seq) = typed_body.for_loop_seq(stmt) {
                edges.extend(
                    source_trait_dispatch_target_keys(
                        db,
                        dispatch_ingot,
                        &seq.get_callable,
                        callee.key,
                    )
                    .into_iter()
                    .map(|(target, flow)| SourceCallGraphEdgeSeed {
                        call_site: Some(CallSiteId::ForLoopGet(stmt)),
                        callee_key: target,
                        flow,
                    }),
                );
            }
        }
    }
    // Multiple calls in one body can produce the same topology-only dispatch
    // edge (notably tuple ABI methods dispatching each field through the same
    // blanket implementation). The coordinate relation depends only on the
    // source node, target key, and flow kind; retaining every call-site copy
    // makes the graph quadratic without adding a distinct termination path.
    let mut seen = FxHashSet::default();
    edges.retain(|edge| seen.insert((edge.callee_key, edge.flow)));
    edges
}

fn source_trait_dispatch_target_keys<'db>(
    db: &'db dyn HirAnalysisDb,
    dispatch_ingot: Ingot<'db>,
    callable: &Callable<'db>,
    direct_callee_key: SemanticInstanceKey<'db>,
) -> Vec<(SemanticInstanceKey<'db>, SourceCallGraphEdgeFlow)> {
    let Some(inst) = callable.trait_inst() else {
        return Vec::new();
    };
    let CallableDef::Func(trait_method) = callable.callable_def() else {
        return Vec::new();
    };
    if !matches!(
        direct_callee_key.owner(db),
        BodyOwner::Func(direct) if direct == trait_method
    ) {
        // The provisional instance already selected an explicit implementation.
        return Vec::new();
    }
    let unresolved = TyFlags::HAS_PARAM | TyFlags::HAS_VAR | TyFlags::HAS_PROJECTION;
    if !inst
        .args(db)
        .iter()
        .chain(inst.assoc_type_bindings(db).values())
        .any(|ty| ty.flags(db).intersects(unresolved))
    {
        // A concrete default method cannot switch to another implementation
        // as its caller is specialized.
        return Vec::new();
    }
    source_trait_dispatch_target_keys_uncached(
        db,
        dispatch_ingot,
        inst,
        trait_method,
        direct_callee_key,
    )
}

fn source_trait_dispatch_target_keys_uncached<'db>(
    db: &'db dyn HirAnalysisDb,
    dispatch_ingot: Ingot<'db>,
    inst: TraitInstId<'db>,
    trait_method: Func<'db>,
    direct_callee_key: SemanticInstanceKey<'db>,
) -> Vec<(SemanticInstanceKey<'db>, SourceCallGraphEdgeFlow)> {
    let Some(name) = trait_method.name(db).to_opt() else {
        return Vec::new();
    };
    // Use the graph root's ingot rather than the generic callee's defining
    // ingot. A dependency-defined dispatcher can select an implementation
    // supplied by the application, and that implementation body may be the
    // edge that closes a growing recursive component.
    let mut targets = IndexSet::new();
    for implementor in impls_for_trait_def(db, dispatch_ingot, inst.def(db)) {
        if !trait_dispatch_impl_may_apply(db, inst, *implementor) {
            continue;
        }
        let candidate = implementor.skip_binder();
        let Some(target) = candidate.methods(db).get(&name).copied() else {
            continue;
        };
        if target.body(db).is_none() {
            continue;
        }
        let (target_key, flow) = specialized_trait_dispatch_target_key(
            db,
            inst,
            *implementor,
            target,
            direct_callee_key,
        )
        .unwrap_or_else(|| {
            (
                trait_aggregate_carry_target_key(
                    db,
                    *candidate,
                    target,
                    direct_callee_key,
                    inst.args(db).len(),
                ),
                SourceCallGraphEdgeFlow::TraitAggregateCarry {
                    impl_param_count: candidate.params(db).len(),
                },
            )
        });
        targets.insert((target_key, flow));
    }
    targets.into_iter().collect()
}

fn trait_aggregate_carry_target_key<'db>(
    db: &'db dyn HirAnalysisDb,
    candidate: ImplementorId<'db>,
    target: Func<'db>,
    direct_callee_key: SemanticInstanceKey<'db>,
    trait_arg_len: usize,
) -> SemanticInstanceKey<'db> {
    let generic_args = complete_resolved_trait_method_args(
        db,
        target,
        candidate.params(db).to_vec(),
        direct_callee_key.subst(db).generic_args(db),
        trait_arg_len,
    );
    let owner = BodyOwner::Func(target);
    SemanticInstanceKey::new(
        db,
        owner,
        GenericSubst::new(db, generic_args),
        EffectProviderSubst::empty(db),
        ImplEnv::empty(db, owner.scope()),
    )
}

fn specialized_trait_dispatch_target_key<'db>(
    db: &'db dyn HirAnalysisDb,
    dispatch_inst: TraitInstId<'db>,
    implementor: Binder<ImplementorId<'db>>,
    target: crate::hir_def::Func<'db>,
    direct_callee_key: SemanticInstanceKey<'db>,
) -> Option<(SemanticInstanceKey<'db>, SourceCallGraphEdgeFlow)> {
    let candidate = implementor.skip_binder();
    let may_need_normalization = |inst: TraitInstId<'db>| {
        inst.args(db)
            .iter()
            .chain(inst.assoc_type_bindings(db).values())
            .any(|ty| {
                ty.flags(db)
                    .intersects(TyFlags::HAS_PROJECTION | TyFlags::HAS_INVALID)
            })
    };
    if may_need_normalization(dispatch_inst) || may_need_normalization(candidate.trait_inst(db)) {
        return Some((
            identity_semantic_instance_key(db, BodyOwner::Func(target)),
            SourceCallGraphEdgeFlow::UnknownGrowth,
        ));
    }

    // Keep the dispatcher's source parameters rigid and instantiate only the
    // candidate binder. When this succeeds, the solved implementation
    // parameters are symbolic expressions in the caller coordinates (for
    // example `P = T` or `P = Wrap<T>`), which is precisely the topology
    // relation needed by the size-change graph.
    let mut table = UnificationTable::new(db);
    let instantiated = table.instantiate_with_fresh_vars(implementor);
    if !unify_trait_dispatch_candidate_args(
        db,
        &mut table,
        dispatch_inst,
        instantiated.trait_inst(db),
    ) {
        // A blanket pattern such as `Wrap<P>` can still apply to an unresolved
        // dispatch `Self = T` by deconstructing a future specialization of T.
        // The aggregate carry is exact; the extracted P is a strict subterm
        // and intentionally does not retain the caller's size lineage.
        return None;
    }

    let impl_args = instantiated
        .params(db)
        .iter()
        .map(|param| param.fold_with(db, &mut table))
        .collect();
    let generic_args = complete_resolved_trait_method_args(
        db,
        target,
        impl_args,
        direct_callee_key.subst(db).generic_args(db),
        dispatch_inst.args(db).len(),
    );
    let owner = BodyOwner::Func(target);
    Some((
        SemanticInstanceKey::new(
            db,
            owner,
            GenericSubst::new(db, generic_args),
            EffectProviderSubst::empty(db),
            ImplEnv::empty(db, owner.scope()),
        ),
        SourceCallGraphEdgeFlow::Classify,
    ))
}

fn unify_trait_dispatch_candidate_args<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    dispatch_inst: TraitInstId<'db>,
    candidate_inst: TraitInstId<'db>,
) -> bool {
    if dispatch_inst.def(db) != candidate_inst.def(db)
        || dispatch_inst.args(db).len() != candidate_inst.args(db).len()
    {
        return false;
    }
    for (&dispatch_arg, &candidate_arg) in
        dispatch_inst.args(db).iter().zip(candidate_inst.args(db))
    {
        if table.unify(dispatch_arg, candidate_arg).is_err() {
            return false;
        }
    }
    for (&name, &dispatch_binding) in dispatch_inst.assoc_type_bindings(db) {
        let Some(&candidate_binding) = candidate_inst.assoc_type_bindings(db).get(&name) else {
            // An absent candidate binding is not evidence of incompatibility:
            // it may be supplied by a default or remain abstract. The source
            // topology only needs to reject candidates whose known headers
            // conflict with the dispatch.
            continue;
        };
        if table.unify(dispatch_binding, candidate_binding).is_err() {
            return false;
        }
    }
    true
}

fn trait_dispatch_impl_may_apply<'db>(
    db: &'db dyn HirAnalysisDb,
    dispatch_inst: TraitInstId<'db>,
    implementor: Binder<ImplementorId<'db>>,
) -> bool {
    let candidate_inst = implementor.skip_binder().trait_inst(db);
    let may_need_normalization = |inst: TraitInstId<'db>| {
        inst.args(db)
            .iter()
            .chain(inst.assoc_type_bindings(db).values())
            .any(|ty| {
                ty.flags(db)
                    .intersects(TyFlags::HAS_PROJECTION | TyFlags::HAS_INVALID)
            })
    };
    if may_need_normalization(dispatch_inst) || may_need_normalization(candidate_inst) {
        // Candidate selection normalizes projection-heavy goals before
        // unification. Keep those targets in this conservative topology.
        return true;
    }

    let mut table = UnificationTable::new(db);
    let dispatch_inst = table.instantiate_with_fresh_vars(Binder::bind(dispatch_inst));
    let implementor = table.instantiate_with_fresh_vars(implementor);
    unify_trait_dispatch_candidate_args(db, &mut table, dispatch_inst, implementor.trait_inst(db))
}

fn source_contained_closure_keys<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> Vec<SemanticInstanceKey<'db>> {
    if matches!(key.owner(db), BodyOwner::Closure { .. }) {
        return Vec::new();
    }
    let typed_body = key.typed_body(db);
    typed_body
        .closure_infos()
        .filter_map(|(expr, _)| typed_body.expr_ty(db, expr).as_closure(db))
        .map(|closure_ty| {
            source_graph_key(
                db,
                identity_semantic_instance_key(db, BodyOwner::closure(db, closure_ty)),
            )
        })
        .collect()
}

fn push_source_graph_root<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    nodes: &mut Vec<SourceCallGraphNode<'db>>,
    node_by_key: &mut FxHashMap<SemanticInstanceKey<'db>, usize>,
) {
    let key = source_graph_key(db, key);
    let owner = key.owner(db);
    if owner.body(db).is_none() {
        return;
    }
    if node_by_key.contains_key(&key) {
        return;
    }
    let node_idx = nodes.len();
    node_by_key.insert(key, node_idx);
    nodes.push(SourceCallGraphNode {
        key,
        edges: Vec::new(),
    });
}

fn raw_alias_targets<'db>(
    db: &'db dyn HirAnalysisDb,
    name: IdentId<'db>,
    aliases: &[(IdentId<'db>, IdentId<'db>)],
) -> FxHashSet<IdentId<'db>> {
    let mut targets = FxHashSet::default();
    let mut pending = vec![name];
    while let Some(name) = pending.pop() {
        if !targets.insert(name) {
            continue;
        }
        pending.extend(
            aliases
                .iter()
                .filter(|(alias, _)| *alias == name)
                .map(|(_, target)| *target),
        );
    }
    let _ = db;
    targets
}

fn raw_func_qualifier_names<'db>(db: &'db dyn HirAnalysisDb, func: Func<'db>) -> Vec<IdentId<'db>> {
    fn type_root_name<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: Partial<crate::hir_def::TypeId<'db>>,
    ) -> Option<IdentId<'db>> {
        let ty = ty.to_opt()?;
        match ty.data(db) {
            TypeKind::Path(Partial::Present(path)) => path.ident(db).to_opt(),
            TypeKind::Mode(_, inner) | TypeKind::Ptr(inner) => type_root_name(db, *inner),
            TypeKind::Path(Partial::Absent)
            | TypeKind::Tuple(_)
            | TypeKind::Array(_, _)
            | TypeKind::Never => None,
        }
    }

    let mut qualifiers = Vec::new();
    match func.scope().parent_item(db) {
        Some(ItemKind::TopMod(top_mod)) => qualifiers.push(top_mod.name(db)),
        Some(ItemKind::Mod(mod_)) => qualifiers.extend(mod_.name(db).to_opt()),
        Some(ItemKind::Trait(trait_)) => qualifiers.extend(trait_.name(db).to_opt()),
        Some(ItemKind::Impl(impl_)) => {
            qualifiers.extend(type_root_name(db, impl_.hir_type_ref(db)));
        }
        Some(ItemKind::ImplTrait(impl_trait)) => {
            qualifiers.extend(
                impl_trait
                    .hir_trait_ref(db)
                    .to_opt()
                    .and_then(|trait_ref| trait_ref.path(db).to_opt())
                    .and_then(|path| path.ident(db).to_opt()),
            );
            qualifiers.extend(type_root_name(db, impl_trait.hir_type_ref(db)));
        }
        Some(
            ItemKind::Func(_)
            | ItemKind::Struct(_)
            | ItemKind::Contract(_)
            | ItemKind::Enum(_)
            | ItemKind::TypeAlias(_)
            | ItemKind::Const(_)
            | ItemKind::StaticAssert(_)
            | ItemKind::Use(_)
            | ItemKind::Body(_),
        )
        | None => {}
    }
    qualifiers
}

fn raw_func_generic_param_names<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> FxHashSet<IdentId<'db>> {
    let mut names = FxHashSet::default();
    let mut extend = |params: crate::hir_def::GenericParamListId<'db>| {
        names.extend(
            params
                .data(db)
                .iter()
                .filter_map(|param| param.name().to_opt()),
        );
    };
    extend(func.hir_generic_params(db));
    match func.scope().parent_item(db) {
        Some(ItemKind::Trait(trait_)) => extend(trait_.hir_generic_params(db)),
        Some(ItemKind::Impl(impl_)) => extend(impl_.hir_generic_params(db)),
        Some(ItemKind::ImplTrait(impl_trait)) => extend(impl_trait.hir_generic_params(db)),
        Some(
            ItemKind::TopMod(_)
            | ItemKind::Mod(_)
            | ItemKind::Func(_)
            | ItemKind::Struct(_)
            | ItemKind::Contract(_)
            | ItemKind::Enum(_)
            | ItemKind::TypeAlias(_)
            | ItemKind::Const(_)
            | ItemKind::StaticAssert(_)
            | ItemKind::Use(_)
            | ItemKind::Body(_),
        )
        | None => {}
    }
    names
}

fn raw_path_may_refer_to_func<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    candidate: Func<'db>,
    aliases: &[(IdentId<'db>, IdentId<'db>)],
    type_alias_names: &FxHashSet<IdentId<'db>>,
    namespace_names: &FxHashSet<IdentId<'db>>,
    source_generic_names: &FxHashSet<IdentId<'db>>,
) -> bool {
    let Some(path_name) = path.ident(db).to_opt() else {
        return true;
    };
    let Some(candidate_name) = candidate.name(db).to_opt() else {
        return true;
    };
    if !raw_alias_targets(db, path_name, aliases).contains(&candidate_name) {
        return false;
    }
    if path.segment_index(db) == 0 {
        return true;
    }
    let Some(actual_qualifier) = path
        .segment(db, path.segment_index(db) - 1)
        .and_then(|segment| segment.ident(db).to_opt())
    else {
        return true;
    };
    if actual_qualifier.is_self_ty(db) {
        return candidate.is_associated_func(db);
    }
    if source_generic_names.contains(&actual_qualifier) {
        return candidate.is_associated_func(db);
    }
    if actual_qualifier.is_self(db)
        || actual_qualifier.is_super(db)
        || actual_qualifier.is_ingot(db)
        || actual_qualifier.is_core(db)
    {
        return true;
    }
    let actual_qualifiers = raw_alias_targets(db, actual_qualifier, aliases);
    if actual_qualifiers
        .iter()
        .any(|qualifier| type_alias_names.contains(qualifier))
    {
        // Resolving an alias target while type checking can introduce a query
        // cycle. Treat any type alias as a possible associated qualifier.
        return candidate.is_associated_func(db);
    }
    if !actual_qualifiers
        .iter()
        .any(|qualifier| namespace_names.contains(qualifier))
    {
        // A generic parameter, projection, or unresolved/re-exported name is
        // not a syntactically definite namespace. A qualifier mismatch is
        // therefore not evidence that the associated target is different.
        return candidate.is_associated_func(db);
    }
    let expected = raw_func_qualifier_names(db, candidate);
    expected.is_empty()
        || expected
            .into_iter()
            .any(|expected| actual_qualifiers.contains(&expected))
}

fn raw_func_matches_core_trait_method<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    trait_name: &str,
    method_name: &str,
    aliases: &[(IdentId<'db>, IdentId<'db>)],
) -> bool {
    if func.name(db).to_opt().map(|name| name.data(db).as_str()) != Some(method_name) {
        return false;
    }
    let raw_trait_name = match func.scope().parent_item(db) {
        Some(ItemKind::Trait(trait_)) => trait_.name(db).to_opt(),
        Some(ItemKind::ImplTrait(impl_trait)) => impl_trait
            .hir_trait_ref(db)
            .to_opt()
            .and_then(|trait_ref| trait_ref.path(db).to_opt())
            .and_then(|path| path.ident(db).to_opt()),
        Some(
            ItemKind::TopMod(_)
            | ItemKind::Mod(_)
            | ItemKind::Func(_)
            | ItemKind::Struct(_)
            | ItemKind::Contract(_)
            | ItemKind::Enum(_)
            | ItemKind::TypeAlias(_)
            | ItemKind::Impl(_)
            | ItemKind::Const(_)
            | ItemKind::StaticAssert(_)
            | ItemKind::Use(_)
            | ItemKind::Body(_),
        )
        | None => None,
    };
    raw_trait_name.is_some_and(|name| {
        raw_alias_targets(db, name, aliases)
            .iter()
            .any(|name| name.data(db) == trait_name)
    })
}

fn raw_bin_trait_method(op: BinOp) -> Option<(&'static str, &'static str)> {
    Some(match op {
        BinOp::Arith(ArithBinOp::Add) => ("Add", "add"),
        BinOp::Arith(ArithBinOp::Sub) => ("Sub", "sub"),
        BinOp::Arith(ArithBinOp::Mul) => ("Mul", "mul"),
        BinOp::Arith(ArithBinOp::Div) => ("Div", "div"),
        BinOp::Arith(ArithBinOp::Rem) => ("Rem", "rem"),
        BinOp::Arith(ArithBinOp::Pow) => ("Pow", "pow"),
        BinOp::Arith(ArithBinOp::LShift) => ("Shl", "shl"),
        BinOp::Arith(ArithBinOp::RShift) => ("Shr", "shr"),
        BinOp::Arith(ArithBinOp::BitAnd) => ("BitAnd", "bitand"),
        BinOp::Arith(ArithBinOp::BitOr) => ("BitOr", "bitor"),
        BinOp::Arith(ArithBinOp::BitXor) => ("BitXor", "bitxor"),
        BinOp::Comp(CompBinOp::Eq) => ("Eq", "eq"),
        BinOp::Comp(CompBinOp::NotEq) => ("Eq", "ne"),
        BinOp::Comp(CompBinOp::Lt) => ("Ord", "lt"),
        BinOp::Comp(CompBinOp::LtEq) => ("Ord", "le"),
        BinOp::Comp(CompBinOp::Gt) => ("Ord", "gt"),
        BinOp::Comp(CompBinOp::GtEq) => ("Ord", "ge"),
        BinOp::Index => ("Index", "index"),
        BinOp::Arith(ArithBinOp::Range) | BinOp::Logical(_) => return None,
    })
}

fn raw_un_trait_method(op: UnOp) -> Option<(&'static str, &'static str)> {
    Some(match op {
        UnOp::Plus => ("UnaryPlus", "add"),
        UnOp::Minus => ("Neg", "neg"),
        UnOp::Not => ("Not", "not"),
        UnOp::BitNot => ("BitNot", "bit_not"),
        UnOp::Mut | UnOp::Ref => return None,
    })
}

fn raw_aug_assign_trait_method(op: ArithBinOp) -> Option<(&'static str, &'static str)> {
    Some(match op {
        ArithBinOp::Add => ("AddAssign", "add_assign"),
        ArithBinOp::Sub => ("SubAssign", "sub_assign"),
        ArithBinOp::Mul => ("MulAssign", "mul_assign"),
        ArithBinOp::Div => ("DivAssign", "div_assign"),
        ArithBinOp::Rem => ("RemAssign", "rem_assign"),
        ArithBinOp::Pow => ("PowAssign", "pow_assign"),
        ArithBinOp::LShift => ("ShlAssign", "shl_assign"),
        ArithBinOp::RShift => ("ShrAssign", "shr_assign"),
        ArithBinOp::BitAnd => ("BitAndAssign", "bitand_assign"),
        ArithBinOp::BitOr => ("BitOrAssign", "bitor_assign"),
        ArithBinOp::BitXor => ("BitXorAssign", "bitxor_assign"),
        ArithBinOp::Range => return None,
    })
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
struct RawSourceCallTopology<'db> {
    component_by_func: FxHashMap<Func<'db>, usize>,
    cyclic_components: FxHashSet<usize>,
}

#[salsa::tracked(return_ref)]
fn raw_source_call_topology<'db>(
    db: &'db dyn HirAnalysisDb,
    dispatch_ingot: Ingot<'db>,
) -> RawSourceCallTopology<'db> {
    let mut pending_ingots = vec![dispatch_ingot];
    let mut seen_ingots = FxHashSet::default();
    let mut aliases = Vec::new();
    let mut funcs = FxHashSet::default();
    let mut type_alias_names = FxHashSet::default();
    let mut namespace_names = FxHashSet::default();
    while let Some(ingot) = pending_ingots.pop() {
        if !seen_ingots.insert(ingot) {
            continue;
        }
        pending_ingots.extend(
            ingot
                .resolved_external_ingots(db)
                .iter()
                .map(|(_, dependency)| *dependency),
        );
        for item in ingot.all_items(db) {
            if let ItemKind::Use(use_) = item
                && let Some(imported_name) = use_.imported_name(db)
                && let Some(imported_path_name) =
                    use_.path(db).to_opt().and_then(|path| path.last_ident(db))
            {
                aliases.push((imported_name, imported_path_name));
            }
            match item {
                ItemKind::Func(func) => {
                    funcs.insert(*func);
                }
                ItemKind::TypeAlias(alias) => {
                    type_alias_names.extend(alias.name(db).to_opt());
                    namespace_names.extend(alias.name(db).to_opt());
                }
                ItemKind::TopMod(top_mod) => {
                    namespace_names.insert(top_mod.name(db));
                }
                ItemKind::Mod(mod_) => {
                    namespace_names.extend(mod_.name(db).to_opt());
                }
                ItemKind::Struct(struct_) => {
                    namespace_names.extend(struct_.name(db).to_opt());
                }
                ItemKind::Contract(contract) => {
                    namespace_names.extend(contract.name(db).to_opt());
                }
                ItemKind::Enum(enum_) => {
                    namespace_names.extend(enum_.name(db).to_opt());
                }
                ItemKind::Trait(trait_) => {
                    namespace_names.extend(trait_.name(db).to_opt());
                }
                ItemKind::Impl(_)
                | ItemKind::ImplTrait(_)
                | ItemKind::Const(_)
                | ItemKind::StaticAssert(_)
                | ItemKind::Use(_)
                | ItemKind::Body(_) => {}
            }
        }
    }
    let funcs = funcs.into_iter().collect::<Vec<_>>();
    let mut funcs_by_name: FxHashMap<IdentId<'db>, Vec<Func<'db>>> = FxHashMap::default();
    for &func in &funcs {
        if let Some(name) = func.name(db).to_opt() {
            funcs_by_name.entry(name).or_default().push(func);
        }
    }

    let func_indices = funcs
        .iter()
        .enumerate()
        .map(|(idx, &func)| (func, idx))
        .collect::<FxHashMap<_, _>>();
    let callable_funcs = funcs
        .iter()
        .copied()
        .filter(|&candidate| {
            raw_func_matches_core_trait_method(db, candidate, "Fn", "call", &aliases)
                || raw_func_matches_core_trait_method(
                    db,
                    candidate,
                    "FnOnce",
                    "call_once",
                    &aliases,
                )
        })
        .collect::<Vec<_>>();
    let method_arities = funcs
        .iter()
        .copied()
        .filter(|func| func.is_method(db))
        .map(|func| (func, func.params(db).count().saturating_sub(1)))
        .collect::<FxHashMap<_, _>>();
    let mut adjacency = vec![Vec::new(); funcs.len()];
    for (source, &func) in funcs.iter().enumerate() {
        let Some(body) = func.body(db) else {
            continue;
        };
        let source_generic_names = raw_func_generic_param_names(db, func);
        let mut targets = FxHashSet::default();
        for (_, expr) in body.exprs(db).iter() {
            let Partial::Present(expr) = expr else {
                continue;
            };
            match expr {
                Expr::Path(Partial::Present(path)) => {
                    if let Some(path_name) = path.ident(db).to_opt() {
                        for name in raw_alias_targets(db, path_name, &aliases) {
                            for &candidate in funcs_by_name.get(&name).into_iter().flatten() {
                                if raw_path_may_refer_to_func(
                                    db,
                                    *path,
                                    candidate,
                                    &aliases,
                                    &type_alias_names,
                                    &namespace_names,
                                    &source_generic_names,
                                ) {
                                    targets.insert(candidate);
                                }
                            }
                        }
                    }
                }
                Expr::MethodCall(_, Partial::Present(name), _, args) => {
                    for &candidate in funcs_by_name.get(name).into_iter().flatten() {
                        if callable_funcs.contains(&candidate)
                            || method_arities
                                .get(&candidate)
                                .is_some_and(|arity| *arity == args.len())
                        {
                            targets.insert(candidate);
                        }
                    }
                }
                Expr::Call(_, _) => {
                    // A call through a callable value need not repeat its
                    // eventual trait-method name in source.
                    targets.extend(callable_funcs.iter().copied());
                }
                Expr::Bin(_, _, op) => {
                    if let Some((trait_name, method_name)) = raw_bin_trait_method(*op) {
                        for &candidate in funcs_by_name
                            .get(&IdentId::new(db, method_name.to_string()))
                            .into_iter()
                            .flatten()
                        {
                            if raw_func_matches_core_trait_method(
                                db,
                                candidate,
                                trait_name,
                                method_name,
                                &aliases,
                            ) {
                                targets.insert(candidate);
                            }
                        }
                    }
                }
                Expr::Un(_, op) => {
                    if let Some((trait_name, method_name)) = raw_un_trait_method(*op) {
                        for &candidate in funcs_by_name
                            .get(&IdentId::new(db, method_name.to_string()))
                            .into_iter()
                            .flatten()
                        {
                            if raw_func_matches_core_trait_method(
                                db,
                                candidate,
                                trait_name,
                                method_name,
                                &aliases,
                            ) {
                                targets.insert(candidate);
                            }
                        }
                    }
                }
                Expr::AugAssign(_, _, op) => {
                    if let Some((trait_name, method_name)) = raw_aug_assign_trait_method(*op) {
                        for &candidate in funcs_by_name
                            .get(&IdentId::new(db, method_name.to_string()))
                            .into_iter()
                            .flatten()
                        {
                            if raw_func_matches_core_trait_method(
                                db,
                                candidate,
                                trait_name,
                                method_name,
                                &aliases,
                            ) {
                                targets.insert(candidate);
                            }
                        }
                    }
                }
                Expr::Lit(_)
                | Expr::Block(_)
                | Expr::Closure { .. }
                | Expr::Cast(_, _)
                | Expr::Assert(_)
                | Expr::MethodCall(..)
                | Expr::Path(Partial::Absent)
                | Expr::RecordInit(_, _)
                | Expr::Field(_, _)
                | Expr::Tuple(_)
                | Expr::Array(_)
                | Expr::ArrayRep(_, _)
                | Expr::If(_, _, _)
                | Expr::Match(_, _)
                | Expr::Assign(_, _)
                | Expr::With(_, _) => {}
            }
        }
        if body
            .stmts(db)
            .iter()
            .any(|(_, stmt)| matches!(stmt, Partial::Present(Stmt::For(..))))
        {
            for (trait_name, method_name) in [("Seq", "len"), ("Seq", "get")] {
                for &candidate in funcs_by_name
                    .get(&IdentId::new(db, method_name.to_string()))
                    .into_iter()
                    .flatten()
                {
                    if raw_func_matches_core_trait_method(
                        db,
                        candidate,
                        trait_name,
                        method_name,
                        &aliases,
                    ) {
                        targets.insert(candidate);
                    }
                }
            }
        }

        adjacency[source].extend(
            targets
                .into_iter()
                .filter_map(|target| func_indices.get(&target).copied()),
        );
    }
    let components = adjacency_graph_components(&adjacency);
    let mut component_sizes = FxHashMap::default();
    for &component in &components {
        *component_sizes.entry(component).or_insert(0usize) += 1;
    }
    let cyclic_components = adjacency
        .iter()
        .enumerate()
        .filter(|(source, targets)| {
            component_sizes
                .get(&components[*source])
                .is_some_and(|size| *size > 1)
                || targets.contains(source)
        })
        .map(|(source, _)| components[source])
        .collect();
    RawSourceCallTopology {
        component_by_func: funcs
            .into_iter()
            .zip(components)
            .collect::<FxHashMap<_, _>>(),
        cyclic_components,
    }
}

fn func_may_participate_in_source_call_cycle<'db>(
    db: &'db dyn HirAnalysisDb,
    root: Func<'db>,
    dispatch_ingot: Ingot<'db>,
) -> bool {
    if root.body(db).is_none() || root.name(db).to_opt().is_none() {
        return false;
    }
    let topology = raw_source_call_topology(db, dispatch_ingot);
    let Some(component) = topology.component_by_func.get(&root) else {
        return true;
    };
    topology.cyclic_components.contains(component)
}

fn source_graph_template_callee_key<'db>(
    db: &'db dyn HirAnalysisDb,
    caller_key: SemanticInstanceKey<'db>,
    edge_seed: SourceCallGraphEdgeSeed<'db>,
    dispatch_ingot: Ingot<'db>,
) -> Option<SemanticInstanceKey<'db>> {
    let template_caller = source_graph_identity_key(db, caller_key.owner(db));
    let target_owner = SyntacticBodyOwner::from(edge_seed.callee_key.owner(db));
    let candidates = source_call_graph_edges(db, template_caller, dispatch_ingot)
        .iter()
        .copied()
        .filter(|candidate| {
            candidate.call_site == edge_seed.call_site
                && SyntacticBodyOwner::from(candidate.callee_key.owner(db)) == target_owner
        })
        .collect::<Vec<_>>();
    candidates
        .iter()
        .find(|candidate| candidate.flow == edge_seed.flow)
        .or_else(|| candidates.first())
        .map(|candidate| candidate.callee_key)
}

fn build_source_call_graph<'db>(
    db: &'db dyn HirAnalysisDb,
    root_key: SemanticInstanceKey<'db>,
    dispatch_ingot: Ingot<'db>,
) -> Vec<SourceCallGraphNode<'db>> {
    let mut nodes = Vec::new();
    let mut node_by_key = FxHashMap::default();
    let root_owner = root_key.owner(db);
    push_source_graph_root(db, root_key, &mut nodes, &mut node_by_key);
    if nodes.is_empty() {
        return nodes;
    }
    let raw_topology = raw_source_call_topology(db, dispatch_ingot);
    let root_raw_component = match root_owner {
        BodyOwner::Func(func) => raw_topology.component_by_func.get(&func).copied(),
        BodyOwner::Const(_)
        | BodyOwner::AnonConstBody { .. }
        | BodyOwner::ContractInit { .. }
        | BodyOwner::ContractRecvArm { .. }
        | BodyOwner::Closure { .. } => None,
    };
    let can_return_to_root = |owner: BodyOwner<'db>| {
        let Some(root_component) = root_raw_component else {
            return true;
        };
        match owner {
            BodyOwner::Func(func) => raw_topology
                .component_by_func
                .get(&func)
                .is_none_or(|component| *component == root_component),
            // Raw bodies include their nested closure expressions, so a
            // closure-to-function return edge is reflected in the enclosing
            // function's raw component. Analyze the closure itself to retain
            // the precise specialization flow.
            BodyOwner::Closure { .. } => true,
            BodyOwner::Const(_)
            | BodyOwner::AnonConstBody { .. }
            | BodyOwner::ContractInit { .. }
            | BodyOwner::ContractRecvArm { .. } => true,
        }
    };

    let mut pending = vec![0];
    let mut analyzed = FxHashSet::default();
    while let Some(node_idx) = pending.pop() {
        if !analyzed.insert(node_idx) {
            continue;
        }
        let key = nodes[node_idx].key;
        let mut edges = Vec::new();
        for &edge_seed in source_call_graph_edges(db, key, dispatch_ingot) {
            let SourceCallGraphEdgeSeed {
                call_site,
                callee_key,
                flow,
            } = edge_seed;
            let template_key = source_graph_template_callee_key(db, key, edge_seed, dispatch_ingot);
            let source_key = source_graph_key_with_context(db, callee_key, Some(key), template_key);
            let target = if let Some(target) = node_by_key.get(&source_key).copied() {
                target
            } else {
                let target = nodes.len();
                node_by_key.insert(source_key, target);
                nodes.push(SourceCallGraphNode {
                    key: source_key,
                    edges: Vec::new(),
                });
                target
            };
            let edge = SourceCallGraphEdge {
                call_site,
                callee_key,
                target,
                flow,
            };
            let dependency =
                source_edge_coordinate_flows(db, key, nodes[target].key, callee_key, flow);
            if can_return_to_root(nodes[target].key.owner(db))
                && (dependency.structurally_unknown || !dependency.flows.is_empty())
            {
                pending.push(target);
            }
            edges.push(edge);
        }
        for closure_key in source_contained_closure_keys(db, key) {
            let target = if let Some(target) = node_by_key.get(&closure_key).copied() {
                target
            } else {
                let target = nodes.len();
                node_by_key.insert(closure_key, target);
                nodes.push(SourceCallGraphNode {
                    key: closure_key,
                    edges: Vec::new(),
                });
                target
            };
            let edge = SourceCallGraphEdge {
                call_site: None,
                callee_key: closure_key,
                target,
                flow: SourceCallGraphEdgeFlow::ClosureContainment,
            };
            let dependency =
                source_edge_coordinate_flows(db, key, nodes[target].key, closure_key, edge.flow);
            if can_return_to_root(nodes[target].key.owner(db))
                && (dependency.structurally_unknown || !dependency.flows.is_empty())
            {
                pending.push(target);
            }
            edges.push(edge);
        }
        nodes[node_idx].edges = edges;
    }
    nodes
}

fn source_call_graph_components(nodes: &[SourceCallGraphNode<'_>]) -> Vec<usize> {
    let mut visited = vec![false; nodes.len()];
    let mut finish_order = Vec::with_capacity(nodes.len());
    for root in 0..nodes.len() {
        if visited[root] {
            continue;
        }
        visited[root] = true;
        let mut pending = vec![(root, 0)];
        while let Some((node, edge_idx)) = pending.pop() {
            if let Some(edge) = nodes[node].edges.get(edge_idx) {
                pending.push((node, edge_idx + 1));
                if !visited[edge.target] {
                    visited[edge.target] = true;
                    pending.push((edge.target, 0));
                }
            } else {
                finish_order.push(node);
            }
        }
    }

    let mut reverse = vec![Vec::new(); nodes.len()];
    for (source, node) in nodes.iter().enumerate() {
        for edge in &node.edges {
            reverse[edge.target].push(source);
        }
    }
    let mut components = vec![usize::MAX; nodes.len()];
    let mut component = 0;
    for root in finish_order.into_iter().rev() {
        if components[root] != usize::MAX {
            continue;
        }
        components[root] = component;
        let mut pending = vec![root];
        while let Some(node) = pending.pop() {
            for predecessor in reverse[node].iter().copied() {
                if components[predecessor] == usize::MAX {
                    components[predecessor] = component;
                    pending.push(predecessor);
                }
            }
        }
        component += 1;
    }
    components
}

fn adjacency_graph_components(adjacency: &[Vec<usize>]) -> Vec<usize> {
    let mut visited = vec![false; adjacency.len()];
    let mut finish_order = Vec::with_capacity(adjacency.len());
    for root in 0..adjacency.len() {
        if visited[root] {
            continue;
        }
        visited[root] = true;
        let mut pending = vec![(root, 0)];
        while let Some((node, edge_idx)) = pending.pop() {
            if let Some(target) = adjacency[node].get(edge_idx).copied() {
                pending.push((node, edge_idx + 1));
                if !visited[target] {
                    visited[target] = true;
                    pending.push((target, 0));
                }
            } else {
                finish_order.push(node);
            }
        }
    }

    let mut reverse = vec![Vec::new(); adjacency.len()];
    for (source, targets) in adjacency.iter().enumerate() {
        for target in targets {
            reverse[*target].push(source);
        }
    }
    let mut components = vec![usize::MAX; adjacency.len()];
    let mut component = 0;
    for root in finish_order.into_iter().rev() {
        if components[root] != usize::MAX {
            continue;
        }
        components[root] = component;
        let mut pending = vec![root];
        while let Some(node) = pending.pop() {
            for predecessor in reverse[node].iter().copied() {
                if components[predecessor] == usize::MAX {
                    components[predecessor] = component;
                    pending.push(predecessor);
                }
            }
        }
        component += 1;
    }
    components
}

struct ShallowStateTyCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    tys: Vec<TyId<'db>>,
}

impl<'db> TyVisitor<'db> for ShallowStateTyCollector<'db> {
    fn db(&self) -> &'db dyn HirAnalysisDb {
        self.db
    }

    fn visit_ty(&mut self, ty: TyId<'db>) {
        self.tys.push(ty);
    }
}

fn semantic_instance_state_tys<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> Vec<TyId<'db>> {
    let mut collector = ShallowStateTyCollector {
        db,
        tys: Vec::new(),
    };
    key.subst(db)
        .generic_args(db)
        .as_slice()
        .visit_with(&mut collector);
    match key.owner(db) {
        BodyOwner::AnonConstBody { expected, .. } => collector.tys.push(expected),
        // A closure's signature and captures are derived from its parent
        // substitution. They can contain a caller parameter structurally
        // without introducing another independent instance axis; the
        // substitution above is the state that can actually grow around a
        // recursive cycle.
        BodyOwner::Closure { .. } => {}
        BodyOwner::Func(_)
        | BodyOwner::Const(_)
        | BodyOwner::ContractInit { .. }
        | BodyOwner::ContractRecvArm { .. } => {}
    }
    for provider in key.effect_providers(db).providers(db) {
        provider.visit_with(&mut collector);
        if let ProviderLayoutEvidence::ResolvedHandle(instance) =
            provider.provider.semantics.evidence
        {
            instance.visit_with(&mut collector);
        }
    }
    let impl_env = key.impl_env(db);
    impl_env.assumptions(db).visit_with(&mut collector);
    impl_env.witnesses(db).as_slice().visit_with(&mut collector);
    collector.tys
}

fn semantic_instance_primary_state_tys<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> Vec<TyId<'db>> {
    let mut collector = ShallowStateTyCollector {
        db,
        tys: Vec::new(),
    };
    key.subst(db)
        .generic_args(db)
        .as_slice()
        .visit_with(&mut collector);
    if let BodyOwner::AnonConstBody { expected, .. } = key.owner(db) {
        collector.tys.push(expected);
    }
    collector.tys
}

#[derive(Debug, Clone, Copy)]
struct SourceStateCoordinate<'db> {
    ty: TyId<'db>,
    /// An impl/closure aggregate is derived from the actual specialization
    /// axes. Reconstructing it preserves a lineage but does not itself grow
    /// the family.
    derived_aggregate: bool,
}

fn semantic_instance_state_coordinate_shapes<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> Vec<SourceStateCoordinate<'db>> {
    let mut coordinates = semantic_instance_primary_state_tys(db, key)
        .into_iter()
        .map(|ty| SourceStateCoordinate {
            ty,
            derived_aggregate: false,
        })
        .collect::<Vec<_>>();
    match key.owner(db) {
        BodyOwner::Func(func) => {
            if let Some(ItemKind::ImplTrait(impl_trait)) = func.scope().parent_item(db) {
                let self_ty =
                    Binder::bind(impl_trait.ty(db)).instantiate(db, key.subst(db).generic_args(db));
                coordinates.push(SourceStateCoordinate {
                    ty: self_ty,
                    derived_aggregate: true,
                });
            }
        }
        BodyOwner::Closure { ty, .. } => coordinates.push(SourceStateCoordinate {
            ty: TyId::closure(db, ty),
            derived_aggregate: true,
        }),
        BodyOwner::Const(_)
        | BodyOwner::AnonConstBody { .. }
        | BodyOwner::ContractInit { .. }
        | BodyOwner::ContractRecvArm { .. } => {}
    }
    coordinates
}

fn source_state_coordinates<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> Vec<(usize, SourceStateCoordinate<'db>)> {
    let expandable = TyFlags::HAS_PARAM | TyFlags::HAS_VAR | TyFlags::HAS_PROJECTION;
    semantic_instance_state_coordinate_shapes(db, key)
        .into_iter()
        .enumerate()
        .filter(|(_, coordinate)| coordinate.ty.flags(db).intersects(expandable))
        .collect()
}

fn source_owner_has_parametric_state<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> bool {
    !source_state_coordinates(db, identity_semantic_instance_key(db, owner)).is_empty()
}

fn ty_properly_contains<'db>(
    db: &'db dyn HirAnalysisDb,
    haystack: TyId<'db>,
    needle: TyId<'db>,
) -> bool {
    if haystack == needle {
        return false;
    }
    struct Contains<'db> {
        db: &'db dyn HirAnalysisDb,
        needle: TyId<'db>,
        found: bool,
        visited: FxHashSet<TyId<'db>>,
    }
    impl<'db> TyVisitor<'db> for Contains<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.found || !self.visited.insert(ty) {
                return;
            }
            if ty == self.needle {
                self.found = true;
                return;
            }
            walk_ty(self, ty);
        }
    }
    let mut contains = Contains {
        db,
        needle,
        found: false,
        visited: FxHashSet::default(),
    };
    walk_ty(&mut contains, haystack);
    contains.found
}

fn ty_is_unconstrained_const_inference_var<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    // Omitted layout-root arguments intentionally remain as bare const
    // inference variables until layout evidence binds them. They carry no
    // caller lineage, so unlike an abstract expression or projection they
    // cannot prove structural growth around a caller coordinate.
    matches!(
        ty.data(db),
        TyData::ConstTy(const_ty)
            if matches!(const_ty.data(db), ConstTyData::TyVar(..))
    )
}

fn unresolved_ty_is_covered_by<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    covered: &FxHashSet<TyId<'db>>,
) -> bool {
    struct Coverage<'db, 'a> {
        db: &'db dyn HirAnalysisDb,
        covered: &'a FxHashSet<TyId<'db>>,
        visited: FxHashSet<TyId<'db>>,
        uncovered: bool,
    }
    impl<'db> TyVisitor<'db> for Coverage<'db, '_> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.uncovered || !self.visited.insert(ty) || self.covered.contains(&ty) {
                return;
            }
            walk_ty(self, ty);
        }

        fn visit_var(&mut self, _var: &crate::analysis::ty::ty_def::TyVar<'db>) {
            self.uncovered = true;
        }

        fn visit_param(&mut self, _param: &crate::analysis::ty::ty_def::TyParam<'db>) {
            self.uncovered = true;
        }

        fn visit_const_param(
            &mut self,
            _param: &crate::analysis::ty::ty_def::TyParam<'db>,
            _const_ty_ty: TyId<'db>,
        ) {
            self.uncovered = true;
        }
    }
    let mut coverage = Coverage {
        db,
        covered,
        visited: FxHashSet::default(),
        uncovered: false,
    };
    coverage.visit_ty(ty);
    !coverage.uncovered
}

#[derive(Debug, Clone, Copy)]
struct SourceCoordinateFlow {
    source: usize,
    target: usize,
    growing: bool,
}

struct SourceEdgeFlowAnalysis {
    flows: Vec<SourceCoordinateFlow>,
    structurally_unknown: bool,
}

fn source_edge_coordinate_flows<'db>(
    db: &'db dyn HirAnalysisDb,
    caller_key: SemanticInstanceKey<'db>,
    target_key: SemanticInstanceKey<'db>,
    actual_target_key: SemanticInstanceKey<'db>,
    flow_kind: SourceCallGraphEdgeFlow,
) -> SourceEdgeFlowAnalysis {
    let caller = source_state_coordinates(db, caller_key);
    let target = source_state_coordinates(db, target_key);
    if caller.is_empty() || target.is_empty() {
        return SourceEdgeFlowAnalysis {
            flows: Vec::new(),
            structurally_unknown: matches!(flow_kind, SourceCallGraphEdgeFlow::UnknownGrowth),
        };
    }

    let all_to_all = |growing: bool| {
        caller
            .iter()
            .enumerate()
            .flat_map(|(source, _)| {
                target
                    .iter()
                    .enumerate()
                    .map(move |(target, _)| SourceCoordinateFlow {
                        source,
                        target,
                        growing,
                    })
            })
            .collect::<Vec<_>>()
    };
    let aggregate_carry_impl_params = match flow_kind {
        SourceCallGraphEdgeFlow::TraitAggregateCarry { impl_param_count } => Some(impl_param_count),
        SourceCallGraphEdgeFlow::Classify
        | SourceCallGraphEdgeFlow::UnknownGrowth
        | SourceCallGraphEdgeFlow::ClosureContainment => None,
    };
    let mut flows = Vec::new();
    match flow_kind {
        SourceCallGraphEdgeFlow::UnknownGrowth => {
            return SourceEdgeFlowAnalysis {
                flows: all_to_all(true),
                structurally_unknown: false,
            };
        }
        SourceCallGraphEdgeFlow::TraitAggregateCarry { .. } => {
            let derived_targets = target
                .iter()
                .enumerate()
                .filter(|(_, (_, coordinate))| coordinate.derived_aggregate)
                .map(|(target, _)| target)
                .collect::<Vec<_>>();
            flows.extend(
                caller
                    .iter()
                    .enumerate()
                    .flat_map(|(source, _)| {
                        derived_targets
                            .iter()
                            .copied()
                            .map(move |target| SourceCoordinateFlow {
                                source,
                                target,
                                growing: false,
                            })
                    })
                    .collect::<Vec<_>>(),
            );
        }
        SourceCallGraphEdgeFlow::Classify | SourceCallGraphEdgeFlow::ClosureContainment => {}
    }

    let actual = semantic_instance_state_coordinate_shapes(db, actual_target_key);
    let canonical = semantic_instance_state_coordinate_shapes(db, target_key);
    if actual.len() != canonical.len() {
        return SourceEdgeFlowAnalysis {
            flows: Vec::new(),
            structurally_unknown: true,
        };
    }

    let force_carry = matches!(flow_kind, SourceCallGraphEdgeFlow::ClosureContainment);
    let mut structurally_unknown = false;
    for (target_idx, (slot, target_coordinate)) in target.iter().enumerate() {
        if aggregate_carry_impl_params.is_some() && target_coordinate.derived_aggregate {
            continue;
        }
        if aggregate_carry_impl_params.is_some_and(|count| *slot < count) {
            continue;
        }
        let actual_ty = actual[*slot].ty;
        let exact_sources = caller
            .iter()
            .enumerate()
            .filter(|(_, (_, source_coordinate))| source_coordinate.ty == actual_ty)
            .map(|(source_idx, _)| source_idx)
            .collect::<Vec<_>>();
        if !exact_sources.is_empty() {
            flows.extend(
                exact_sources
                    .into_iter()
                    .map(|source| SourceCoordinateFlow {
                        source,
                        target: target_idx,
                        growing: false,
                    }),
            );
            continue;
        }

        let containing_sources = caller
            .iter()
            .enumerate()
            .filter(|(_, (_, source_coordinate))| {
                ty_properly_contains(db, actual_ty, source_coordinate.ty)
            })
            .map(|(source_idx, _)| source_idx)
            .collect::<Vec<_>>();
        if !containing_sources.is_empty() {
            flows.extend(
                containing_sources
                    .into_iter()
                    .map(|source| SourceCoordinateFlow {
                        source,
                        target: target_idx,
                        growing: !force_carry && !target_coordinate.derived_aggregate,
                    }),
            );
            continue;
        }

        let is_proven_descent = caller.iter().any(|(_, source_coordinate)| {
            ty_properly_contains(db, source_coordinate.ty, actual_ty)
        });
        let unresolved = TyFlags::HAS_PARAM | TyFlags::HAS_VAR | TyFlags::HAS_PROJECTION;
        if !is_proven_descent
            && actual_ty.flags(db).intersects(unresolved)
            && !ty_is_unconstrained_const_inference_var(db, actual_ty)
        {
            structurally_unknown = true;
        }
    }
    if !matches!(actual_target_key.owner(db), BodyOwner::Closure { .. }) {
        let primary_len = semantic_instance_primary_state_tys(db, actual_target_key).len();
        for auxiliary_ty in semantic_instance_state_tys(db, actual_target_key)
            .into_iter()
            .skip(primary_len)
        {
            // Inherited assumptions often contain a fixed projection such as
            // `A::Encoder`. It is not an independent specialization axis when
            // every caller lineage it mentions is also carried unchanged by
            // a primary callee argument; any change to that state is already
            // represented by the primary coordinate. Keep the conservative
            // fallback if the auxiliary state also mentions a lineage dropped
            // or transformed at this edge.
            let exactly_carried_lineages = caller
                .iter()
                .filter_map(|(_, source_coordinate)| {
                    actual
                        .iter()
                        .any(|coordinate| coordinate.ty == source_coordinate.ty)
                        .then_some(source_coordinate.ty)
                })
                .collect::<FxHashSet<_>>();
            // A projection over a concrete receiver (for example
            // `Sol::Encoder`) is fixed even though normalization has not
            // exposed its result yet. A projection over caller parameters is
            // likewise derived state when every unresolved leaf is already an
            // exactly-carried primary lineage. `unresolved_ty_is_covered_by`
            // deliberately accepts the empty set only when the auxiliary type
            // has no parameter or inference-variable leaves.
            let caller_independent_or_derived =
                unresolved_ty_is_covered_by(db, auxiliary_ty, &exactly_carried_lineages);
            if caller
                .iter()
                .any(|(_, source_coordinate)| source_coordinate.ty == auxiliary_ty)
                || !auxiliary_ty
                    .flags(db)
                    .intersects(TyFlags::HAS_PARAM | TyFlags::HAS_VAR | TyFlags::HAS_PROJECTION)
                || ty_is_unconstrained_const_inference_var(db, auxiliary_ty)
                || caller.iter().any(|(_, source_coordinate)| {
                    ty_properly_contains(db, source_coordinate.ty, auxiliary_ty)
                })
                || caller_independent_or_derived
            {
                continue;
            }
            // Auxiliary provider/evidence state is not a formal source
            // coordinate of the target node. If it constructs around a caller
            // lineage (or a projection hides that relation), retain the old
            // conservative rejection rather than dropping a possible
            // specialization axis. Closure auxiliary state is derived entirely
            // from the parent arguments represented above.
            structurally_unknown = true;
        }
    }
    SourceEdgeFlowAnalysis {
        flows,
        structurally_unknown,
    }
}

fn source_dispatch_ingot_for_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> Ingot<'db> {
    let impl_env = key.impl_env(db);
    let witnesses = impl_env.witnesses(db);
    witnesses
        .iter()
        .rev()
        .find_map(|witness| witness.self_ty(db).ingot(db))
        .unwrap_or_else(|| {
            if witnesses.is_empty() {
                // Free generic helpers are analyzed in their defining ingot.
                // An application-local explicit implementation is guarded by
                // its own owner query. Resolved/inherited trait bodies carry a
                // witness and retain the caller's dispatch context below.
                key.owner(db).scope().ingot(db)
            } else {
                impl_env.normalization_scope(db).ingot(db)
            }
        })
}

#[salsa::tracked(return_ref)]
fn non_regular_recursive_calls_from_owner<'db>(
    db: &'db dyn HirAnalysisDb,
    root_key: SemanticInstanceKey<'db>,
    dispatch_ingot: Ingot<'db>,
) -> NonRegularRecursiveCallGraph<'db> {
    if let BodyOwner::Func(func) = root_key.owner(db)
        && !func_may_participate_in_source_call_cycle(db, func, dispatch_ingot)
    {
        return NonRegularRecursiveCallGraph {
            calls: Vec::new(),
            blocked_owners: Vec::new(),
            component_diagnostic_calls: Vec::new(),
        };
    }
    let nodes = build_source_call_graph(db, root_key, dispatch_ingot);
    let source_components = source_call_graph_components(&nodes);
    let coordinates = nodes
        .iter()
        .map(|node| source_state_coordinates(db, node.key))
        .collect::<Vec<_>>();
    let mut coordinate_offsets = Vec::with_capacity(nodes.len() + 1);
    coordinate_offsets.push(0);
    for node_coordinates in &coordinates {
        coordinate_offsets
            .push(coordinate_offsets.last().copied().unwrap_or_default() + node_coordinates.len());
    }
    let mut coordinate_adjacency =
        vec![Vec::new(); coordinate_offsets.last().copied().unwrap_or_default()];
    let mut flow_records = Vec::new();
    let mut calls = IndexSet::new();
    let mut diagnostic_call_by_source_component = FxHashMap::default();
    let mut conservatively_invalid_source_components = FxHashSet::default();
    for (source, node) in nodes.iter().enumerate() {
        for (edge_idx, edge) in node.edges.iter().enumerate() {
            let analysis = source_edge_coordinate_flows(
                db,
                node.key,
                nodes[edge.target].key,
                edge.callee_key,
                edge.flow,
            );
            if analysis.structurally_unknown
                && source_components[source] == source_components[edge.target]
            {
                conservatively_invalid_source_components.insert(source_components[source]);
                if let Some(call_site) = edge.call_site {
                    let call = NonRegularRecursiveCallSite {
                        owner: node.key.owner(db),
                        call_site,
                        callee: edge.callee_key.owner(db),
                    };
                    calls.insert(call);
                    diagnostic_call_by_source_component
                        .entry(source_components[source])
                        .or_insert(call);
                }
            }
            for flow in analysis.flows {
                let source_coordinate = coordinate_offsets[source] + flow.source;
                let target_coordinate = coordinate_offsets[edge.target] + flow.target;
                coordinate_adjacency[source_coordinate].push(target_coordinate);
                flow_records.push((
                    source_coordinate,
                    target_coordinate,
                    flow.growing,
                    source,
                    edge_idx,
                ));
            }
        }
    }
    let coordinate_components = adjacency_graph_components(&coordinate_adjacency);
    let mut invalid_coordinate_components = FxHashSet::default();
    for (source_coordinate, target_coordinate, growing, source, edge_idx) in flow_records {
        if !growing
            || coordinate_components[source_coordinate] != coordinate_components[target_coordinate]
        {
            continue;
        }
        invalid_coordinate_components.insert(coordinate_components[source_coordinate]);
        let edge = nodes[source].edges[edge_idx];
        if let Some(call_site) = edge.call_site {
            let call = NonRegularRecursiveCallSite {
                owner: nodes[source].key.owner(db),
                call_site,
                callee: edge.callee_key.owner(db),
            };
            calls.insert(call);
            diagnostic_call_by_source_component
                .entry(source_components[source])
                .or_insert(call);
        }
    }
    let mut blocked_owners = Vec::new();
    let mut component_diagnostic_calls = IndexSet::new();
    for (node_idx, node) in nodes.iter().enumerate() {
        let blocked = conservatively_invalid_source_components
            .contains(&source_components[node_idx])
            || (coordinate_offsets[node_idx]..coordinate_offsets[node_idx + 1]).any(|coordinate| {
                invalid_coordinate_components.contains(&coordinate_components[coordinate])
            });
        if !blocked {
            continue;
        }
        let owner = node.key.owner(db);
        blocked_owners.push(owner);
        if let Some(call) = diagnostic_call_by_source_component.get(&source_components[node_idx]) {
            component_diagnostic_calls.insert((owner, *call));
        }
    }
    NonRegularRecursiveCallGraph {
        calls: calls.into_iter().collect(),
        blocked_owners,
        component_diagnostic_calls: component_diagnostic_calls.into_iter().collect(),
    }
}

fn non_regular_recursive_calls_for_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> &'db NonRegularRecursiveCallGraph<'db> {
    let owner = key.owner(db);
    non_regular_recursive_calls_from_owner(
        db,
        identity_semantic_instance_key(db, owner),
        source_dispatch_ingot_for_key(db, key),
    )
}

fn non_regular_recursive_call_sites_for_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> Vec<NonRegularRecursiveCallSite<'db>> {
    let owner = key.owner(db);
    if !source_owner_has_parametric_state(db, owner) {
        return Vec::new();
    }
    non_regular_recursive_calls_for_key(db, key)
        .calls
        .iter()
        .copied()
        .filter(|call| same_syntactic_callable_owner(call.owner, owner))
        .collect()
}

pub fn non_regular_recursive_call_sites<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Vec<NonRegularRecursiveCallSite<'db>> {
    non_regular_recursive_call_sites_for_key(db, identity_semantic_instance_key(db, owner))
}

fn key_is_in_non_regular_recursive_component<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> bool {
    let owner = key.owner(db);
    if !source_owner_has_parametric_state(db, owner) {
        return false;
    }
    non_regular_recursive_calls_for_key(db, key)
        .blocked_owners
        .iter()
        .copied()
        .any(|blocked| same_syntactic_callable_owner(blocked, owner))
}

pub fn semantic_instance_is_in_non_regular_recursive_component<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> bool {
    key_is_in_non_regular_recursive_component(db, instance.key(db))
}

pub fn non_regular_recursive_call_diagnostic<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Option<BorrowDiagnosticId<'db>> {
    let owner = instance.key(db).owner(db);
    if !source_owner_has_parametric_state(db, owner) {
        return None;
    }
    let contained_closures = source_contained_closure_keys(db, instance.key(db))
        .into_iter()
        .map(|key| SyntacticBodyOwner::from(key.owner(db)))
        .collect::<FxHashSet<_>>();
    let graph = non_regular_recursive_calls_for_key(db, instance.key(db));
    let call = graph
        .calls
        .iter()
        .copied()
        .find(|call| {
            same_syntactic_callable_owner(call.owner, owner)
                || contained_closures.contains(&SyntacticBodyOwner::from(call.owner))
        })
        .or_else(|| {
            graph
                .component_diagnostic_calls
                .iter()
                .find(|(blocked_owner, _)| same_syntactic_callable_owner(*blocked_owner, owner))
                .map(|(_, call)| *call)
        })?;
    let diagnostic_instance = if same_syntactic_callable_owner(call.owner, owner) {
        instance
    } else {
        get_or_build_semantic_instance(db, identity_semantic_instance_key(db, call.owner))
    };
    let diagnostic_owner = diagnostic_instance.key(db).owner(db);
    let origin = match call.call_site {
        CallSiteId::Expr(expr) => crate::analysis::semantic::SemOrigin::Expr(expr),
        CallSiteId::ForLoopLen(stmt) | CallSiteId::ForLoopGet(stmt) => {
            crate::analysis::semantic::SemOrigin::Stmt(stmt)
        }
    };
    Some(BorrowDiagnosticId::new(
        db,
        SemanticBorrowDiagnostic {
            kind: SemanticBorrowDiagKind::NonRegularPolymorphicRecursion,
            instance: diagnostic_instance,
            primary: SemanticBorrowDiagnosticLabel {
                message: "this recursive call nests a caller generic parameter inside a different type or constant, which would create an unbounded family of specialized functions".to_string(),
                span: SemanticBorrowDiagnosticSpan::Origin {
                    owner: diagnostic_owner,
                    origin,
                },
            },
            secondaries: Vec::new(),
        },
    ))
}

fn collect_semantic_callees<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Vec<SemanticCalleeRef<'db>> {
    let blocked_sites = non_regular_recursive_call_sites_for_key(db, instance.key(db))
        .into_iter()
        .map(|call| call.call_site)
        .collect::<FxHashSet<_>>();
    let mut seen = FxHashSet::default();
    let mut callees = Vec::new();
    for (expr_idx, site) in instance.call_sites(db).iter().enumerate() {
        if blocked_sites.contains(&CallSiteId::Expr(ExprId::new(expr_idx))) {
            continue;
        }
        let Some(site) = site else {
            continue;
        };
        if let Some(callee) = site.callee
            && seen.insert(callee.key)
        {
            callees.push(callee);
        }
    }
    for (stmt_idx, sites) in instance.for_loop_call_sites(db).iter().enumerate() {
        let Some(sites) = sites else {
            continue;
        };
        let stmt = crate::hir_def::StmtId::new(stmt_idx);
        if let Some(callee) = sites.len.callee
            && !blocked_sites.contains(&CallSiteId::ForLoopLen(stmt))
            && seen.insert(callee.key)
        {
            callees.push(callee);
        }
        if let Some(callee) = sites.get.callee
            && !blocked_sites.contains(&CallSiteId::ForLoopGet(stmt))
            && seen.insert(callee.key)
        {
            callees.push(callee);
        }
    }
    callees
}

fn classify_binding_role<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    ty: TyId<'db>,
    assumptions: PredicateListId<'db>,
    provider: Option<ProviderBinding<'db>>,
) -> SemanticLocalRole<'db> {
    let owner = instance.key(db).owner(db);
    let scope = owner.scope();
    let ty = normalize_ty(db, ty, scope, assumptions);
    if let Some((_, value_ty)) = ty.as_capability(db) {
        let value_ty = normalize_ty(db, value_ty, scope, assumptions);
        return SemanticLocalRole::PlaceCarrier { provider, value_ty };
    }
    let type_semantics = provider_semantics(db, scope, assumptions, ty);
    if matches!(
        type_semantics.evidence,
        ProviderLayoutEvidence::ResolvedHandle(_)
    ) && let Some(target_ty) = type_semantics.target_ty
    {
        return SemanticLocalRole::DirectCarrier {
            provider,
            target_ty,
        };
    }
    if let Some(provider) = provider {
        return match provider.semantics.kind {
            ProviderKind::RootObject => SemanticLocalRole::DirectValue {
                provenance: ValueProvenance::RootProvider(provider),
            },
            ProviderKind::Handle | ProviderKind::RawAddress => SemanticLocalRole::PlaceBoundValue {
                provenance: PlaceProvenance::RootProvider(provider),
                value_ty: ty,
            },
            ProviderKind::InvalidHandle => SemanticLocalRole::Erased,
        };
    }
    SemanticLocalRole::DirectValue {
        provenance: ValueProvenance::Ordinary,
    }
}

pub fn validate_instantiated_effect_env_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> Result<(), SemanticEffectEnvInstantiationError<'db>> {
    instantiate_effect_env_data_for_key(db, key).map(|_| ())
}

fn instantiate_effect_env_data_for_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> Result<Option<InstantiatedEffectEnvData<'db>>, SemanticEffectEnvInstantiationError<'db>> {
    let owner = key.owner(db);
    let Some(site) = effect_param_site(owner) else {
        return Ok(None);
    };
    let base_assumptions = semantic_instance_base_assumptions_for_key(db, key);
    let view = EffectEnvView::new(site);
    let requirements = view
        .requirements(db)
        .into_iter()
        .map(|requirement| instantiate_effect_requirement(db, key, requirement))
        .collect::<Result<Vec<_>, _>>()?;
    let resolutions = view.resolutions(db);
    let providers =
        instantiate_provider_bindings_for_key(db, key, site, view.providers(db), &resolutions)?;
    let forwarded_witnesses =
        instantiated_effect_env_forwarded_witnesses(db, &requirements, &providers, &resolutions);
    let assumptions = if forwarded_witnesses.is_empty() {
        base_assumptions
    } else {
        let mut predicates: IndexSet<_> = base_assumptions.list(db).iter().copied().collect();
        predicates.extend(forwarded_witnesses.iter().copied());
        PredicateListId::new(db, predicates.into_iter().collect::<Vec<_>>()).extend_all_bounds(db)
    };
    Ok(Some((
        site,
        requirements,
        providers,
        resolutions,
        forwarded_witnesses,
        assumptions,
    )))
}

fn instantiate_provider_bindings_for_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    site: crate::analysis::ty::ty_check::EffectParamSite<'db>,
    canonical: Vec<ProviderBinding<'db>>,
    resolutions: &[ResolvedEffectBinding],
) -> Result<Vec<ProviderBinding<'db>>, SemanticEffectEnvInstantiationError<'db>> {
    let mut specializations = FxHashMap::default();
    for specialization in key.effect_providers(db).providers(db) {
        specializations.insert(
            specialization.provider.provider_idx,
            instantiate_provider_binding(db, key, specialization.provider.clone())?,
        );
    }
    if matches!(
        site,
        crate::analysis::ty::ty_check::EffectParamSite::Func(_)
    ) && !specializations.is_empty()
    {
        for resolution in resolutions {
            assert!(
                specializations.contains_key(&resolution.provider_idx),
                "missing call-site provider specialization for function effect provider slot {} in {:?}",
                resolution.provider_idx,
                key.owner(db),
            );
        }
    }
    canonical
        .into_iter()
        .map(|provider| {
            specializations
                .get(&provider.provider_idx)
                .cloned()
                .map(Ok)
                .unwrap_or_else(|| instantiate_provider_binding(db, key, provider))
        })
        .collect()
}

pub(crate) fn semantic_instance_base_assumptions_for_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> PredicateListId<'db> {
    let typed_body = key.typed_body(db);
    let impl_env = key.impl_env(db);
    let mut predicates: IndexSet<_> = typed_body.assumptions().list(db).iter().copied().collect();
    predicates.extend(impl_env.assumptions(db).list(db).iter().copied());
    predicates.extend(impl_env.witnesses(db).iter().copied());
    PredicateListId::new(db, predicates.into_iter().collect::<Vec<_>>()).extend_all_bounds(db)
}

fn instantiated_effect_env_forwarded_witnesses<'db>(
    db: &'db dyn HirAnalysisDb,
    requirements: &[EffectRequirement<'db>],
    providers: &[ProviderBinding<'db>],
    resolutions: &[ResolvedEffectBinding],
) -> Vec<crate::analysis::ty::trait_def::TraitInstId<'db>> {
    let provider_by_idx = providers
        .iter()
        .map(|provider| (provider.provider_idx, provider.provider_ty))
        .collect::<IndexMap<_, _>>();
    let resolution_by_req = resolutions
        .iter()
        .map(|resolution| (resolution.requirement_idx, resolution.provider_idx))
        .collect::<IndexMap<_, _>>();
    let mut witnesses = IndexSet::new();
    for requirement in requirements {
        let Some(trait_inst) = requirement.key.key_trait() else {
            continue;
        };
        let witness = resolution_by_req
            .get(&requirement.binding_idx)
            .and_then(|provider_idx| provider_by_idx.get(provider_idx))
            .copied()
            .map_or(trait_inst, |provider_ty| {
                instantiate_trait_self(db, trait_inst, provider_ty)
            });
        witnesses.insert(witness);
    }
    witnesses.into_iter().collect()
}

fn root_owner_generic_args<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Result<Vec<TyId<'db>>, RootSemanticInstanceError<'db>> {
    match owner {
        BodyOwner::Func(func) => root_func_generic_args(db, func),
        BodyOwner::Const(_)
        | BodyOwner::AnonConstBody { .. }
        | BodyOwner::ContractInit { .. }
        | BodyOwner::ContractRecvArm { .. }
        | BodyOwner::Closure { .. } => Ok(Vec::new()),
    }
}

fn owner_identity_generic_args<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Vec<TyId<'db>> {
    match owner {
        BodyOwner::Func(func) => CallableDef::Func(func).params(db).to_vec(),
        BodyOwner::Closure { ty, .. } => ty.parent_args(db).clone(),
        BodyOwner::Const(_)
        | BodyOwner::AnonConstBody { .. }
        | BodyOwner::ContractInit { .. }
        | BodyOwner::ContractRecvArm { .. } => Vec::new(),
    }
}

fn root_owner_effect_providers<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Vec<EffectProviderSpecialization<'db>> {
    let BodyOwner::Func(func) = owner else {
        return Vec::new();
    };
    let site = effect_param_site(owner).expect("function owners should always have an effect site");
    let view = EffectEnvView::new(site);
    let assumptions =
        crate::analysis::ty::trait_resolution::constraint::collect_func_decl_constraints(
            db,
            func.into(),
            true,
        )
        .instantiate_identity();
    let providers = view.providers(db);
    let root_provider = providers.iter().find(|provider| {
        matches!(
            provider.source,
            ProviderSource::RootProvider {
                site: provider_site,
                ..
            } if provider_site == site
        )
    });
    let provider_slots = providers
        .iter()
        .filter_map(|provider| match provider.source {
            ProviderSource::UsesParam {
                site: provider_site,
                requirement_idx,
            } if provider_site == site => Some((requirement_idx, provider.clone())),
            ProviderSource::UsesParam { .. }
            | ProviderSource::ContractField { .. }
            | ProviderSource::RootProvider { .. } => None,
        })
        .collect::<FxHashMap<_, _>>();
    view.requirements(db)
        .into_iter()
        .filter_map(|requirement| {
            let slot = provider_slots.get(&requirement.binding_idx)?;
            let (provider_ty, source, target_ty) = if let Some(root_provider) = root_provider
                .filter(|provider| {
                    root_provider_satisfies_effect_requirement(
                        db,
                        func,
                        assumptions,
                        provider,
                        &requirement,
                    )
                }) {
                (
                    root_provider.provider_ty,
                    root_provider.source.clone(),
                    specialized_root_provider_target_ty(
                        db,
                        func.scope(),
                        assumptions,
                        &requirement,
                        root_provider,
                    ),
                )
            } else {
                let target_ty =
                    requirement_provider_target_ty(db, func.scope(), assumptions, &requirement)?;
                let provider_ty = if requirement.is_mut {
                    TyId::borrow_mut_of(db, target_ty)
                } else {
                    TyId::borrow_ref_of(db, target_ty)
                };
                (provider_ty, slot.source.clone(), Some(target_ty))
            };
            let provider = ProviderBinding {
                provider_idx: slot.provider_idx,
                provider_ty,
                is_mut: slot.is_mut,
                source,
                semantics: provider_semantics_for_specialized_call(
                    db,
                    func.scope(),
                    assumptions,
                    provider_ty,
                    target_ty,
                    Some(ProviderAddressSpace::Memory),
                    ProviderTransport::ByValue,
                ),
                layout_env: None,
            };
            Some(EffectProviderSpecialization {
                provider,
                provenance: EffectProviderProvenance::Binding {
                    owner,
                    binding: LocalBinding::EffectParam {
                        site,
                        idx: requirement.binding_idx as usize,
                        binding_name: requirement.binding_name,
                        provider_idx: slot.provider_idx,
                        key_path: requirement.binding_path,
                        is_mut: requirement.is_mut,
                    },
                },
            })
        })
        .collect()
}

fn root_provider_satisfies_effect_requirement<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    assumptions: PredicateListId<'db>,
    root_provider: &ProviderBinding<'db>,
    requirement: &EffectRequirement<'db>,
) -> bool {
    match requirement.key {
        EffectRequirementKey::Type(provider_ty) => {
            provider_ty == root_provider.provider_ty
                || matches!(
                    provider_semantics(db, func.scope(), assumptions, provider_ty).evidence,
                    ProviderLayoutEvidence::ResolvedHandle(_)
                )
        }
        EffectRequirementKey::Trait(trait_inst) => {
            let goal = instantiate_trait_self(db, trait_inst, root_provider.provider_ty);
            matches!(
                is_goal_satisfiable(
                    db,
                    TraitSolveCx::new(db, func.scope()).with_assumptions(assumptions),
                    goal,
                ),
                GoalSatisfiability::Satisfied(_) | GoalSatisfiability::NeedsConfirmation { .. }
            )
        }
        EffectRequirementKey::Other => false,
    }
}

fn root_func_generic_args<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
) -> Result<Vec<TyId<'db>>, RootSemanticInstanceError<'db>> {
    let owner = BodyOwner::Func(func);
    let owner_scope = func.scope();
    let provider_param_idxs = place_effect_provider_param_index_map(db, func)
        .iter()
        .flatten()
        .copied()
        .collect::<FxHashSet<_>>();
    let params = CallableDef::Func(func).params(db);
    if provider_param_idxs.is_empty() {
        if let Some((param_idx, &offending_ty)) = params.iter().enumerate().next() {
            return Err(RootSemanticInstanceError::UnsupportedGenericParam {
                owner,
                owner_scope,
                offending_ty,
                param_idx,
            });
        }
        return Ok(Vec::new());
    }
    let site = effect_param_site(owner).expect("function owners should always have an effect site");
    let provider_ty_by_idx = root_owner_effect_providers(db, owner)
        .into_iter()
        .map(|provider| {
            (
                provider.provider.provider_idx,
                provider.provider.provider_ty,
            )
        })
        .collect::<FxHashMap<_, _>>();
    let resolved_provider_by_effect = EffectEnvView::new(site)
        .resolutions(db)
        .into_iter()
        .map(|resolution| (resolution.requirement_idx as usize, resolution.provider_idx))
        .collect::<FxHashMap<_, _>>();
    let provider_param_by_effect = place_effect_provider_param_index_map(db, func);
    let effect_idx_by_param = provider_param_by_effect
        .iter()
        .enumerate()
        .filter_map(|(effect_idx, param_idx)| param_idx.map(|param_idx| (param_idx, effect_idx)))
        .collect::<FxHashMap<_, _>>();
    for (param_idx, &param_ty) in params.iter().enumerate() {
        let is_effect_provider = matches!(
            param_ty.data(db),
            crate::analysis::ty::ty_def::TyData::TyParam(param)
                if param.owner == owner_scope && param.is_effect_provider() && provider_param_idxs.contains(&param_idx)
        );
        if !is_effect_provider {
            return Err(RootSemanticInstanceError::UnsupportedGenericParam {
                owner,
                owner_scope,
                offending_ty: param_ty,
                param_idx,
            });
        }
    }
    params
        .iter()
        .enumerate()
        .map(|(param_idx, _)| {
            let effect_idx = effect_idx_by_param
                .get(&param_idx)
                .copied()
                .ok_or(RootSemanticInstanceError::MissingRootProvider { owner })?;
            let provider_idx = resolved_provider_by_effect
                .get(&effect_idx)
                .copied()
                .ok_or(RootSemanticInstanceError::MissingRootProvider { owner })?;
            provider_ty_by_idx
                .get(&provider_idx)
                .copied()
                .ok_or(RootSemanticInstanceError::MissingRootProvider { owner })
        })
        .collect()
}

fn instantiate_effect_requirement<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    requirement: EffectRequirement<'db>,
) -> Result<EffectRequirement<'db>, SemanticEffectEnvInstantiationError<'db>> {
    Ok(EffectRequirement {
        key: instantiate_effect_requirement_key(db, key, requirement.key.clone())?,
        ..requirement
    })
}

fn instantiate_effect_requirement_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    requirement_key: EffectRequirementKey<'db>,
) -> Result<EffectRequirementKey<'db>, SemanticEffectEnvInstantiationError<'db>> {
    Ok(match requirement_key {
        EffectRequirementKey::Type(ty) => {
            EffectRequirementKey::Type(instantiate_normalized_ty(db, key, ty)?)
        }
        EffectRequirementKey::Trait(trait_inst) => {
            EffectRequirementKey::Trait(instantiate_normalized_trait_inst(db, key, trait_inst)?)
        }
        EffectRequirementKey::Other => EffectRequirementKey::Other,
    })
}

fn instantiate_provider_binding<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    provider: ProviderBinding<'db>,
) -> Result<ProviderBinding<'db>, SemanticEffectEnvInstantiationError<'db>> {
    let scope = key.owner(db).scope();
    let assumptions = semantic_instance_base_assumptions_for_key(db, key);
    let provider_ty = instantiate_normalized_ty(db, key, provider.provider_ty)?;
    let source = match provider.source.clone() {
        ProviderSource::RootProvider { site, registration } => ProviderSource::RootProvider {
            site,
            registration: crate::analysis::ty::provider::RootProviderRegistration {
                provider_ty: instantiate_normalized_ty(db, key, registration.provider_ty)?,
                ..registration
            },
        },
        source => source,
    };
    let target_ty = provider
        .semantics
        .target_ty
        .map(|ty| instantiate_normalized_ty(db, key, ty))
        .transpose()?;
    let semantics = if matches!(
        provider.semantics.evidence,
        ProviderLayoutEvidence::ContractField
    ) {
        crate::analysis::ty::provider::ProviderSemantics {
            provider_ty,
            kind: target_ty.map_or(provider.semantics.kind, |target| {
                if target.is_struct(db)
                    || target.is_array(db)
                    || target.is_tuple(db)
                    || target.as_enum(db).is_some()
                {
                    ProviderKind::Handle
                } else {
                    ProviderKind::RawAddress
                }
            }),
            address_space: provider.semantics.address_space,
            target_ty,
            transport: provider.semantics.transport,
            evidence: ProviderLayoutEvidence::ContractField,
        }
    } else {
        provider_semantics_for_specialized_call(
            db,
            scope,
            assumptions,
            provider_ty,
            target_ty,
            provider.semantics.address_space,
            provider.semantics.transport,
        )
    };
    Ok(ProviderBinding {
        provider_ty,
        source,
        semantics,
        ..provider
    })
}

fn instantiate_normalized_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    ty: TyId<'db>,
) -> Result<TyId<'db>, SemanticEffectEnvInstantiationError<'db>> {
    let scope = key.owner(db).scope();
    let assumptions = semantic_instance_base_assumptions_for_key(db, key);
    let ty = instantiate_checked(db, key.owner(db), scope, ty, key.subst(db).generic_args(db))?;
    Ok(normalize_ty(db, ty, scope, assumptions))
}

fn instantiate_normalized_trait_inst<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    trait_inst: crate::analysis::ty::trait_def::TraitInstId<'db>,
) -> Result<
    crate::analysis::ty::trait_def::TraitInstId<'db>,
    SemanticEffectEnvInstantiationError<'db>,
> {
    let scope = key.owner(db).scope();
    let assumptions = semantic_instance_base_assumptions_for_key(db, key);
    let trait_inst = instantiate_checked(
        db,
        key.owner(db),
        scope,
        trait_inst,
        key.subst(db).generic_args(db),
    )?;
    let args = trait_inst
        .args(db)
        .iter()
        .map(|&arg| normalize_ty(db, arg, scope, assumptions))
        .collect::<Vec<_>>();
    let assoc_type_bindings = trait_inst
        .assoc_type_bindings(db)
        .iter()
        .map(|(&name, &ty)| (name, normalize_ty(db, ty, scope, assumptions)))
        .collect::<IndexMap<_, _>>();
    Ok(crate::analysis::ty::trait_def::TraitInstId::new(
        db,
        trait_inst.def(db),
        args,
        assoc_type_bindings,
    ))
}

fn instantiate_checked<'db, T>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    owner_scope: ScopeId<'db>,
    value: T,
    args: &[TyId<'db>],
) -> Result<T, SemanticEffectEnvInstantiationError<'db>>
where
    T: crate::analysis::ty::fold::TyFoldable<'db>,
{
    let mut folder = CheckedInstantiateFolder {
        owner,
        owner_scope,
        args,
        error: None,
    };
    let value = value.fold_with(db, &mut folder);
    folder.error.map_or(Ok(value), Err)
}

struct CheckedInstantiateFolder<'db, 'a> {
    owner: BodyOwner<'db>,
    owner_scope: ScopeId<'db>,
    args: &'a [TyId<'db>],
    error: Option<SemanticEffectEnvInstantiationError<'db>>,
}

impl<'db> crate::analysis::ty::fold::TyFolder<'db> for CheckedInstantiateFolder<'db, '_> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(db) {
            crate::analysis::ty::ty_def::TyData::TyParam(param)
                if param.owner == self.owner_scope && !param.is_effect() =>
            {
                if let Some(arg) = self.args.get(param.idx).copied() {
                    return arg;
                }
                self.error
                    .get_or_insert(SemanticEffectEnvInstantiationError {
                        owner: self.owner,
                        owner_scope: self.owner_scope,
                        offending_ty: ty,
                        param_idx: param.idx,
                        args_len: self.args.len(),
                    });
                ty
            }
            crate::analysis::ty::ty_def::TyData::ConstTy(const_ty) => {
                if let crate::analysis::ty::const_ty::ConstTyData::TyParam(param, _) =
                    const_ty.data(db)
                    && param.owner == self.owner_scope
                {
                    if let Some(arg) = self.args.get(param.idx).copied() {
                        return arg;
                    }
                    self.error
                        .get_or_insert(SemanticEffectEnvInstantiationError {
                            owner: self.owner,
                            owner_scope: self.owner_scope,
                            offending_ty: ty,
                            param_idx: param.idx,
                            args_len: self.args.len(),
                        });
                    return ty;
                }
                ty.super_fold_with(db, self)
            }
            _ => ty.super_fold_with(db, self),
        }
    }
}

#[cfg(test)]
mod non_regular_recursion_tests {
    use rustc_hash::FxHashSet;

    use super::{Binder, TyFlags, unresolved_ty_is_covered_by};
    use crate::{
        analysis::ty::{
            const_ty::{ConstTyData, ConstTyId},
            ty_def::{TyData, TyId},
        },
        hir_def::CallableDef,
        test_db::{HirAnalysisTestDb, find_func},
    };

    #[test]
    fn auxiliary_projection_coverage_requires_every_unresolved_leaf() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "auxiliary_projection_coverage.fe".into(),
            r#"
trait Project {
    type Assoc
}

struct Ground {}
struct ConstRoot<const N: u256> {}

impl Project for Ground {
    type Assoc = u256
}

fn concrete(_ value: own Ground) {}

fn shapes<T: Project, U: Project, const N: u256>(
    _ carried_root: own T,
    _ carried_projection: own <T as Project>::Assoc,
    _ uncovered_projection: own <U as Project>::Assoc,
    _ mixed_projection: own (
        <T as Project>::Assoc,
        <U as Project>::Assoc,
    ),
    _ uncovered_const: own ConstRoot<N>,
) {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let concrete = find_func(&db, top_mod, "concrete");
        let ground = concrete
            .params(&db)
            .next()
            .expect("concrete value parameter")
            .ty(&db);
        let shapes = find_func(&db, top_mod, "shapes");
        let generic_params = CallableDef::Func(shapes).params(&db);
        let [carried, uncovered, const_param] = generic_params else {
            panic!("expected two type parameters and one const parameter")
        };
        let value_params = shapes
            .params(&db)
            .map(|param| param.ty(&db))
            .collect::<Vec<TyId<'_>>>();
        let [
            _carried_root,
            carried_projection,
            uncovered_projection,
            mixed_projection,
            uncovered_const,
        ] = value_params.as_slice()
        else {
            panic!("unexpected shape parameter count")
        };

        let ground_projection = Binder::bind(*carried_projection).instantiate_with(&db, |_| ground);
        assert!(
            ground_projection
                .flags(&db)
                .contains(TyFlags::HAS_PROJECTION),
            "the ground associated type must remain a projection for this regression"
        );
        assert!(
            unresolved_ty_is_covered_by(&db, ground_projection, &FxHashSet::default()),
            "a projection over a ground receiver is caller-independent"
        );

        let carried_only = FxHashSet::from_iter([*carried]);
        assert!(
            unresolved_ty_is_covered_by(&db, *carried_projection, &carried_only),
            "a projection wholly derived from an exactly-carried primary coordinate is stable"
        );
        assert!(
            !unresolved_ty_is_covered_by(&db, *uncovered_projection, &carried_only),
            "a projection rooted in a dropped generic must remain conservative"
        );
        assert!(
            !unresolved_ty_is_covered_by(&db, *mixed_projection, &carried_only),
            "one carried lineage must not hide another uncovered lineage"
        );
        assert!(
            !unresolved_ty_is_covered_by(&db, *uncovered_const, &carried_only),
            "an uncovered const parameter must remain conservative"
        );

        let all_type_lineages = FxHashSet::from_iter([*carried, *uncovered]);
        assert!(
            unresolved_ty_is_covered_by(&db, *mixed_projection, &all_type_lineages),
            "a derived projection aggregate is stable when every lineage is carried"
        );

        let TyData::ConstTy(const_param_ty) = const_param.data(&db) else {
            panic!("expected a const parameter")
        };
        let body = shapes.body(&db).expect("shapes body");
        let unevaluated_with = |generic_args| {
            TyId::const_ty(
                &db,
                ConstTyId::new(
                    &db,
                    ConstTyData::UnEvaluated {
                        body,
                        ty: Some(const_param_ty.ty(&db)),
                        const_def: None,
                        generic_args,
                        preserve_unevaluated: true,
                        defer_validation: false,
                    },
                ),
            )
        };
        let unevaluated_type_arg = unevaluated_with(vec![*uncovered]);
        let unevaluated_const_arg = unevaluated_with(vec![*const_param]);
        for hidden_param in [unevaluated_type_arg, unevaluated_const_arg] {
            assert!(
                hidden_param.flags(&db).contains(TyFlags::HAS_PARAM),
                "unevaluated const generic arguments must contribute visitor-derived flags"
            );
            assert!(
                !unresolved_ty_is_covered_by(&db, hidden_param, &FxHashSet::default()),
                "an unevaluated const must not hide an uncovered generic argument"
            );
        }
        assert!(
            unresolved_ty_is_covered_by(
                &db,
                unevaluated_type_arg,
                &FxHashSet::from_iter([*uncovered]),
            ),
            "an unevaluated const generic argument is stable when its lineage is carried"
        );
    }
}
