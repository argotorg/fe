use cranelift_entity::EntityRef;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            CallSiteId, PlaceProvenance, SBlockId, SExpr, SStmtKind, STerminatorKind, SemanticBody,
            SemanticCalleeRef, SemanticLocalRole, ValueProvenance, VariantIndex, effect_param_site,
            lower::BindingRoleMode,
            lower::{lower_to_smir, lower_to_smir_with_call_sites},
            verify_semantic_body,
        },
        ty::{
            adt_def::{AdtDef, AdtRef},
            corelib::{RuntimeBuiltinFuncKind, runtime_builtin_func_kind},
            effect_handle_metadata,
            effects::place_effect_provider_param_index_map,
            fold::TyFoldable,
            instantiate_trait_self,
            normalize::normalize_ty,
            provider::{
                ProviderAddressSpace, ProviderKind, ProviderTransport, RootProviderRegistration,
                RootProviderSiteKind, provider_semantics, provider_semantics_for_specialized_call,
            },
            trait_resolution::{
                GoalSatisfiability, PredicateListId, TraitSolveCx, is_goal_satisfiable,
            },
            ty_check::{
                BodyOwner, EffectParamSite, EffectProviderProvenance, EffectProviderSpecialization,
                LocalBinding, ResolvedEffectArg, SemanticExprLowering, TypedBody,
            },
            ty_def::{BorrowKind, CapabilityKind, TyId, instantiate_adt_field_ty},
        },
    },
    hir_def::{CallableDef, Expr, ExprId, Partial, scope_graph::ScopeId},
    semantic::{
        EffectEnvView, EffectRequirement, EffectRequirementKey, ProviderBinding, ProviderSource,
        ResolvedEffectBinding,
    },
};
use common::indexmap::IndexMap;
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

impl<'db> SemanticInstanceKey<'db> {
    pub fn typed_body(self, db: &'db dyn HirAnalysisDb) -> &'db TypedBody<'db> {
        instantiated_typed_body(db, self)
    }

    pub fn instantiate_typed_body(self, db: &'db dyn HirAnalysisDb) -> TypedBody<'db> {
        self.typed_body(db).clone()
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
    pub offending_ty: crate::analysis::ty::ty_def::TyId<'db>,
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
        offending_ty: crate::analysis::ty::ty_def::TyId<'db>,
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
        instantiate_effect_env_data(db, instance).unwrap_or_else(|err| {
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
    callable: &crate::analysis::ty::ty_check::Callable<'db>,
    typed_body: &TypedBody<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<ReceiverLoweringPlan<'db>> {
    let receiver = call_like_receiver_expr(expr_data)?;
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

fn call_like_receiver_expr<'db>(expr_data: &Expr<'db>) -> Option<ExprId> {
    match expr_data {
        Expr::MethodCall(receiver, ..)
        | Expr::Un(receiver, ..)
        | Expr::Bin(receiver, ..)
        | Expr::AugAssign(receiver, ..) => Some(*receiver),
        Expr::Call(..)
        | Expr::Lit(..)
        | Expr::Path(..)
        | Expr::Tuple(..)
        | Expr::Array(..)
        | Expr::ArrayRep(..)
        | Expr::RecordInit(..)
        | Expr::Field(..)
        | Expr::DynField(..)
        | Expr::Cast(..)
        | Expr::Assign(..)
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
        let Some(SemanticExprLowering::Call { callable }) = typed_body.semantic_expr_lowering(expr)
        else {
            continue;
        };
        sites[expr.index()] = Some(CallSiteLowering {
            callee: provisional_semantic_callee_key(db, instance.key(db), callable, assumptions)
                .map(|key| SemanticCalleeRef { key }),
            receiver: receiver_lowering_plan(
                db,
                expr_data,
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
        let Some(SemanticExprLowering::Call { callable }) = typed_body.semantic_expr_lowering(expr)
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

    #[salsa::tracked]
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
        classify_binding_role(db, self, binding)
    }

    pub(crate) fn provisional_binding_role(
        self,
        db: &'db dyn HirAnalysisDb,
        binding: LocalBinding<'db>,
    ) -> SemanticLocalRole<'db> {
        classify_binding_role_from_ty(
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
                site: crate::analysis::ty::ty_check::ParamSite::EffectField(_),
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
        self.normalized_ty(db, self.key(db).typed_body(db).result_ty())
    }

    #[salsa::tracked(
        cycle_fn=known_never_returns_cycle_recover,
        cycle_initial=known_never_returns_cycle_initial
    )]
    pub fn known_never_returns(self, db: &'db dyn HirAnalysisDb) -> bool {
        if self.is_intrinsically_never_returning(db) {
            return true;
        }

        let body = self.body(db);
        if body.blocks.is_empty() {
            return false;
        }

        let mut pending = vec![SBlockId::from_u32(0)];
        let mut visited = FxHashSet::default();
        while let Some(block_id) = pending.pop() {
            if !visited.insert(block_id) {
                continue;
            }
            let Some(block) = body.block(block_id) else {
                continue;
            };
            let mut terminated_in_stmt = false;
            for stmt in &block.stmts {
                let SStmtKind::Assign {
                    expr: SExpr::Call { callee, .. },
                    ..
                } = &stmt.kind
                else {
                    continue;
                };
                let callee = SemanticInstance::new(db, callee.key);
                if callee.is_intrinsically_never_returning(db)
                    || (callee.contains_direct_intrinsic_never_returning_call(db)
                        && callee.known_never_returns(db))
                {
                    terminated_in_stmt = true;
                    break;
                }
            }
            if terminated_in_stmt {
                continue;
            }

            match &block.terminator.kind {
                STerminatorKind::Return(_) => return false,
                STerminatorKind::Goto(next) => pending.push(*next),
                STerminatorKind::Branch {
                    then_bb, else_bb, ..
                } => {
                    pending.push(*then_bb);
                    pending.push(*else_bb);
                }
                STerminatorKind::MatchEnum { cases, default, .. } => {
                    pending.extend(cases.iter().map(|(_, block)| *block));
                    if let Some(default) = default {
                        pending.push(*default);
                    }
                }
            }
        }

        true
    }

    #[salsa::tracked]
    pub fn body(self, db: &'db dyn HirAnalysisDb) -> SemanticBody<'db> {
        lower_semantic_body(db, self)
    }

    #[salsa::tracked(return_ref)]
    pub fn callees(self, db: &'db dyn HirAnalysisDb) -> Vec<SemanticCalleeRef<'db>> {
        collect_semantic_callees(db, self)
    }
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

    fn contains_direct_intrinsic_never_returning_call(self, db: &'db dyn HirAnalysisDb) -> bool {
        if self.is_intrinsically_never_returning(db) {
            return true;
        }

        self.body(db).blocks.iter().any(|block| {
            block.stmts.iter().any(|stmt| {
                if let SStmtKind::Assign {
                    expr: SExpr::Call { callee, .. },
                    ..
                } = &stmt.kind
                {
                    SemanticInstance::new(db, callee.key).is_intrinsically_never_returning(db)
                } else {
                    false
                }
            })
        })
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
            let field_ty = instantiate_adt_field_ty(db, adt_def, variant_idx, field_idx, args);
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
            site: crate::analysis::ty::ty_check::ParamSite::EffectField(_),
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

pub(crate) fn provisional_provider_binding_for_instance_effect<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    binding: LocalBinding<'db>,
) -> Option<ProviderBinding<'db>> {
    let key = instance.key(db);
    let provider_from_subst = |provider_idx| {
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
            site: crate::analysis::ty::ty_check::ParamSite::EffectField(effect_site),
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
                .field_layout(db)
                .values()
                .enumerate()
                .map(|(provider_idx, field)| (field.index, provider_idx as u32))
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
                });
            }
            provisional_root_provider_binding(db, key, site, requirement_idx, provider_idx, is_mut)
        }
        EffectParamSite::Contract(contract)
        | EffectParamSite::ContractInit { contract }
        | EffectParamSite::ContractRecvArm { contract, .. } => {
            if let Some((_, field)) = contract
                .field_layout(db)
                .values()
                .enumerate()
                .find(|(idx, _)| *idx as u32 == provider_idx)
            {
                let scope = contract.scope();
                let provider_ty = field.target_ty;
                return Some(ProviderBinding {
                    provider_idx,
                    provider_ty,
                    is_mut: true,
                    source: ProviderSource::ContractField {
                        contract,
                        field_idx: field.index,
                    },
                    semantics: crate::analysis::ty::provider::ProviderSemantics {
                        provider_ty,
                        kind: if field.target_ty.is_struct(db)
                            || field.target_ty.is_array(db)
                            || field.target_ty.is_tuple(db)
                            || field.target_ty.as_enum(db).is_some()
                        {
                            ProviderKind::Handle
                        } else {
                            ProviderKind::RawAddress
                        },
                        address_space: crate::analysis::ty::address_space_from_ty(
                            db,
                            scope,
                            field.address_space,
                        ),
                        target_ty: Some(field.target_ty),
                        transport: ProviderTransport::ByValue,
                    },
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
    })
}

fn effect_binding_ty_from_env<'db>(
    db: &'db dyn HirAnalysisDb,
    env: Option<InstantiatedEffectEnv<'db>>,
    idx: usize,
    provider_idx: Option<u32>,
) -> crate::analysis::ty::ty_def::TyId<'db> {
    let Some(env) = env else {
        return crate::analysis::ty::ty_def::TyId::invalid(
            db,
            crate::analysis::ty::ty_def::InvalidCause::Other,
        );
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
    .unwrap_or_else(|| {
        crate::analysis::ty::ty_def::TyId::invalid(
            db,
            crate::analysis::ty::ty_def::InvalidCause::Other,
        )
    })
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
) -> Option<crate::analysis::ty::ty_def::TyId<'db>> {
    let target_ty = requirement.key.binding_ty(db)?;
    Some(
        effect_handle_metadata(db, scope, assumptions, target_ty)
            .map_or(target_ty, |metadata| metadata.target_ty),
    )
}

fn specialized_root_provider_target_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    requirement: &EffectRequirement<'db>,
    root_provider: &ProviderBinding<'db>,
) -> Option<crate::analysis::ty::ty_def::TyId<'db>> {
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

fn collect_semantic_callees<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Vec<SemanticCalleeRef<'db>> {
    let mut seen = FxHashSet::default();
    let mut callees = Vec::new();
    for site in instance.call_sites(db).iter().flatten() {
        if let Some(callee) = site.callee
            && seen.insert(callee.key)
        {
            callees.push(callee);
        }
    }
    for sites in instance.for_loop_call_sites(db).iter().flatten() {
        if let Some(callee) = sites.len.callee
            && seen.insert(callee.key)
        {
            callees.push(callee);
        }
        if let Some(callee) = sites.get.callee
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
    binding: LocalBinding<'db>,
) -> SemanticLocalRole<'db> {
    classify_binding_role_with_provider(
        db,
        instance,
        binding,
        resolved_provider_binding_for_instance_effect(db, instance, binding),
    )
}

fn classify_binding_role_with_provider<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    binding: LocalBinding<'db>,
    provider: Option<ProviderBinding<'db>>,
) -> SemanticLocalRole<'db> {
    classify_binding_role_from_ty(
        db,
        instance,
        instance.binding_ty(db, binding),
        instance.assumptions(db),
        provider,
    )
}

fn classify_binding_role_from_ty<'db>(
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
    if let Some(metadata) = effect_handle_metadata(db, scope, assumptions, ty) {
        return SemanticLocalRole::DirectCarrier {
            provider: provider.clone(),
            target_ty: metadata.target_ty,
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
        };
    }
    SemanticLocalRole::DirectValue {
        provenance: ValueProvenance::Ordinary,
    }
}

pub fn validate_instantiated_effect_env<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<(), SemanticEffectEnvInstantiationError<'db>> {
    instantiate_effect_env_data_for_key(db, instance.key(db)).map(|_| ())
}

pub fn validate_instantiated_effect_env_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> Result<(), SemanticEffectEnvInstantiationError<'db>> {
    instantiate_effect_env_data_for_key(db, key).map(|_| ())
}

fn instantiate_effect_env_data<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<Option<InstantiatedEffectEnvData<'db>>, SemanticEffectEnvInstantiationError<'db>> {
    instantiate_effect_env_data_for_key(db, instance.key(db))
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
    let specializations = key
        .effect_providers(db)
        .providers(db)
        .iter()
        .map(|specialization| {
            (
                specialization.provider.provider_idx,
                specialization.provider.clone(),
            )
        })
        .collect::<FxHashMap<_, _>>();
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
) -> Result<Vec<crate::analysis::ty::ty_def::TyId<'db>>, RootSemanticInstanceError<'db>> {
    match owner {
        BodyOwner::Func(func) => root_func_generic_args(db, func),
        BodyOwner::Const(_)
        | BodyOwner::AnonConstBody { .. }
        | BodyOwner::ContractInit { .. }
        | BodyOwner::ContractRecvArm { .. } => Ok(Vec::new()),
    }
}

fn owner_identity_generic_args<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Vec<crate::analysis::ty::ty_def::TyId<'db>> {
    match owner {
        BodyOwner::Func(func) => CallableDef::Func(func).params(db).to_vec(),
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
                || effect_handle_metadata(db, func.scope(), assumptions, provider_ty).is_some()
        }
        EffectRequirementKey::Trait(trait_inst) => {
            let goal = instantiate_trait_self(db, trait_inst, root_provider.provider_ty);
            matches!(
                is_goal_satisfiable(
                    db,
                    TraitSolveCx::new(db, func.scope()).with_assumptions(assumptions),
                    goal,
                ),
                GoalSatisfiability::Satisfied(_) | GoalSatisfiability::NeedsConfirmation(_)
            )
        }
        EffectRequirementKey::Other => false,
    }
}

fn root_func_generic_args<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
) -> Result<Vec<crate::analysis::ty::ty_def::TyId<'db>>, RootSemanticInstanceError<'db>> {
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
    let semantics = match source {
        ProviderSource::ContractField { .. } => crate::analysis::ty::provider::ProviderSemantics {
            provider_ty,
            target_ty: provider
                .semantics
                .target_ty
                .map(|ty| instantiate_normalized_ty(db, key, ty))
                .transpose()?,
            ..provider.semantics
        },
        ProviderSource::UsesParam { .. } | ProviderSource::RootProvider { .. } => {
            provider_semantics(db, scope, assumptions, provider_ty)
        }
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
    ty: crate::analysis::ty::ty_def::TyId<'db>,
) -> Result<crate::analysis::ty::ty_def::TyId<'db>, SemanticEffectEnvInstantiationError<'db>> {
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
    args: &[crate::analysis::ty::ty_def::TyId<'db>],
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
    args: &'a [crate::analysis::ty::ty_def::TyId<'db>],
    error: Option<SemanticEffectEnvInstantiationError<'db>>,
}

impl<'db> crate::analysis::ty::fold::TyFolder<'db> for CheckedInstantiateFolder<'db, '_> {
    fn fold_ty(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        ty: crate::analysis::ty::ty_def::TyId<'db>,
    ) -> crate::analysis::ty::ty_def::TyId<'db> {
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

fn known_never_returns_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _instance: SemanticInstance<'db>,
) -> bool {
    false
}

fn known_never_returns_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &bool,
    _count: u32,
    _instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<bool> {
    salsa::CycleRecoveryAction::Iterate
}
