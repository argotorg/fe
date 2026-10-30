use cranelift_entity::EntityRef;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    analysis::{
        HirAnalysisDb,
        place::{Place, PlaceBase},
        semantic::{
            CallSiteId, PlaceProvenance, RuntimeSizeError, SExpr, SLocalId, SStmtKind, SemOrigin,
            SemanticBody, SemanticCalleeRef, SemanticLocalRole, ValueProvenance, VariantIndex,
            access::{CallSiteRefinements, provisional_call_site_provider_refinements},
            concrete_layout_fault,
            diagnostics::{
                SemanticDiagnostic, SemanticDiagnosticId, SemanticDiagnosticKind,
                SemanticDiagnosticLabel, SemanticDiagnosticSpan, checker_name,
            },
            effect_param_site,
            lower::{BindingRoleMode, lower_to_smir, lower_to_smir_with_call_sites},
            owner_effect_bindings, runtime_size_bytes, verify_semantic_body,
        },
        ty::{
            adt_def::{AdtDef, AdtRef, instantiate_adt_field_shape},
            closure::closure_regions,
            corelib::{
                ContractMetadataKind, RuntimeBuiltinFuncKind, contract_metadata_kind,
                runtime_builtin_func_kind,
            },
            effects::{
                EffectKeyKind, instantiate_trait_effect_key, place_effect_provider_param_index_map,
                rows::{RowExpansion, RowKey, expand_rows},
            },
            evidence::{EvidenceList, instance_evidence},
            fold::{TyFoldable, TyFolder},
            instantiate_trait_self,
            normalize::{
                PROJECTION_DEPTH_LIMIT, PROJECTION_WORK_LIMIT, normalize_ty, tree_depth_within,
                tree_size_within,
            },
            provider::{
                AddressSpace, ProviderKind, ProviderTransport, RootProviderRegistration,
                RootProviderScope, provider_semantics_for_specialized_call,
                registered_root_providers,
            },
            subst::substitute_complete,
            trait_def::{MethodArgMapError, TraitInstId},
            trait_resolution::{
                GoalSatisfiability, PredicateListId, TraitSolveCx, is_goal_satisfiable,
            },
            ty_check::{
                BodyOwner, Callable, ConstIntrinsicKind, EffectArg, EffectParamSite,
                EffectPassMode, EffectProviderProvenance, EffectProviderSpecialization,
                ForLoopStep, LocalBinding, ParamSite, ResolvedEffectArg, RowArg,
                SemanticExprLowering, SmirLoweringIssue, TypedBody,
            },
            ty_def::{BorrowKind, InvalidCause, TyData, TyId},
            ty_is_snapshot,
            ty_lower::{ParamSchemaId, SubstError},
            visitor::collect_flags,
        },
    },
    hir_def::{CallableDef, Contract, ExprId, FuncParamMode, StmtId, scope_graph::ScopeId},
    semantic::{
        EffectEnvView, EffectRequirement, EffectRequirementKey, ProviderBinding, ProviderSource,
        ResolvedEffectBinding, effect_param_provider_semantics, effect_requirements_for_site,
        storage_slot_span,
    },
};
use common::indexmap::IndexMap;
use indexmap::IndexSet;
use salsa::SalsaValue;
use thin_vec::ThinVec;

use super::{
    EffectProviderSubst, GenericSubst, ImplEnv,
    const_ref::{is_ground, root_impl_env},
    instantiate_typed_body, provisional_semantic_callee_key,
    semantic_callee_key_with_effect_providers, typed_body_template,
};

#[salsa::interned]
#[derive(Debug)]
pub struct SemanticInstanceKey<'db> {
    #[returns(copy)]
    pub owner: BodyOwner<'db>,
    #[returns(copy)]
    pub subst: GenericSubst<'db>,
    #[returns(copy)]
    pub effect_providers: EffectProviderSubst<'db>,
    #[returns(copy)]
    pub impl_env: ImplEnv<'db>,
    /// Proofs of the assumptions the owner's body is checked under, from the
    /// call that made this instance. One the list does not supply is proved
    /// where the body needs it.
    #[returns(copy)]
    pub evidence: EvidenceList<'db>,
}

impl<'db> SemanticInstanceKey<'db> {
    /// The key for `owner`'s instance with `subst`, given the proofs a use
    /// supplies of the assumptions its body is checked under. One with
    /// closed arguments carries none: its instance proves every assumption
    /// itself (`instance_evidence`), so the keys for one body and
    /// substitution are the same however they are made, and a key never
    /// holds a proof whose types can hold the key.
    pub fn of(
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
        subst: GenericSubst<'db>,
        effect_providers: EffectProviderSubst<'db>,
        impl_env: ImplEnv<'db>,
        supplied: EvidenceList<'db>,
    ) -> Self {
        let evidence = if is_ground(collect_flags(db, subst.generic_args(db))) {
            EvidenceList::none(db)
        } else {
            supplied
        };
        Self::new(db, owner, subst, effect_providers, impl_env, evidence)
    }

    /// This key instantiated with its data parameters naming places in
    /// `param_spaces`.
    fn with_param_spaces(
        self,
        db: &'db dyn HirAnalysisDb,
        param_spaces: Vec<(u32, AddressSpace)>,
    ) -> Self {
        if param_spaces.is_empty() {
            return self;
        }
        let providers = self.effect_providers(db).providers(db).clone();
        Self::new(
            db,
            self.owner(db),
            self.subst(db),
            EffectProviderSubst::new(db, providers, param_spaces),
            self.impl_env(db),
            self.evidence(db),
        )
    }

    pub fn typed_body(self, db: &'db dyn HirAnalysisDb) -> &'db TypedBody<'db> {
        instantiated_typed_body(db, self)
    }

    pub fn instantiate_typed_body(self, db: &'db dyn HirAnalysisDb) -> TypedBody<'db> {
        self.typed_body(db).clone()
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct SemanticInstance<'db> {
    #[returns(copy)]
    pub key: SemanticInstanceKey<'db>,
}

#[derive(Debug, Clone)]
pub enum SemanticEffectEnvInstantiationError<'db> {
    Generic {
        owner: BodyOwner<'db>,
        error: SubstError<'db>,
    },
    MissingDomain {
        owner: BodyOwner<'db>,
    },
    WrongOwner {
        owner: BodyOwner<'db>,
        schema: ParamSchemaId<'db>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum SemanticBodyAdmissionError<'db> {
    BlockedByUpstreamDiagnostics(Box<[crate::analysis::ty::ty_check::SmirLoweringIssue]>),
    IncompleteLoweringPlan(Box<[crate::analysis::ty::ty_check::SmirLoweringIssue]>),
    CallSiteFinalization(crate::analysis::semantic::SemanticDiagnosticId<'db>),
    InvalidConcreteType(crate::analysis::semantic::SemanticDiagnosticId<'db>),
    InstanceLimit(crate::analysis::semantic::SemanticDiagnosticId<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
pub struct CallSiteLowering<'db> {
    pub callee: Option<SemanticCalleeRef<'db>>,
    pub effect_args: Box<[ResolvedEffectArg<'db>]>,
    pub effect_pairs: Box<[(usize, usize)]>,
    pub provider_pairs: Box<[(u32, u32)]>,
    /// Whether the callee takes the operands after the call's receiver (a
    /// function item called through a callable trait).
    pub drops_receiver: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
pub struct ForLoopCallSites<'db> {
    /// The protocol's calls, in `ForLoopStep` order.
    pub sites: [CallSiteLowering<'db>; 4],
}

impl<'db> ForLoopCallSites<'db> {
    pub fn site(&self, step: ForLoopStep) -> &CallSiteLowering<'db> {
        &self.sites[step as usize]
    }
}

/// The address space of the place a call supplies to one of its callee's
/// inputs, which the callee is instantiated for.
#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
pub(crate) struct CallSiteProviderRefinement {
    pub call_site: CallSiteId,
    pub input: RefinedInput,
    pub address_space: AddressSpace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, SalsaValue)]
pub(crate) enum RefinedInput {
    Effect {
        binding_idx: u32,
        provider_idx: Option<u32>,
    },
    Param(u32),
}

#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
struct CallSiteFinalizationData<'db> {
    call_sites: Vec<Option<CallSiteLowering<'db>>>,
    for_loop_call_sites: Vec<Option<ForLoopCallSites<'db>>>,
    diagnostic: Option<crate::analysis::semantic::SemanticDiagnosticId<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
struct ProvisionalCallSiteData<'db, T: SalsaValue + PartialEq + Eq + Clone> {
    sites: Vec<Option<T>>,
    diagnostic: Option<SemanticDiagnosticId<'db>>,
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
    Vec<TraitInstId<'db>>,
    PredicateListId<'db>,
);

#[salsa::tracked]
#[derive(Debug)]
pub struct InstantiatedEffectEnv<'db> {
    #[returns(copy)]
    pub site: crate::analysis::ty::ty_check::EffectParamSite<'db>,
    pub requirements: Vec<EffectRequirement<'db>>,
    pub providers: Vec<ProviderBinding<'db>>,
    pub resolutions: Vec<ResolvedEffectBinding>,
    pub forwarded_witnesses: Vec<TraitInstId<'db>>,
    #[returns(copy)]
    pub assumptions: PredicateListId<'db>,
}

#[salsa::tracked(returns(copy))]
pub fn instantiated_effect_env<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Option<InstantiatedEffectEnv<'db>> {
    let (site, requirements, providers, resolutions, forwarded_witnesses, assumptions) =
        instantiate_effect_env_data_for_key(db, instance.key(db))
            .unwrap_or_else(|err| panic!("failed to instantiate effect env: {err:?}"))?;
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

#[salsa::tracked]
pub fn instantiated_typed_body<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> TypedBody<'db> {
    let mut body =
        instantiate_typed_body(db, typed_body_template(db, key.owner(db)), key.subst(db));
    // A spread's inputs are those its instantiated shape lists.
    if let BodyOwner::Func(func, _) = key.owner(db)
        && let Some(spread) = CallableDef::Func(func).spread(db)
        && let Some(shape) = func.arg_ty(db, spread)
    {
        let env = key.impl_env(db);
        let shape = normalize_ty(
            db,
            shape.instantiate(db, key.subst(db).generic_args(db)),
            env.normalization_scope(db),
            env.assumptions(db),
        );
        if let Some(listed) = shape.as_params_shape(db) {
            body = body.with_spread_inputs(db, func, spread, listed);
        }
    }
    // An opaque parameter's inputs are those its instantiated shape lists.
    if let BodyOwner::Func(..) | BodyOwner::Closure { .. } = key.owner(db) {
        let env = key.impl_env(db);
        let (scope, assumptions) = (env.normalization_scope(db), env.assumptions(db));
        body = body.with_known_params(db, |shape| normalize_ty(db, shape, scope, assumptions));
    }
    let evidence = instance_evidence(
        db,
        body.signature(),
        key.evidence(db),
        TraitSolveCx::new(db, key.impl_env(db).normalization_scope(db)),
    );
    body.bind_evidence(db, evidence);
    // A body's row provider parameters are the providers its call binds.
    let owner = match key.owner(db) {
        BodyOwner::Closure { def, .. } => (def.body.scope(), Some(def.expr)),
        BodyOwner::Func(func, mode)
            if let Some(func_body) = func.body(db)
                && func
                    .effect_requirements(db, mode)
                    .iter()
                    .any(|requirement| requirement.key.key_row().is_some()) =>
        {
            (func_body.scope(), None)
        }
        _ => return body,
    };
    body.fold_with(
        db,
        &mut RowProviderSubst {
            owner,
            providers: key.effect_providers(db).providers(db),
        },
    )
}

struct RowProviderSubst<'a, 'db> {
    owner: (ScopeId<'db>, Option<ExprId>),
    providers: &'a [ProviderBinding<'db>],
}

impl<'db> TyFolder<'db> for RowProviderSubst<'_, 'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        if let TyData::TyParam(param) = ty.data(db)
            && param.row_effect_provider_of() == Some(self.owner)
            && let Some(provider) = self
                .providers
                .iter()
                .find(|provider| provider.provider_idx as usize == param.idx)
        {
            return provider.provider_ty;
        }
        ty.super_fold_with(db, self)
    }
}

#[salsa::tracked]
fn provisional_call_sites<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> ProvisionalCallSiteData<'db, CallSiteLowering<'db>> {
    let typed_body = instance.key(db).typed_body(db);
    let Some(body) = typed_body.body() else {
        return ProvisionalCallSiteData {
            sites: Vec::new(),
            diagnostic: None,
        };
    };
    let assumptions = semantic_instance_base_assumptions_for_key(db, instance.key(db));
    let mut sites = vec![None; body.exprs(db).len()];
    let mut diagnostic = None;
    let (regions, region) = (
        closure_regions(db, body),
        instance.key(db).owner(db).closure_region(),
    );

    for (expr, _) in body.exprs(db).iter() {
        let Some(SemanticExprLowering::Call { callable }) = typed_body.semantic_expr_lowering(expr)
        else {
            continue;
        };
        if regions.expr(expr) != region {
            continue;
        }
        let site = provisional_call_site(
            db,
            instance,
            callable,
            typed_body.call_effect_args(expr).unwrap_or(&[]),
            assumptions,
            SemOrigin::Expr(expr),
            &mut diagnostic,
        );
        sites[expr.index()] = Some(site);
    }

    ProvisionalCallSiteData { sites, diagnostic }
}

#[salsa::tracked]
fn provisional_for_loop_call_sites<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> ProvisionalCallSiteData<'db, ForLoopCallSites<'db>> {
    let typed_body = instance.key(db).typed_body(db);
    let Some(body) = typed_body.body() else {
        return ProvisionalCallSiteData {
            sites: Vec::new(),
            diagnostic: None,
        };
    };
    let assumptions = semantic_instance_base_assumptions_for_key(db, instance.key(db));
    let mut sites = vec![None; body.stmts(db).len()];
    let mut diagnostic = None;
    let (regions, region) = (
        closure_regions(db, body),
        instance.key(db).owner(db).closure_region(),
    );
    for (stmt, _) in body.stmts(db).iter() {
        let Some(plan) = typed_body.for_loop_plan(stmt) else {
            continue;
        };
        if regions.stmt(stmt) != region {
            continue;
        }
        sites[stmt.index()] = Some(ForLoopCallSites {
            sites: plan.calls.each_ref().map(|call| {
                provisional_call_site(
                    db,
                    instance,
                    &call.callable,
                    &call.effect_args,
                    assumptions,
                    SemOrigin::Stmt(stmt),
                    &mut diagnostic,
                )
            }),
        });
    }
    ProvisionalCallSiteData { sites, diagnostic }
}

/// A call's row arguments in the instance's view. The call numbered the
/// components of the callee's rows as its own view expanded them; the
/// instance renumbers them by path. A row the call forwards passes as the
/// components the instance expands it to, each from the caller's component
/// at the same place in the caller's own row. What stays abstract carries
/// nothing and is dropped.
fn expand_row_args<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    callable: &Callable<'db>,
    args: &[ResolvedEffectArg<'db>],
) -> (Callable<'db>, Vec<ResolvedEffectArg<'db>>) {
    let mut callable = callable.clone();
    let CallableDef::Func(func) = callable.callable_def() else {
        return (callable, args.to_vec());
    };
    if args.iter().all(|arg| arg.row_arg.is_none()) {
        return (callable, args.to_vec());
    }
    let key = instance.key(db);
    let scope = key.owner(db).scope();
    let assumptions = semantic_instance_base_assumptions_for_key(db, key);
    let requirements: Vec<_> = func
        .effect_requirements(db, callable.mode())
        .iter()
        .map(|requirement| match requirement.key {
            EffectRequirementKey::Row(row) => EffectRequirement {
                key: EffectRequirementKey::Row(RowKey {
                    inst: normalize_ty(
                        db,
                        instantiate_trait_effect_key(db, row.inst, &callable),
                        scope,
                        assumptions,
                    ),
                    row: row.row,
                }),
                ..requirement.clone()
            },
            _ => requirement.clone(),
        })
        .collect();
    let callee_rows = expand_rows(db, &requirements, scope, assumptions);
    let caller_rows = row_expansion_for_key(db, key);
    let caller_providers = instantiated_effect_env(db, instance)
        .map(|env| env.providers(db).clone())
        .unwrap_or_default();
    let mut slots = FxHashMap::default();
    let mut expanded = Vec::with_capacity(args.len());
    let mut forwarded_providers = Vec::new();
    for arg in args {
        let (path, own) = match &arg.row_arg {
            None => {
                expanded.push(arg.clone());
                continue;
            }
            Some(RowArg::Component(path)) => {
                if let Some(component) = callee_rows.component(path) {
                    let slot = component.requirement.binding_idx;
                    slots.insert(arg.binding_idx, slot);
                    expanded.push(ResolvedEffectArg {
                        binding_idx: slot,
                        ..arg.clone()
                    });
                }
                continue;
            }
            Some(RowArg::Forwarded { callee, own }) => (callee, own),
        };
        for callee in &callee_rows.components {
            let Some(caller) = callee
                .path
                .rebase(path, own)
                .and_then(|path| caller_rows.component(&path))
                .filter(|_| callee.requirement.key.key_row().is_none())
            else {
                continue;
            };
            let slot = caller.requirement.binding_idx;
            let binding = LocalBinding::EffectParam {
                site: caller.requirement.binding_site,
                idx: slot as usize,
                binding_name: caller.requirement.binding_name,
                provider_idx: slot,
                is_mut: caller.requirement.is_mut,
            };
            let Some(provider) = caller_providers
                .iter()
                .find(|provider| provider.provider_idx == slot)
            else {
                continue;
            };
            let (arg_value, pass_mode) = match provider.semantics.transport {
                ProviderTransport::ByValue => {
                    (EffectArg::Binding(binding), EffectPassMode::ByValue)
                }
                ProviderTransport::ByPlace | ProviderTransport::ByTempPlace => (
                    EffectArg::Place(Place::new(PlaceBase::Binding(binding))),
                    EffectPassMode::ByPlace,
                ),
            };
            let target = callee.requirement.binding_idx;
            expanded.push(ResolvedEffectArg {
                param_idx: expanded.len(),
                binding_idx: target,
                key: callee.requirement.binding_ty,
                arg: arg_value,
                with_source: None,
                pass_mode,
                required_mut: callee.requirement.is_mut,
                key_kind: callee.requirement.key.kind(),
                instantiated_key_ty: callee.requirement.key.key_ty(),
                provider_target_ty: provider.semantics.target_ty,
                provider: provider.semantics.address_space,
                row_arg: Some(RowArg::Component(callee.path.clone())),
            });
            forwarded_providers.push(EffectProviderSpecialization {
                provider: ProviderBinding {
                    provider_idx: target,
                    source: ProviderSource::UsesParam {
                        site: EffectParamSite::Func(func, callable.mode()).slot_site(db, target),
                        requirement_idx: target,
                    },
                    ..provider.clone()
                },
                provenance: EffectProviderProvenance::Binding {
                    owner: key.owner(db),
                    binding,
                },
            });
        }
    }
    // Renumber the callee's slots, not their origins: a provider's source can
    // name a caller effect whose index happens to overlap a callee row slot.
    for specialization in callable.effect_providers_mut() {
        let provider = &mut specialization.provider;
        if let Some(&slot) = slots.get(&provider.provider_idx) {
            provider.provider_idx = slot;
        }
    }
    callable.effect_providers_mut().extend(forwarded_providers);
    (callable, expanded)
}

/// Plans a call before provider refinement. The first argument-mapping
/// failure is recorded in `diagnostic`; the site then keeps its nominal
/// effect order.
fn provisional_call_site<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    callable: &Callable<'db>,
    nominal_effect_args: &[ResolvedEffectArg<'db>],
    assumptions: PredicateListId<'db>,
    origin: SemOrigin<'db>,
    diagnostic: &mut Option<SemanticDiagnosticId<'db>>,
) -> CallSiteLowering<'db> {
    let (callable, nominal_effect_args) =
        expand_row_args(db, instance, callable, nominal_effect_args);
    let (callable, nominal_effect_args) = (&callable, nominal_effect_args.as_slice());
    let mut report = |error| {
        diagnostic.get_or_insert_with(|| method_arg_map_diagnostic(db, instance, error, origin));
    };
    let callee = provisional_semantic_callee_key(
        db,
        instance.key(db),
        callable,
        nominal_effect_args,
        assumptions,
    )
    .unwrap_or_else(|error| {
        report(error);
        None
    });
    let effect_pairs = callee
        .as_ref()
        .map_or(&[][..], |plan| plan.effect_pairs.as_slice());
    let effect_args =
        rebase_effect_args(nominal_effect_args, effect_pairs).unwrap_or_else(|error| {
            report(error);
            nominal_effect_args.into()
        });
    let (callee, effect_pairs, provider_pairs, drops_receiver) = match callee {
        Some(plan) => (
            Some(SemanticCalleeRef { key: plan.key }),
            plan.effect_pairs,
            plan.provider_pairs,
            plan.drops_receiver,
        ),
        None => (None, Vec::new(), Vec::new(), false),
    };
    CallSiteLowering {
        callee,
        effect_args,
        effect_pairs: effect_pairs.into(),
        provider_pairs: provider_pairs.into(),
        drops_receiver,
    }
}

fn provisional_call_site_diagnostic<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Option<SemanticDiagnosticId<'db>> {
    provisional_call_sites(db, instance)
        .diagnostic
        .or(provisional_for_loop_call_sites(db, instance).diagnostic)
}

#[salsa::tracked(cycle_initial=final_call_site_data_cycle_initial)]
fn final_call_site_data<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> CallSiteFinalizationData<'db> {
    let mut call_sites = provisional_call_sites(db, instance).sites.clone();
    let mut for_loop_call_sites = provisional_for_loop_call_sites(db, instance).sites.clone();
    if let Some(diagnostic) = provisional_call_site_diagnostic(db, instance) {
        return CallSiteFinalizationData {
            call_sites,
            for_loop_call_sites,
            diagnostic: Some(diagnostic),
        };
    }
    let typed_body = instance.key(db).typed_body(db);
    let Some(body) = typed_body.body() else {
        return CallSiteFinalizationData {
            call_sites,
            for_loop_call_sites,
            diagnostic: None,
        };
    };
    // Only effects, non-memory parameters and parameters holding a pinned
    // type, whose paths reach the pin's space, can supply non-memory places.
    let pinned_param = typed_body.input_bindings().iter().any(|&binding| {
        let ty = instance.normalized_binding_ty(db, binding);
        ty.as_capability(db)
            .map_or(ty, |(_, inner)| inner)
            .pinned_part(db)
            .is_some()
    });
    let refinements = if call_sites_have_effect_args(&call_sites, &for_loop_call_sites)
        || !instance
            .key(db)
            .effect_providers(db)
            .param_spaces(db)
            .is_empty()
        || !instance.effect_bindings(db).is_empty()
        || pinned_param
    {
        match provisional_call_site_provider_refinements(db, instance).clone() {
            CallSiteRefinements::Refined(refinements) => refinements,
            CallSiteRefinements::Blocked => {
                return CallSiteFinalizationData {
                    call_sites,
                    for_loop_call_sites,
                    diagnostic: None,
                };
            }
            CallSiteRefinements::Rejected(diagnostic) => {
                return CallSiteFinalizationData {
                    call_sites,
                    for_loop_call_sites,
                    diagnostic: Some(diagnostic),
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

    let diagnostic = finalize_call_sites(
        db,
        instance,
        typed_body,
        body,
        &mut call_sites,
        &mut for_loop_call_sites,
        &by_site,
    )
    .err();
    CallSiteFinalizationData {
        call_sites,
        for_loop_call_sites,
        diagnostic,
    }
}

fn finalize_call_sites<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    typed_body: &TypedBody<'db>,
    body: crate::hir_def::Body<'db>,
    call_sites: &mut [Option<CallSiteLowering<'db>>],
    for_loop_call_sites: &mut [Option<ForLoopCallSites<'db>>],
    by_site: &FxHashMap<CallSiteId, Vec<CallSiteProviderRefinement>>,
) -> Result<(), SemanticDiagnosticId<'db>> {
    let refinements = |site| by_site.get(&site).map(Vec::as_slice);
    let same_assumptions = instance.assumptions(db)
        == semantic_instance_base_assumptions_for_key(db, instance.key(db));
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
            typed_body.call_effect_args(expr).unwrap_or(&[]),
            refinements(CallSiteId::Expr(expr)),
            SemOrigin::Expr(expr),
            same_assumptions,
        )?;
    }

    for (stmt, _) in body.stmts(db).iter() {
        let Some(sites) = for_loop_call_sites
            .get_mut(stmt.index())
            .and_then(Option::as_mut)
        else {
            continue;
        };
        let Some(plan) = typed_body.for_loop_plan(stmt) else {
            continue;
        };
        for step in ForLoopStep::ALL {
            let call = plan.call(step);
            finalize_call_site(
                db,
                instance,
                &call.callable,
                &mut sites.sites[step as usize],
                &call.effect_args,
                refinements(CallSiteId::ForLoop(stmt, step)),
                SemOrigin::Stmt(stmt),
                same_assumptions,
            )?;
        }
    }
    Ok(())
}

fn final_call_site_data_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    instance: SemanticInstance<'db>,
) -> CallSiteFinalizationData<'db> {
    CallSiteFinalizationData {
        call_sites: provisional_call_sites(db, instance).sites.clone(),
        for_loop_call_sites: provisional_for_loop_call_sites(db, instance).sites.clone(),
        diagnostic: provisional_call_site_diagnostic(db, instance),
    }
}

fn call_sites_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    instance: SemanticInstance<'db>,
) -> Vec<Option<CallSiteLowering<'db>>> {
    provisional_call_sites(db, instance).sites.clone()
}

fn for_loop_call_sites_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    instance: SemanticInstance<'db>,
) -> Vec<Option<ForLoopCallSites<'db>>> {
    provisional_for_loop_call_sites(db, instance).sites.clone()
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
            .flat_map(|sites| &sites.sites)
            .any(|site| !site.effect_args.is_empty())
}

fn rebase_effect_args<'db>(
    args: &[ResolvedEffectArg<'db>],
    effect_pairs: &[(usize, usize)],
) -> Result<Box<[ResolvedEffectArg<'db>]>, MethodArgMapError<'db>> {
    if effect_pairs.is_empty() {
        return Ok(args.to_vec().into_boxed_slice());
    }
    let mut rebased = Vec::with_capacity(args.len());
    for arg in args {
        let nominal = arg.binding_idx as usize;
        let Some((_, body_idx)) = effect_pairs.iter().find(|(index, _)| *index == nominal) else {
            return Err(MethodArgMapError::MissingEffectRole(nominal));
        };
        let Ok(binding_idx) = u32::try_from(*body_idx) else {
            return Err(MethodArgMapError::MissingEffectRole(nominal));
        };
        let mut arg = arg.clone();
        arg.binding_idx = binding_idx;
        arg.param_idx = *body_idx;
        rebased.push(arg);
    }
    rebased.sort_by_key(|arg| arg.param_idx);
    Ok(rebased.into_boxed_slice())
}

fn method_arg_map_diagnostic<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    error: MethodArgMapError<'db>,
    origin: SemOrigin<'db>,
) -> SemanticDiagnosticId<'db> {
    let message = match error {
        MethodArgMapError::SignatureMismatch {
            nominal,
            body,
            nominal_args,
            body_args,
            evidence,
            nominal_signature,
            body_signature,
        } => {
            let nominal_args = nominal_args
                .iter()
                .map(|arg| arg.pretty_print(db).to_string())
                .collect::<Vec<_>>();
            let body_args = body_args
                .iter()
                .map(|arg| arg.pretty_print(db).to_string())
                .collect::<Vec<_>>();
            format!(
                "checked call signature mismatch: nominal={nominal:?} target={body:?}, nominal_args={nominal_args:?}, body_args={body_args:?}, evidence={}, nominal_signature=({nominal_signature}), body_signature=({body_signature})",
                evidence.pretty_print(db),
            )
        }
        error => format!("checked call body argument mapping failed: {error:?}"),
    };
    SemanticDiagnosticId::new(
        db,
        SemanticDiagnostic {
            kind: SemanticDiagnosticKind::Internal,
            instance,
            primary: SemanticDiagnosticLabel {
                message,
                span: SemanticDiagnosticSpan::Origin {
                    owner: instance.key(db).owner(db),
                    origin,
                },
            },
            secondaries: Vec::new(),
        },
    )
}

#[allow(clippy::too_many_arguments)]
fn finalize_call_site<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    callable: &Callable<'db>,
    site: &mut CallSiteLowering<'db>,
    nominal_effect_args: &[ResolvedEffectArg<'db>],
    refinements: Option<&[CallSiteProviderRefinement]>,
    origin: SemOrigin<'db>,
    same_assumptions: bool,
) -> Result<(), SemanticDiagnosticId<'db>> {
    let (callable, nominal_effect_args) =
        expand_row_args(db, instance, callable, nominal_effect_args);
    let (callable, nominal_effect_args) = (&callable, nominal_effect_args.as_slice());
    // A plain call without provider refinements or effect providers, under the
    // provisional assumptions, resolves exactly as its provisional plan did:
    // the provider resolution modes differ only for providers and for selected
    // trait methods.
    if refinements.is_none()
        && same_assumptions
        && callable.trait_inst().is_none()
        && callable.effect_providers().is_empty()
    {
        return Ok(());
    }
    replan_call_site(
        db,
        instance,
        callable,
        site,
        nominal_effect_args,
        refinements,
        origin,
    )
}

fn replan_call_site<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    callable: &Callable<'db>,
    site: &mut CallSiteLowering<'db>,
    nominal_effect_args: &[ResolvedEffectArg<'db>],
    refinements: Option<&[CallSiteProviderRefinement]>,
    origin: SemOrigin<'db>,
) -> Result<(), SemanticDiagnosticId<'db>> {
    let mut effect_providers = callable.effect_providers().to_vec();
    let mapping_diag = |error| method_arg_map_diagnostic(db, instance, error, origin);
    let mut refined_nominal_effect_args = nominal_effect_args.to_vec();
    let mut param_spaces = Vec::new();
    for refinement in refinements.into_iter().flatten() {
        let (binding_idx, provider_idx) = match refinement.input {
            RefinedInput::Param(param) => {
                param_spaces.push((param, refinement.address_space));
                continue;
            }
            RefinedInput::Effect {
                binding_idx,
                provider_idx,
            } => (binding_idx, provider_idx),
        };
        let nominal_binding_idx = if site.effect_pairs.is_empty() {
            binding_idx
        } else {
            let Some((nominal, _)) = site
                .effect_pairs
                .iter()
                .find(|(_, body)| *body == binding_idx as usize)
            else {
                return Err(mapping_diag(MethodArgMapError::MissingEffectRole(
                    binding_idx as usize,
                )));
            };
            u32::try_from(*nominal)
                .map_err(|_| mapping_diag(MethodArgMapError::MissingEffectRole(*nominal)))?
        };
        for arg in &mut refined_nominal_effect_args {
            if arg.binding_idx == nominal_binding_idx {
                arg.provider = Some(refinement.address_space);
            }
        }
        if let Some(provider_idx) = provider_idx
            && let Some(specialization) = effect_providers.iter_mut().find(|provider| {
                site.provider_pairs
                    .iter()
                    .find(|(nominal, _)| *nominal == provider.provider.provider_idx)
                    .map_or(provider.provider.provider_idx, |(_, body)| *body)
                    == provider_idx
            })
        {
            specialize_provider_address_space(db, specialization, refinement.address_space);
        }
    }
    let callee = semantic_callee_key_with_effect_providers(
        db,
        instance.key(db),
        callable,
        &refined_nominal_effect_args,
        &effect_providers,
    )
    .map_err(mapping_diag)?;
    if let Some(callee) = callee {
        site.effect_args = rebase_effect_args(&refined_nominal_effect_args, &callee.effect_pairs)
            .map_err(mapping_diag)?;
        site.effect_pairs = callee.effect_pairs.into_boxed_slice();
        site.provider_pairs = callee.provider_pairs.into_boxed_slice();
        site.drops_receiver = callee.drops_receiver;
        site.callee = Some(SemanticCalleeRef {
            key: callee.key.with_param_spaces(db, param_spaces),
        });
    } else {
        site.callee = None;
        site.drops_receiver = false;
    }
    Ok(())
}

fn invalid_size_diagnostic<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    span: SemanticDiagnosticSpan<'db>,
    ty: TyId<'db>,
    error: RuntimeSizeError<'db>,
) -> SemanticDiagnosticId<'db> {
    let subject = match span {
        SemanticDiagnosticSpan::LocalSourceOrBody { .. } => "value",
        _ => "operation",
    };
    let mut diagnostic = SemanticDiagnostic::new(
        instance,
        SemanticDiagnosticKind::InvalidConcreteType,
        format!("this {subject} requires a valid concrete type size"),
        span,
    );
    let message = match error {
        RuntimeSizeError::Overflow => format!(
            "`{}` exceeds the supported 64-bit raw-memory layout size",
            ty.pretty_print(db)
        ),
        RuntimeSizeError::UnavailableConcrete => {
            "concrete type size could not be determined".to_string()
        }
        RuntimeSizeError::InvalidType(cause) => match cause.const_eval_fault() {
            Some((body, expr, message)) => {
                diagnostic.push_secondary(
                    message.clone(),
                    SemanticDiagnosticSpan::HirExpr { body, expr },
                );
                message
            }
            None => format!("invalid const value: {}", cause.pretty_print(db)),
        },
    };
    if diagnostic.secondaries.is_empty() {
        diagnostic.primary.message = message;
    }
    SemanticDiagnosticId::new(db, diagnostic)
}

fn specialize_provider_address_space<'db>(
    db: &'db dyn HirAnalysisDb,
    specialization: &mut EffectProviderSpecialization<'db>,
    address_space: AddressSpace,
) {
    let provider = &specialization.provider;
    let semantics = provider_semantics_for_specialized_call(
        db,
        provider.provider_ty,
        provider.semantics.target_ty,
        Some(address_space),
        provider.semantics.transport,
    );
    specialization.provider.semantics = semantics;
}

/// The body whose types a concrete instance's layout demand reads.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub(crate) enum LayoutDemandBody {
    Admitted,
    Provisional,
    /// Lowering is blocked, so only expression types are read.
    Expressions,
}

#[salsa::tracked]
impl<'db> SemanticInstance<'db> {
    #[salsa::tracked(returns(copy))]
    pub fn assumptions(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        instantiated_effect_env(db, self).map_or_else(
            || semantic_instance_base_assumptions_for_key(db, self.key(db)),
            |env| env.assumptions(db),
        )
    }

    #[salsa::tracked(cycle_initial=call_sites_cycle_initial)]
    pub fn call_sites(self, db: &'db dyn HirAnalysisDb) -> Vec<Option<CallSiteLowering<'db>>> {
        final_call_site_data(db, self).call_sites.clone()
    }

    #[salsa::tracked(cycle_initial=for_loop_call_sites_cycle_initial)]
    pub fn for_loop_call_sites(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<Option<ForLoopCallSites<'db>>> {
        final_call_site_data(db, self).for_loop_call_sites.clone()
    }

    #[salsa::tracked(returns(copy))]
    pub fn call_site_finalization_diagnostic(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<crate::analysis::semantic::SemanticDiagnosticId<'db>> {
        final_call_site_data(db, self).diagnostic
    }

    #[salsa::tracked]
    pub(crate) fn provisional_body(self, db: &'db dyn HirAnalysisDb) -> SemanticBody<'db> {
        let key = self.key(db);
        let typed_body = key.typed_body(db);
        let call_sites = &provisional_call_sites(db, self).sites;
        let for_loop_call_sites = &provisional_for_loop_call_sites(db, self).sites;
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

    #[salsa::tracked(returns(clone))]
    pub fn binding_role(
        self,
        db: &'db dyn HirAnalysisDb,
        binding: LocalBinding<'db>,
    ) -> SemanticLocalRole<'db> {
        classify_binding_role(
            db,
            self,
            binding,
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
            binding,
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
            LocalBinding::EffectParam { site, idx, .. } => {
                let requirement = EffectEnvView::new(site)
                    .requirements(db)
                    .into_iter()
                    .chain(
                        row_expansion_for_key(db, self.key(db))
                            .components
                            .iter()
                            .map(|component| component.requirement.clone()),
                    )
                    .find(|requirement| requirement.binding_idx as usize == idx);
                let requirement_ty = requirement
                    .as_ref()
                    .and_then(|requirement| requirement.key.binding_ty(db))
                    .and_then(|ty| instantiate_normalized_ty(db, self.key(db), ty).ok());
                let provider_ty =
                    provisional_provider_binding_for_instance_effect(db, self, binding)
                        .map(|provider| provider.provider_ty);
                match requirement.as_ref().map(|requirement| &requirement.key) {
                    Some(EffectRequirementKey::Trait(_)) => provider_ty.or(requirement_ty),
                    Some(
                        EffectRequirementKey::Type(_)
                        | EffectRequirementKey::Row(_)
                        | EffectRequirementKey::Other,
                    ) => requirement_ty.or(provider_ty),
                    None => None,
                }
                .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
            }
            LocalBinding::Local { .. }
            | LocalBinding::Param { .. }
            | LocalBinding::ParamPart { .. } => {
                self.key(db).typed_body(db).binding_carrier_ty(db, binding)
            }
        }
    }

    /// The effect requirements a call to the instance supplies: its declared
    /// ones other than rows, then the effects its rows expand to.
    pub fn call_effect_requirements(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<EffectRequirement<'db>> {
        let key = self.key(db);
        effect_param_site(key.owner(db))
            .map(|site| EffectEnvView::new(site).requirements(db))
            .unwrap_or_default()
            .into_iter()
            .filter(|requirement| requirement.key.key_row().is_none())
            .chain(
                row_expansion_for_key(db, key)
                    .components
                    .iter()
                    .filter(|component| component.requirement.key.key_row().is_none())
                    .map(|component| component.requirement.clone()),
            )
            .collect()
    }

    /// The bindings of the instance's effects: its owner's declared effects,
    /// then the components of the rows they name.
    pub fn effect_bindings(self, db: &'db dyn HirAnalysisDb) -> Vec<LocalBinding<'db>> {
        let mut bindings = owner_effect_bindings(db, self.key(db).owner(db));
        bindings.extend(self.row_component_bindings(db));
        bindings
    }

    /// The bindings of the components of the rows the instance's effects
    /// name.
    pub fn row_component_bindings(self, db: &'db dyn HirAnalysisDb) -> Vec<LocalBinding<'db>> {
        row_expansion_for_key(db, self.key(db))
            .components
            .iter()
            .filter(|component| {
                matches!(
                    component.requirement.key.kind(),
                    EffectKeyKind::Type | EffectKeyKind::Trait
                )
            })
            .map(|component| LocalBinding::EffectParam {
                site: component.requirement.binding_site,
                idx: component.requirement.binding_idx as usize,
                binding_name: component.requirement.binding_name,
                provider_idx: component.requirement.binding_idx,
                is_mut: component.requirement.is_mut,
            })
            .collect()
    }

    #[salsa::tracked(returns(copy))]
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
            LocalBinding::Param { ty, .. } if self.binding_is_session_copy(db, binding) => ty,
            LocalBinding::Local { .. }
            | LocalBinding::Param { .. }
            | LocalBinding::ParamPart { .. } => {
                self.key(db).typed_body(db).binding_carrier_ty(db, binding)
            }
        }
    }

    /// Whether `binding` is a projection's snapshot view parameter (a scalar
    /// or handle): a copy the session owns, never the caller's place. A copy
    /// of code or calldata, which nothing writes, is the place itself.
    pub fn binding_is_session_copy(
        self,
        db: &'db dyn HirAnalysisDb,
        binding: LocalBinding<'db>,
    ) -> bool {
        matches!(
            binding,
            LocalBinding::Param {
                mode: FuncParamMode::View,
                ty,
                ..
            } if self.is_projection(db)
                && ty_is_snapshot(db, self.normalization_scope(db), ty, self.assumptions(db))
                && self.key(db).typed_body(db).input_index(binding).is_some_and(|input| {
                    !matches!(
                        self.param_space(db, input as u32),
                        AddressSpace::Code | AddressSpace::Calldata
                    )
                })
        )
    }

    /// For an instance of a `Contract` metadata builtin, which part of which
    /// contract's code it measures: the contract is its type argument.
    pub fn contract_metadata(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(ContractMetadataKind, Contract<'db>)> {
        let key = self.key(db);
        let BodyOwner::Func(func, _) = key.owner(db) else {
            return None;
        };
        let kind = contract_metadata_kind(db, func)?;
        let contract = key
            .subst(db)
            .generic_args(db)
            .iter()
            .find_map(|ty| ty.as_contract(db))?;
        Some((kind, contract))
    }

    pub fn is_projection(self, db: &'db dyn HirAnalysisDb) -> bool {
        matches!(self.key(db).owner(db), BodyOwner::Func(func, _) if func.is_projection(db))
    }

    /// The address space of the place data input `param` (a parameter, or a
    /// product parameter's part) names in this instantiation: the caller's,
    /// or else its type's default.
    pub fn param_space(self, db: &'db dyn HirAnalysisDb, param: u32) -> AddressSpace {
        let key = self.key(db);
        key.effect_providers(db)
            .param_spaces(db)
            .iter()
            .find(|(index, _)| *index == param)
            .map_or_else(
                || match key.typed_body(db).input_bindings().get(param as usize) {
                    Some(LocalBinding::Param { ty, .. } | LocalBinding::ParamPart { ty, .. }) => {
                        let ty = self.normalized_ty(db, *ty);
                        ty.as_capability(db)
                            .map_or(ty, |(_, inner)| inner)
                            .default_space(db)
                    }
                    _ => AddressSpace::Memory,
                },
                |(_, space)| *space,
            )
    }

    #[salsa::tracked(returns(copy))]
    pub fn normalized_ty(self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        normalize_ty(db, ty, self.normalization_scope(db), self.assumptions(db))
    }

    /// The state space `ty` is pinned to (`core::marker::Pinned`), where a
    /// collection's entries lie, in this instance.
    #[salsa::tracked(returns(copy))]
    pub fn pinned_space(self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<AddressSpace> {
        crate::analysis::ty::provider::pinned_space(
            db,
            self.normalization_scope(db),
            self.assumptions(db),
            self.normalized_ty(db, ty),
        )
    }

    /// How `ty` packs its elements into lanes when its elements are places
    /// (`core::ops::StateIndex`), in this instance.
    #[salsa::tracked(returns(copy))]
    pub fn state_index_lanes(
        self,
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
    ) -> Option<crate::analysis::ty::provider::StateIndexLanes<'db>> {
        crate::analysis::ty::provider::state_index_lanes(
            db,
            self.normalization_scope(db),
            self.assumptions(db),
            self.normalized_ty(db, ty),
        )
    }

    /// The key and element types of `ty` when its elements are places
    /// (`core::ops::StateIndex`), in this instance.
    #[salsa::tracked(returns(copy))]
    pub fn state_index_tys(
        self,
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
    ) -> Option<(TyId<'db>, TyId<'db>)> {
        crate::analysis::ty::state_index_tys(
            db,
            self.normalization_scope(db),
            ty,
            self.assumptions(db),
        )
    }

    #[salsa::tracked]
    pub fn normalized_field_types(
        self,
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
    ) -> ThinVec<TyId<'db>> {
        let ty = self.normalized_ty(db, ty);
        if ty.is_tuple(db) || ty.as_closure(db).is_some() {
            return ty
                .field_types(db)
                .into_iter()
                .map(|field| self.normalized_ty(db, field))
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

    #[salsa::tracked]
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

    #[salsa::tracked(returns(copy))]
    pub fn normalized_binding_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        binding: LocalBinding<'db>,
    ) -> TyId<'db> {
        self.normalized_ty(db, self.binding_ty(db, binding))
    }

    #[salsa::tracked(returns(copy))]
    /// The semantic-IR result type: a projection returns the carriers of its
    /// grants.
    pub fn normalized_result_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        if let BodyOwner::Func(func, mode) = self.key(db).owner(db)
            && let Some(shape) = func.return_shape(db, mode)
            && let Ok(ty) = instantiate_normalized_ty(db, self.key(db), shape.carrier_ty(db))
        {
            return ty;
        }
        self.normalized_ty(db, self.key(db).typed_body(db).result_ty())
    }

    #[salsa::tracked]
    pub fn body(self, db: &'db dyn HirAnalysisDb) -> SemanticBody<'db> {
        lower_semantic_body(db, self)
    }

    #[salsa::tracked]
    pub fn callees(self, db: &'db dyn HirAnalysisDb) -> Vec<SemanticCalleeRef<'db>> {
        collect_semantic_callees(db, self)
    }

    /// Borrow checking and runtime lowering consume concrete layouts, so a
    /// concrete instance demands the layout of every type its body consumes:
    /// each expression's and, given the semantic `body`, each local's
    /// (parameters and bindings included) and each callee parameter's. A
    /// deferred fault in a specialized extent, whether this body writes it or
    /// receives it through a callee signature or a field, rejects the body
    /// with its source, even when the value is unused or empty. Admission
    /// demands this before canonicalization folds any value of such a type,
    /// and from expression types alone when lowering is blocked. Compile-time
    /// evaluation forces the same extents itself when it materializes them.
    #[salsa::tracked(returns(copy))]
    pub(crate) fn concrete_layout_diagnostic(
        self,
        db: &'db dyn HirAnalysisDb,
        body: LayoutDemandBody,
    ) -> Option<SemanticDiagnosticId<'db>> {
        let body = match body {
            LayoutDemandBody::Admitted => Some(self.body(db)),
            LayoutDemandBody::Provisional => Some(self.provisional_body(db)),
            LayoutDemandBody::Expressions => None,
        };
        let key = self.key(db);
        let typed_body = key.typed_body(db);
        let at = move |origin| SemanticDiagnosticSpan::Origin {
            owner: key.owner(db),
            origin,
        };
        let exprs = typed_body.body().into_iter().flat_map(|hir| {
            hir.exprs(db).iter().map(move |(expr, _)| {
                (
                    self.normalized_ty(db, typed_body.expr_ty(db, expr)),
                    at(SemOrigin::Expr(expr)),
                )
            })
        });
        // A place carrier's own type is a reference; its layout is the value's.
        let locals = body
            .into_iter()
            .flat_map(|body| body.locals.iter().enumerate())
            .map(|(index, local)| {
                (
                    self.normalized_ty(db, local.role.layout_ty(local.ty)),
                    SemanticDiagnosticSpan::LocalSourceOrBody {
                        instance: self,
                        local: SLocalId::new(index),
                    },
                )
            });
        // Normalization reads each argument's target type from the callee.
        let params = body
            .into_iter()
            .flat_map(|body| &body.blocks)
            .flat_map(|block| &block.stmts)
            .filter_map(|stmt| match &stmt.kind {
                SStmtKind::Assign {
                    expr: SExpr::Call { callee, args, .. },
                    ..
                } => Some((stmt.origin, SemanticInstance::new(db, callee.key), args)),
                _ => None,
            })
            .flat_map(|(origin, callee, args)| {
                let typed_body = callee.key(db).typed_body(db);
                args.iter().enumerate().filter_map(move |(index, _)| {
                    Some((
                        callee.normalized_binding_ty(db, *typed_body.input_bindings().get(index)?),
                        at(origin),
                    ))
                })
            });
        let mut demanded = FxHashSet::default();
        exprs.chain(locals).chain(params).find_map(|(ty, span)| {
            let cause = demanded
                .insert(ty)
                .then(|| concrete_layout_fault(db, ty, &|ty| self.normalized_ty(db, ty)))??;
            // A blocked body's other invalid types were reported upstream.
            if body.is_none() && cause.const_eval_fault().is_none() {
                return None;
            }

            Some(invalid_size_diagnostic(
                db,
                self,
                span,
                ty,
                RuntimeSizeError::InvalidType(cause),
            ))
        })
    }
}

impl<'db> SemanticInstance<'db> {
    fn ensure_body_admitted(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Result<(), SemanticBodyAdmissionError<'db>> {
        let typed_body = self.key(db).typed_body(db);
        let causes = typed_body.smir_lowering_issues(db).into_boxed_slice();
        if causes.iter().copied().any(SmirLoweringIssue::is_incomplete) {
            return Err(SemanticBodyAdmissionError::IncompleteLoweringPlan(causes));
        }
        if !causes.is_empty() {
            return Err(SemanticBodyAdmissionError::BlockedByUpstreamDiagnostics(
                causes,
            ));
        }

        if let Some(body) = typed_body.body() {
            for (expr, _) in body.exprs(db).iter() {
                let Some(SemanticExprLowering::ConstIntrinsic { callable, kind }) =
                    typed_body.semantic_expr_lowering(expr)
                else {
                    continue;
                };
                let Some(arg) = callable.generic_args().first().copied() else {
                    continue;
                };
                let arg = normalize_ty(db, arg, body.scope(), self.assumptions(db));
                let origin = SemOrigin::Expr(expr);
                match kind {
                    ConstIntrinsicKind::SizeOf => {
                        if let Err(error) = runtime_size_bytes(db, arg) {
                            return Err(SemanticBodyAdmissionError::InvalidConcreteType(
                                invalid_size_diagnostic(
                                    db,
                                    self,
                                    SemanticDiagnosticSpan::Origin {
                                        owner: self.key(db).owner(db),
                                        origin,
                                    },
                                    arg,
                                    error,
                                ),
                            ));
                        }
                    }
                    ConstIntrinsicKind::StorageSlots => {
                        if let Err(error) = storage_slot_span(db, arg) {
                            return Err(SemanticBodyAdmissionError::InvalidConcreteType(
                                SemanticDiagnosticId::new(
                                    db,
                                    SemanticDiagnostic::new(
                                        self,
                                        SemanticDiagnosticKind::InvalidConcreteType,
                                        format!(
                                            "`{}` cannot be laid out in storage: {}",
                                            arg.pretty_print(db),
                                            error.summary()
                                        ),
                                        SemanticDiagnosticSpan::Origin {
                                            owner: self.key(db).owner(db),
                                            origin,
                                        },
                                    ),
                                ),
                            ));
                        }
                    }
                }
            }
        }
        if let Some(diag) = instance_limit_violation(db, self) {
            return Err(SemanticBodyAdmissionError::InstanceLimit(diag));
        }
        Ok(())
    }

    pub(crate) fn admitted_body(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Result<&'db SemanticBody<'db>, SemanticBodyAdmissionError<'db>> {
        self.ensure_body_admitted(db)?;
        if let Some(diag) = self.call_site_finalization_diagnostic(db) {
            return Err(SemanticBodyAdmissionError::CallSiteFinalization(diag));
        }
        Ok(self.body(db))
    }

    pub(crate) fn admitted_provisional_body(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Result<&'db SemanticBody<'db>, SemanticBodyAdmissionError<'db>> {
        self.ensure_body_admitted(db)?;
        if let Some(diagnostic) = provisional_call_site_diagnostic(db, self) {
            return Err(SemanticBodyAdmissionError::CallSiteFinalization(diagnostic));
        }
        Ok(self.provisional_body(db))
    }

    fn normalization_scope(self, db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        self.key(db).owner(db).scope()
    }

    pub fn is_intrinsically_never_returning(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.is_nonreturning_builtin(db) || self.normalized_result_ty(db).is_never(db)
    }

    fn is_nonreturning_builtin(self, db: &'db dyn HirAnalysisDb) -> bool {
        let BodyOwner::Func(func, _) = self.key(db).owner(db) else {
            return false;
        };
        matches!(
            runtime_builtin_func_kind(db, func),
            Some(
                RuntimeBuiltinFuncKind::ReturnData
                    | RuntimeBuiltinFuncKind::Revert
                    | RuntimeBuiltinFuncKind::RevertEmpty
                    | RuntimeBuiltinFuncKind::SelfDestruct
                    | RuntimeBuiltinFuncKind::Stop
                    | RuntimeBuiltinFuncKind::Panic
                    | RuntimeBuiltinFuncKind::PanicWithValue
                    | RuntimeBuiltinFuncKind::Todo
            )
        )
    }
}

/// The first call, in `instance` or in any instance it makes, that makes an
/// instance whose generic arguments nest more than [`PROJECTION_DEPTH_LIMIT`]
/// levels deep or have more than [`PROJECTION_WORK_LIMIT`] type nodes.
///
/// Recursion that grows its generic arguments, as `f<T>` calling
/// `f<(T, T)>`, makes a new instance at every call. Bounding each instance's
/// arguments ends every chain of such calls, and rejecting every instance
/// that reaches a rejected one stops at the first chain to end: recursion
/// growing its arguments in two ways makes exponentially many instances
/// within the bounds, of which this follows one chain.
#[salsa::tracked(returns(copy), cycle_initial = instance_limit_violation_cycle_initial)]
fn instance_limit_violation<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Option<SemanticDiagnosticId<'db>> {
    if !instance
        .key(db)
        .typed_body(db)
        .smir_lowering_issues(db)
        .is_empty()
    {
        return None;
    }
    // Growth a function's own calls make is visible in its identity instance,
    // where `{ N + 1 }` nests though concrete values do not grow.
    if let BodyOwner::Func(_, _) = instance.key(db).owner(db) {
        let identity = identity_semantic_instance_key(db, instance.key(db).owner(db));
        if identity != instance.key(db)
            && let Some(diag) = instance_limit_violation(db, SemanticInstance::new(db, identity))
        {
            return Some(diag);
        }
    }
    let calls = provisional_call_sites(db, instance)
        .sites
        .iter()
        .enumerate()
        .filter_map(|(idx, site)| {
            Some((SemOrigin::Expr(ExprId::new(idx)), site.as_ref()?.callee?))
        });
    let loops = provisional_for_loop_call_sites(db, instance)
        .sites
        .iter()
        .enumerate()
        .flat_map(|(idx, sites)| {
            sites.iter().flat_map(move |sites| {
                sites
                    .sites
                    .iter()
                    .filter_map(move |site| Some((SemOrigin::Stmt(StmtId::new(idx)), site.callee?)))
            })
        });
    let calls: Vec<_> = calls.chain(loops).collect();
    for &(origin, callee) in &calls {
        let args = callee.key.subst(db).generic_args(db);
        let within = args
            .iter()
            .all(|&arg| tree_depth_within(db, arg, PROJECTION_DEPTH_LIMIT))
            && args
                .iter()
                .try_fold(0usize, |total, &arg| {
                    let size = tree_size_within(db, arg, PROJECTION_WORK_LIMIT - total)?;
                    Some(total + size)
                })
                .is_some();
        if !within {
            let callee = checker_name(db, SemanticInstance::new(db, callee.key));
            return Some(SemanticDiagnosticId::new(
                db,
                SemanticDiagnostic::new(
                    instance,
                    SemanticDiagnosticKind::InstanceLimit,
                    format!(
                        "this call instantiates `{callee}` with generic arguments that nest more \
                         than {PROJECTION_DEPTH_LIMIT} levels deep or have more than \
                         {PROJECTION_WORK_LIMIT} type nodes"
                    ),
                    SemanticDiagnosticSpan::Origin {
                        owner: instance.key(db).owner(db),
                        origin,
                    },
                ),
            ));
        }
    }
    calls
        .iter()
        .find_map(|(_, callee)| instance_limit_violation(db, SemanticInstance::new(db, callee.key)))
}

fn instance_limit_violation_cycle_initial<'db>(
    _: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _: SemanticInstance<'db>,
) -> Option<SemanticDiagnosticId<'db>> {
    None
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

#[salsa::tracked(returns(clone))]
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
        LocalBinding::Local { .. }
        | LocalBinding::Param { .. }
        | LocalBinding::ParamPart { .. } => return None,
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
            .find(|provider| provider.provider_idx == provider_idx)
            .cloned()
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
        LocalBinding::Local { .. }
        | LocalBinding::Param { .. }
        | LocalBinding::ParamPart { .. } => None,
    }
}

pub(crate) fn provisional_provider_idx_for_requirement<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
    requirement_idx: u32,
) -> Option<u32> {
    match site {
        EffectParamSite::Func(func, _) => place_effect_provider_param_index_map(db, func)
            .get(requirement_idx as usize)
            .and_then(|param_idx| param_idx.map(|_| requirement_idx))
            .or_else(|| {
                registered_root_providers(db, site)
                    .first()
                    .map(RootProviderRegistration::func_provider_idx)
            }),
        EffectParamSite::Contract(contract)
        | EffectParamSite::ContractInit { contract }
        | EffectParamSite::ContractRecvArm { contract, .. } => {
            let field_provider_idx = contract
                .storage_layout(db)
                .valid_fields()
                .enumerate()
                .map(|(provider_idx, field)| (field.field.index, provider_idx as u32))
                .collect::<IndexMap<_, _>>();
            let fields = contract.fields(db);
            let requirement = EffectEnvView::new(site)
                .requirements(db)
                .into_iter()
                .find(|requirement| requirement.binding_idx == requirement_idx)?;
            if let Some(binding_path) = requirement.binding_path(db)
                && binding_path.len(db) == 1
                && let Some(name) = binding_path.ident(db).to_opt()
                && let Some(field) = fields.get(&name)
                && let Some(provider_idx) = field_provider_idx.get(&field.index).copied()
            {
                return Some(provider_idx);
            }
            Some(field_provider_idx.len() as u32)
        }
        EffectParamSite::Closure(_) => Some(requirement_idx),
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
        EffectParamSite::Func(func, _) => {
            let provider_map = place_effect_provider_param_index_map(db, func);
            if provider_map
                .get(requirement_idx as usize)
                .is_some_and(Option::is_some)
                && provider_idx == requirement_idx
            {
                let provider_param_idx = provider_map[requirement_idx as usize]?;
                let provider_ty = *CallableDef::Func(func).params(db).get(provider_param_idx)?;
                let requirement = effect_requirements_for_site(db, site)
                    .into_iter()
                    .find(|requirement| requirement.binding_idx == requirement_idx)?;
                return Some(ProviderBinding {
                    provider_idx,
                    provider_ty,
                    is_mut,
                    source: ProviderSource::UsesParam {
                        site: site.slot_site(db, requirement_idx),
                        requirement_idx,
                    },
                    semantics: effect_param_provider_semantics(db, provider_ty, &requirement.key),
                });
            }
            provisional_root_provider_binding(db, key, site, provider_idx)
        }
        EffectParamSite::Contract(contract)
        | EffectParamSite::ContractInit { contract }
        | EffectParamSite::ContractRecvArm { contract, .. } => {
            if let Some(field) = contract
                .storage_layout(db)
                .valid_fields()
                .nth(provider_idx as usize)
            {
                let provider_ty = field.declared;
                return Some(ProviderBinding {
                    provider_idx,
                    provider_ty,
                    is_mut: true,
                    source: ProviderSource::ContractField { field: field.field },
                    semantics: crate::analysis::ty::provider::ProviderSemantics {
                        provider_ty,
                        kind: ProviderKind::Place,
                        address_space: Some(field.address_space),
                        target_ty: Some(provider_ty),
                        transport: ProviderTransport::ByValue,
                    },
                });
            }
            provisional_root_provider_binding(db, key, site, provider_idx)
        }
        EffectParamSite::Closure(_) => EffectEnvView::new(site)
            .providers(db)
            .into_iter()
            .find(|provider| provider.provider_idx == provider_idx)
            .and_then(|provider| instantiate_provider_binding(db, key, provider).ok()),
    }
}

fn provisional_root_provider_binding<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    site: EffectParamSite<'db>,
    provider_idx: u32,
) -> Option<ProviderBinding<'db>> {
    EffectEnvView::new(site)
        .providers(db)
        .into_iter()
        .find(|provider| {
            provider.provider_idx == provider_idx
                && matches!(provider.source, ProviderSource::RootProvider { .. })
        })
        .and_then(|provider| instantiate_provider_binding(db, key, provider).ok())
}

fn effect_binding_ty_from_env<'db>(
    db: &'db dyn HirAnalysisDb,
    env: Option<InstantiatedEffectEnv<'db>>,
    idx: usize,
    provider_idx: Option<u32>,
) -> TyId<'db> {
    let Some(env) = env else {
        return TyId::invalid(db, InvalidCause::Other);
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
        Some(EffectRequirementKey::Trait(_)) => provider
            .map(|binding| binding.provider_ty)
            .or_else(|| requirement.and_then(|requirement| requirement.key.binding_ty(db))),
        Some(
            EffectRequirementKey::Type(_)
            | EffectRequirementKey::Row(_)
            | EffectRequirementKey::Other,
        ) => requirement
            .and_then(|requirement| requirement.key.binding_ty(db))
            .or_else(|| provider.map(|binding| binding.provider_ty)),
        None => None,
    }
    .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
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

fn specialized_root_provider_target_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    requirement: &EffectRequirement<'db>,
    root_provider: &ProviderBinding<'db>,
) -> Option<TyId<'db>> {
    match requirement.key {
        EffectRequirementKey::Trait(_) => Some(root_provider.provider_ty),
        EffectRequirementKey::Type(_)
        | EffectRequirementKey::Row(_)
        | EffectRequirementKey::Other => requirement
            .key
            .binding_ty(db)
            .or(root_provider.semantics.target_ty),
    }
}

pub fn root_semantic_instance_key<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Result<SemanticInstanceKey<'db>, RootSemanticInstanceError<'db>> {
    let generic_args = root_owner_generic_args(db, owner)?;
    let effect_providers = root_owner_effect_providers(db, owner);
    let subst = match owner {
        BodyOwner::Func(func, _) => GenericSubst::for_owner(db, func.into(), generic_args),
        BodyOwner::Const(_)
        | BodyOwner::AnonConstBody { .. }
        | BodyOwner::ContractInit { .. }
        | BodyOwner::ContractRecvArm { .. }
        | BodyOwner::Closure { .. } => GenericSubst::none(db),
    };
    let key = SemanticInstanceKey::of(
        db,
        owner,
        subst,
        EffectProviderSubst::new(db, effect_providers, Vec::new()),
        root_impl_env(db, owner, subst),
        EvidenceList::none(db),
    );
    validate_instantiated_effect_env_key(db, key)
        .map_err(RootSemanticInstanceError::UnclosedEffectEnv)?;
    Ok(key)
}

pub fn identity_semantic_instance_key<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> SemanticInstanceKey<'db> {
    let subst = GenericSubst::for_body_owner(db, owner, Vec::new());
    SemanticInstanceKey::of(
        db,
        owner,
        subst,
        EffectProviderSubst::empty(db),
        root_impl_env(db, owner, subst),
        EvidenceList::none(db),
    )
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
    collect_callees(instance.call_sites(db), instance.for_loop_call_sites(db))
}

fn collect_callees<'db>(
    call_sites: &[Option<CallSiteLowering<'db>>],
    for_loop_call_sites: &[Option<ForLoopCallSites<'db>>],
) -> Vec<SemanticCalleeRef<'db>> {
    let mut seen = FxHashSet::default();
    let mut callees = Vec::new();
    for site in call_sites.iter().flatten().chain(
        for_loop_call_sites
            .iter()
            .flatten()
            .flat_map(|sites| &sites.sites),
    ) {
        if let Some(callee) = site.callee
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
    // A read-only effect of a snapshot type provided by a memory place is
    // passed as a copy, as a view parameter is: nothing can write the place
    // during the call, and a reentrant call has memory of its own.
    if let Some(provider) = &provider
        && matches!(binding, LocalBinding::EffectParam { is_mut: false, .. })
        && provider.semantics.kind == ProviderKind::Place
        && provider.semantics.address_space == Some(AddressSpace::Memory)
        && ty_is_snapshot(db, scope, ty, assumptions)
    {
        return SemanticLocalRole::DirectValue {
            provenance: ValueProvenance::Ordinary,
        };
    }
    if let Some(provider) = provider {
        return match provider.semantics.kind {
            ProviderKind::RootObject => SemanticLocalRole::DirectValue {
                provenance: ValueProvenance::RootProvider(provider),
            },
            ProviderKind::Place => SemanticLocalRole::PlaceBoundValue {
                provenance: PlaceProvenance::RootProvider(provider),
                value_ty: ty,
            },
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

/// The rows an instance's effects name. Their components are effect
/// requirements of the instance's own, numbered after its declared ones,
/// each resolved to the provider of the same index.
#[salsa::tracked]
pub fn row_expansion_for_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
) -> RowExpansion<'db> {
    let Some(site) = effect_param_site(key.owner(db)) else {
        return RowExpansion::default();
    };
    let Ok(requirements) = EffectEnvView::new(site)
        .requirements(db)
        .into_iter()
        .map(|requirement| instantiate_effect_requirement(db, key, requirement))
        .collect::<Result<Vec<_>, _>>()
    else {
        return RowExpansion::default();
    };
    expand_rows(
        db,
        &requirements,
        key.owner(db).scope(),
        semantic_instance_base_assumptions_for_key(db, key),
    )
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
    let components = &row_expansion_for_key(db, key).components;
    let requirements = view
        .requirements(db)
        .into_iter()
        .map(|requirement| instantiate_effect_requirement(db, key, requirement))
        .chain(
            components
                .iter()
                .map(|component| Ok(component.requirement.clone())),
        )
        .collect::<Result<Vec<_>, _>>()?;
    // A component that is a row forwarded unexpanded has no provider, as a
    // declared row has none.
    let mut resolutions = view.resolutions(db);
    resolutions.extend(
        components
            .iter()
            .filter(|component| {
                matches!(
                    component.requirement.key.kind(),
                    EffectKeyKind::Type | EffectKeyKind::Trait
                )
            })
            .map(|component| ResolvedEffectBinding {
                requirement_idx: component.requirement.binding_idx,
                provider_idx: component.requirement.binding_idx,
            }),
    );
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
    for provider in key.effect_providers(db).providers(db) {
        specializations.insert(
            provider.provider_idx,
            instantiate_provider_binding(db, key, provider.clone())?,
        );
    }
    if matches!(
        site,
        crate::analysis::ty::ty_check::EffectParamSite::Func(..)
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
    let mut providers = canonical
        .into_iter()
        .map(|provider| {
            specializations
                .remove(&provider.provider_idx)
                .map_or_else(|| instantiate_provider_binding(db, key, provider), Ok)
        })
        .collect::<Result<Vec<_>, _>>()?;
    // The providers a call gave the instance's row components.
    providers.extend(
        row_expansion_for_key(db, key)
            .components
            .iter()
            .filter_map(|component| specializations.remove(&component.requirement.binding_idx)),
    );
    Ok(providers)
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
) -> Vec<TraitInstId<'db>> {
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
        BodyOwner::Func(func, mode) => root_func_generic_args(db, func, mode),
        BodyOwner::Const(_)
        | BodyOwner::AnonConstBody { .. }
        | BodyOwner::ContractInit { .. }
        | BodyOwner::ContractRecvArm { .. }
        | BodyOwner::Closure { .. } => Ok(Vec::new()),
    }
}

fn root_owner_effect_providers<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Vec<ProviderBinding<'db>> {
    let BodyOwner::Func(func, _) = owner else {
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
                scope: RootProviderScope::Func(provider_func),
                ..
            } if provider_func == func
        )
    });
    let provider_slots = providers
        .iter()
        .filter_map(|provider| match provider.source {
            ProviderSource::UsesParam {
                site: provider_site,
                requirement_idx,
            } if provider_site == site.slot_site(db, requirement_idx) => {
                Some((requirement_idx, provider.clone()))
            }
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
                    specialized_root_provider_target_ty(db, &requirement, root_provider),
                )
            } else {
                let target_ty = requirement.key.binding_ty(db)?;
                let provider_ty = if requirement.is_mut {
                    TyId::borrow_mut_of(db, target_ty)
                } else {
                    TyId::borrow_ref_of(db, target_ty)
                };
                (provider_ty, slot.source.clone(), Some(target_ty))
            };
            Some(ProviderBinding {
                provider_idx: slot.provider_idx,
                provider_ty,
                is_mut: slot.is_mut,
                source,
                semantics: provider_semantics_for_specialized_call(
                    db,
                    provider_ty,
                    target_ty,
                    Some(AddressSpace::Memory),
                    ProviderTransport::ByValue,
                ),
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
        EffectRequirementKey::Type(provider_ty) => provider_ty == root_provider.provider_ty,
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
        EffectRequirementKey::Row(_) | EffectRequirementKey::Other => false,
    }
}

fn root_func_generic_args<'db>(
    db: &'db dyn HirAnalysisDb,
    func: crate::hir_def::Func<'db>,
    mode: Option<BorrowKind>,
) -> Result<Vec<TyId<'db>>, RootSemanticInstanceError<'db>> {
    let owner = BodyOwner::Func(func, mode);
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
        .map(|provider| (provider.provider_idx, provider.provider_ty))
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
        EffectRequirementKey::Row(row) => EffectRequirementKey::Row(RowKey {
            inst: instantiate_normalized_trait_inst(db, key, row.inst)?,
            row: row.row,
        }),
        EffectRequirementKey::Other => EffectRequirementKey::Other,
    })
}

fn instantiate_provider_binding<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    provider: ProviderBinding<'db>,
) -> Result<ProviderBinding<'db>, SemanticEffectEnvInstantiationError<'db>> {
    let provider_ty = instantiate_normalized_ty(db, key, provider.provider_ty)?;
    let source = match provider.source.clone() {
        ProviderSource::RootProvider {
            scope,
            registration,
        } => ProviderSource::RootProvider {
            scope,
            registration: RootProviderRegistration {
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
    let semantics = if matches!(provider.source, ProviderSource::ContractField { .. }) {
        crate::analysis::ty::provider::ProviderSemantics {
            provider_ty,
            target_ty,
            ..provider.semantics
        }
    } else {
        let mut semantics = provider_semantics_for_specialized_call(
            db,
            provider_ty,
            target_ty,
            provider.semantics.address_space,
            provider.semantics.transport,
        );
        // A root object lies where its binding says, which its type alone
        // cannot tell (`effect_param_provider_semantics`).
        if provider_ty.as_capability(db).is_none()
            && provider.semantics.provider_ty.as_capability(db).is_none()
        {
            semantics.address_space = provider.semantics.address_space;
        }
        semantics
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
    let ty = instantiate_checked(db, key.owner(db), ty, key.subst(db))?;
    Ok(normalize_ty(db, ty, scope, assumptions))
}

fn instantiate_normalized_trait_inst<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    trait_inst: TraitInstId<'db>,
) -> Result<TraitInstId<'db>, SemanticEffectEnvInstantiationError<'db>> {
    let scope = key.owner(db).scope();
    let assumptions = semantic_instance_base_assumptions_for_key(db, key);
    let trait_inst = instantiate_checked(db, key.owner(db), trait_inst, key.subst(db))?;
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
    Ok(TraitInstId::new(
        db,
        trait_inst.def(db),
        args,
        assoc_type_bindings,
        trait_inst.mode(db),
    ))
}

fn instantiate_checked<'db, T>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    value: T,
    subst: GenericSubst<'db>,
) -> Result<T, SemanticEffectEnvInstantiationError<'db>>
where
    T: TyFoldable<'db>,
{
    let Some(mapping) = subst.mapping(db).as_ref() else {
        return if matches!(owner, BodyOwner::Func(_, _)) {
            Err(SemanticEffectEnvInstantiationError::MissingDomain { owner })
        } else {
            Ok(value)
        };
    };
    if let BodyOwner::Func(func, _) = owner
        && mapping.domain().schema(db).owner(db) != func.into()
    {
        return Err(SemanticEffectEnvInstantiationError::WrongOwner {
            owner,
            schema: mapping.domain().schema(db),
        });
    }
    substitute_complete(db, value, mapping)
        .map_err(|error| SemanticEffectEnvInstantiationError::Generic { owner, error })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::semantic::identity_semantic_instance_key,
        test_db::{HirAnalysisTestDb, find_func},
    };

    #[test]
    fn provisional_plain_calls_equal_full_final_plans() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "plain_call_plans.fe".into(),
            "struct Item { n: u256 }\n\
             impl Item { fn read(ref self) -> u256 { self.n } }\n\
             struct Slot<const ROOT: u256> {}\n\
             fn target(_ slot: Slot<1>) {}\n\
             fn generic<T>(_ value: T) {}\n\
             fn caller<T>(value: T, slot: Slot<1>, item: own Item) {\n\
                 target(slot)\n\
                 generic(value)\n\
                 item.read()\n\
             }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = SemanticInstance::new(
            &db,
            identity_semantic_instance_key(
                &db,
                BodyOwner::Func(find_func(&db, module, "caller"), None),
            ),
        );
        assert_plain_call_plans(&db, instance);
    }

    #[salsa::tracked]
    fn assert_plain_call_plans<'db>(db: &'db dyn HirAnalysisDb, instance: SemanticInstance<'db>) {
        let assumptions = instance.assumptions(db);
        assert_eq!(
            assumptions,
            semantic_instance_base_assumptions_for_key(db, instance.key(db))
        );
        let typed_body = instance.key(db).typed_body(db);
        let body = typed_body.body().unwrap();
        let provisional = provisional_call_sites(db, instance);
        assert!(provisional.diagnostic.is_none());
        let mut calls = 0;
        for (expr, _) in body.exprs(db).iter() {
            let Some(SemanticExprLowering::Call { callable }) =
                typed_body.semantic_expr_lowering(expr)
            else {
                continue;
            };
            assert!(callable.trait_inst().is_none());
            assert!(callable.effect_providers().is_empty());
            let site = provisional.sites[expr.index()].as_ref().unwrap();
            let mut replanned = site.clone();
            replan_call_site(
                db,
                instance,
                callable,
                &mut replanned,
                typed_body.call_effect_args(expr).unwrap_or(&[]),
                None,
                SemOrigin::Expr(expr),
            )
            .unwrap();
            assert_eq!(site, &replanned);
            assert_eq!(instance.call_sites(db)[expr.index()].as_ref(), Some(site));
            calls += 1;
        }
        assert_eq!(calls, 3);
    }

    #[test]
    fn trait_calls_still_validate_selected_body_signatures() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "trait_call_plans.fe".into(),
            "trait T { fn value(self) -> bool }\n\
             impl T for bool { fn value(self) -> bool { self } }\n\
             fn caller(value: bool) -> bool { value.value() }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = SemanticInstance::new(
            &db,
            identity_semantic_instance_key(
                &db,
                BodyOwner::Func(find_func(&db, module, "caller"), None),
            ),
        );
        assert_trait_signature_validation(&db, instance);
    }

    #[salsa::tracked]
    fn assert_trait_signature_validation<'db>(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
    ) {
        let typed_body = instance.key(db).typed_body(db);
        let (expr, mut callable) = typed_body
            .body()
            .unwrap()
            .exprs(db)
            .iter()
            .find_map(|(expr, _)| match typed_body.semantic_expr_lowering(expr) {
                Some(SemanticExprLowering::Call { callable }) => Some((expr, callable.clone())),
                _ => None,
            })
            .unwrap();
        assert!(callable.trait_inst().is_some());
        // Keep the selected impl for bool, but corrupt the nominal Self argument.
        // Provisional planning permits it; final planning must reject it.
        callable.generic_args_mut()[0] = TyId::u256(db);
        let mut diagnostic = None;
        let mut site = provisional_call_site(
            db,
            instance,
            &callable,
            &[],
            instance.assumptions(db),
            SemOrigin::Expr(expr),
            &mut diagnostic,
        );
        assert!(diagnostic.is_none());
        let error = finalize_call_site(
            db,
            instance,
            &callable,
            &mut site,
            &[],
            None,
            SemOrigin::Expr(expr),
            true,
        )
        .unwrap_err();
        assert!(
            error
                .diag(db)
                .primary
                .message
                .contains("checked call signature mismatch")
        );
    }

    #[test]
    fn final_planning_applies_effect_address_space_refinements() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "refined_call_plans.fe".into(),
            "fn needs() uses (slot: u256) {}\n\
             fn caller() uses (slot: u256) { needs() }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = SemanticInstance::new(
            &db,
            identity_semantic_instance_key(
                &db,
                BodyOwner::Func(find_func(&db, module, "caller"), None),
            ),
        );
        assert_effect_refinement(&db, instance);
    }

    #[salsa::tracked]
    fn assert_effect_refinement<'db>(db: &'db dyn HirAnalysisDb, instance: SemanticInstance<'db>) {
        let typed_body = instance.key(db).typed_body(db);
        let (expr, callable) = typed_body
            .body()
            .unwrap()
            .exprs(db)
            .iter()
            .find_map(|(expr, _)| match typed_body.semantic_expr_lowering(expr) {
                Some(SemanticExprLowering::Call { callable }) => Some((expr, callable)),
                _ => None,
            })
            .unwrap();
        let mut site = provisional_call_sites(db, instance).sites[expr.index()]
            .clone()
            .unwrap();
        assert_eq!(site.effect_args.len(), 1);
        let before = site.clone();
        let refinement = CallSiteProviderRefinement {
            call_site: CallSiteId::Expr(expr),
            input: RefinedInput::Effect {
                binding_idx: site.effect_args[0].binding_idx,
                provider_idx: None,
            },
            address_space: AddressSpace::Storage,
        };
        finalize_call_site(
            db,
            instance,
            callable,
            &mut site,
            typed_body.call_effect_args(expr).unwrap(),
            Some(&[refinement]),
            SemOrigin::Expr(expr),
            true,
        )
        .unwrap();
        assert_eq!(site.effect_args[0].provider, Some(AddressSpace::Storage));
        assert_ne!(site, before);
    }
}
