use super::{
    EffectProviderSubst, GenericSubst, ImplEnv, SemanticInstance, SemanticInstanceKey,
    provisional_provider_binding_for_instance_effect, provisional_provider_idx_for_requirement,
    resolved_provider_binding_for_instance_effect, row_expansion_for_key,
};
use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{SemOrigin, SemanticConstRef},
        ty::{
            assoc_const::InherentConstUse,
            binder::Binder,
            closure::{callee_closure, callee_function_item, closure_template_ty},
            const_ty::{
                ConstCanonEnv, ConstCanonMode, canonicalize_ty_for_mode,
                inherent_const_body_and_impl_args, inherent_const_decl_ty,
            },
            corelib::resolve_core_trait,
            effects::{
                place_effect_provider_param_index_map,
                rows::{RowExpansion, RowKey, RowPath, expand_rows},
            },
            evidence::{Evidence, EvidenceList, impl_method_bound_evidence},
            fold::{TyFoldable, TyFolder},
            method_cmp::{normalize_compare_assoc_consts, normalize_predicate_for_comparison},
            normalize::normalize_ty,
            shape::Shape,
            trait_def::{ImplementorOrigin, MethodArgMapError, ResolvedImplInstance, TraitInstId},
            trait_resolution::{
                GoalSatisfiability, PredicateListId, TraitSolveCx,
                constraint::collect_func_decl_constraints, is_goal_satisfiable,
            },
            ty_check::{
                BodyOwner, Callable, ConstRef, EffectParamSite, EffectProviderSpecialization,
                ResolvedEffectArg,
            },
            ty_def::{BorrowKind, TyFlags, TyId},
            ty_lower::{ParamKey, ParamSchemaId},
            visitor::{TyVisitable, TyVisitor, collect_flags},
        },
    },
    core::semantic::{EffectEnvView, EffectRequirement, EffectRequirementKey, ProviderBinding},
    hir_def::{
        CallableDef, Const, Func, GenericParamOwner, HirIngot, IdentId, scope_graph::ScopeId,
    },
    span::DynLazySpan,
};
use common::indexmap::IndexSet;
use rustc_hash::FxHashMap;

#[derive(Clone, Copy)]
enum ProviderResolutionMode {
    Final,
    Provisional,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct InstantiatedMethodSignature<'db> {
    inputs: Vec<Shape<'db>>,
    result: TyId<'db>,
    /// Each effect's index, mutability, key and provider, with the rows the
    /// signature's instantiation selects expanded to their components.
    effects: Vec<(u32, bool, EffectRequirementKey<'db>, Option<TyId<'db>>)>,
}

impl<'db> TyVisitable<'db> for InstantiatedMethodSignature<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.inputs.visit_with(visitor);
        self.result.visit_with(visitor);
        for (_, _, key, provider_ty) in &self.effects {
            key.visit_with(visitor);
            if let Some(ty) = provider_ty {
                ty.visit_with(visitor);
            }
        }
    }
}

impl<'db> TyFoldable<'db> for InstantiatedMethodSignature<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let inputs = self.inputs.fold_with(db, folder);
        let result = self.result.fold_with(db, folder);
        let effects = self
            .effects
            .into_iter()
            .map(|(idx, is_mut, key, provider_ty)| {
                (
                    idx,
                    is_mut,
                    key.fold_with(db, folder),
                    provider_ty.map(|ty| ty.fold_with(db, folder)),
                )
            })
            .collect();
        Self {
            inputs,
            result,
            effects,
        }
    }
}

impl<'db> InstantiatedMethodSignature<'db> {
    fn describe(&self, db: &'db dyn HirAnalysisDb) -> String {
        let inputs = self
            .inputs
            .iter()
            .map(|shape| shape.pretty_print_product(db))
            .collect::<Vec<_>>();
        let effects = self
            .effects
            .iter()
            .map(|(_, is_mut, key, provider_ty)| {
                let key = match key {
                    EffectRequirementKey::Type(ty) => ty.pretty_print(db).to_string(),
                    EffectRequirementKey::Trait(inst) => inst.pretty_print(db, true),
                    EffectRequirementKey::Row(row) => format!(
                        "{}::{}",
                        row.inst.pretty_print(db, true),
                        row.name(db).map_or("<row>", |name| name.data(db))
                    ),
                    EffectRequirementKey::Other => "<unresolved>".to_string(),
                };
                format!(
                    "{}{key} via {}",
                    if *is_mut { "mut " } else { "" },
                    provider_ty
                        .map_or_else(|| "<none>".to_string(), |ty| ty.pretty_print(db).clone())
                )
            })
            .collect::<Vec<_>>();
        format!(
            "inputs={inputs:?}, result={}, effects={effects:?}",
            self.result.pretty_print(db)
        )
    }
}

fn instantiated_method_signature<'db>(
    db: &'db dyn HirAnalysisDb,
    (func, mode): (Func<'db>, Option<BorrowKind>),
    args: &[TyId<'db>],
    scope: ScopeId<'db>,
    evidence: PredicateListId<'db>,
    trait_inst: TraitInstId<'db>,
    rebase_same_trait_uses: bool,
) -> Result<InstantiatedMethodSignature<'db>, MethodArgMapError<'db>> {
    let schema = ParamSchemaId::full(db, func.into());
    let arg_tys = func.arg_tys(db);
    let params = func.params(db).collect::<Vec<_>>();
    if args.len() != schema.keys(db).len() || arg_tys.len() != params.len() {
        return Err(MethodArgMapError::SignatureArity {
            func,
            expected_generics: schema.keys(db).len(),
            given_generics: args.len(),
            declared_inputs: arg_tys.len(),
            parameter_inputs: params.len(),
        });
    }
    let mut normalize = |ty| {
        normalize_compare_assoc_consts(
            db,
            normalize_ty(db, ty, scope, evidence),
            scope,
            evidence,
            trait_inst,
            rebase_same_trait_uses,
        )
    };
    let (declared, spread) = CallableDef::Func(func).param_shapes(db, mode);
    let mut inputs: Vec<_> = declared
        .into_iter()
        .map(|shape| shape.instantiate(db, args).map_tys(db, &mut normalize))
        .collect();
    if let Some(spread) = spread {
        let shape = normalize(spread.instantiate(db, args));
        let listed = shape
            .as_params_shape(db)
            .ok_or(MethodArgMapError::SpreadNotListed { func, shape })?;
        inputs.extend(listed);
    }
    let result = normalize(CallableDef::Func(func).ret_ty(db).instantiate(db, args));
    let provider_slots = place_effect_provider_param_index_map(db, func);
    let (requirements, rows) = instantiated_effects(db, (func, mode), args, scope, evidence);
    // A row's components take their providers from the call.
    let effects = requirements
        .iter()
        .filter(|requirement| {
            requirement.key.key_row().is_none() || rows.is_abstract(requirement.binding_idx)
        })
        .map(|requirement| {
            let provider_ty = provider_slots
                .get(requirement.binding_idx as usize)
                .and_then(|slot| *slot)
                .filter(|_| !func.effect_from_row(db, requirement.binding_idx as usize))
                .and_then(|slot| args.get(slot))
                .copied()
                .map(normalize);
            (requirement, provider_ty)
        })
        .chain(
            rows.components
                .iter()
                .map(|component| (&component.requirement, None)),
        )
        .map(|(requirement, provider_ty)| {
            let normalize_inst = |inst| {
                normalize_predicate_for_comparison(
                    db,
                    inst,
                    scope,
                    evidence,
                    trait_inst,
                    rebase_same_trait_uses,
                )
            };
            let key = match requirement.key.clone() {
                EffectRequirementKey::Type(ty) => EffectRequirementKey::Type(normalize(ty)),
                EffectRequirementKey::Trait(inst) => {
                    EffectRequirementKey::Trait(normalize_inst(inst))
                }
                // A row still abstract here is its instance's, normalized as a
                // trait key's is: `Enumerate<Identity<C>>::Base: Collection`'s
                // row is `C: Collection`'s.
                EffectRequirementKey::Row(row) => EffectRequirementKey::Row(RowKey {
                    inst: normalize_inst(row.inst),
                    ..row
                }),
                EffectRequirementKey::Other => EffectRequirementKey::Other,
            };
            (
                requirement.binding_idx,
                requirement.is_mut,
                key,
                provider_ty,
            )
        })
        .collect();
    Ok(InstantiatedMethodSignature {
        inputs,
        result,
        effects,
    })
}

/// `func`'s effect requirements at `mode` as `args` instantiate them, and
/// their rows as `scope` expands them.
fn instantiated_effects<'db>(
    db: &'db dyn HirAnalysisDb,
    (func, mode): (Func<'db>, Option<BorrowKind>),
    args: &[TyId<'db>],
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> (Vec<EffectRequirement<'db>>, RowExpansion<'db>) {
    let requirements: Vec<_> = func
        .effect_requirements(db, mode)
        .iter()
        .map(|requirement| EffectRequirement {
            key: Binder::bind(func.into(), requirement.key.clone()).instantiate(db, args),
            ..requirement.clone()
        })
        .collect();
    let rows = expand_rows(db, &requirements, scope, assumptions);
    (requirements, rows)
}

fn signatures_match<'db>(
    nominal: &InstantiatedMethodSignature<'db>,
    body: &InstantiatedMethodSignature<'db>,
    effect_pairs: &[(usize, usize)],
) -> bool {
    if nominal.inputs.len() != body.inputs.len()
        || nominal.effects.len() != body.effects.len()
        || effect_pairs.len() != nominal.effects.len()
        || effect_pairs
            .iter()
            .map(|&(nominal, _)| nominal)
            .collect::<IndexSet<_>>()
            .len()
            != nominal.effects.len()
        || effect_pairs
            .iter()
            .map(|&(_, body)| body)
            .collect::<IndexSet<_>>()
            .len()
            != body.effects.len()
    {
        return false;
    }

    let mut body = body.clone();
    let ordered_effects = nominal
        .effects
        .iter()
        .map(|&(nominal_idx, ..)| {
            let body_idx = effect_pairs
                .iter()
                .find(|(index, _)| *index == nominal_idx as usize)?
                .1;
            let (_, is_mut, key, provider_ty) = body
                .effects
                .iter()
                .find(|(index, ..)| *index as usize == body_idx)?
                .clone();
            Some((nominal_idx, is_mut, key, provider_ty))
        })
        .collect::<Option<Vec<_>>>();
    let Some(ordered_effects) = ordered_effects else {
        return false;
    };
    body.effects = ordered_effects;

    *nominal == body
}

pub(crate) struct SemanticCallCallee<'db> {
    pub key: SemanticInstanceKey<'db>,
    /// Whether the callee body is known without deferring to a generic bound.
    pub concrete_dispatch: bool,
    pub effect_pairs: Vec<(usize, usize)>,
    pub provider_pairs: Vec<(u32, u32)>,
    /// Whether the callee takes the call's operands after its receiver: a
    /// callable trait's method called on a function item runs the function,
    /// and the item holds nothing.
    pub drops_receiver: bool,
}

pub(crate) fn semantic_callee_key_with_effect_providers<'db>(
    db: &'db dyn HirAnalysisDb,
    caller_key: SemanticInstanceKey<'db>,
    callable: &Callable<'db>,
    effect_args: &[ResolvedEffectArg<'db>],
    effect_providers: &[EffectProviderSpecialization<'db>],
) -> Result<Option<SemanticCallCallee<'db>>, MethodArgMapError<'db>> {
    let caller = SemanticInstance::new(db, caller_key);
    semantic_callee_key_with_assumptions(
        db,
        Some(caller),
        caller_key.impl_env(db),
        callable,
        effect_args,
        effect_providers,
        caller.assumptions(db),
        ProviderResolutionMode::Final,
    )
}

pub(crate) fn provisional_semantic_callee_key<'db>(
    db: &'db dyn HirAnalysisDb,
    caller_key: SemanticInstanceKey<'db>,
    callable: &Callable<'db>,
    effect_args: &[ResolvedEffectArg<'db>],
    assumptions: PredicateListId<'db>,
) -> Result<Option<SemanticCallCallee<'db>>, MethodArgMapError<'db>> {
    semantic_callee_key_with_assumptions(
        db,
        Some(SemanticInstance::new(db, caller_key)),
        caller_key.impl_env(db),
        callable,
        effect_args,
        callable.effect_providers(),
        assumptions,
        ProviderResolutionMode::Provisional,
    )
}

/// Give a closed, effect-free root the same proof environment as an ordinary
/// call from its own ingot. Symbolic roots and provider-bearing roots retain
/// their declaration environment; their identity is not an ordinary call.
pub(super) fn root_impl_env<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    subst: GenericSubst<'db>,
) -> ImplEnv<'db> {
    let env = ImplEnv::empty(db, owner.scope());
    let BodyOwner::Func(func, mode) = owner else {
        return env;
    };
    if !(func.containing_impl(db).is_some() || !func.is_associated_func(db))
        || !func.effect_requirements(db, mode).is_empty()
        || !is_ground(collect_flags(db, subst.generic_args(db)))
    {
        return env;
    }
    let callable = Callable::from_item(
        db,
        CallableDef::Func(func),
        subst.generic_args(db).to_vec(),
        None,
    );
    semantic_callee_key_with_assumptions(
        db,
        None,
        env,
        &callable,
        &[],
        &[],
        PredicateListId::empty_list(db),
        ProviderResolutionMode::Final,
    )
    .expect("ordinary root has no trait argument mapping")
    .expect("ordinary root is a function")
    .key
    .impl_env(db)
}

/// Finalizes a compiler-generated call that has no caller body through the
/// same selection, argument mapping, signature check, and context pruning as
/// source calls. Inherited arguments come from `trait_inst`; `own_args` are the
/// callee's own source generic arguments. Generated calls must dispatch to a
/// concrete body and cannot carry hidden layout or effect-provider slots.
pub fn generated_callee_key<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    func: Func<'db>,
    trait_inst: Option<TraitInstId<'db>>,
    own_args: &[TyId<'db>],
) -> Result<SemanticInstanceKey<'db>, MethodArgMapError<'db>> {
    let keys = ParamSchemaId::full(db, func.into()).keys(db);
    let inherited = trait_inst.map_or(&[][..], |inst| inst.args(db));
    let inherited_len = GenericParamOwner::Func(func)
        .parent(db)
        .map_or(0, |parent| ParamSchemaId::full(db, parent).keys(db).len());
    if inherited.len() != inherited_len || inherited_len + own_args.len() != keys.len() {
        return Err(MethodArgMapError::NominalArity {
            expected: keys.len(),
            given: inherited.len() + own_args.len(),
        });
    }
    if let Some(&key) = keys[inherited_len..].iter().find(
        |key| !matches!(key, ParamKey::Source { owner, .. } if *owner == GenericParamOwner::Func(func)),
    ) {
        return Err(MethodArgMapError::MissingNominalRole(key));
    }
    let mut callable = Callable::from_item(
        db,
        CallableDef::Func(func),
        inherited.iter().chain(own_args).copied().collect(),
        trait_inst,
    );
    // The call proves its goal and its callee's bounds where it is made, as
    // the checker proves a written call's.
    let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
    callable.record_evidence(db, solve_cx);
    callable.record_bound_evidence(db, solve_cx);
    let callee = semantic_callee_key_with_assumptions(
        db,
        None,
        ImplEnv::new(db, scope, assumptions, Vec::new()),
        &callable,
        &[],
        &[],
        assumptions,
        ProviderResolutionMode::Final,
    )?
    .expect("function callee");
    if !callee.concrete_dispatch {
        return Err(MethodArgMapError::MissingBody);
    }
    Ok(callee.key)
}

/// The instance a compiler-generated call of core trait `trait_path`'s
/// method `name`, on trait arguments `args`, reaches: e.g. the
/// `StateIndex::locate` an entry `c[k]` calls.
pub fn core_method_callee_key<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    trait_path: &[&str],
    name: &str,
    args: Vec<TyId<'db>>,
) -> Result<SemanticInstanceKey<'db>, MethodArgMapError<'db>> {
    let trait_def = resolve_core_trait(db, scope, trait_path)
        .unwrap_or_else(|| panic!("core declares {trait_path:?}"));
    let func = trait_def
        .method_defs(db)
        .get(&IdentId::new(db, name.to_string()))
        .copied()
        .unwrap_or_else(|| panic!("{trait_path:?} declares {name}"));
    let trait_inst = TraitInstId::new_simple(db, trait_def, args);
    generated_callee_key(db, scope, assumptions, func, Some(trait_inst), &[])
}

#[allow(clippy::too_many_arguments)]
fn semantic_callee_key_with_assumptions<'db>(
    db: &'db dyn HirAnalysisDb,
    caller: Option<SemanticInstance<'db>>,
    impl_env: ImplEnv<'db>,
    callable: &Callable<'db>,
    effect_args: &[ResolvedEffectArg<'db>],
    effect_providers: &[EffectProviderSpecialization<'db>],
    assumptions: PredicateListId<'db>,
    provider_resolution_mode: ProviderResolutionMode,
) -> Result<Option<SemanticCallCallee<'db>>, MethodArgMapError<'db>> {
    let CallableDef::Func(nominal_func) = callable.callable_def() else {
        return Ok(None);
    };
    // A callable trait's method called on a closure runs the closure's body.
    if let Some(inst) = callable.trait_inst()
        && let Some((closure, receiver)) = callee_closure(
            db,
            nominal_func,
            normalize_ty(db, inst, impl_env.normalization_scope(db), assumptions),
        )
    {
        let owner = BodyOwner::closure(db, closure_template_ty(db, closure), receiver);
        let subst = GenericSubst::for_body_owner(db, owner, closure.parent_args(db).clone());
        // Its effects are its row's components: those of `call`'s row, in
        // order, with the providers the call binds them to. A component of
        // one of the closure's own rows is the body's expansion of that row's
        // component at the same place.
        let (_, rows) = instantiated_effects(
            db,
            (nominal_func, callable.mode()),
            callable.generic_args(),
            impl_env.normalization_scope(db),
            assumptions,
        );
        let body_rows = row_expansion_for_key(
            db,
            SemanticInstanceKey::of(
                db,
                owner,
                subst,
                EffectProviderSubst::empty(db),
                ImplEnv::empty(db, owner.scope()),
                EvidenceList::none(db),
            ),
        );
        let effect_pairs: Vec<_> = rows
            .components
            .iter()
            .filter_map(|component| {
                let (&entry, steps) = component.path.steps.split_first()?;
                let body = if steps.is_empty() {
                    entry
                } else {
                    body_rows
                        .component(&RowPath {
                            entry,
                            steps: steps.to_vec(),
                        })?
                        .requirement
                        .binding_idx
                };
                Some((component.requirement.binding_idx as usize, body as usize))
            })
            .collect();
        let provider_pairs: Vec<_> = effect_pairs
            .iter()
            .map(|&(nominal, body)| (nominal as u32, body as u32))
            .collect();
        let providers = resolve_provider_specializations(
            db,
            caller,
            effect_providers,
            provider_resolution_mode,
        )
        .into_iter()
        .filter_map(|specialization| {
            let &(_, body) = provider_pairs
                .iter()
                .find(|(nominal, _)| *nominal == specialization.provider.provider_idx)?;
            Some(ProviderBinding {
                provider_idx: body,
                ..specialization.provider
            })
        })
        .collect::<Vec<_>>();
        return Ok(Some(SemanticCallCallee {
            key: SemanticInstanceKey::of(
                db,
                owner,
                subst,
                EffectProviderSubst::new(db, providers, Vec::new()),
                ImplEnv::empty(db, owner.scope()),
                EvidenceList::none(db),
            ),
            concrete_dispatch: true,
            effect_pairs,
            provider_pairs,
            drops_receiver: false,
        }));
    }
    // A callable trait's method called on a function item runs the function,
    // with the call's operands after the receiver. The components of `call`'s
    // row are the function's effects, in order.
    if let Some(inst) = callable.trait_inst()
        && let Some(item) = callee_function_item(
            db,
            nominal_func,
            normalize_ty(db, inst, impl_env.normalization_scope(db), assumptions),
        )
    {
        let target = Callable::new(db, item, DynLazySpan::invalid(), None)
            .expect("a function item is callable");
        let CallableDef::Func(func) = target.callable_def() else {
            unreachable!("a callable function item is a function")
        };
        // A `mut?` function is no callable value.
        let owner = BodyOwner::Func(func, None);
        let (_, rows) = instantiated_effects(
            db,
            (nominal_func, callable.mode()),
            callable.generic_args(),
            impl_env.normalization_scope(db),
            assumptions,
        );
        let func_rows = row_expansion_for_key(
            db,
            SemanticInstanceKey::of(
                db,
                owner,
                GenericSubst::for_owner(db, func.into(), target.generic_args().to_vec()),
                EffectProviderSubst::empty(db),
                ImplEnv::empty(db, owner.scope()),
                EvidenceList::none(db),
            ),
        );
        // Each component's index in the call and among the function's
        // effects.
        let pairs: Vec<(usize, usize)> = rows
            .components
            .iter()
            .filter_map(|component| {
                let (&entry, steps) = component.path.steps.split_first()?;
                let effect = if steps.is_empty() {
                    entry
                } else {
                    func_rows
                        .component(&RowPath {
                            entry,
                            steps: steps.to_vec(),
                        })?
                        .requirement
                        .binding_idx
                };
                Some((component.requirement.binding_idx as usize, effect as usize))
            })
            .collect();
        let effect_of = |nominal: usize| {
            pairs
                .iter()
                .find(|(index, _)| *index == nominal)
                .map(|&(_, effect)| effect)
        };
        // A declared effect's provider is its resolution's; a row
        // component's is numbered as the component, as the instance's
        // effect env numbers it.
        let provider_of = |effect: usize| {
            EffectEnvView::new(EffectParamSite::Func(func, None))
                .resolved_binding(db, effect)
                .map(|binding| binding.provider.provider_idx)
                .or_else(|| {
                    func_rows
                        .components
                        .iter()
                        .any(|component| component.requirement.binding_idx as usize == effect)
                        .then_some(effect as u32)
                })
        };
        let target_effect_args = effect_args
            .iter()
            .map(|arg| {
                let effect = effect_of(arg.binding_idx as usize).ok_or(
                    MethodArgMapError::MissingEffectRole(arg.binding_idx as usize),
                )?;
                let mut arg = arg.clone();
                arg.binding_idx = effect as u32;
                arg.param_idx = effect;
                Ok(arg)
            })
            .collect::<Result<Vec<_>, _>>()?;
        let target_providers: Vec<_> = effect_providers
            .iter()
            .filter_map(|specialization| {
                let provider_idx =
                    provider_of(effect_of(specialization.provider.provider_idx as usize)?)?;
                let mut specialization = specialization.clone();
                specialization.provider.provider_idx = provider_idx;
                Some(specialization)
            })
            .collect();
        let Some(callee) = semantic_callee_key_with_assumptions(
            db,
            caller,
            impl_env,
            &target,
            &target_effect_args,
            &target_providers,
            assumptions,
            provider_resolution_mode,
        )?
        else {
            return Ok(None);
        };
        // The function's effects and providers lie where its own call puts
        // them.
        let body_effect = |effect: usize| {
            if callee.effect_pairs.is_empty() {
                Some(effect)
            } else {
                callee
                    .effect_pairs
                    .iter()
                    .find(|(index, _)| *index == effect)
                    .map(|&(_, body)| body)
            }
        };
        let body_provider = |provider: u32| {
            callee
                .provider_pairs
                .iter()
                .find(|(index, _)| *index == provider)
                .map_or(provider, |&(_, body)| body)
        };
        let effect_pairs = pairs
            .iter()
            .filter_map(|&(nominal, effect)| Some((nominal, body_effect(effect)?)))
            .collect();
        let provider_pairs = pairs
            .iter()
            .filter_map(|&(nominal, effect)| {
                Some((nominal as u32, body_provider(provider_of(effect)?)))
            })
            .collect();
        return Ok(Some(SemanticCallCallee {
            effect_pairs,
            provider_pairs,
            drops_receiver: true,
            ..callee
        }));
    }
    let mut selected_trait_method: Option<(ResolvedImplInstance<'db>, TraitInstId<'db>)> = None;
    let mut effect_pairs = Vec::new();
    let mut provider_pairs = Vec::new();
    let checked_effect_inputs = effect_args
        .iter()
        .filter_map(|arg| {
            arg.instantiated_key_ty
                .or(arg.provider_target_ty)
                .map(|ty| (arg.binding_idx as usize, ty))
        })
        .collect::<Vec<_>>();
    let mut subst_args = callable.generic_args().to_vec();
    let body_func = if let Some(inst) = callable.trait_inst()
        && let Some(method) = callable.selected_method(
            db,
            TraitSolveCx::new(db, impl_env.normalization_scope(db)).with_assumptions(assumptions),
        )
        && let Some(impl_func) = method.body()
    {
        let scope = impl_env.normalization_scope(db);
        let (nominal_effects, nominal_rows) = instantiated_effects(
            db,
            (nominal_func, callable.mode()),
            callable.generic_args(),
            scope,
            assumptions,
        );
        // A component of a nominal row is in the body's splice of the row,
        // or in the body row the method pairs it with.
        let body_path = |path: &RowPath| -> Option<RowPath> {
            let method_pairs = method.effect_pairs()?;
            if let Some(&(_, entry)) = method_pairs
                .iter()
                .find(|(entry, _)| *entry == path.entry as usize)
            {
                return Some(RowPath {
                    entry: entry as u32,
                    steps: path.steps.clone(),
                });
            }
            let name = nominal_effects
                .get(path.entry as usize)?
                .key
                .key_row()?
                .name(db)?;
            let splice = impl_func.row_splice(db, name)?;
            let (first, rest) = path.steps.split_first()?;
            let entry = splice.start + *first as usize;
            (entry < splice.end).then(|| RowPath {
                entry: entry as u32,
                steps: rest.to_vec(),
            })
        };
        // A default body's effects are its declaration's.
        let component_pairs = |body_rows: Option<&RowExpansion<'db>>| {
            nominal_rows
                .components
                .iter()
                .filter_map(|component| {
                    let nominal = component.requirement.binding_idx;
                    let body = if method.effect_pairs().is_none() {
                        nominal
                    } else {
                        let path = body_path(&component.path)?;
                        if path.steps.is_empty() {
                            path.entry
                        } else {
                            body_rows?.component(&path)?.requirement.binding_idx
                        }
                    };
                    Some((nominal as usize, body as usize))
                })
                .chain(
                    method
                        .effect_pairs()
                        .map_or_else(
                            || (0..nominal_effects.len()).map(|idx| (idx, idx)).collect(),
                            <[_]>::to_vec,
                        )
                        .into_iter()
                        .filter(|&(nominal, _)| {
                            nominal_effects[nominal].key.key_row().is_none()
                                || nominal_rows.is_abstract(nominal as u32)
                        }),
                )
                .collect::<Vec<_>>()
        };

        subst_args = method.complete_body_args(db, &subst_args)?.into_values();
        let impl_mode = BodyOwner::func_at(db, impl_func, callable.mode()).mode();
        let (_, body_rows) = instantiated_effects(
            db,
            (impl_func, impl_mode),
            &subst_args,
            impl_func.scope(),
            assumptions,
        );
        effect_pairs = component_pairs(Some(&body_rows));
        // A row component's provider has the component's index.
        let provider_idx = |site, idx| {
            let env = EffectEnvView::new(site);
            if idx < env.requirements(db).len() {
                env.resolved_binding(db, idx)
                    .map(|binding| binding.provider.provider_idx)
            } else {
                Some(idx as u32)
            }
        };
        for &(nominal_idx, body_idx) in &effect_pairs {
            let (Some(nominal), Some(body)) = (
                provider_idx(
                    EffectParamSite::Func(nominal_func, callable.mode()),
                    nominal_idx,
                ),
                provider_idx(EffectParamSite::Func(impl_func, impl_mode), body_idx),
            ) else {
                continue;
            };
            if let Some((_, existing)) = provider_pairs.iter().find(|(index, _)| *index == nominal)
            {
                if *existing != body {
                    return Err(MethodArgMapError::InconsistentProviderRole {
                        nominal,
                        first: *existing,
                        second: body,
                    });
                }
            } else {
                provider_pairs.push((nominal, body));
            }
        }
        selected_trait_method = Some((method.resolved(), inst));
        impl_func
    } else {
        nominal_func
    };
    let owner = BodyOwner::func_at(db, body_func, callable.mode());
    // An implementation selected without relying on a caller assumption.
    let selected_impl = selected_trait_method.is_some_and(|(resolved, _)| {
        !matches!(
            resolved.selected().origin(db),
            ImplementorOrigin::Assumption
        )
    });
    let effect_providers = effect_providers
        .iter()
        .cloned()
        .map(|mut provider| {
            if selected_trait_method.is_some() {
                let nominal = provider.provider.provider_idx;
                let Some((_, body)) = provider_pairs.iter().find(|(index, _)| *index == nominal)
                else {
                    return Err(MethodArgMapError::MissingProviderRole(nominal));
                };
                provider.provider.provider_idx = *body;
            }
            Ok(provider)
        })
        .collect::<Result<Vec<_>, _>>()?;
    let effect_providers = resolve_callable_effect_providers(
        db,
        caller,
        (body_func, owner.mode()),
        &mut subst_args,
        &effect_providers,
        provider_resolution_mode,
    );
    // Checked call arguments can contain structural default templates. Resolve
    // ground nested consts before they become part of the callee's identity.
    let canon_env = ConstCanonEnv::new(owner.scope(), PredicateListId::empty_list(db), None);
    for arg in &mut subst_args {
        *arg = canonicalize_ty_for_mode(db, *arg, canon_env, ConstCanonMode::Identity);
    }

    // A fully concrete implementation can shed caller context only when the
    // call's proof of it names none of the caller's assumptions. Symbolic
    // calls and assumption-selected defaults keep the caller's proof
    // environment.
    let (context_independent, nominal_witness_required) = if let Some((resolved, inst)) =
        selected_trait_method
        && selected_impl
    {
        let body_ground = is_ground(
            collect_flags(db, subst_args.as_slice())
                | collect_flags(db, effect_providers.as_slice())
                | collect_flags(db, effect_args)
                | collect_flags(db, resolved.trait_inst()),
        );
        let original_ground = is_ground(collect_flags(db, inst));
        let independent =
            body_ground && callable.evidence().is_some_and(|proof| proof.is_closed(db));
        // A symbolic nominal receiver can be discarded after selection only if
        // it normalizes to the independent implementation and neither signature
        // has additional effects, method bounds, or layout slots to carry.
        let nominal_redundant = independent
            && inst.assoc_type_bindings(db).is_empty()
            && nominal_func
                .effect_requirements(db, callable.mode())
                .is_empty()
            && body_func.effect_requirements(db, owner.mode()).is_empty()
            && effect_args.is_empty()
            && effect_providers.is_empty()
            && [nominal_func, body_func].into_iter().all(|func| {
                collect_func_decl_constraints(db, CallableDef::Func(func), false)
                    .instantiate_identity()
                    .is_empty(db)
            })
            && inst.args(db).len() == resolved.trait_inst().args(db).len()
            && inst
                .args(db)
                .iter()
                .zip(resolved.trait_inst().args(db))
                .all(|(&nominal, &selected)| {
                    normalize_ty(db, nominal, impl_env.normalization_scope(db), assumptions)
                        == selected
                })
            && {
                let body_args = body_func.arg_tys(db);
                let params = callable.params(db).unwrap_or_default();
                params.len() == body_args.len()
                    && params.iter().zip(body_args).all(|(param, body)| {
                        let nominal = param.shape.erased_ty(db);
                        let body = body.instantiate(db, &subst_args);
                        normalize_ty(db, nominal, impl_env.normalization_scope(db), assumptions)
                            == normalize_ty(
                                db,
                                body,
                                owner.scope(),
                                PredicateListId::empty_list(db),
                            )
                    })
            }
            && normalize_ty(
                db,
                callable.ret_ty(db),
                impl_env.normalization_scope(db),
                assumptions,
            ) == normalize_ty(
                db,
                CallableDef::Func(body_func)
                    .ret_ty(db)
                    .instantiate(db, &subst_args),
                owner.scope(),
                PredicateListId::empty_list(db),
            );
        (
            independent && (original_ground || nominal_redundant),
            !nominal_redundant,
        )
    } else if selected_trait_method.is_none()
        && (nominal_func.containing_impl(db).is_some() || !nominal_func.is_associated_func(db))
        && callable.trait_inst().is_none()
        && !collect_flags(db, subst_args.as_slice())
            .intersects(TyFlags::HAS_INVALID | TyFlags::HAS_VAR | TyFlags::HAS_PROJECTION)
        && is_ground(
            collect_flags(db, effect_providers.as_slice()) | collect_flags(db, effect_args),
        )
    {
        // A free or inherent function sheds caller context when its
        // arguments, signature and effect keys normalize the same without the
        // caller's assumptions, and its ground bounds hold without them. A
        // bound on a caller's parameter is one of the callee instance's own
        // assumptions, so equal calls written in different bodies share one
        // instance.
        let empty = PredicateListId::empty_list(db);
        let scope = nominal_func.scope();
        let caller_scope = impl_env.normalization_scope(db);
        let same_without_caller = |ty| {
            normalize_ty(db, ty, caller_scope, assumptions) == normalize_ty(db, ty, scope, empty)
        };
        let bounds = collect_func_decl_constraints(db, CallableDef::Func(nominal_func), true)
            .instantiate(db, &subst_args);
        let independent = bounds.list(db).iter().copied().all(|bound| {
            collect_flags(db, bound).contains(TyFlags::HAS_PARAM)
                || matches!(
                    is_goal_satisfiable(db, TraitSolveCx::new(db, scope), bound),
                    GoalSatisfiability::Satisfied(_)
                )
        }) && subst_args.iter().all(|&arg| same_without_caller(arg))
            && callable
                .params(db)
                .unwrap_or_default()
                .iter()
                .all(|param| same_without_caller(param.shape.erased_ty(db)))
            && same_without_caller(callable.ret_ty(db))
            && checked_effect_inputs
                .iter()
                .all(|&(_, ty)| same_without_caller(ty));
        (independent, true)
    } else {
        (false, true)
    };
    let mut witnesses: IndexSet<_> = if context_independent {
        IndexSet::new()
    } else {
        impl_env.witnesses(db).iter().copied().collect()
    };
    if let Some(witness) = callable.trait_inst().filter(|_| nominal_witness_required) {
        witnesses.insert(witness);
    }
    if let Some((resolved, _)) = selected_trait_method {
        witnesses.insert(resolved.trait_inst());
    }
    let normalization_scope = if context_independent {
        // Library paths and impl selection resolve per ingot, and a body can
        // reach impls only its caller's ingot sees, such as `Encode<Sol>` for a
        // panic payload. Keep that ingot, not the module.
        impl_env
            .normalization_scope(db)
            .ingot(db)
            .root_mod(db)
            .scope()
    } else if selected_impl {
        owner.scope()
    } else {
        impl_env.normalization_scope(db)
    };
    let impl_env = ImplEnv::new(
        db,
        normalization_scope,
        if context_independent {
            PredicateListId::empty_list(db)
        } else {
            assumptions
        },
        witnesses.into_iter().collect::<Vec<_>>(),
    );

    if matches!(provider_resolution_mode, ProviderResolutionMode::Final)
        && let Some((resolved, nominal_inst)) = selected_trait_method
    {
        let body_constraints =
            collect_func_decl_constraints(db, CallableDef::Func(body_func), true)
                .instantiate(db, &subst_args);
        let mut predicates: IndexSet<_> =
            impl_env.assumptions(db).list(db).iter().copied().collect();
        predicates.extend(impl_env.witnesses(db).iter().copied());
        predicates.extend(body_constraints.list(db).iter().copied());
        let evidence = PredicateListId::new(db, predicates.into_iter().collect::<Vec<_>>())
            .extend_all_bounds(db);
        let nominal_signature = instantiated_method_signature(
            db,
            (nominal_func, callable.mode()),
            callable.generic_args(),
            normalization_scope,
            evidence,
            nominal_inst,
            true,
        )?;
        let body_signature = instantiated_method_signature(
            db,
            (body_func, owner.mode()),
            &subst_args,
            normalization_scope,
            evidence,
            resolved.trait_inst(),
            false,
        )?;
        if !(collect_flags(db, nominal_signature.clone())
            | collect_flags(db, body_signature.clone()))
        .contains(TyFlags::HAS_INVALID)
            && !signatures_match(&nominal_signature, &body_signature, &effect_pairs)
        {
            return Err(MethodArgMapError::SignatureMismatch {
                nominal: nominal_func,
                body: body_func,
                nominal_args: callable.generic_args().to_vec(),
                body_args: subst_args,
                evidence,
                nominal_signature: nominal_signature.describe(db),
                body_signature: body_signature.describe(db),
            });
        }
    }
    // A callee that runs its own body receives the call's proofs of the
    // assumptions that body is checked under; an implementation's method
    // receives them from the selection's proofs of the implementation's
    // conditions and the call's proofs of the trait method's.
    let subst = GenericSubst::for_owner(db, body_func.into(), subst_args);
    let evidence = match (callable.bound_evidence(), callable.evidence()) {
        (Some(bound), _) if body_func == nominal_func => bound,
        (Some(bound), Some(selection)) if matches!(selection.data(db), Evidence::Impl { .. }) => {
            impl_method_bound_evidence(
                db,
                (body_func, nominal_func, callable.mode()),
                selection,
                bound,
                subst,
                assumptions,
            )
        }
        _ => EvidenceList::none(db),
    };
    let evidence = if collect_flags(db, evidence).contains(TyFlags::HAS_PROJECTION) {
        normalize_ty(db, evidence, impl_env.normalization_scope(db), assumptions)
    } else {
        evidence
    };
    Ok(Some(SemanticCallCallee {
        concrete_dispatch: callable.trait_inst().is_none() || selected_impl,
        key: SemanticInstanceKey::of(
            db,
            owner,
            subst,
            EffectProviderSubst::new(
                db,
                effect_providers
                    .into_iter()
                    .map(|specialization| specialization.provider)
                    .collect::<Vec<_>>(),
                Vec::new(),
            ),
            impl_env,
            evidence,
        ),
        effect_pairs,
        provider_pairs,
        drops_receiver: false,
    }))
}

/// Whether a value has no inference state, symbolic parameters, or
/// unresolved projections.
pub(super) fn is_ground(flags: TyFlags) -> bool {
    !flags.intersects(
        TyFlags::HAS_INVALID | TyFlags::HAS_VAR | TyFlags::HAS_PARAM | TyFlags::HAS_PROJECTION,
    )
}

fn resolve_callable_effect_providers<'db>(
    db: &'db dyn HirAnalysisDb,
    caller: Option<SemanticInstance<'db>>,
    (func, mode): (Func<'db>, Option<BorrowKind>),
    subst_args: &mut [TyId<'db>],
    effect_providers: &[EffectProviderSpecialization<'db>],
    provider_resolution_mode: ProviderResolutionMode,
) -> Vec<EffectProviderSpecialization<'db>> {
    let providers =
        resolve_provider_specializations(db, caller, effect_providers, provider_resolution_mode);
    let site = EffectParamSite::Func(func, mode);
    let effect_env = EffectEnvView::new(site);
    let resolution_by_req = match provider_resolution_mode {
        ProviderResolutionMode::Final => effect_env
            .resolutions(db)
            .into_iter()
            .map(|resolution| (resolution.requirement_idx as usize, resolution.provider_idx))
            .collect::<FxHashMap<_, _>>(),
        ProviderResolutionMode::Provisional => effect_env
            .requirements(db)
            .into_iter()
            .filter_map(|requirement| {
                provisional_provider_idx_for_requirement(db, site, requirement.binding_idx)
                    .map(|provider_idx| (requirement.binding_idx as usize, provider_idx))
            })
            .collect::<FxHashMap<_, _>>(),
    };
    let provider_by_idx = providers
        .iter()
        .map(|provider| (provider.provider.provider_idx, provider))
        .collect::<FxHashMap<_, _>>();
    for (effect_idx, param_idx) in place_effect_provider_param_index_map(db, func)
        .iter()
        .enumerate()
        .filter_map(|(effect_idx, param_idx)| param_idx.map(|param_idx| (effect_idx, param_idx)))
    {
        let Some(provider_idx) = resolution_by_req.get(&effect_idx).copied() else {
            continue;
        };
        let Some(provider) = provider_by_idx.get(&provider_idx) else {
            continue;
        };
        if let Some(slot) = subst_args.get_mut(param_idx) {
            *slot = provider.provider.provider_ty;
        }
    }
    providers
}

/// A call's provider specializations, each a caller binding's provider
/// resolved in the caller.
fn resolve_provider_specializations<'db>(
    db: &'db dyn HirAnalysisDb,
    caller: Option<SemanticInstance<'db>>,
    effect_providers: &[EffectProviderSpecialization<'db>],
    provider_resolution_mode: ProviderResolutionMode,
) -> Vec<EffectProviderSpecialization<'db>> {
    let caller = || caller.expect("effect providers require a caller instance");
    let mut providers = effect_providers
        .iter()
        .map(|specialization| {
            let provider_idx = specialization.provider.provider_idx;
            let provider = match specialization.provenance {
                crate::analysis::ty::ty_check::EffectProviderProvenance::Binding {
                    binding,
                    ..
                } => provider_resolution_mode
                    .resolve_binding(db, caller(), binding)
                    .filter(|provider| {
                        provider.effective_target_ty()
                            == specialization.provider.effective_target_ty()
                    })
                    .map(|provider| crate::semantic::ProviderBinding {
                        provider_idx,
                        ..provider
                    })
                    .unwrap_or(specialization.provider.clone()),
                crate::analysis::ty::ty_check::EffectProviderProvenance::Expr { .. } => {
                    specialization.provider.clone()
                }
            };
            EffectProviderSpecialization {
                provider,
                provenance: specialization.provenance,
            }
        })
        .collect::<Vec<_>>();
    providers.sort_by_key(|provider| provider.provider.provider_idx);
    providers
}

impl ProviderResolutionMode {
    fn resolve_binding<'db>(
        self,
        db: &'db dyn HirAnalysisDb,
        caller: SemanticInstance<'db>,
        binding: crate::analysis::ty::ty_check::LocalBinding<'db>,
    ) -> Option<ProviderBinding<'db>> {
        match self {
            Self::Final => resolved_provider_binding_for_instance_effect(db, caller, binding),
            Self::Provisional => {
                provisional_provider_binding_for_instance_effect(db, caller, binding)
            }
        }
    }
}

pub(crate) fn resolve_semantic_const_ref<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ref: ConstRef<'db>,
    ty: TyId<'db>,
    origin: SemOrigin<'db>,
) -> Option<SemanticConstRef<'db>> {
    let instance = match const_ref {
        ConstRef::Const(const_) => semantic_const_key_for_const(db, const_),
        ConstRef::TraitConst { use_, .. } => {
            let (body, ty, subst, evidence) = const_ref.trait_const_body(db)?;
            let evidence = if collect_flags(db, evidence).contains(TyFlags::HAS_PROJECTION) {
                normalize_ty(db, evidence, use_.origin_scope(), use_.assumptions())
            } else {
                evidence
            };
            let subst = GenericSubst::complete(db, subst);
            Some(SemanticInstanceKey::of(
                db,
                BodyOwner::AnonConstBody { body, expected: ty },
                subst,
                EffectProviderSubst::empty(db),
                ImplEnv::new(
                    db,
                    use_.origin_scope(),
                    use_.assumptions(),
                    vec![use_.inst()],
                ),
                evidence,
            ))
        }
        ConstRef::InherentConst(use_) => semantic_const_key_for_inherent_const(db, use_),
    }?;
    Some(SemanticConstRef::new(db, instance, ty, origin))
}

fn semantic_const_key_for_const<'db>(
    db: &'db dyn HirAnalysisDb,
    const_: Const<'db>,
) -> Option<SemanticInstanceKey<'db>> {
    let owner = BodyOwner::Const(const_);
    Some(SemanticInstanceKey::of(
        db,
        owner,
        GenericSubst::none(db),
        EffectProviderSubst::empty(db),
        ImplEnv::empty(db, owner.scope()),
        EvidenceList::none(db),
    ))
}

fn semantic_const_key_for_inherent_const<'db>(
    db: &'db dyn HirAnalysisDb,
    use_: InherentConstUse<'db>,
) -> Option<SemanticInstanceKey<'db>> {
    let (body, impl_args) =
        inherent_const_body_and_impl_args(db, use_.impl_(), use_.receiver_ty(), use_.name())?;
    let ty = inherent_const_decl_ty(db, use_.impl_(), use_.name())?;
    Some(SemanticInstanceKey::of(
        db,
        BodyOwner::AnonConstBody { body, expected: ty },
        GenericSubst::for_owner(db, use_.impl_().into(), impl_args),
        EffectProviderSubst::empty(db),
        ImplEnv::new(db, use_.origin_scope(), use_.assumptions(), vec![]),
        EvidenceList::none(db),
    ))
}

#[cfg(test)]
mod tests {
    use super::{InstantiatedMethodSignature, signatures_match};
    use crate::{
        analysis::ty::ty_def::TyId, core::semantic::EffectRequirementKey,
        test_db::HirAnalysisTestDb,
    };

    #[test]
    fn selected_signature_comparison_includes_provider_types() {
        let db = HirAnalysisTestDb::default();
        let key = EffectRequirementKey::Type(TyId::u256(&db));
        let signature = |provider_ty| InstantiatedMethodSignature {
            inputs: Vec::new(),
            result: TyId::unit(&db),
            effects: vec![(0, false, key.clone(), Some(provider_ty))],
        };
        let nominal = signature(TyId::u256(&db));
        assert!(signatures_match(
            &nominal,
            &signature(TyId::u256(&db)),
            &[(0, 0)],
        ));
        assert!(!signatures_match(
            &nominal,
            &signature(TyId::bool(&db)),
            &[(0, 0)],
        ));
    }
}
