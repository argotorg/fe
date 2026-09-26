use super::{
    EffectProviderSubst, GenericSubst, ImplEnv, SemanticInstance, SemanticInstanceKey,
    provisional_provider_binding_for_instance_effect, provisional_provider_idx_for_requirement,
    resolved_effect_binding_ty_for_instance_effect, resolved_provider_binding_for_instance_effect,
};
use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{SemOrigin, SemanticConstRef},
        ty::{
            assoc_const::{AssocConstUse, InherentConstUse},
            binder::Binder,
            const_ty::{
                ConstCanonEnv, ConstCanonMode, canonicalize_ty_for_mode,
                inherent_const_body_and_impl_args, inherent_const_decl_ty,
            },
            effects::place_effect_provider_param_index_map,
            fold::{TyFoldable, TyFolder},
            layout_holes::layout_shape_value,
            method_cmp::{normalize_compare_assoc_consts, normalize_predicate_for_comparison},
            normalize::normalize_ty,
            trait_def::{
                ImplementorOrigin, MethodArgMapError, ResolvedImplInstance, TraitInstId,
                assoc_const_body_template_for_trait_inst, resolve_trait_method_instance,
            },
            trait_resolution::{
                GoalSatisfiability, PredicateListId, Selection, TraitSolveCx,
                constraint::collect_func_decl_constraints, is_goal_satisfiable,
            },
            ty_check::{
                BodyOwner, Callable, ConstRef, EffectParamSite, EffectProviderSpecialization,
                ResolvedEffectArg,
            },
            ty_def::{TyFlags, TyId},
            ty_lower::{
                ParamBasis, ParamKey, ParamSchemaId, collect_layout_arg_bindings,
                instantiate_callable_effect_layout_args, param_schema,
            },
            visitor::{TyVisitable, TyVisitor, collect_flags},
        },
    },
    core::semantic::{EffectEnvView, EffectRequirementKey, ProviderBinding},
    hir_def::{
        CallableDef, Const, Func, GenericParamOwner, params::FuncParamMode, scope_graph::ScopeId,
    },
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
    inputs: Vec<(FuncParamMode, TyId<'db>)>,
    result: TyId<'db>,
    effects: Vec<(bool, EffectRequirementKey<'db>, Option<TyId<'db>>)>,
}

impl<'db> TyVisitable<'db> for InstantiatedMethodSignature<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        for (_, ty) in &self.inputs {
            ty.visit_with(visitor);
        }
        self.result.visit_with(visitor);
        for (_, key, provider_ty) in &self.effects {
            match key {
                EffectRequirementKey::Type(ty) => ty.visit_with(visitor),
                EffectRequirementKey::Trait(inst) => inst.visit_with(visitor),
                EffectRequirementKey::Other => {}
            }
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
        let inputs = self
            .inputs
            .into_iter()
            .map(|(mode, ty)| (mode, ty.fold_with(db, folder)))
            .collect();
        let result = self.result.fold_with(db, folder);
        let effects = self
            .effects
            .into_iter()
            .map(|(is_mut, key, provider_ty)| {
                let key = match key {
                    EffectRequirementKey::Type(ty) => {
                        EffectRequirementKey::Type(ty.fold_with(db, folder))
                    }
                    EffectRequirementKey::Trait(inst) => {
                        EffectRequirementKey::Trait(inst.fold_with(db, folder))
                    }
                    EffectRequirementKey::Other => EffectRequirementKey::Other,
                };
                (is_mut, key, provider_ty.map(|ty| ty.fold_with(db, folder)))
            })
            .collect();
        Self {
            inputs,
            result,
            effects,
        }
    }
}

struct LayoutBindingSubst<'db>(FxHashMap<TyId<'db>, TyId<'db>>);

impl<'db> TyFolder<'db> for LayoutBindingSubst<'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        self.0
            .get(&ty)
            .copied()
            .unwrap_or_else(|| ty.super_fold_with(db, self))
    }
}

impl<'db> InstantiatedMethodSignature<'db> {
    fn describe(&self, db: &'db dyn HirAnalysisDb) -> String {
        let inputs = self
            .inputs
            .iter()
            .map(|(mode, ty)| format!("{mode:?} {}", ty.pretty_print(db)))
            .collect::<Vec<_>>();
        let effects = self
            .effects
            .iter()
            .map(|(is_mut, key, provider_ty)| {
                let key = match key {
                    EffectRequirementKey::Type(ty) => ty.pretty_print(db).to_string(),
                    EffectRequirementKey::Trait(inst) => inst.pretty_print(db, true),
                    EffectRequirementKey::Other => "<unresolved>".to_string(),
                };
                format!(
                    "{}{key} via {}",
                    if *is_mut { "mut " } else { "" },
                    provider_ty.map_or_else(
                        || "<none>".to_string(),
                        |ty| ty.pretty_print(db).to_string()
                    )
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
    func: Func<'db>,
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
    let normalize = |ty| {
        normalize_compare_assoc_consts(
            db,
            normalize_ty(db, ty, scope, evidence),
            scope,
            evidence,
            trait_inst,
            rebase_same_trait_uses,
        )
    };
    let inputs = arg_tys
        .iter()
        .zip(params)
        .map(|(ty, param)| (param.mode(db), normalize(ty.instantiate(db, args))))
        .collect();
    let result = normalize(CallableDef::Func(func).ret_ty(db).instantiate(db, args));
    let provider_slots = place_effect_provider_param_index_map(db, func);
    let effects = func
        .effect_requirements(db)
        .iter()
        .map(|requirement| {
            let key = match requirement.key {
                EffectRequirementKey::Type(ty) => EffectRequirementKey::Type(normalize(
                    Binder::bind(func.into(), ty).instantiate(db, args),
                )),
                EffectRequirementKey::Trait(inst) => {
                    EffectRequirementKey::Trait(normalize_predicate_for_comparison(
                        db,
                        Binder::bind(func.into(), inst).instantiate(db, args),
                        scope,
                        evidence,
                        trait_inst,
                        rebase_same_trait_uses,
                    ))
                }
                EffectRequirementKey::Other => EffectRequirementKey::Other,
            };
            let provider_ty = provider_slots
                .get(requirement.binding_idx as usize)
                .and_then(|slot| *slot)
                .and_then(|slot| args.get(slot))
                .copied()
                .map(normalize);
            (requirement.is_mut, key, provider_ty)
        })
        .collect();
    Ok(InstantiatedMethodSignature {
        inputs,
        result,
        effects,
    })
}

fn signatures_match<'db>(
    db: &'db dyn HirAnalysisDb,
    nominal: &InstantiatedMethodSignature<'db>,
    body: &InstantiatedMethodSignature<'db>,
    effect_pairs: &[(usize, usize)],
    checked_inputs: Option<&[TyId<'db>]>,
    checked_effect_inputs: &[(usize, TyId<'db>)],
) -> bool {
    if nominal.inputs.len() != body.inputs.len()
        || checked_inputs.is_some_and(|inputs| inputs.len() != nominal.inputs.len())
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
    let ordered_effects = (0..nominal.effects.len())
        .map(|nominal_idx| {
            let body_idx = effect_pairs
                .iter()
                .find(|(index, _)| *index == nominal_idx)?
                .1;
            body.effects.get(body_idx).cloned()
        })
        .collect::<Option<Vec<_>>>();
    let Some(ordered_effects) = ordered_effects else {
        return false;
    };
    body.effects = ordered_effects;

    // Both signatures are in nominal effect order here, so checked value and
    // effect-key inputs bind layout holes at the same positions on each side.
    let bind_checked_inputs = |signature: InstantiatedMethodSignature<'db>| {
        let value_inputs = signature
            .inputs
            .iter()
            .map(|&(_, formal)| formal)
            .zip(checked_inputs.into_iter().flatten().copied());
        let effect_inputs =
            checked_effect_inputs.iter().filter_map(|&(idx, actual)| {
                match signature.effects.get(idx)? {
                    (_, EffectRequirementKey::Type(formal), _) => Some((*formal, actual)),
                    _ => None,
                }
            });
        let mut bindings = Vec::new();
        for (formal, actual) in value_inputs.chain(effect_inputs) {
            let mut candidate = bindings.clone();
            if collect_layout_arg_bindings(db, formal, actual, &mut candidate) {
                bindings = candidate;
            }
        }
        signature.fold_with(db, &mut LayoutBindingSubst(bindings.into_iter().collect()))
    };
    layout_shape_value(db, bind_checked_inputs(nominal.clone()))
        == layout_shape_value(db, bind_checked_inputs(body))
}

pub(crate) struct SemanticCallCallee<'db> {
    pub key: SemanticInstanceKey<'db>,
    /// Whether the callee body is known without deferring to a generic bound.
    pub concrete_dispatch: bool,
    pub effect_pairs: Vec<(usize, usize)>,
    pub provider_pairs: Vec<(u32, u32)>,
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
    let callable = Callable::from_item(
        db,
        CallableDef::Func(func),
        inherited.iter().chain(own_args).copied().collect(),
        trait_inst,
    );
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
        && let Some(name) = nominal_func.name(db).to_opt()
        && let Selection::Unique(method) = resolve_trait_method_instance(
            db,
            TraitSolveCx::new(db, impl_env.normalization_scope(db)).with_assumptions(assumptions),
            inst,
            name,
        )
        && let Some(impl_func) = method.body()
    {
        subst_args = method
            .complete_body_args(
                db,
                &subst_args,
                callable.checked_input_tys(),
                Some(&checked_effect_inputs),
            )?
            .into_values();
        effect_pairs = method.effect_pairs().to_vec();
        let nominal_env = EffectEnvView::new(EffectParamSite::Func(nominal_func));
        let body_env = EffectEnvView::new(EffectParamSite::Func(impl_func));
        if effect_pairs.len() != nominal_func.effect_requirements(db).len() {
            return Err(MethodArgMapError::MissingEffectRole(effect_pairs.len()));
        }
        for &(nominal_idx, body_idx) in &effect_pairs {
            let Some(nominal) = nominal_env.resolved_binding(db, nominal_idx) else {
                continue;
            };
            let Some(body) = body_env.resolved_binding(db, body_idx) else {
                continue;
            };
            let nominal = nominal.provider.provider_idx;
            let body = body.provider.provider_idx;
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
    let owner = BodyOwner::Func(body_func);
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
        body_func,
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
    // same implementation is selected without it. Symbolic calls and
    // assumption-selected defaults keep the caller's proof environment.
    let (context_independent, nominal_witness_required) = if let Some((resolved, inst)) =
        selected_trait_method
        && selected_impl
        && let Some(name) = nominal_func.name(db).to_opt()
    {
        let body_ground = is_ground(
            collect_flags(db, subst_args.as_slice())
                | collect_flags(db, effect_providers.as_slice())
                | collect_flags(db, effect_args)
                | collect_flags(db, resolved.trait_inst()),
        );
        let original_ground = is_ground(collect_flags(db, inst));
        let independent = body_ground
            && matches!(
                resolve_trait_method_instance(
                    db,
                    TraitSolveCx::new(db, owner.scope()),
                    resolved.trait_inst(),
                    name,
                ),
                Selection::Unique(method) if method.resolved() == resolved
            );
        // A symbolic nominal receiver can be discarded after selection only if
        // it normalizes to the independent implementation and neither signature
        // has additional effects, method bounds, or layout slots to carry.
        let nominal_redundant = independent
            && inst.assoc_type_bindings(db).is_empty()
            && nominal_func.effect_requirements(db).is_empty()
            && body_func.effect_requirements(db).is_empty()
            && effect_args.is_empty()
            && effect_providers.is_empty()
            && [nominal_func, body_func].into_iter().all(|func| {
                collect_func_decl_constraints(db, CallableDef::Func(func), false)
                    .instantiate_identity()
                    .is_empty(db)
                    && !has_callable_layout_slots(db, func)
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
            && nominal_func.arg_tys(db).len() == body_func.arg_tys(db).len()
            && (0..nominal_func.arg_tys(db).len()).all(|idx| {
                let nominal = callable
                    .arg_ty(db, idx)
                    .expect("nominal input arity changed");
                let body =
                    CallableDef::Func(body_func).arg_tys(db)[idx].instantiate(db, &subst_args);
                normalize_ty(db, nominal, impl_env.normalization_scope(db), assumptions)
                    == normalize_ty(db, body, owner.scope(), PredicateListId::empty_list(db))
            })
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
        && !nominal_func.is_associated_func(db)
        && callable.trait_inst().is_none()
        && effect_args.is_empty()
        && effect_providers.is_empty()
        && nominal_func.effect_requirements(db).is_empty()
        && !has_callable_layout_slots(db, nominal_func)
        && is_ground(collect_flags(db, subst_args.as_slice()))
    {
        let empty = PredicateListId::empty_list(db);
        let scope = nominal_func.scope();
        let bounds = collect_func_decl_constraints(db, CallableDef::Func(nominal_func), true)
            .instantiate(db, &subst_args);
        let independent = bounds.list(db).iter().copied().all(|bound| {
            matches!(
                is_goal_satisfiable(db, TraitSolveCx::new(db, scope), bound),
                GoalSatisfiability::Satisfied(_)
            )
        }) && (0..nominal_func.arg_tys(db).len()).all(|idx| {
            let arg = callable
                .arg_ty(db, idx)
                .expect("nominal input arity changed");
            normalize_ty(db, arg, impl_env.normalization_scope(db), assumptions)
                == normalize_ty(db, arg, scope, empty)
        }) && normalize_ty(
            db,
            callable.ret_ty(db),
            impl_env.normalization_scope(db),
            assumptions,
        ) == normalize_ty(db, callable.ret_ty(db), scope, empty);
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
    let normalization_scope = if context_independent || selected_impl {
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
            nominal_func,
            callable.generic_args(),
            normalization_scope,
            evidence,
            nominal_inst,
            true,
        )?;
        let body_signature = instantiated_method_signature(
            db,
            body_func,
            &subst_args,
            normalization_scope,
            evidence,
            resolved.trait_inst(),
            false,
        )?;
        if !(collect_flags(db, nominal_signature.clone())
            | collect_flags(db, body_signature.clone()))
        .contains(TyFlags::HAS_INVALID)
            && !signatures_match(
                db,
                &nominal_signature,
                &body_signature,
                &effect_pairs,
                callable.checked_input_tys(),
                &checked_effect_inputs,
            )
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
    Ok(Some(SemanticCallCallee {
        concrete_dispatch: callable.trait_inst().is_none() || selected_impl,
        key: SemanticInstanceKey::new(
            db,
            owner,
            GenericSubst::for_owner(db, body_func.into(), subst_args),
            EffectProviderSubst::new(db, effect_providers),
            impl_env,
        ),
        effect_pairs,
        provider_pairs,
    }))
}

/// Whether a value has no inference state, symbolic parameters, or
/// unresolved projections.
fn is_ground(flags: TyFlags) -> bool {
    !flags.intersects(
        TyFlags::HAS_INVALID | TyFlags::HAS_VAR | TyFlags::HAS_PARAM | TyFlags::HAS_PROJECTION,
    )
}

fn has_callable_layout_slots<'db>(db: &'db dyn HirAnalysisDb, func: Func<'db>) -> bool {
    param_schema(db, func.into(), ParamBasis::Full)
        .keys(db)
        .iter()
        .any(|key| matches!(key, ParamKey::CallableLayout { .. }))
}

fn resolve_callable_effect_providers<'db>(
    db: &'db dyn HirAnalysisDb,
    caller: Option<SemanticInstance<'db>>,
    func: Func<'db>,
    subst_args: &mut [TyId<'db>],
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
    let effect_env = EffectEnvView::new(EffectParamSite::Func(func));
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
                provisional_provider_idx_for_requirement(
                    db,
                    EffectParamSite::Func(func),
                    requirement.binding_idx,
                )
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
        let actual_key_ty = effect_provider_target_ty(db, caller(), provider);
        instantiate_callable_effect_layout_args(db, func, effect_idx, actual_key_ty, subst_args);
    }
    providers
}

fn effect_provider_target_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    caller: SemanticInstance<'db>,
    provider: &EffectProviderSpecialization<'db>,
) -> TyId<'db> {
    let fallback = provider.provider.effective_target_ty();
    let crate::analysis::ty::ty_check::EffectProviderProvenance::Binding { binding, .. } =
        provider.provenance
    else {
        return fallback;
    };
    resolved_effect_binding_ty_for_instance_effect(db, caller, binding).unwrap_or(fallback)
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
        ConstRef::TraitConst(assoc) => semantic_const_key_for_assoc_const(db, assoc),
        ConstRef::InherentConst(use_) => semantic_const_key_for_inherent_const(db, use_),
    }?;
    Some(SemanticConstRef::new(db, instance, ty, origin))
}

fn semantic_const_key_for_const<'db>(
    db: &'db dyn HirAnalysisDb,
    const_: Const<'db>,
) -> Option<SemanticInstanceKey<'db>> {
    let owner = BodyOwner::Const(const_);
    Some(SemanticInstanceKey::new(
        db,
        owner,
        GenericSubst::none(db),
        EffectProviderSubst::empty(db),
        ImplEnv::empty(db, owner.scope()),
    ))
}

fn semantic_const_key_for_inherent_const<'db>(
    db: &'db dyn HirAnalysisDb,
    use_: InherentConstUse<'db>,
) -> Option<SemanticInstanceKey<'db>> {
    let (body, impl_args) =
        inherent_const_body_and_impl_args(db, use_.impl_(), use_.receiver_ty(), use_.name())?;
    let ty = inherent_const_decl_ty(db, use_.impl_(), use_.name())?;
    Some(SemanticInstanceKey::new(
        db,
        BodyOwner::AnonConstBody { body, expected: ty },
        GenericSubst::for_owner(db, use_.impl_().into(), impl_args),
        EffectProviderSubst::empty(db),
        ImplEnv::new(db, use_.origin_scope(), use_.assumptions(), vec![]),
    ))
}

fn semantic_const_key_for_assoc_const<'db>(
    db: &'db dyn HirAnalysisDb,
    assoc: AssocConstUse<'db>,
) -> Option<SemanticInstanceKey<'db>> {
    let (body, ty, subst) = assoc_const_body_template_for_trait_inst(
        db,
        assoc.solve_cx(db),
        assoc.inst(),
        assoc.name(),
    )?;
    Some(SemanticInstanceKey::new(
        db,
        BodyOwner::AnonConstBody { body, expected: ty },
        GenericSubst::complete(db, subst),
        EffectProviderSubst::empty(db),
        ImplEnv::new(
            db,
            assoc.origin_scope(),
            assoc.assumptions(),
            vec![assoc.inst()],
        ),
    ))
}

#[cfg(test)]
mod tests {
    use super::{InstantiatedMethodSignature, signatures_match};
    use crate::{
        analysis::ty::{
            const_ty::{BoundHoleId, ConstTyId, HoleId, LayoutShapeHoleKind},
            ty_def::TyId,
        },
        core::semantic::EffectRequirementKey,
        hir_def::params::FuncParamMode,
        test_db::HirAnalysisTestDb,
    };

    #[test]
    fn selected_signature_comparison_preserves_cross_input_layout_sharing() {
        let db = HirAnalysisTestDb::default();
        let hole = |ordinal| {
            TyId::const_ty(
                &db,
                ConstTyId::hole_with_id(
                    &db,
                    TyId::u256(&db),
                    HoleId::Bound(BoundHoleId::LayoutShape {
                        ordinal,
                        kind: LayoutShapeHoleKind::DefaultHoleParam,
                    }),
                ),
            )
        };
        let signature = |first, second| InstantiatedMethodSignature {
            inputs: vec![(FuncParamMode::View, first), (FuncParamMode::View, second)],
            result: TyId::unit(&db),
            effects: Vec::new(),
        };
        let nominal = signature(hole(0), hole(0));
        assert!(signatures_match(
            &db,
            &nominal,
            &signature(hole(1), hole(1)),
            &[],
            None,
            &[],
        ));
        assert!(!signatures_match(
            &db,
            &nominal,
            &signature(hole(1), hole(2)),
            &[],
            None,
            &[],
        ));
    }

    #[test]
    fn selected_signature_comparison_includes_provider_types() {
        let db = HirAnalysisTestDb::default();
        let key = EffectRequirementKey::Type(TyId::u256(&db));
        let signature = |provider_ty| InstantiatedMethodSignature {
            inputs: Vec::new(),
            result: TyId::unit(&db),
            effects: vec![(false, key.clone(), Some(provider_ty))],
        };
        let nominal = signature(TyId::u256(&db));
        assert!(signatures_match(
            &db,
            &nominal,
            &signature(TyId::u256(&db)),
            &[(0, 0)],
            None,
            &[],
        ));
        assert!(!signatures_match(
            &db,
            &nominal,
            &signature(TyId::bool(&db)),
            &[(0, 0)],
            None,
            &[],
        ));
    }

    #[test]
    fn selected_signature_comparison_binds_effect_key_layout_holes() {
        let db = HirAnalysisTestDb::default();
        let hole = TyId::const_ty(
            &db,
            ConstTyId::hole_with_id(
                &db,
                TyId::u256(&db),
                HoleId::Bound(BoundHoleId::LayoutShape {
                    ordinal: 0,
                    kind: LayoutShapeHoleKind::DefaultHoleParam,
                }),
            ),
        );
        let concrete = TyId::const_ty(&db, ConstTyId::integer(&db, TyId::u256(&db), 7.into()));
        let signature = |key| InstantiatedMethodSignature {
            inputs: Vec::new(),
            result: TyId::unit(&db),
            effects: vec![(false, EffectRequirementKey::Type(key), None)],
        };
        let nominal = signature(hole);
        let body = signature(concrete);
        assert!(!signatures_match(
            &db,
            &nominal,
            &body,
            &[(0, 0)],
            None,
            &[]
        ));
        assert!(signatures_match(
            &db,
            &nominal,
            &body,
            &[(0, 0)],
            None,
            &[(0, concrete)],
        ));
    }
}
