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
            const_ty::inherent_const_body_and_impl_args,
            effects::place_effect_provider_param_index_map,
            trait_def::{
                assoc_const_body_and_impl_args_for_trait_inst, complete_resolved_trait_method_args,
                resolve_trait_method_instance,
            },
            trait_resolution::{PredicateListId, TraitSolveCx},
            ty_check::{
                BodyOwner, Callable, ConstRef, EffectParamSite, EffectProviderSpecialization,
            },
            ty_def::TyId,
            ty_lower::instantiate_callable_effect_layout_args,
        },
    },
    core::semantic::{EffectEnvView, ProviderBinding},
    hir_def::{CallableDef, Const},
};
use common::indexmap::IndexSet;
use rustc_hash::FxHashMap;

#[derive(Clone, Copy)]
enum ProviderResolutionMode {
    Final,
    Provisional,
}

pub(crate) fn semantic_callee_key_with_effect_providers<'db>(
    db: &'db dyn HirAnalysisDb,
    caller_key: SemanticInstanceKey<'db>,
    callable: &Callable<'db>,
    effect_providers: &[EffectProviderSpecialization<'db>],
) -> Option<SemanticInstanceKey<'db>> {
    let assumptions = SemanticInstance::new(db, caller_key).assumptions(db);
    semantic_callee_key_with_assumptions(
        db,
        caller_key,
        callable,
        effect_providers,
        assumptions,
        ProviderResolutionMode::Final,
    )
}

pub(crate) fn provisional_semantic_callee_key<'db>(
    db: &'db dyn HirAnalysisDb,
    caller_key: SemanticInstanceKey<'db>,
    callable: &Callable<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<SemanticInstanceKey<'db>> {
    semantic_callee_key_with_assumptions(
        db,
        caller_key,
        callable,
        callable.effect_providers(),
        assumptions,
        ProviderResolutionMode::Provisional,
    )
}

fn semantic_callee_key_with_assumptions<'db>(
    db: &'db dyn HirAnalysisDb,
    caller_key: SemanticInstanceKey<'db>,
    callable: &Callable<'db>,
    effect_providers: &[EffectProviderSpecialization<'db>],
    assumptions: PredicateListId<'db>,
    provider_resolution_mode: ProviderResolutionMode,
) -> Option<SemanticInstanceKey<'db>> {
    let impl_env = caller_key.impl_env(db);
    let mut resolved_trait_witness = None;
    let (owner, mut subst_args) = match callable.callable_def() {
        CallableDef::Func(func) => {
            let mut subst_args = callable.generic_args().to_vec();
            let owner = if let Some(inst) = callable.trait_inst()
                && let Some(name) = func.name(db).to_opt()
                && let Some((impl_func, impl_args)) = resolve_trait_method_instance(
                    db,
                    TraitSolveCx::new(db, impl_env.normalization_scope(db))
                        .with_assumptions(assumptions),
                    inst,
                    name,
                ) {
                subst_args = complete_resolved_trait_method_args(
                    db,
                    impl_func,
                    impl_args,
                    &subst_args,
                    inst.args(db).len(),
                );
                resolved_trait_witness = Some(inst);
                BodyOwner::Func(impl_func)
            } else {
                BodyOwner::Func(func)
            };
            (owner, subst_args)
        }
        CallableDef::VariantCtor(_) => return None,
    };
    let effect_providers = resolve_callable_effect_providers(
        db,
        caller_key,
        owner,
        &mut subst_args,
        effect_providers,
        provider_resolution_mode,
    );

    let impl_env = if let Some(witness) = resolved_trait_witness {
        ImplEnv::new(
            db,
            owner.scope(),
            PredicateListId::empty_list(db),
            vec![witness],
        )
    } else {
        let mut witnesses: IndexSet<_> = impl_env.witnesses(db).iter().copied().collect();
        if let Some(witness) = callable.trait_inst() {
            witnesses.insert(witness);
        }
        ImplEnv::new(
            db,
            impl_env.normalization_scope(db),
            assumptions,
            witnesses.into_iter().collect::<Vec<_>>(),
        )
    };

    Some(SemanticInstanceKey::new(
        db,
        owner,
        GenericSubst::new(db, subst_args),
        EffectProviderSubst::new(db, effect_providers),
        impl_env,
    ))
}

fn resolve_callable_effect_providers<'db>(
    db: &'db dyn HirAnalysisDb,
    caller_key: SemanticInstanceKey<'db>,
    owner: BodyOwner<'db>,
    subst_args: &mut [TyId<'db>],
    effect_providers: &[EffectProviderSpecialization<'db>],
    provider_resolution_mode: ProviderResolutionMode,
) -> Vec<EffectProviderSpecialization<'db>> {
    let caller = SemanticInstance::new(db, caller_key);
    let mut providers = effect_providers
        .iter()
        .map(|specialization| {
            let provider_idx = specialization.provider.provider_idx;
            let provider = match specialization.provenance {
                crate::analysis::ty::ty_check::EffectProviderProvenance::Binding {
                    binding,
                    ..
                } => provider_resolution_mode
                    .resolve_binding(db, caller, binding)
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
    if let BodyOwner::Func(func) = owner {
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
            .filter_map(|(effect_idx, param_idx)| {
                param_idx.map(|param_idx| (effect_idx, param_idx))
            })
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
            let actual_key_ty = effect_provider_target_ty(db, caller, provider);
            instantiate_callable_effect_layout_args(
                db,
                func,
                effect_idx,
                actual_key_ty,
                subst_args,
            );
        }
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
        ConstRef::TraitConst(assoc) => semantic_const_key_for_assoc_const(db, assoc, ty),
        ConstRef::InherentConst(use_) => semantic_const_key_for_inherent_const(db, use_, ty),
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
        GenericSubst::empty(db),
        EffectProviderSubst::empty(db),
        ImplEnv::empty(db, owner.scope()),
    ))
}

fn semantic_const_key_for_inherent_const<'db>(
    db: &'db dyn HirAnalysisDb,
    use_: InherentConstUse<'db>,
    ty: TyId<'db>,
) -> Option<SemanticInstanceKey<'db>> {
    let (body, impl_args) =
        inherent_const_body_and_impl_args(db, use_.impl_(), use_.receiver_ty(), use_.name())?;
    Some(SemanticInstanceKey::new(
        db,
        BodyOwner::AnonConstBody { body, expected: ty },
        GenericSubst::new(db, impl_args),
        EffectProviderSubst::empty(db),
        ImplEnv::new(db, use_.origin_scope(), use_.assumptions(), vec![]),
    ))
}

fn semantic_const_key_for_assoc_const<'db>(
    db: &'db dyn HirAnalysisDb,
    assoc: AssocConstUse<'db>,
    ty: TyId<'db>,
) -> Option<SemanticInstanceKey<'db>> {
    let (body, impl_args) = assoc_const_body_and_impl_args_for_trait_inst(
        db,
        assoc.solve_cx(db),
        assoc.inst(),
        assoc.name(),
    )?;
    Some(SemanticInstanceKey::new(
        db,
        BodyOwner::AnonConstBody { body, expected: ty },
        GenericSubst::new(db, impl_args),
        EffectProviderSubst::empty(db),
        ImplEnv::new(
            db,
            assoc.origin_scope(),
            assoc.assumptions(),
            vec![assoc.inst()],
        ),
    ))
}
