use super::{
    EffectProviderSubst, GenericSubst, ImplEnv, SemanticInstance, SemanticInstanceKey,
    provisional_provider_binding_for_instance_effect, provisional_provider_idx_for_requirement,
    resolved_effect_binding_ty_for_instance_effect, resolved_provider_binding_for_instance_effect,
};
use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{EvalOutcome, SemConstValue, SemOrigin, SemanticConstRef},
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
                ImplApplicationRecord, ImplementorOrigin, MethodArgMapError, ResolvedImplInstance,
                TraitInstId, assoc_const_body_template_for_trait_inst,
                resolve_trait_method_instance,
            },
            trait_resolution::{
                GoalSatisfiability, PredicateListId, Selection, TraitSolveCx,
                constraint::collect_func_decl_constraints, is_goal_satisfiable,
            },
            ty_check::{
                BodyOwner, Callable, ConstRef, EffectParamSite, EffectProviderSpecialization,
                LocalBinding, ResolvedEffectArg, ReturnProjectionStep,
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
        CallableDef, Const, Expr, ExprId, Func, GenericParamOwner, HirIngot, Partial,
        params::FuncParamMode, scope_graph::ScopeId,
    },
};
use common::indexmap::IndexSet;
use num_traits::ToPrimitive;
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

/// Give a closed, effect-free root the same proof environment as an ordinary
/// call from its own ingot. Symbolic roots and provider-bearing roots retain
/// their declaration environment; their identity is not an ordinary call.
pub(super) fn root_impl_env<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    subst: GenericSubst<'db>,
) -> ImplEnv<'db> {
    let env = ImplEnv::empty(db, owner.scope());
    let BodyOwner::Func(func) = owner else {
        return env;
    };
    if !(func.containing_impl(db).is_some() || !func.is_associated_func(db))
        || !func.effect_requirements(db).is_empty()
        || has_callable_layout_slots(db, func)
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
    let callable = Callable::from_item(
        db,
        CallableDef::Func(func),
        inherited.iter().chain(own_args).copied().collect(),
        trait_inst,
    );
    let callee = semantic_callee_key_with_assumptions(
        db,
        None,
        ImplEnv::new(db, scope, assumptions, Vec::new(), Vec::new(), Vec::new()),
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

/// Recover the issued choice through immutable binding aliases. This reads
/// already checked argument/intro records; admission remains at the method use.
pub(super) fn opened_operand_application<'db>(
    db: &'db dyn HirAnalysisDb,
    caller: SemanticInstance<'db>,
    operand: ExprId,
) -> Option<ImplApplicationRecord<'db>> {
    opened_operand_application_at(db, caller, operand, Vec::new())
}

fn compiler_known_operand_value<'db>(
    db: &'db dyn HirAnalysisDb,
    caller: SemanticInstance<'db>,
    operand: ExprId,
) -> Option<crate::analysis::semantic::SemConstId<'db>> {
    use crate::analysis::semantic::{
        ConstDesc, CtfeConfig, VerifiedConstValueId, const_computation_for_instance,
        force_const_computation, issue_checked_evidence,
    };
    let typed = caller.key(db).typed_body(db);
    let ty = typed.expr_ty(db, operand);
    let body = typed.body()?;
    match operand.data(db, body) {
        Partial::Present(Expr::Tuple(elems) | Expr::Array(elems)) => {
            let values = elems
                .iter()
                .map(|&value| compiler_known_operand_value(db, caller, value))
                .collect::<Option<Vec<_>>>()?;
            return Some(
                if matches!(operand.data(db, body), Partial::Present(Expr::Tuple(_))) {
                    crate::analysis::semantic::tuple_const(db, ty, values.into_boxed_slice())
                } else {
                    crate::analysis::semantic::array_const(db, ty, values.into_boxed_slice())
                },
            );
        }
        Partial::Present(Expr::Lit(crate::hir_def::LitKind::Int(value))) => {
            return Some(crate::analysis::semantic::int_const(
                db,
                ty,
                num_bigint::BigInt::from(value.data(db).clone()),
            ));
        }
        Partial::Present(Expr::Lit(crate::hir_def::LitKind::Bool(value))) => {
            return Some(crate::analysis::semantic::bool_const(db, *value));
        }
        _ => {}
    }
    if let Some(binding @ LocalBinding::Local { is_mut: false, .. }) =
        typed.expr_prop(db, operand).binding
    {
        let source = typed.binding_source(db, binding)?;
        let mut value = compiler_known_operand_value(db, caller, source.init_expr)?;
        for step in source.projection {
            value = match (step, value.value(db)) {
                (ReturnProjectionStep::Field(index), SemConstValue::Tuple { elems, .. }) => {
                    *elems.get(usize::from(index))?
                }
                (ReturnProjectionStep::Field(index), SemConstValue::Struct { fields, .. }) => {
                    *fields.get(usize::from(index))?
                }
                (
                    ReturnProjectionStep::ConstantIndex(index),
                    SemConstValue::Array { elems, .. },
                ) => *elems.get(index)?,
                _ => return None,
            };
        }
        return Some(value);
    }
    if typed.expr_prop(db, operand).binding.is_some() {
        let record = opened_operand_application(db, caller, operand)?;
        return issue_checked_evidence(db, record, ty, SemOrigin::Expr(operand))
            .ok()
            .map(|value| value.value());
    }
    let callable = typed.callable_expr(operand)?;
    if let Some(record) = callable.evidence_introduction() {
        return issue_checked_evidence(db, record, ty, SemOrigin::Expr(operand))
            .ok()
            .map(|value| value.value());
    }
    let CallableDef::Func(func) = callable.callable_def() else {
        return None;
    };
    if !func.is_const(db)
        || callable.checked_input_tys()?.len() != callable.argument_operands().len()
        || !typed.call_effect_args(operand).unwrap_or(&[]).is_empty()
        || callable.generic_args().iter().any(|arg| {
            arg.has_param(db)
                || arg.has_var(db)
                || arg.has_invalid(db)
                || arg.has_hole(db)
                || arg.has_projection(db)
        })
    {
        return None;
    }
    let values = callable
        .argument_operands()
        .iter()
        .map(|&argument| compiler_known_operand_value(db, caller, argument))
        .collect::<Option<Vec<_>>>()?;
    let callee =
        provisional_semantic_callee_key(db, caller.key(db), callable, &[], caller.assumptions(db))
            .ok()??;
    let parameters = values.iter().copied().enumerate().collect::<Vec<_>>();
    let env = callee
        .key
        .impl_env(db)
        .with_argument_evidence_values(db, &parameters);
    let key = SemanticInstanceKey::new(
        db,
        callee.key.owner(db),
        callee.key.subst(db),
        callee.key.effect_providers(db),
        env,
    );
    let inputs = values
        .into_iter()
        .map(|value| {
            VerifiedConstValueId::from_complete_execution(db, value)
                .ok()
                .map(|value| ConstDesc::value(db, value, key.owner(db).scope()))
        })
        .collect::<Option<Vec<_>>>()?;
    let EvalOutcome::Ready(value) = force_const_computation(
        db,
        const_computation_for_instance(db, key, inputs),
        CtfeConfig::default(),
    ) else {
        return None;
    };
    Some(value.value())
}

fn opened_operand_application_at<'db>(
    db: &'db dyn HirAnalysisDb,
    caller: SemanticInstance<'db>,
    mut operand: ExprId,
    mut projection: Vec<ReturnProjectionStep>,
) -> Option<ImplApplicationRecord<'db>> {
    let typed = caller.key(db).typed_body(db);
    let mut seen = Vec::new();
    loop {
        if seen.contains(&operand) {
            return None;
        }
        seen.push(operand);
        let body = typed.body()?;
        if let Partial::Present(Expr::Field(base, _)) = operand.data(db, body) {
            projection.insert(
                0,
                ReturnProjectionStep::Field(typed.resolved_field_index(operand)?),
            );
            operand = *base;
            continue;
        }
        if let Partial::Present(Expr::Bin(base, index, crate::hir_def::BinOp::Index)) =
            operand.data(db, body)
        {
            let Partial::Present(Expr::Lit(crate::hir_def::LitKind::Int(value))) =
                index.data(db, body)
            else {
                return None;
            };
            projection.insert(
                0,
                ReturnProjectionStep::ConstantIndex(value.data(db).to_usize()?),
            );
            operand = *base;
            continue;
        }
        if let Partial::Present(Expr::Array(elems)) = operand.data(db, body)
            && let Some(ReturnProjectionStep::ConstantIndex(index)) = projection.first().copied()
        {
            operand = *elems.get(index)?;
            projection.remove(0);
            continue;
        }
        if let Partial::Present(Expr::Tuple(elems)) = operand.data(db, body)
            && let Some(ReturnProjectionStep::Field(index)) = projection.first().copied()
        {
            operand = *elems.get(usize::from(index))?;
            projection.remove(0);
            continue;
        }
        match typed.expr_prop(db, operand).binding {
            Some(LocalBinding::Param {
                idx, is_mut: false, ..
            }) => {
                if !projection.is_empty() {
                    return caller
                        .key(db)
                        .impl_env(db)
                        .projected_parameter_evidence(db)
                        .iter()
                        .find(|(index, path, _)| *index == idx && *path == projection)
                        .map(|(_, _, record)| *record);
                }
                return caller
                    .key(db)
                    .impl_env(db)
                    .parameter_evidence(db)
                    .iter()
                    .find(|(index, _)| *index == idx)
                    .map(|(_, record)| *record);
            }
            Some(binding @ LocalBinding::Local { is_mut: false, .. }) => {
                let source = typed.binding_source(db, binding)?;
                let mut source_projection = source.projection;
                source_projection.extend(projection);
                projection = source_projection;
                operand = source.init_expr;
            }
            _ => {
                let callable = typed.callable_expr(operand)?;
                if let Some(record) = callable.evidence_introduction() {
                    return projection.is_empty().then_some(record);
                }
                let value = compiler_known_operand_value(db, caller, operand)?;
                let mut value = value;
                for step in &projection {
                    value = match (step, value.value(db)) {
                        (
                            ReturnProjectionStep::Field(index),
                            SemConstValue::Tuple { elems, .. },
                        ) => *elems.get(usize::from(*index))?,
                        (
                            ReturnProjectionStep::Field(index),
                            SemConstValue::Struct { fields, .. },
                        ) => *fields.get(usize::from(*index))?,
                        (
                            ReturnProjectionStep::ConstantIndex(index),
                            SemConstValue::Array { elems, .. },
                        ) => *elems.get(*index)?,
                        _ => return None,
                    };
                }
                return match value.value(db) {
                    SemConstValue::Evidence { application, .. } => Some(*application),
                    _ => None,
                };
            }
        }
    }
}

fn evidence_slots<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
    path: Vec<ReturnProjectionStep>,
    active: &mut IndexSet<TyId<'db>>,
    slots: &mut Vec<Vec<ReturnProjectionStep>>,
) {
    let ty = ty.as_capability(db).map_or(ty, |(_, inner)| inner);
    if crate::analysis::ty::corelib::evidence_goal(db, scope, ty).is_some() {
        slots.push(path);
        return;
    }
    if ty.is_array(db) {
        let mut element_path = path;
        element_path.push(ReturnProjectionStep::AnyIndex);
        evidence_slots(
            db,
            scope,
            ty.generic_args(db)[0],
            element_path,
            active,
            slots,
        );
        return;
    }
    let base = ty.base_ty(db);
    if !active.insert(base) {
        return;
    }
    // Tuple nesting is finite and may reuse the same tuple constructor.
    if base.is_tuple(db) {
        active.remove(&base);
    }
    for (index, field) in ty.field_types(db).into_iter().enumerate() {
        let mut field_path = path.clone();
        field_path.push(ReturnProjectionStep::Field(
            u16::try_from(index).expect("field index"),
        ));
        evidence_slots(db, scope, field, field_path, active, slots);
    }
    active.remove(&base);
}

pub(super) fn visible_applications<'db>(
    db: &'db dyn HirAnalysisDb,
    caller: SemanticInstance<'db>,
    callable: &Callable<'db>,
) -> Vec<ImplApplicationRecord<'db>> {
    let mut records = Vec::new();
    for opening in callable.lexical_openings() {
        let record = match *opening {
            crate::analysis::ty::ty_check::TraitOpening::Named(record) => Some(record),
            crate::analysis::ty::ty_check::TraitOpening::Evidence { operand, .. } => {
                opened_operand_application(db, caller, operand)
            }
        };
        if let Some(record) = record {
            records.push(record);
        }
    }
    for record in caller.key(db).impl_env(db).lexical_applications(db) {
        if !records.iter().any(|existing| {
            existing.goal().def(db) == record.goal().def(db)
                && existing.goal().args(db) == record.goal().args(db)
        }) {
            records.push(*record);
        }
    }
    records
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
        && let Selection::Unique(method) =
            if let Some((operand, destination)) = callable.opened_trait_application() {
                let caller = caller.ok_or(MethodArgMapError::MissingBody)?;
                match opened_operand_application(db, caller, operand) {
                    Some(record) => record
                        .revalidate(db, destination, inst)
                        .map_err(MethodArgMapError::ApplicationNotAdmitted)?
                        .method(name),
                    // The layout template has no issued argument yet. Keep its
                    // symbolic obligation; a concrete use still requires the record.
                    None if collect_flags(db, inst).contains(TyFlags::HAS_PARAM) => {
                        resolve_trait_method_instance(db, destination, inst, name)
                    }
                    None => return Err(MethodArgMapError::MissingBody),
                }
            } else if let Some(record) = callable.selected_trait_application() {
                record
                    .revalidate(db, record.origin_context(), inst)
                    .map_err(MethodArgMapError::ApplicationNotAdmitted)?
                    .method(name)
            } else if let Some(caller) = caller
                && let Some(record) =
                    visible_applications(db, caller, callable)
                        .into_iter()
                        .find(|record| {
                            record.goal().def(db) == inst.def(db)
                                && record.goal().args(db) == inst.args(db)
                        })
            {
                let records = visible_applications(db, caller, callable);
                let premises = PredicateListId::new(
                    db,
                    records
                        .iter()
                        .map(|record| record.goal())
                        .collect::<Vec<_>>(),
                )
                .extend_all_bounds(db);
                let mut facts = assumptions.list(db).to_vec();
                facts.extend(premises.list(db).iter().copied());
                let destination = TraitSolveCx::new(db, caller.key(db).owner(db).scope())
                    .with_assumptions(PredicateListId::new(db, facts).extend_all_bounds(db))
                    .with_lexical_premises(Some(premises));
                record
                    .revalidate(db, destination, inst)
                    .map_err(MethodArgMapError::ApplicationNotAdmitted)?
                    .method(name)
            } else {
                resolve_trait_method_instance(
                    db,
                    TraitSolveCx::new(db, impl_env.normalization_scope(db))
                        .with_assumptions(assumptions),
                    inst,
                    name,
                )
            }
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
            && {
                let body_args = body_func.arg_tys(db);
                nominal_func.arg_tys(db).len() == body_args.len()
                    && body_args.iter().enumerate().all(|(idx, body)| {
                        let nominal = callable
                            .arg_ty(db, idx)
                            .expect("nominal input arity changed");
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
        && !has_callable_layout_slots(db, nominal_func)
        && is_ground(
            collect_flags(db, subst_args.as_slice())
                | collect_flags(db, effect_providers.as_slice())
                | collect_flags(db, effect_args),
        )
    {
        // A concrete free or inherent function sheds caller context when its
        // bounds hold and its signature and effect keys normalize the same
        // without the caller's assumptions.
        let empty = PredicateListId::empty_list(db);
        let scope = nominal_func.scope();
        let caller_scope = impl_env.normalization_scope(db);
        let same_without_caller = |ty| {
            normalize_ty(db, ty, caller_scope, assumptions) == normalize_ty(db, ty, scope, empty)
        };
        let bounds = collect_func_decl_constraints(db, CallableDef::Func(nominal_func), true)
            .instantiate(db, &subst_args);
        let independent = bounds.list(db).iter().copied().all(|bound| {
            matches!(
                is_goal_satisfiable(db, TraitSolveCx::new(db, scope), bound),
                GoalSatisfiability::Satisfied(_)
            )
        }) && (0..nominal_func.arg_tys(db).len()).all(|idx| {
            same_without_caller(
                callable
                    .arg_ty(db, idx)
                    .expect("nominal input arity changed"),
            )
        }) && same_without_caller(callable.ret_ty(db))
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
    let lexical_applications = caller
        .map(|caller| visible_applications(db, caller, callable))
        .unwrap_or_default();
    let parameter_evidence = caller
        .map(|caller| {
            let typed = caller.key(db).typed_body(db);
            callable
                .argument_operands()
                .iter()
                .enumerate()
                .filter_map(|(index, &operand)| {
                    let ty = typed.expr_ty(db, operand);
                    let ty = ty.as_capability(db).map_or(ty, |(_, inner)| inner);
                    crate::analysis::ty::corelib::evidence_goal(
                        db,
                        caller.key(db).owner(db).scope(),
                        ty,
                    )?;
                    opened_operand_application(db, caller, operand).map(|record| (index, record))
                })
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    let projected_parameter_evidence = caller
        .map(|caller| {
            let typed = caller.key(db).typed_body(db);
            let mut records = Vec::new();
            for (index, &operand) in callable.argument_operands().iter().enumerate() {
                let mut slots = Vec::new();
                evidence_slots(
                    db,
                    caller.key(db).owner(db).scope(),
                    typed.expr_ty(db, operand),
                    Vec::new(),
                    &mut IndexSet::default(),
                    &mut slots,
                );
                if slots
                    .iter()
                    .any(|path| path.contains(&ReturnProjectionStep::AnyIndex))
                {
                    if let Some(LocalBinding::Param {
                        idx, is_mut: false, ..
                    }) = typed.expr_prop(db, operand).binding
                    {
                        records.extend(
                            caller
                                .key(db)
                                .impl_env(db)
                                .projected_parameter_evidence(db)
                                .iter()
                                .filter(|(source, _, _)| *source == idx)
                                .map(|(_, path, record)| (index, path.clone(), *record)),
                        );
                    } else if let Some(value) = compiler_known_operand_value(db, caller, operand) {
                        let env = ImplEnv::empty(db, caller.key(db).owner(db).scope())
                            .with_argument_evidence_values(db, &[(index, value)]);
                        records.extend(env.projected_parameter_evidence(db).iter().cloned());
                    }
                    continue;
                }
                for path in slots.into_iter().filter(|path| !path.is_empty()) {
                    if let Some(record) =
                        opened_operand_application_at(db, caller, operand, path.clone())
                    {
                        records.push((index, path, record));
                    }
                }
            }
            records
        })
        .unwrap_or_default();
    let impl_env = ImplEnv::new(
        db,
        normalization_scope,
        if context_independent {
            PredicateListId::empty_list(db)
        } else {
            assumptions
        },
        witnesses.into_iter().collect::<Vec<_>>(),
        parameter_evidence,
        lexical_applications,
    )
    .with_projected_parameter_evidence(db, projected_parameter_evidence);

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
            EffectProviderSubst::new(
                db,
                effect_providers
                    .into_iter()
                    .map(|specialization| specialization.provider)
                    .collect::<Vec<_>>(),
            ),
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
        ImplEnv::new(
            db,
            use_.origin_scope(),
            use_.assumptions(),
            vec![],
            Vec::new(),
            Vec::new(),
        ),
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
            Vec::new(),
            Vec::new(),
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
