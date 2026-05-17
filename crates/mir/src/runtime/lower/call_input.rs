use hir::analysis::{
    semantic::{NEffectArg, NEffectArgValue, SemanticInstance},
    ty::ty_check::EffectPassMode,
};

use crate::{
    db::MirDb,
    runtime::{RuntimeBoundarySpec, RuntimeClass, RuntimeParamPlan},
};

use super::{
    boundary::{BoundarySiteAllocator, StagedBoundary, default_by_place_boundary},
    classify::{desired_runtime_effect_arg_boundary, runtime_effect_binding_plan_for_binding_idx},
    place::resolved_effect_arg_address_space,
    type_info::{
        RuntimeTypeEnv, provider_class_for_target_in_env, runtime_zero_sized_transport_ty,
        runtime_zero_sized_ty,
    },
};

#[derive(Clone, Debug)]
pub(super) enum CompiledMaterializationPlan<'db> {
    Erased,
    SemanticValue,
    AggregateFromSource,
    AggregateFromSourceOrFallback(RuntimeClass<'db>),
}

#[derive(Clone, Debug)]
pub(super) enum CompiledValuePassPlan<'db> {
    Erased,
    VisibleValue,
    ActualValue,
    ExactTransport(RuntimeClass<'db>),
    ExactShapeAggregate(RuntimeClass<'db>),
    ExactShapeRefLike(StagedBoundary<'db>),
    ReadOnlyView {
        value: RuntimeClass<'db>,
        borrow: StagedBoundary<'db>,
    },
    BorrowLike(StagedBoundary<'db>),
}

#[derive(Clone, Debug)]
pub(super) enum CompiledEffectArgPlan<'db> {
    Erased,
    Value(CompiledEffectValuePlan<'db>),
    Place(CompiledEffectPlacePlan<'db>),
}

#[derive(Clone, Debug)]
pub(super) enum CompiledEffectValuePlan<'db> {
    ErasedPlainValue,
    ByValue(CompiledValuePassPlan<'db>),
    ByValueFallback(RuntimeClass<'db>),
    ByPlace {
        boundary: StagedBoundary<'db>,
        allow_materialize: bool,
    },
}

#[derive(Clone, Debug)]
pub(super) enum CompiledEffectPlacePlan<'db> {
    Boundary(StagedBoundary<'db>),
    Fallback(RuntimeClass<'db>),
}

#[derive(Clone, Debug)]
pub(super) struct CompiledCallInputPlan<'db> {
    pub(super) param_plans: Box<[CompiledValuePassPlan<'db>]>,
    pub(super) effect_plans: Box<[CompiledEffectArgPlan<'db>]>,
}

pub(super) fn compile_value_pass_plan<'db>(
    plan: RuntimeParamPlan<'db>,
    boundary_sites: &mut BoundarySiteAllocator,
) -> CompiledValuePassPlan<'db> {
    match plan {
        RuntimeParamPlan::Erased => CompiledValuePassPlan::Erased,
        RuntimeParamPlan::PassActual => CompiledValuePassPlan::VisibleValue,
        RuntimeParamPlan::ReadOnlyView { value, borrow } => CompiledValuePassPlan::ReadOnlyView {
            value,
            borrow: boundary_sites.stage(borrow),
        },
        RuntimeParamPlan::Boundary(RuntimeBoundarySpec::ExactTransport(exact)) => {
            CompiledValuePassPlan::ExactTransport(exact)
        }
        RuntimeParamPlan::Boundary(RuntimeBoundarySpec::ExactShape(
            exact @ RuntimeClass::AggregateValue { .. },
        )) => CompiledValuePassPlan::ExactShapeAggregate(exact),
        RuntimeParamPlan::Boundary(
            boundary @ RuntimeBoundarySpec::ExactShape(
                RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. },
            ),
        ) => CompiledValuePassPlan::ExactShapeRefLike(boundary_sites.stage(boundary)),
        RuntimeParamPlan::Boundary(RuntimeBoundarySpec::ExactShape(exact)) => {
            CompiledValuePassPlan::ExactTransport(exact)
        }
        RuntimeParamPlan::Boundary(boundary @ RuntimeBoundarySpec::BorrowLike { .. }) => {
            CompiledValuePassPlan::BorrowLike(boundary_sites.stage(boundary))
        }
    }
}

pub(super) fn compile_call_input_plan_for_semantic<'db>(
    db: &'db dyn MirDb,
    body: &hir::analysis::semantic::borrowck::NormalizedSemanticBody<'db>,
    semantic: SemanticInstance<'db>,
    type_env: RuntimeTypeEnv<'db>,
    effect_args: &[NEffectArg<'db>],
    boundary_sites: &mut BoundarySiteAllocator,
) -> CompiledCallInputPlan<'db> {
    let param_plans = super::interface::runtime_param_plans(db, semantic)
        .iter()
        .cloned()
        .map(|plan| compile_value_pass_plan(plan, boundary_sites))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let effect_plans = effect_args
        .iter()
        .map(|arg| compile_effect_arg_plan(db, body, semantic, type_env, arg, boundary_sites))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    CompiledCallInputPlan {
        param_plans,
        effect_plans,
    }
}

fn compile_effect_arg_plan<'db>(
    db: &'db dyn MirDb,
    body: &hir::analysis::semantic::borrowck::NormalizedSemanticBody<'db>,
    semantic: SemanticInstance<'db>,
    type_env: RuntimeTypeEnv<'db>,
    arg: &NEffectArg<'db>,
    boundary_sites: &mut BoundarySiteAllocator,
) -> CompiledEffectArgPlan<'db> {
    let space = resolved_effect_arg_address_space(db, body, arg);
    let boundary = desired_runtime_effect_arg_boundary(
        db,
        type_env,
        arg,
        runtime_effect_binding_plan_for_binding_idx(db, semantic, arg.binding_idx).as_ref(),
        space,
    );
    if boundary.is_none() && effect_arg_is_runtime_zst(db, body, type_env, arg) {
        return CompiledEffectArgPlan::Erased;
    }
    if boundary.is_none() && arg.provider.is_none() && arg.target_ty.is_none() {
        return match (&arg.pass_mode, &arg.arg) {
            (EffectPassMode::ByValue | EffectPassMode::Unknown, NEffectArgValue::Value(_)) => {
                CompiledEffectArgPlan::Value(CompiledEffectValuePlan::ErasedPlainValue)
            }
            (EffectPassMode::ByValue | EffectPassMode::Unknown, NEffectArgValue::Place(_))
            | (
                EffectPassMode::ByPlace | EffectPassMode::ByTempPlace,
                NEffectArgValue::Value(_) | NEffectArgValue::Place(_),
            ) => panic!(
                "effect arg without provider/target should compile as a plain value: owner={:?}; arg={arg:?}",
                body.owner.key(db).owner(db),
            ),
        };
    }
    match (&arg.pass_mode, &arg.arg) {
        (EffectPassMode::ByValue | EffectPassMode::Unknown, NEffectArgValue::Value(_)) => {
            let plan = boundary.map_or(CompiledValuePassPlan::ActualValue, |boundary| {
                compile_value_pass_plan(RuntimeParamPlan::Boundary(boundary), boundary_sites)
            });
            if matches!(plan, CompiledValuePassPlan::ActualValue)
                && (arg.provider.is_some() || arg.target_ty.is_some())
            {
                CompiledEffectArgPlan::Value(CompiledEffectValuePlan::ByValueFallback(
                    provider_class_for_target_in_env(db, type_env, arg.target_ty, space),
                ))
            } else {
                CompiledEffectArgPlan::Value(CompiledEffectValuePlan::ByValue(plan))
            }
        }
        (EffectPassMode::ByValue | EffectPassMode::Unknown, NEffectArgValue::Place(_)) => boundary
            .map_or_else(
                || {
                    CompiledEffectArgPlan::Place(CompiledEffectPlacePlan::Fallback(
                        provider_class_for_target_in_env(db, type_env, arg.target_ty, space),
                    ))
                },
                |boundary| {
                    CompiledEffectArgPlan::Place(CompiledEffectPlacePlan::Boundary(
                        boundary_sites.stage(boundary),
                    ))
                },
            ),
        (EffectPassMode::ByPlace | EffectPassMode::ByTempPlace, NEffectArgValue::Value(_)) => {
            let boundary = boundary
                .unwrap_or_else(|| default_by_place_boundary(db, type_env, arg.target_ty, space));
            CompiledEffectArgPlan::Value(CompiledEffectValuePlan::ByPlace {
                boundary: boundary_sites.stage(boundary),
                allow_materialize: matches!(arg.pass_mode, EffectPassMode::ByTempPlace),
            })
        }
        (EffectPassMode::ByPlace | EffectPassMode::ByTempPlace, NEffectArgValue::Place(_)) => {
            let boundary = boundary
                .unwrap_or_else(|| default_by_place_boundary(db, type_env, arg.target_ty, space));
            CompiledEffectArgPlan::Place(CompiledEffectPlacePlan::Boundary(
                boundary_sites.stage(boundary),
            ))
        }
    }
}

fn effect_arg_is_runtime_zst<'db>(
    db: &'db dyn MirDb,
    body: &hir::analysis::semantic::borrowck::NormalizedSemanticBody<'db>,
    type_env: RuntimeTypeEnv<'db>,
    arg: &NEffectArg<'db>,
) -> bool {
    if arg.target_ty.is_some_and(|target_ty| {
        runtime_zero_sized_ty(db, target_ty, type_env.scope, type_env.assumptions)
    }) {
        return true;
    }
    match &arg.arg {
        NEffectArgValue::Value(value) => body.local(value.local).is_some_and(|local| {
            runtime_zero_sized_transport_ty(db, local.ty, type_env.scope, type_env.assumptions)
        }),
        NEffectArgValue::Place(_) => false,
    }
}
