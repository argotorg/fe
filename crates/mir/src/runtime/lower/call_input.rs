use hir::analysis::semantic::{
    SemanticInstance,
    normalized::{NEffectArg, NEffectArgValue},
};

use crate::{
    db::MirDb,
    runtime::{RuntimeBoundarySpec, RuntimeClass, RuntimeParamPlan},
};

use super::{
    boundary::{BoundarySiteAllocator, StagedBoundary},
    classify::runtime_effect_binding_plan_for_binding_idx,
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
    Value(CompiledValuePassPlan<'db>),
    Place(StagedBoundary<'db>),
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
    semantic: SemanticInstance<'db>,
    effect_args: &[NEffectArg<'db>],
    boundary_sites: &mut BoundarySiteAllocator,
) -> CompiledCallInputPlan<'db> {
    let param_plans = super::interface::runtime_param_plans(db, semantic)
        .iter()
        .cloned()
        .map(|plan| compile_value_pass_plan(plan, boundary_sites))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    // The callee's plan for the effect decides how its provider is passed.
    let effect_plans = effect_args
        .iter()
        .map(|arg| {
            let Some(plan) =
                runtime_effect_binding_plan_for_binding_idx(db, semantic, arg.binding_idx)
            else {
                return CompiledEffectArgPlan::Erased;
            };
            match arg.arg {
                NEffectArgValue::Value(_) => CompiledEffectArgPlan::Value(compile_value_pass_plan(
                    RuntimeParamPlan::Boundary(plan.boundary),
                    boundary_sites,
                )),
                NEffectArgValue::Place(_) => {
                    CompiledEffectArgPlan::Place(boundary_sites.stage(plan.boundary))
                }
            }
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    CompiledCallInputPlan {
        param_plans,
        effect_plans,
    }
}
