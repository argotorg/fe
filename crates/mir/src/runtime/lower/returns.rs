use std::collections::{BTreeMap, VecDeque};

use cranelift_entity::{EntityRef, PrimaryMap, SecondaryMap, entity_impl};
use hir::analysis::{
    semantic::{
        SLocalId, SemanticInstance,
        capability::{
            external::{ExternalOrigin, ExternalSource},
            guard::ValueOccurrence,
            handle::HandleAddressSpace,
            path::{Projection, project_referent_ty},
            semantics::CapabilityClass,
            source::{InputOrigin, SourceExpr},
            value::{ValueInterner, ValueLimits},
        },
        normalized::NTerminatorKind,
        semantic_borrow_summary,
    },
    ty::{
        ty_check::{ReturnProjectionStep, ReturnProvenance},
        ty_def::TyId,
    },
};
use rustc_hash::FxHashSet;
use salsa::Update;

use crate::{
    db::MirDb,
    instance::{RuntimeInstanceKey, RuntimeInstanceSource},
    runtime::{
        AddressSpaceKind, EnumLayoutKey, Layout, LayoutId, LayoutKey, RawPointeeId, RefKind,
        RuntimeClass, RuntimeExitBehavior, relation::runtime_classes_equivalent,
    },
};

use super::{
    boundary::BoundaryMatcher,
    classify::{
        AssignmentId, BodyEnv, BodyStaticFacts, RuntimeVisibleReturnPlan, default_return_class,
        desired_runtime_return_plan, selected_visible_return_for_operand,
    },
    infer::{
        AssignmentSpace, CarrierInferer, ReturnClassLookup, join_reference_transports,
        merge_runtime_class,
    },
    interface::{runtime_visible_binding_local, runtime_visible_binding_plans},
    provider_space::address_space_from_provider,
    semantic_body::{RuntimeOperand, RuntimeSemanticBody},
    type_info::{RuntimeTypeEnv, stored_class_for_ty_in_env},
};
use crate::runtime::synthetic::runtime_synthetic_exit_behavior;

#[derive(Clone, Debug, PartialEq, Eq, Update)]
pub(crate) enum StaticRuntimeReturnDecision<'db> {
    Known(Option<RuntimeClass<'db>>),
    Dynamic,
}

#[derive(Clone)]
pub(crate) struct RuntimeReturnSummary<'db> {
    pub(crate) semantic_body: RuntimeSemanticBody<'db>,
    pub(crate) facts: BodyStaticFacts<'db>,
    pub(crate) return_plan: RuntimeVisibleReturnPlan<'db>,
    pub(crate) default_return_class: Option<RuntimeClass<'db>>,
    pub(crate) param_locals: Box<[SLocalId]>,
    pub(crate) return_operands: Box<[RuntimeOperand]>,
    pub(crate) slice_assignment_ids: PrimaryMap<SliceAssignmentId, AssignmentId>,
    pub(crate) slice_assignment_positions: SecondaryMap<AssignmentId, Option<SliceAssignmentId>>,
    pub(crate) slice_assignments_by_local: Vec<Vec<AssignmentId>>,
    pub(crate) slice_dynamic_dependents_by_local: Vec<Vec<SLocalId>>,
}

impl<'db> PartialEq for RuntimeReturnSummary<'db> {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl<'db> Eq for RuntimeReturnSummary<'db> {}

unsafe impl<'db> salsa::Update for RuntimeReturnSummary<'db> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        unsafe {
            *old_pointer = new_value;
        }
        true
    }
}

impl<'db> RuntimeReturnSummary<'db> {
    fn build(
        db: &'db dyn MirDb,
        semantic: SemanticInstance<'db>,
        semantic_body: &RuntimeSemanticBody<'db>,
    ) -> Self {
        let facts = BodyStaticFacts::new(db, semantic_body);
        let param_locals = runtime_visible_binding_plans(db, semantic)
            .iter()
            .map(|entry| runtime_visible_binding_local(&semantic_body.source, entry.binding))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let return_operands = semantic_body
            .normalized
            .blocks
            .iter()
            .filter_map(|block| match &block.terminator.kind {
                NTerminatorKind::Return(Some(value)) => semantic_body.runtime_operand(*value),
                NTerminatorKind::Goto(_)
                | NTerminatorKind::Branch { .. }
                | NTerminatorKind::MatchEnum { .. }
                | NTerminatorKind::Assert { .. }
                | NTerminatorKind::Return(None) => None,
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let env = BodyEnv::new(db, semantic_body, &facts);
        let mut return_plan = desired_runtime_return_plan(db, semantic);
        let mut default_return_class = default_return_class(db, semantic);
        if matches!(return_plan, RuntimeVisibleReturnPlan::Erased) {
            let mut fallback = None;
            let mut all_fallbacks_match = true;
            for operand in return_operands.iter().copied() {
                let Some(class) = env.root_transport_fallback_class(operand.local) else {
                    continue;
                };
                match &fallback {
                    Some(fallback) if fallback != &class => all_fallbacks_match = false,
                    None => fallback = Some(class),
                    Some(_) => {}
                }
            }
            if fallback.is_some() {
                return_plan = RuntimeVisibleReturnPlan::PassActual;
                if default_return_class.is_none() && all_fallbacks_match {
                    default_return_class = fallback;
                }
            }
        }

        let mut needed_assignments = FxHashSet::default();
        let mut needed_locals = FxHashSet::default();
        let mut pending = return_operands
            .iter()
            .map(|operand| operand.local)
            .collect::<VecDeque<_>>();
        while let Some(local) = pending.pop_front() {
            if !needed_locals.insert(local) {
                continue;
            }
            for dependency in facts.source_locals(local).iter().copied() {
                pending.push_back(dependency);
            }
            for assign_id in facts.assignments_defining_local(local).iter().copied() {
                if needed_assignments.insert(assign_id) && facts.assignment(assign_id).is_some() {
                    for used in facts.assignment_uses(assign_id).iter().copied() {
                        pending.push_back(used);
                    }
                }
            }
        }

        let mut slice_assignment_ids = needed_assignments.into_iter().collect::<Vec<_>>();
        slice_assignment_ids.sort_unstable();
        let slice_assignment_ids: PrimaryMap<SliceAssignmentId, AssignmentId> =
            slice_assignment_ids.into_iter().collect();
        let mut slice_assignment_positions = SecondaryMap::new();
        slice_assignment_positions.resize(facts.assignments().len());
        for (slice_idx, &assign_id) in slice_assignment_ids.iter() {
            slice_assignment_positions[assign_id] = Some(slice_idx);
        }

        let mut slice_assignments_by_local = vec![Vec::new(); semantic_body.locals.len()];
        for (_, &assign_id) in slice_assignment_ids.iter() {
            facts
                .assignment(assign_id)
                .unwrap_or_else(|| panic!("missing sliced assignment {assign_id:?}"));
            for used in facts.assignment_uses(assign_id).iter().copied() {
                slice_assignments_by_local[used.index()].push(assign_id);
            }
        }

        let mut slice_dynamic_dependents_by_local = vec![Vec::new(); semantic_body.locals.len()];
        for local in needed_locals.iter().copied() {
            for dependency in facts.source_locals(local).iter().copied() {
                slice_dynamic_dependents_by_local[dependency.index()].push(local);
            }
        }

        Self {
            semantic_body: semantic_body.clone(),
            facts,
            return_plan,
            default_return_class,
            param_locals,
            return_operands,
            slice_assignment_ids,
            slice_assignment_positions,
            slice_assignments_by_local,
            slice_dynamic_dependents_by_local,
        }
    }

    fn env(&self, db: &'db dyn MirDb) -> BodyEnv<'_, 'db> {
        BodyEnv::new(db, &self.semantic_body, &self.facts)
    }
}

pub(crate) fn runtime_return_class_for_body<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
    body: &RuntimeSemanticBody<'db>,
) -> Option<RuntimeClass<'db>> {
    let semantic = key.semantic(db)?;
    if let StaticRuntimeReturnDecision::Known(class) = static_runtime_return_decision(db, semantic)
    {
        return class;
    }
    let summary = RuntimeReturnSummary::build(db, semantic, body);
    evaluate_runtime_return_class(db, &summary, key.params(db), &mut |callee_key| {
        declaration_runtime_return_class(db, callee_key)
    })
}

pub(crate) fn declaration_runtime_return_class<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
) -> Option<RuntimeClass<'db>> {
    let semantic = key.semantic(db)?;
    if let StaticRuntimeReturnDecision::Known(class) = static_runtime_return_decision(db, semantic)
    {
        return class;
    }

    let mut class = default_return_class(db, semantic)?;
    let bindings = runtime_visible_binding_plans(db, semantic);
    if bindings.len() != key.params(db).len() {
        return Some(class);
    }
    let typed_body = semantic.key(db).typed_body(db);
    let fully_forwarded = matches!(
        typed_body.return_provenance(db),
        ReturnProvenance::Forwarded(_)
    );
    let mut replaced = FxHashSet::default();
    for source in typed_body.forwarded_return_sources(db) {
        let Some((_, source_class)) = bindings
            .iter()
            .zip(key.params(db))
            .find(|(binding, _)| binding.binding.callable_input_origin(db) == Some(source.origin))
        else {
            continue;
        };
        let Some(projected) =
            project_declaration_return_source(db, source_class.clone(), &source.projection)
        else {
            continue;
        };
        let merge = if !fully_forwarded {
            ReturnSourceMerge::Specialize
        } else if replaced.insert(source.result_projection.clone()) {
            ReturnSourceMerge::Replace
        } else {
            ReturnSourceMerge::Join
        };
        let Some(updated) = merge_declaration_return_source(
            db,
            class.clone(),
            &source.result_projection,
            source_class,
            &projected,
            merge,
        ) else {
            continue;
        };
        class = updated;
    }
    // Native references into raw memory retain the pointer's layout. Type-level
    // layout forwarding alone cannot describe a pointer loaded from a container.
    // The shared borrow summary retains those load/dereference transitions.
    if let Ok(Some(summary)) = semantic_borrow_summary(db, semantic) {
        let values = ValueInterner::<SourceExpr<'db>>::new(db, ValueLimits::default());
        let mut transports = BTreeMap::new();
        for leaf in values.leaves(&summary.result, ValueOccurrence::Summary) {
            if !matches!(leaf.semantics.class, CapabilityClass::Borrow(_)) {
                continue;
            }
            let projection: Vec<_> = leaf
                .path
                .as_slice()
                .iter()
                .map(|step| match step {
                    Projection::Field(field) => ReturnProjectionStep::Field(field.0),
                    Projection::VariantField { variant, field } => {
                        ReturnProjectionStep::VariantField {
                            variant: variant.0,
                            field: field.0,
                        }
                    }
                    Projection::Index(_) => ReturnProjectionStep::AnyIndex,
                })
                .collect();
            let space = raw_return_space(db, semantic, &leaf.payload.source);
            transports
                .entry(projection)
                .and_modify(|previous| {
                    if *previous != space {
                        *previous = None;
                    }
                })
                .or_insert(space);
        }
        for (projection, space) in transports {
            let Some(space) = space else {
                continue;
            };
            if projection.is_empty()
                && let Some((_, target)) = semantic.normalized_result_ty(db).as_borrow(db)
            {
                // The summary excludes nonreturning paths and retains pointer
                // dereferences that type-level forwarding may lose. A bare
                // borrow keeps that raw transport with its declared target;
                // stored borrow fields still use their canonical descriptors.
                let pointee = stored_class_for_ty_in_env(
                    db,
                    RuntimeTypeEnv::for_semantic(db, semantic),
                    target,
                );
                class = RuntimeClass::raw_addr(db, space, pointee);
                continue;
            }
            let Some(RuntimeClass::Ref { pointee, .. }) =
                project_declaration_return_source(db, class.clone(), &projection)
            else {
                continue;
            };
            let source = RuntimeClass::raw_addr(db, space, *pointee);
            if let Some(updated) = merge_declaration_return_source(
                db,
                class.clone(),
                &projection,
                &source,
                &source,
                ReturnSourceMerge::Replace,
            ) {
                class = updated;
            }
        }
    }
    // Apply the same boundary recipe as body return selection, including
    // canonical raw pointees for forwarded aggregate references.
    if let RuntimeVisibleReturnPlan::Constrained(boundary) =
        desired_runtime_return_plan(db, semantic)
        && let Some(selected) = BoundaryMatcher::selected_class(db, &class, &boundary)
    {
        class = selected;
    }
    Some(class)
}

fn raw_return_space<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    source: &ExternalSource<'db>,
) -> Option<AddressSpaceKind> {
    if source.is_reachable() {
        return None;
    }
    let mut steps = Vec::new();
    let (mut target, mut raw) = match &source.origin {
        ExternalOrigin::Input(input) => {
            let binding = semantic
                .key(db)
                .typed_body(db)
                .param_binding(input.param() as usize)?;
            let param_ty = semantic.normalized_binding_ty(db, binding);
            let carrier = match input.origin() {
                InputOrigin::Place(_) => param_ty,
                InputOrigin::Slot { slot, .. } => project_referent_ty(
                    db,
                    semantic,
                    param_ty.as_view(db).unwrap_or(param_ty),
                    slot.as_slice(),
                )?,
            };
            steps.extend(input.dereferences().iter());
            (
                carrier
                    .as_ptr(db)
                    .or_else(|| carrier.as_capability(db).map(|(_, target)| target))?,
                carrier.as_ptr(db).is_some(),
            )
        }
        ExternalOrigin::Memory { target_ty, .. } => (*target_ty, true),
        ExternalOrigin::Allocation(handle) | ExternalOrigin::OpaqueHandle(handle) => (
            handle.contract.target_ty,
            handle.contract.handle_ty.as_ptr(db).is_some(),
        ),
        ExternalOrigin::Provider {
            target_ty,
            provider,
            ..
        } => (
            *target_ty,
            provider.binding(db).provider_ty.as_ptr(db).is_some(),
        ),
        // An unknown semantic referent supplies no raw-carrier layout evidence.
        ExternalOrigin::Local(_)
        | ExternalOrigin::Unknown { .. }
        | ExternalOrigin::OpaqueMemory => {
            return None;
        }
    };
    steps.extend(source.dereferences().iter());
    for path in steps {
        let carrier = project_referent_ty(db, semantic, target, path.as_slice())?;
        raw = carrier.as_ptr(db).is_some();
        target = carrier
            .as_ptr(db)
            .or_else(|| carrier.as_capability(db).map(|(_, target)| target))?;
    }
    if raw && let HandleAddressSpace::Known(space) = source.contract.address_space {
        Some(address_space_from_provider(space))
    } else {
        None
    }
}

fn project_declaration_return_source<'db>(
    db: &'db dyn MirDb,
    mut class: RuntimeClass<'db>,
    projection: &[ReturnProjectionStep],
) -> Option<RuntimeClass<'db>> {
    for step in projection {
        let layout = class.aggregate_layout()?.data(db);
        class = match (*step, layout) {
            (ReturnProjectionStep::Field(field), Layout::Struct(layout)) => {
                layout.fields.get(field as usize)?.clone()
            }
            (ReturnProjectionStep::VariantField { variant, field }, Layout::Enum(layout)) => layout
                .variants
                .get(variant as usize)?
                .fields
                .get(field as usize)?
                .clone(),
            (
                ReturnProjectionStep::ConstantIndex(_)
                | ReturnProjectionStep::ParamIndex(_)
                | ReturnProjectionStep::AnyIndex,
                Layout::Array(layout),
            ) => layout.elem,
            (ReturnProjectionStep::Field(_), Layout::Array(_) | Layout::Enum(_))
            | (ReturnProjectionStep::VariantField { .. }, Layout::Struct(_) | Layout::Array(_))
            | (
                ReturnProjectionStep::ConstantIndex(_)
                | ReturnProjectionStep::ParamIndex(_)
                | ReturnProjectionStep::AnyIndex,
                Layout::Struct(_) | Layout::Enum(_),
            ) => return None,
        };
    }
    Some(class)
}

#[derive(Clone, Copy)]
enum ReturnSourceMerge {
    Replace,
    Specialize,
    Join,
}

fn merge_declaration_return_source<'db>(
    db: &'db dyn MirDb,
    current: RuntimeClass<'db>,
    projection: &[ReturnProjectionStep],
    source_root: &RuntimeClass<'db>,
    projected_source: &RuntimeClass<'db>,
    merge: ReturnSourceMerge,
) -> Option<RuntimeClass<'db>> {
    let Some((step, suffix)) = projection.split_first() else {
        let source = retarget_declaration_return_transport(
            db,
            current.clone(),
            source_root,
            projected_source,
        );
        return match merge {
            ReturnSourceMerge::Join if current.is_transport() && source.is_transport() => {
                join_reference_transports(db, &current, &source)
            }
            ReturnSourceMerge::Join | ReturnSourceMerge::Specialize => {
                merge_runtime_class(db, &current, &source).or(Some(current))
            }
            ReturnSourceMerge::Replace
                if declaration_return_value_shapes_match(db, &current, &source) =>
            {
                Some(source)
            }
            ReturnSourceMerge::Replace => Some(current),
        };
    };
    let layout = current.aggregate_layout()?.data(db);
    let layout = match (*step, layout) {
        (ReturnProjectionStep::Field(field), Layout::Struct(mut layout)) => {
            let field = layout.fields.get_mut(field as usize)?;
            *field = merge_declaration_return_source(
                db,
                field.clone(),
                suffix,
                source_root,
                projected_source,
                merge,
            )?;
            LayoutKey::Struct(layout)
        }
        (ReturnProjectionStep::VariantField { variant, field }, Layout::Enum(mut layout)) => {
            let field = layout
                .variants
                .get_mut(variant as usize)?
                .fields
                .get_mut(field as usize)?;
            *field = merge_declaration_return_source(
                db,
                field.clone(),
                suffix,
                source_root,
                projected_source,
                merge,
            )?;
            LayoutKey::Enum(EnumLayoutKey {
                variants: layout.variants,
            })
        }
        (
            ReturnProjectionStep::ConstantIndex(_)
            | ReturnProjectionStep::ParamIndex(_)
            | ReturnProjectionStep::AnyIndex,
            Layout::Array(mut layout),
        ) => {
            layout.elem = merge_declaration_return_source(
                db,
                layout.elem,
                suffix,
                source_root,
                projected_source,
                merge,
            )?;
            LayoutKey::Array(layout)
        }
        (ReturnProjectionStep::Field(_), Layout::Array(_) | Layout::Enum(_))
        | (ReturnProjectionStep::VariantField { .. }, Layout::Struct(_) | Layout::Array(_))
        | (
            ReturnProjectionStep::ConstantIndex(_)
            | ReturnProjectionStep::ParamIndex(_)
            | ReturnProjectionStep::AnyIndex,
            Layout::Struct(_) | Layout::Enum(_),
        ) => return None,
    };
    Some(RuntimeClass::AggregateValue {
        layout: LayoutId::new(db, layout),
    })
}

fn declaration_return_value_shapes_match<'db>(
    db: &'db dyn MirDb,
    current: &RuntimeClass<'db>,
    source: &RuntimeClass<'db>,
) -> bool {
    match (current, source) {
        (RuntimeClass::Scalar(current), RuntimeClass::Scalar(source)) => current == source,
        (RuntimeClass::AggregateValue { .. }, RuntimeClass::AggregateValue { .. }) => {
            merge_runtime_class(db, current, source).is_some()
        }
        (
            RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. },
            RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. },
        ) => true,
        (
            RuntimeClass::Scalar(_)
            | RuntimeClass::AggregateValue { .. }
            | RuntimeClass::Ref { .. }
            | RuntimeClass::RawAddr { .. },
            RuntimeClass::Scalar(_)
            | RuntimeClass::AggregateValue { .. }
            | RuntimeClass::Ref { .. }
            | RuntimeClass::RawAddr { .. },
        ) => false,
    }
}

fn retarget_declaration_return_transport<'db>(
    db: &'db dyn MirDb,
    target: RuntimeClass<'db>,
    source_root: &RuntimeClass<'db>,
    projected_source: &RuntimeClass<'db>,
) -> RuntimeClass<'db> {
    let projected_transport = matches!(
        projected_source,
        RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. }
    );
    // A transport-valued projection carries its concrete pointee identity. A
    // scalar or aggregate projection only inherits the source root's transport;
    // its pointee shape remains the declared result projection.
    let source = if projected_transport {
        projected_source
    } else {
        source_root
    };
    // Forwarded type provenance may name the containing slot. Loading a stored
    // native carrier returns its value, not the address of that carrier's slot.
    if let RuntimeClass::Ref {
        pointee: target_pointee,
        ..
    } = &target
        && let Some(stored) = source.deref_target(db)
        && let RuntimeClass::Ref {
            pointee,
            kind: RefKind::Native,
            ..
        } = &stored
        && runtime_classes_equivalent(db, pointee, target_pointee)
    {
        return stored;
    }
    match (target, source) {
        (
            target @ RuntimeClass::Ref {
                kind: RefKind::Native,
                ..
            },
            _,
        ) => target,
        (
            RuntimeClass::Ref {
                pointee: target_pointee,
                view,
                ..
            },
            RuntimeClass::Ref {
                pointee: source_pointee,
                kind,
                ..
            },
        ) => RuntimeClass::Ref {
            pointee: if projected_transport {
                source_pointee.clone()
            } else {
                target_pointee
            },
            kind: kind.clone(),
            view,
        },
        (
            RuntimeClass::Ref { pointee, .. },
            RuntimeClass::RawAddr {
                space,
                pointee: source_target,
            },
        ) => RuntimeClass::RawAddr {
            space: *space,
            pointee: match source_target {
                Some(source_target) if projected_transport => Some(*source_target),
                Some(_) | None => Some(RawPointeeId::exact(db, *pointee)),
            },
        },
        (
            RuntimeClass::RawAddr {
                pointee: target, ..
            },
            RuntimeClass::Ref {
                pointee,
                kind: RefKind::Provider { space, .. },
                ..
            },
        ) => RuntimeClass::RawAddr {
            space: *space,
            pointee: if projected_transport {
                Some(RawPointeeId::exact(db, pointee.as_ref().clone()))
            } else {
                target
            },
        },
        (
            RuntimeClass::RawAddr {
                pointee: target, ..
            },
            RuntimeClass::RawAddr {
                space,
                pointee: source_target,
            },
        ) => RuntimeClass::RawAddr {
            space: *space,
            pointee: if projected_transport {
                source_target.or(target)
            } else {
                target
            },
        },
        (_, _) => projected_source.clone(),
    }
}

/// Whether calls to `semantic` never return. The borrow summary decides this,
/// as it does for its callers' solves and so for their executable control flow.
pub(crate) fn semantic_never_returns<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> bool {
    matches!(semantic_borrow_summary(db, semantic), Ok(Some(summary)) if !summary.may_return)
}

pub(crate) fn runtime_exit_behavior<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
) -> RuntimeExitBehavior {
    match key.source(db) {
        RuntimeInstanceSource::Semantic(semantic) if semantic_never_returns(db, semantic) => {
            RuntimeExitBehavior::NeverReturns
        }
        RuntimeInstanceSource::Semantic(_) => RuntimeExitBehavior::MayReturn,
        RuntimeInstanceSource::Synthetic(synthetic) => {
            runtime_synthetic_exit_behavior(synthetic.spec(db).clone())
        }
    }
}

#[salsa::tracked]
pub(crate) fn static_runtime_return_decision<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> StaticRuntimeReturnDecision<'db> {
    let typed_body = semantic.key(db).typed_body(db);
    if typed_body.result_ty() == TyId::unit(db) {
        return StaticRuntimeReturnDecision::Known(None);
    }
    if !typed_body.forwarded_return_sources(db).is_empty() {
        return StaticRuntimeReturnDecision::Dynamic;
    }
    match desired_runtime_return_plan(db, semantic) {
        RuntimeVisibleReturnPlan::Exact(class) => StaticRuntimeReturnDecision::Known(Some(class)),
        RuntimeVisibleReturnPlan::Erased
        | RuntimeVisibleReturnPlan::Constrained(_)
        | RuntimeVisibleReturnPlan::PassActual => StaticRuntimeReturnDecision::Dynamic,
    }
}

pub(crate) fn evaluate_runtime_return_class<'db>(
    db: &'db dyn MirDb,
    summary: &RuntimeReturnSummary<'db>,
    params: &[RuntimeClass<'db>],
    lookup: &mut impl FnMut(RuntimeInstanceKey<'db>) -> Option<RuntimeClass<'db>>,
) -> Option<RuntimeClass<'db>> {
    // A body that never returns constrains no return class.
    if summary.return_operands.is_empty() {
        return None;
    }
    let env = summary.env(db);
    let lookup: ReturnClassLookup<'_, 'db> = lookup;
    let carriers = CarrierInferer::with_space(
        env,
        ReturnSliceSpace(summary),
        params,
        &summary.param_locals,
        Some(lookup),
    )
    .solve_carriers();
    let mut returned = Vec::new();
    for operand in summary.return_operands.iter().copied() {
        let Some(selected) =
            selected_visible_return_for_operand(env, operand, &summary.return_plan, &carriers)
        else {
            return summary.default_return_class.clone();
        };
        returned.push(selected.class);
    }
    let Some(class) = merged_return_class(
        db,
        returned,
        summary
            .semantic_body
            .owner()
            .normalized_result_ty(db)
            .as_borrow(db)
            .is_some(),
    ) else {
        return summary.default_return_class.clone();
    };
    Some(class)
}

/// Whether the declared return class carries every value the body returns:
/// joining the body's class into it leaves it unchanged.
pub(crate) fn declared_return_class_admits<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    declared: &RuntimeClass<'db>,
    returned: &RuntimeClass<'db>,
) -> bool {
    merged_return_class(
        db,
        vec![returned.clone(), declared.clone()],
        semantic.normalized_result_ty(db).as_borrow(db).is_some(),
    )
    .is_some_and(|merged| runtime_classes_equivalent(db, &merged, declared))
}

fn merged_return_class<'db>(
    db: &'db dyn MirDb,
    mut returned: Vec<RuntimeClass<'db>>,
    native_borrow: bool,
) -> Option<RuntimeClass<'db>> {
    let mut merged = returned.pop()?;
    for class in returned {
        merged = if native_borrow {
            join_reference_transports(db, &merged, &class)?
        } else {
            merge_runtime_class(db, &merged, &class)?
        };
    }
    Some(merged)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct SliceAssignmentId(u32);
entity_impl!(SliceAssignmentId);

/// [`AssignmentSpace`] restricted to the backward slice of assignments feeding
/// the return locals; the fixpoint itself is the shared [`CarrierInferer`].
struct ReturnSliceSpace<'s, 'db>(&'s RuntimeReturnSummary<'db>);

impl<'db> AssignmentSpace<'db> for ReturnSliceSpace<'_, 'db> {
    type Node = SliceAssignmentId;

    fn node_count(&self) -> usize {
        self.0.slice_assignment_ids.len()
    }

    fn seed_nodes(&self) -> Vec<SliceAssignmentId> {
        self.0.slice_assignment_ids.keys().collect()
    }

    fn assignment_id(&self, node: SliceAssignmentId) -> AssignmentId {
        self.0.slice_assignment_ids[node]
    }

    fn for_each_node_using_local(&self, local: SLocalId, f: &mut dyn FnMut(SliceAssignmentId)) {
        for &assign_id in &self.0.slice_assignments_by_local[local.index()] {
            let Some(slice_id) = self.0.slice_assignment_positions[assign_id] else {
                continue;
            };
            f(slice_id);
        }
    }

    fn dynamic_dependents(&self, local: SLocalId) -> &[SLocalId] {
        &self.0.slice_dynamic_dependents_by_local[local.index()]
    }
}

#[cfg(test)]
mod tests {
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::{
        analysis::{
            semantic::{
                SemanticLocalKind, get_or_build_semantic_instance, root_semantic_instance_key,
            },
            ty::ty_check::BodyOwner,
        },
        hir_def::TopLevelMod,
    };
    use url::Url;

    use crate::{
        build_runtime_package,
        runtime::{
            AddressSpaceKind, Layout, RExpr, RStmt, RTerminator, RefKind, RuntimeCarrier,
            RuntimeClass, RuntimeExitBehavior,
        },
    };

    use super::*;
    use crate::runtime::{
        lower::{infer::LocalStateInferer, interface::runtime_param_locals},
        package::runtime_instance_for_semantic,
    };

    fn semantic_instance_for_named_func<'db>(
        db: &'db DriverDataBase,
        top_mod: TopLevelMod<'db>,
        name: &str,
    ) -> SemanticInstance<'db> {
        let func = top_mod
            .all_funcs(db)
            .iter()
            .copied()
            .find(|func| {
                func.name(db)
                    .to_opt()
                    .is_some_and(|func_name| func_name.data(db) == name)
            })
            .unwrap_or_else(|| panic!("missing function `{name}`"));
        let key = root_semantic_instance_key(db, BodyOwner::Func(func))
            .unwrap_or_else(|err| panic!("failed to root semantic function instance: {err:?}"));
        get_or_build_semantic_instance(db, key)
    }

    fn legacy_return_class_for_key<'db>(
        db: &'db DriverDataBase,
        key: RuntimeInstanceKey<'db>,
    ) -> Option<RuntimeClass<'db>> {
        let semantic = key
            .semantic(db)
            .expect("legacy return-class inference only applies to semantic runtime instances");
        let semantic_body =
            RuntimeSemanticBody::admitted(db, semantic).expect("semantic body should normalize");
        let summary = RuntimeReturnSummary::build(db, semantic, &semantic_body);
        let env = summary.env(db);
        let inferred = LocalStateInferer::new(
            env,
            key.params(db),
            &runtime_param_locals(db, semantic, &summary.semantic_body.source, key.params(db)),
        )
        .run();
        let mut returned = Vec::new();
        for operand in summary.return_operands.iter().copied() {
            let Some(selected) = selected_visible_return_for_operand(
                env,
                operand,
                &summary.return_plan,
                &inferred.carriers,
            ) else {
                return summary.default_return_class.clone();
            };
            returned.push(selected.class);
        }
        let Some(class) = merged_return_class(
            db,
            returned,
            summary
                .semantic_body
                .owner()
                .normalized_result_ty(db)
                .as_borrow(db)
                .is_some(),
        ) else {
            return summary.default_return_class.clone();
        };
        Some(class)
    }

    fn assert_static_exact_return_matches_full_inference(source: &str, name: &str) {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse(&format!("file:///{name}.fe")).unwrap();
        db.workspace()
            .touch(&mut db, file_url.clone(), Some(source.to_string()));
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let semantic = semantic_instance_for_named_func(&db, top_mod, name);
        let key = runtime_instance_for_semantic(&db, semantic).key(&db);

        assert!(
            matches!(
                desired_runtime_return_plan(&db, semantic),
                RuntimeVisibleReturnPlan::Exact(_)
            ),
            "`{name}` should exercise the static exact return-class path"
        );
        assert!(
            matches!(
                static_runtime_return_decision(&db, semantic),
                StaticRuntimeReturnDecision::Known(Some(_))
            ),
            "`{name}` should use the semantic-level static return decision"
        );
        assert_eq!(
            declaration_runtime_return_class(&db, key),
            legacy_return_class_for_key(&db, key),
            "static exact return class should match full-body carrier inference"
        );
    }

    #[test]
    fn stored_native_field_returns_preserve_their_carrier() {
        for (expression, signature_first) in [
            "holder.first",
            "if take_first { holder.first } else { holder.second }",
        ]
        .into_iter()
        .flat_map(|expression| [true, false].map(|signature_first| (expression, signature_first)))
        {
            let mut db = DriverDataBase::default();
            let file = db.workspace().touch(
                &mut db,
                Url::parse("file:///stored_native_field_returns.fe").unwrap(),
                Some(format!(
                    "struct Holder {{ first: ref u8, second: ref u8 }}\nfn select(holder: Holder, take_first: bool) -> ref u8 {{ {expression} }}\n"
                )),
            );
            let module = db.top_mod(file);
            let diagnostics = db.run_on_top_mod(module);
            assert!(diagnostics.is_empty(), "{}", diagnostics.format_diags(&db));
            let semantic = semantic_instance_for_named_func(&db, module, "select");
            let instance = runtime_instance_for_semantic(&db, semantic);
            let key = instance.key(&db);
            if signature_first {
                instance.interface_signature(&db);
            }
            let body = instance.body(&db);
            assert_eq!(body.signature, instance.interface_signature(&db));
            let semantic_body = RuntimeSemanticBody::admitted(&db, semantic).unwrap();
            let inferred = runtime_return_class_for_body(&db, key, &semantic_body);
            assert_eq!(
                inferred,
                declaration_runtime_return_class(&db, key),
                "{expression}"
            );
            assert_eq!(inferred, legacy_return_class_for_key(&db, key));
            assert!(matches!(
                inferred,
                Some(RuntimeClass::Ref {
                    kind: RefKind::Native,
                    ..
                })
            ));
            let program: &dyn MirDb = &db;
            crate::verify_runtime_body(&db, &program, &body).expect("valid runtime body");
        }
    }

    fn assert_runtime_exit_behavior(
        source: &str,
        case_name: &str,
        expected: &[(&str, RuntimeExitBehavior)],
    ) {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse(&format!("file:///{case_name}.fe")).unwrap();
        db.workspace()
            .touch(&mut db, file_url.clone(), Some(source.to_string()));
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        for &(name, exit) in expected {
            let semantic = semantic_instance_for_named_func(&db, top_mod, name);
            let runtime = runtime_instance_for_semantic(&db, semantic);
            assert_eq!(runtime.exit_behavior(&db), exit, "`{name}` exit behavior");
        }
    }

    #[test]
    fn ordinary_unit_call_may_return_normally() {
        assert_runtime_exit_behavior(
            r#"
fn helper() {}

fn caller() {
    helper()
}
"#,
            "ordinary_unit_call_may_return_normally",
            &[
                ("helper", RuntimeExitBehavior::MayReturn),
                ("caller", RuntimeExitBehavior::MayReturn),
            ],
        );
    }

    #[test]
    fn panic_wrappers_never_return() {
        assert_runtime_exit_behavior(
            r#"
fn fail() {
    core::panic()
}

fn fail_indirect() {
    fail()
}

fn fail_twice() {
    fail_indirect()
}
"#,
            "panic_wrappers_never_return",
            &[
                ("fail", RuntimeExitBehavior::NeverReturns),
                ("fail_indirect", RuntimeExitBehavior::NeverReturns),
                ("fail_twice", RuntimeExitBehavior::NeverReturns),
            ],
        );
    }

    #[test]
    fn mixed_panic_branch_may_return_normally() {
        assert_runtime_exit_behavior(
            r#"
fn maybe(flag: bool) {
    if flag {
        core::panic()
    }
}
"#,
            "mixed_panic_branch_may_return_normally",
            &[("maybe", RuntimeExitBehavior::MayReturn)],
        );
    }

    #[test]
    fn semantic_nonreturning_wrapper_calls_lower_as_terminal_calls() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///semantic_nonreturning_wrapper_calls_lower_as_terminal_calls.fe")
                .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
struct Pair {
    a: u256,
    b: u256,
}

fn fail() -> ! {
    core::panic()
}

fn fail_declared_u256() -> u256 {
    core::panic()
}

fn fail_declared_pair() -> Pair {
    core::panic()
}

fn caller_unit() {
    fail()
}

fn caller_u256_from_never() -> u256 {
    fail()
}

fn caller_u256_from_declared_u256() -> u256 {
    fail_declared_u256()
}

fn caller_pair_from_declared_pair() -> Pair {
    fail_declared_pair()
}

"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        for caller_name in [
            "caller_unit",
            "caller_u256_from_never",
            "caller_u256_from_declared_u256",
            "caller_pair_from_declared_pair",
        ] {
            let caller = semantic_instance_for_named_func(&db, top_mod, caller_name);
            let body = runtime_instance_for_semantic(&db, caller).body(&db);

            assert!(
                body.blocks
                    .iter()
                    .any(|block| matches!(block.terminator, RTerminator::TerminalCall { .. })),
                "`{caller_name}` should terminal-call its nonreturning callee:\n{body:#?}"
            );
            assert!(
                body.blocks.iter().all(|block| {
                    block.stmts.iter().all(|stmt| {
                        !matches!(
                            stmt,
                            RStmt::Assign {
                                expr: RExpr::Call { .. },
                                ..
                            }
                        )
                    })
                }),
                "`{caller_name}` should not lower its nonreturning callee as a normal call:\n{body:#?}"
            );
        }
    }

    #[test]
    fn exact_scalar_return_class_does_not_need_body_inference() {
        assert_static_exact_return_matches_full_inference(
            r#"
fn exact_scalar_return_class_does_not_need_body_inference() -> u256 {
    42
}
"#,
            "exact_scalar_return_class_does_not_need_body_inference",
        );
    }

    #[test]
    fn exact_aggregate_return_class_does_not_need_body_inference() {
        assert_static_exact_return_matches_full_inference(
            r#"
struct Pair {
    a: u256,
    b: u256,
}

fn exact_aggregate_return_class_does_not_need_body_inference() -> Pair {
    Pair { a: 1, b: 2 }
}
"#,
            "exact_aggregate_return_class_does_not_need_body_inference",
        );
    }

    #[test]
    fn unit_return_class_is_statically_known_absent() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///unit_return_class_is_statically_known_absent.fe").unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
fn helper() {}
"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let semantic = semantic_instance_for_named_func(&db, top_mod, "helper");
        let key = runtime_instance_for_semantic(&db, semantic).key(&db);

        assert_eq!(
            static_runtime_return_decision(&db, semantic),
            StaticRuntimeReturnDecision::Known(None)
        );
        assert_eq!(declaration_runtime_return_class(&db, key), None);
    }

    #[test]
    fn mixed_native_return_declaration_matches_body_inference() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse("file:///mixed_native_return.fe").unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
fn choose(first: ref u8, second: ref u8, use_first: bool) -> ref u8 {
    if use_first { first } else { second }
}
"#
                .into(),
            ),
        );
        let file = db.workspace().get(&db, &file_url).unwrap();
        let semantic = semantic_instance_for_named_func(&db, db.top_mod(file), "choose");
        let default_key = runtime_instance_for_semantic(&db, semantic).key(&db);
        let mut params = default_key.params(&db).clone();
        let pointee = params[1].ref_pointee().unwrap().clone();
        params[1] = RuntimeClass::raw_addr(&db, AddressSpaceKind::Memory, pointee);
        let key = RuntimeInstanceKey::new(&db, RuntimeInstanceSource::Semantic(semantic), params);
        let declaration = declaration_runtime_return_class(&db, key);
        assert!(matches!(
            declaration,
            Some(RuntimeClass::Ref {
                kind: RefKind::Native,
                ..
            })
        ));
        assert_eq!(declaration, legacy_return_class_for_key(&db, key));
    }

    #[test]
    fn raw_pointer_borrows_keep_their_return_layout() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse("file:///raw_borrow_returns.fe").unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
struct Buffer { ptr: *u8 }
struct Loan { value: mut u8 }
fn direct(_ ptr: *u8) -> mut u8 { mut *ptr }
fn nested(_ buffer: Buffer) -> Loan { Loan { value: mut *buffer.ptr } }
"#
                .to_string(),
            ),
        );
        let file = db.workspace().get(&db, &file_url).unwrap();
        let top_mod = db.top_mod(file);
        for (name, projection) in [
            ("direct", vec![]),
            ("nested", vec![ReturnProjectionStep::Field(0)]),
        ] {
            let semantic = semantic_instance_for_named_func(&db, top_mod, name);
            let key = runtime_instance_for_semantic(&db, semantic).key(&db);
            let declaration = declaration_runtime_return_class(&db, key).unwrap();
            assert_eq!(
                Some(declaration.clone()),
                legacy_return_class_for_key(&db, key)
            );
            let result = project_declaration_return_source(&db, declaration, &projection).unwrap();
            if projection.is_empty() {
                assert!(
                    matches!(
                        result,
                        RuntimeClass::RawAddr {
                            space: AddressSpaceKind::Memory,
                            ..
                        }
                    ),
                    "direct returns retain their static raw layout"
                );
            } else {
                assert!(
                    matches!(
                        result,
                        RuntimeClass::Ref {
                            kind: RefKind::Native,
                            ..
                        }
                    ),
                    "stored borrows use the canonical address/layout carrier"
                );
            }
        }
    }

    #[test]
    fn borrow_return_class_remains_dynamic() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse("file:///borrow_return_class_remains_dynamic.fe").unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
struct Holder {
    value: u256,
}

impl Holder {
    fn value_mut(mut self) -> mut u256 {
        mut self.value
    }
}
"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let semantic = semantic_instance_for_named_func(&db, top_mod, "value_mut");

        assert!(
            matches!(
                static_runtime_return_decision(&db, semantic),
                StaticRuntimeReturnDecision::Dynamic
            ),
            "borrow-derived returns depend on the returned source transport"
        );
    }

    #[test]
    fn return_slice_includes_all_definitions_of_returned_local() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///return_slice_includes_all_definitions_of_returned_local.fe")
                .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
fn choose(_ flag: bool) -> u256 {
    let mut x = 1
    if flag {
        x = 2
    }
    x
}
"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let semantic = semantic_instance_for_named_func(&db, top_mod, "choose");
        let semantic_body =
            RuntimeSemanticBody::admitted(&db, semantic).expect("semantic body should normalize");
        let summary = RuntimeReturnSummary::build(&db, semantic, &semantic_body);
        let return_local = summary
            .return_operands
            .first()
            .expect("choose should return one operand")
            .local;
        let source_local = summary
            .facts
            .source_locals(return_local)
            .first()
            .copied()
            .expect("returned load should depend on the source local");
        assert_ne!(
            return_local, source_local,
            "the final read needs its own carrier"
        );
        let return_defs = summary
            .facts
            .assignments()
            .iter()
            .filter(|(_, assignment)| assignment.dst == source_local)
            .count();
        let sliced_return_defs = summary
            .slice_assignment_ids
            .iter()
            .filter(|&(_, &assign_id)| {
                summary
                    .facts
                    .assignment(assign_id)
                    .is_some_and(|assignment| assignment.dst == source_local)
            })
            .count();

        assert!(
            return_defs >= 2,
            "expected `choose` to define the loaded source local at least twice"
        );
        assert_eq!(
            sliced_return_defs, return_defs,
            "return slice should keep every definition feeding the returned load"
        );
    }

    #[test]
    fn forwarded_scalar_provenance_does_not_replace_aggregate_return_shape() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse(
            "file:///forwarded_scalar_provenance_does_not_replace_aggregate_return_shape.fe",
        )
        .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
fn local_bytes(value: String<8>) -> [u8; 8] {
    value.as_bytes().to_array()
}
fn first(value: String<8>) -> u8 {
    let bytes: [u8; 8] = local_bytes(value)
    bytes[0]
}

pub fn main() -> u8 {
    first("COOL")
}
"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let package = build_runtime_package(&db, top_mod).expect("runtime package");
        let function = package
            .functions(&db)
            .iter()
            .copied()
            .find(|function| function.symbol(&db).contains("local_bytes"))
            .expect("missing local array-returning runtime function");

        assert!(matches!(
            function.instance(&db).interface_signature(&db).ret,
            Some(RuntimeClass::AggregateValue { .. })
        ));
    }

    #[test]
    fn provider_root_return_slice_matches_full_inference() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///provider_root_return_slice_matches_full_inference.fe").unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
struct Pair {
    a: u256,
    b: u256,
}

fn id_ctx() -> Pair uses (ctx: Pair) {
    ctx
}

msg Msg {
    #[selector = 1]
    Go -> u256
}

pub contract C {
    mut ctx: Pair

    init() uses (mut ctx) {
        ctx = Pair { a: 1, b: 2 }
    }

    recv Msg {
        Go -> u256 uses (ctx) {
            let pair = with (ctx) { id_ctx() }
            pair.a + pair.b
        }
    }
}
"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let package = build_runtime_package(&db, top_mod).expect("runtime package");
        let function = package
            .functions(&db)
            .iter()
            .copied()
            .find(|function| function.symbol(&db).contains("id_ctx"))
            .expect("missing specialized id_ctx runtime function");
        let key = function.instance(&db).key(&db);

        assert_eq!(
            declaration_runtime_return_class(&db, key),
            legacy_return_class_for_key(&db, key),
            "provider-root return slice should match full-body carrier inference:\ninstance={key:#?}"
        );
    }

    #[test]
    fn return_class_merges_default_enum_with_storage_provider_variant() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///return_class_merges_default_enum_with_storage_provider_variant.fe")
                .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                include_str!("../../../../fe/tests/fixtures/fe_test/reentrancy_mutex.fe")
                    .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let package = build_runtime_package(&db, top_mod).expect("runtime package");
        let function = package
            .functions(&db)
            .iter()
            .copied()
            .find(|function| function.symbol(&db).contains("try_lock"))
            .expect("missing specialized try_lock runtime function");
        let instance = function.instance(&db);
        let semantic = instance
            .key(&db)
            .semantic(&db)
            .expect("try_lock should be a semantic runtime instance");
        let return_ty = semantic.key(&db).typed_body(&db).result_ty();
        let option_enum = return_ty
            .as_enum(&db)
            .expect("try_lock should return Option");
        let some_variant_idx = option_enum
            .variants(&db)
            .position(|variant| {
                variant
                    .name(&db)
                    .is_some_and(|name| name.data(&db) == "Some")
            })
            .expect("Option should include Some");
        let ret = instance
            .interface_signature(&db)
            .ret
            .expect("try_lock should return a runtime-visible Option");
        let RuntimeClass::AggregateValue { layout } = ret else {
            panic!("try_lock should return an aggregate enum: {ret:#?}");
        };
        let Layout::Enum(enum_layout) = layout.data(&db) else {
            panic!("try_lock should return an enum layout: {layout:#?}");
        };
        let some_variant = enum_layout
            .variants
            .get(some_variant_idx)
            .expect("Option layout should include Some");

        assert!(
            matches!(
                some_variant.fields.first(),
                Some(RuntimeClass::Ref {
                    kind: RefKind::Native,
                    ..
                })
            ),
            "returned enum fields must use native carriers that preserve storage layout:\n{ret:#?}"
        );
    }

    #[test]
    fn aggregate_temporaries_match_full_inference_in_return_slices() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///aggregate_temporaries_match_full_inference_in_return_slices.fe")
                .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
fn first(_ arr: [u8; 4]) -> u8 {
    let local = arr
    local[0]
}
"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let semantic = semantic_instance_for_named_func(&db, top_mod, "first");
        let instance = runtime_instance_for_semantic(&db, semantic);
        let key = instance.key(&db);
        let semantic_body =
            RuntimeSemanticBody::admitted(&db, semantic).expect("semantic body should normalize");
        let summary = RuntimeReturnSummary::build(&db, semantic, &semantic_body);
        let env = summary.env(&db);

        let legacy = LocalStateInferer::new(
            env,
            key.params(&db),
            &runtime_param_locals(
                &db,
                semantic,
                &summary.semantic_body.source,
                key.params(&db),
            ),
        )
        .run();
        let mut lookup_return_class = |key| declaration_runtime_return_class(&db, key);
        let lookup: ReturnClassLookup<'_, '_> = &mut lookup_return_class;
        let sliced = CarrierInferer::with_space(
            env,
            ReturnSliceSpace(&summary),
            key.params(&db),
            &summary.param_locals,
            Some(lookup),
        )
        .solve_carriers();
        let locals = summary
            .semantic_body
            .locals
            .iter()
            .enumerate()
            .filter_map(|(idx, local)| {
                (idx >= summary.param_locals.len()
                    && matches!(local.role.kind(), SemanticLocalKind::DirectValue)
                    && matches!(
                        legacy.carriers[idx],
                        RuntimeCarrier::Value(RuntimeClass::AggregateValue { .. })
                    ))
                .then_some(SLocalId::from_u32(idx as u32))
            })
            .collect::<Vec<_>>();
        assert_eq!(locals.len(), 2, "expected two aggregate temporaries");
        for local in locals {
            assert_eq!(
                sliced[local.index()],
                legacy.carriers[local.index()],
                "return slice should infer the same aggregate temporary carrier as the full solver"
            );
        }
    }

    #[test]
    fn merged_return_class_is_order_independent_for_irreconcilable_sites() {
        let db = DriverDataBase::default();
        let storage = RuntimeClass::opaque_raw_addr(AddressSpaceKind::Storage);
        let transient = RuntimeClass::opaque_raw_addr(AddressSpaceKind::Transient);

        // Return sites that disagree on a non-Memory space cannot be merged, so the
        // fold reports failure (caller falls back to the default class) regardless of
        // the order the return sites were collected in.
        assert_eq!(
            merged_return_class(&db, vec![storage.clone(), transient.clone()], false),
            None
        );
        assert_eq!(
            merged_return_class(&db, vec![transient, storage], false),
            None
        );
    }

    #[test]
    fn merged_return_class_folds_memory_into_non_memory_regardless_of_order() {
        let db = DriverDataBase::default();
        let memory = RuntimeClass::opaque_raw_addr(AddressSpaceKind::Memory);
        let storage = RuntimeClass::opaque_raw_addr(AddressSpaceKind::Storage);
        let merged = RuntimeClass::opaque_raw_addr(AddressSpaceKind::Storage);

        assert_eq!(
            merged_return_class(&db, vec![memory.clone(), storage.clone()], false),
            Some(merged.clone())
        );
        assert_eq!(
            merged_return_class(&db, vec![storage, memory], false),
            Some(merged)
        );
    }
}
