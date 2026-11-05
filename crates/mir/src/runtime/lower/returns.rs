use std::collections::{BTreeMap, VecDeque};

use cranelift_entity::{EntityRef, PrimaryMap, SecondaryMap, entity_impl};
use hir::analysis::{
    semantic::{
        ExecutableBlock, SLocalId, SemanticInstance,
        normalized::{
            NExpr, NOperand, NPlace, NPlaceBase, NValueDefinition, NValueId, NormalizedBody,
            normalize_semantic_body,
        },
        projection_component_spaces, semantic_executable_control_flow, semantic_may_return,
    },
    ty::{
        AddressSpace,
        const_ty::CallableInputOrigin,
        ty_check::{ReturnProjectionStep, ReturnProvenance},
        ty_def::TyId,
    },
};
use rustc_hash::FxHashSet;
use salsa::SalsaValue;

use crate::{
    db::MirDb,
    instance::{RuntimeInstanceKey, RuntimeInstanceSource},
    runtime::{
        EnumLayoutKey, EnumVariantLayout, Layout, LayoutId, LayoutKey, LowerError, RawPointeeId,
        RefKind, RuntimeClass, RuntimeExitBehavior, StructLayout,
    },
};

use super::{
    boundary::BoundaryMatcher,
    classify::{
        AssignmentId, BodyEnv, BodyStaticFacts, RuntimeVisibleReturnPlan, default_return_class,
        desired_runtime_return_plan, selected_visible_return_for_operand, semantic_return_ty,
    },
    infer::{
        AssignmentSpace, CarrierInferer, ReturnClassLookup, join_reference_transports,
        merge_runtime_class, runtime_class_has_zero_sized_payload,
    },
    interface::{runtime_visible_binding_local, runtime_visible_binding_plans},
    semantic_body::{RuntimeOperand, RuntimeSemanticBody},
    type_info::{RuntimeTypeEnv, stored_class_for_ty_in_env},
};
use crate::runtime::synthetic::runtime_synthetic_exit_behavior;

#[derive(Clone, Debug, PartialEq, Eq, SalsaValue)]
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
            .filter_map(|block| semantic_body.runtime_operand(block.terminator.kind.returned()?))
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

/// A projection is inlined into each of its callers, so its grant has the
/// class its body yields rather than one fixed by its declaration.
#[salsa::tracked(returns(clone))]
fn projection_return_class<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
) -> Option<RuntimeClass<'db>> {
    let semantic = key.semantic(db)?;
    let body = RuntimeSemanticBody::admitted(db, semantic).ok()?;
    runtime_return_class_for_body(db, key, &body)
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
    if semantic.is_projection(db) {
        return projection_return_class(db, key);
    }
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
        // The class of the argument the return reads, and the steps it takes
        // from it: a product parameter's leading steps select its part.
        let mut classes = bindings.iter().zip(key.params(db));
        let found = match source.origin {
            CallableInputOrigin::ValueParam(param) => typed_body
                .return_source_input(param, &source.projection)
                .and_then(|(input, projection)| {
                    let input = typed_body.input_bindings()[input];
                    let (_, class) = classes.find(|(binding, _)| binding.binding == input)?;
                    Some((class, projection))
                }),
            origin => classes
                .find(|(binding, _)| binding.binding.callable_input_origin(db) == Some(origin))
                .map(|(_, class)| (class, source.projection.as_slice())),
        };
        let Some((source_class, projection)) = found else {
            continue;
        };
        let Some(projected) =
            project_declaration_return_source(db, source_class.clone(), projection)
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
    {
        for (projection, space) in returned_raw_transports(db, semantic) {
            let Some(space) = *space else {
                continue;
            };
            if projection.is_empty()
                && let Some((_, target)) = semantic.normalized_result_ty(db).as_borrow(db)
            {
                // A bare borrow keeps the raw transport with its declared
                // target; stored borrow fields use their canonical descriptors.
                let pointee = stored_class_for_ty_in_env(
                    db,
                    RuntimeTypeEnv::for_semantic(db, semantic),
                    target,
                );
                class = RuntimeClass::raw_addr(db, space, pointee);
                continue;
            }
            let Some(RuntimeClass::Ref { pointee, .. }) =
                project_declaration_return_source(db, class.clone(), projection)
            else {
                continue;
            };
            let source = RuntimeClass::raw_addr(db, space, *pointee);
            if let Some(updated) = merge_declaration_return_source(
                db,
                class.clone(),
                projection,
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

/// The raw transport of each borrow a body returns, by its projection in the
/// result: the memory space when every returning path takes the borrow
/// through a raw pointer, `None` when paths disagree.
#[salsa::tracked(cycle_initial=returned_raw_transports_cycle_initial)]
fn returned_raw_transports<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> BTreeMap<Vec<ReturnProjectionStep>, Option<AddressSpace>> {
    let mut transports = BTreeMap::new();
    let (Ok(artifacts), Some(executable)) = (
        normalize_semantic_body(db, semantic),
        semantic_executable_control_flow(db, semantic),
    ) else {
        return transports;
    };
    let body = &artifacts.body;
    let mut tracer = RawTransportTracer {
        db,
        body,
        transports: &mut transports,
        visited: FxHashSet::default(),
    };
    for (block, executable) in body.blocks.iter().zip(&executable.0) {
        if matches!(executable, ExecutableBlock::Continues(_))
            && let Some(value) = block.terminator.kind.returned()
        {
            tracer.trace(value.value, &mut Vec::new());
        }
    }
    transports
}

fn returned_raw_transports_cycle_initial<'db>(
    _: &'db dyn MirDb,
    _: salsa::Id,
    _: SemanticInstance<'db>,
) -> BTreeMap<Vec<ReturnProjectionStep>, Option<AddressSpace>> {
    BTreeMap::new()
}

struct RawTransportTracer<'a, 'db> {
    db: &'db dyn MirDb,
    body: &'a NormalizedBody<'db>,
    transports: &'a mut BTreeMap<Vec<ReturnProjectionStep>, Option<AddressSpace>>,
    visited: FxHashSet<(NValueId, Vec<ReturnProjectionStep>)>,
}

impl<'db> RawTransportTracer<'_, 'db> {
    fn record(&mut self, projection: &[ReturnProjectionStep], space: Option<AddressSpace>) {
        self.transports
            .entry(projection.to_vec())
            .and_modify(|previous| {
                if *previous != space {
                    *previous = None;
                }
            })
            .or_insert(space);
    }

    fn trace(&mut self, value: NValueId, projection: &mut Vec<ReturnProjectionStep>) {
        if !self.visited.insert((value, projection.clone())) {
            return;
        }
        let data = &self.body.values[value.index()];
        let is_borrow = data.ty.as_borrow(self.db).is_some();
        match data.definition {
            NValueDefinition::BlockParam { block, index } => {
                let incoming: Vec<NValueId> = self
                    .body
                    .blocks
                    .iter()
                    .flat_map(|predecessor| predecessor.terminator.kind.successors())
                    .filter(|successor| successor.block == block)
                    .filter_map(|successor| successor.args.get(index as usize))
                    .map(|arg| arg.value)
                    .collect();
                for arg in incoming {
                    self.trace(arg, projection);
                }
                return;
            }
            NValueDefinition::EntryParam { .. } => {}
            NValueDefinition::Statement { .. } => {
                let Some((_, expr)) = self.body.defining_expr(value) else {
                    return;
                };
                match expr {
                    NExpr::Forward { src } => return self.trace(src.value, projection),
                    NExpr::AggregateMake { ty, fields } => {
                        let array = ty.is_array(self.db);
                        for (field, operand) in fields.iter().enumerate() {
                            projection.push(if array {
                                ReturnProjectionStep::AnyIndex
                            } else {
                                ReturnProjectionStep::Field(field as u16)
                            });
                            self.trace(operand.value, projection);
                            projection.pop();
                        }
                        return;
                    }
                    NExpr::EnumMake {
                        variant, fields, ..
                    } => {
                        for (field, operand) in fields.iter().enumerate() {
                            projection.push(ReturnProjectionStep::VariantField {
                                variant: variant.0,
                                field: field as u16,
                            });
                            self.trace(operand.value, projection);
                            projection.pop();
                        }
                        return;
                    }
                    // A memory pointee is reached by its raw address; a state
                    // pointee by its space's provider reference.
                    NExpr::Borrow {
                        place:
                            NPlace {
                                base: NPlaceBase::Deref { pointer },
                                ..
                            },
                        ..
                    } if is_borrow
                        && matches!(
                            self.body.values[pointer.index()].ty.as_ptr(self.db),
                            Some((_, AddressSpace::Memory))
                        ) =>
                    {
                        return self.record(projection, Some(AddressSpace::Memory));
                    }
                    NExpr::Call { callee, .. } => {
                        let callee = SemanticInstance::new(self.db, callee.key);
                        let prefix = projection.len();
                        for (suffix, space) in returned_raw_transports(self.db, callee) {
                            projection.extend_from_slice(suffix);
                            self.record(projection, *space);
                            projection.truncate(prefix);
                        }
                        return;
                    }
                    _ => {}
                }
            }
        }
        if is_borrow {
            self.record(projection, None);
        }
    }
}

/// Holds the access checker's result space of each access component of a
/// projection, which instantiates its callers' callees, to the class lowering
/// returns the component in. A component no yield grants, such as the payload
/// of a variant the projection never returns, and one with no runtime class
/// carry nothing; a native reference keeps its space at run time; constant
/// data is a read-only memory value. Any other disagreement is an error
/// rather than a miscompile.
pub(crate) fn check_projection_result_spaces<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    ret: Option<&RuntimeClass<'db>>,
) -> Result<(), LowerError> {
    for (steps, space) in projection_component_spaces(db, semantic) {
        let (Some(space), Some(class)) = (
            space,
            ret.and_then(|ret| project_declaration_return_source(db, ret.clone(), &steps)),
        ) else {
            continue;
        };
        let agrees = match &class {
            RuntimeClass::Ref {
                kind: RefKind::Native,
                ..
            } => true,
            RuntimeClass::Ref {
                kind: RefKind::Object | RefKind::Const,
                ..
            } => space == AddressSpace::Memory,
            RuntimeClass::Ref {
                kind: RefKind::Provider { space: actual, .. },
                ..
            }
            | RuntimeClass::RawAddr { space: actual, .. } => *actual == space,
            _ => false,
        };
        if !agrees {
            return Err(LowerError::Unsupported(format!(
                "internal error: the access checker places result component {steps:?} of {:?} in {space:?}, but lowering returns it as {class:?}",
                semantic.key(db).owner(db),
            )));
        }
    }
    Ok(())
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
                | ReturnProjectionStep::ParamIndex { .. }
                | ReturnProjectionStep::AnyIndex,
                Layout::Array(layout),
            ) => layout.elem,
            (ReturnProjectionStep::Field(_), Layout::Array(_) | Layout::Enum(_))
            | (ReturnProjectionStep::VariantField { .. }, Layout::Struct(_) | Layout::Array(_))
            | (
                ReturnProjectionStep::ConstantIndex(_)
                | ReturnProjectionStep::ParamIndex { .. }
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
            | ReturnProjectionStep::ParamIndex { .. }
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
            | ReturnProjectionStep::ParamIndex { .. }
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
        && pointee == target_pointee
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
                Some(_) | None => Some(RawPointeeId::new(db, *pointee)),
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
                Some(RawPointeeId::new(db, pointee.as_ref().clone()))
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

/// Whether calls to `semantic` never return, as the executable control flow of
/// their callers assumes.
pub(crate) fn semantic_never_returns<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> bool {
    !semantic_may_return(db, semantic)
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

#[salsa::tracked(returns(clone))]
pub(crate) fn static_runtime_return_decision<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> StaticRuntimeReturnDecision<'db> {
    let typed_body = semantic.key(db).typed_body(db);
    if semantic_return_ty(db, semantic) == TyId::unit(db) {
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
    let normalized = &summary.semantic_body.normalized;
    let mut returned = Vec::new();
    for operand in summary.return_operands.iter().copied() {
        let Some(selected) =
            selected_visible_return_for_operand(env, operand, &summary.return_plan, &carriers)
        else {
            return summary.default_return_class.clone();
        };
        // A value with a zero-sized payload is erased, like its carrier.
        if !runtime_class_has_zero_sized_payload(db, &selected.class) {
            let built = operand
                .value
                .map_or(Built::Unknown, |value| Built::of(normalized, value));
            returned.push((selected.class, built));
        }
    }
    if returned.is_empty() {
        return None;
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
        vec![
            (returned.clone(), Built::Unknown),
            (declared.clone(), Built::Unknown),
        ],
        semantic.normalized_result_ty(db).as_borrow(db).is_some(),
    )
    .is_some_and(|merged| merged == *declared)
}

/// What a returned value is known to build, from the expressions that define
/// it: the variant of each enum it makes, and what the parts of each of its
/// aggregates build.
#[derive(Clone)]
enum Built {
    Unknown,
    Enum(usize, Vec<Built>),
    Aggregate(Vec<Built>),
}

impl Built {
    fn of(normalized: &NormalizedBody<'_>, value: NValueId) -> Self {
        let parts = |fields: &[NOperand]| {
            fields
                .iter()
                .map(|field| Self::of(normalized, field.value))
                .collect()
        };
        match normalized.defining_expr(value) {
            Some((_, NExpr::Forward { src })) => Self::of(normalized, src.value),
            Some((
                _,
                NExpr::EnumMake {
                    variant, fields, ..
                },
            )) => Self::Enum(variant.0 as usize, parts(fields)),
            Some((_, NExpr::AggregateMake { fields, .. })) => Self::Aggregate(parts(fields)),
            _ => Self::Unknown,
        }
    }

    /// What field `idx` of a layout with `len` fields builds.
    fn field(&self, idx: usize, len: usize) -> Self {
        match self {
            Self::Enum(_, fields) | Self::Aggregate(fields) if fields.len() == len => {
                fields[idx].clone()
            }
            _ => Self::Unknown,
        }
    }
}

/// The class carrying every value a body returns, given each with what it
/// builds.
fn merged_return_class<'db>(
    db: &'db dyn MirDb,
    returned: Vec<(RuntimeClass<'db>, Built)>,
    native_borrow: bool,
) -> Option<RuntimeClass<'db>> {
    if !native_borrow {
        return merged_built_class(db, &returned);
    }
    let mut returned = returned.into_iter().map(|(class, _)| class);
    let first = returned.next()?;
    returned.try_fold(first, |merged, class| {
        join_reference_transports(db, &merged, &class)
    })
}

/// The join of values, each given with what it builds. An enum value built
/// as one variant has no payload in the others, so it constrains only its
/// own variant's fields, wherever it lies in the value; a variant no value
/// builds keeps the fields its type gives it.
fn merged_built_class<'db>(
    db: &'db dyn MirDb,
    values: &[(RuntimeClass<'db>, Built)],
) -> Option<RuntimeClass<'db>> {
    let known = values
        .iter()
        .any(|(_, built)| !matches!(built, Built::Unknown));
    let layouts = values
        .iter()
        .map(|(class, built)| match class {
            RuntimeClass::AggregateValue { layout } => Some((layout.data(db), built)),
            _ => None,
        })
        .collect::<Option<Vec<_>>>();
    let layout = match layouts.as_deref() {
        Some(all @ [(Layout::Struct(first), _), ..]) if known => {
            let fields = all
                .iter()
                .map(|(layout, built)| match layout {
                    Layout::Struct(data) if data.pinned == first.pinned => {
                        Some((&data.fields[..], *built))
                    }
                    _ => None,
                })
                .collect::<Option<Vec<_>>>()?;
            LayoutKey::Struct(StructLayout {
                fields: merged_built_fields(db, &fields)?.into(),
                pinned: first.pinned,
            })
        }
        Some(all @ [(Layout::Enum(first), _), ..]) if known => {
            let enums = all
                .iter()
                .map(|(layout, built)| match layout {
                    Layout::Enum(data) if data.variants.len() == first.variants.len() => {
                        Some((data, *built))
                    }
                    _ => None,
                })
                .collect::<Option<Vec<_>>>()?;
            let variants = (0..first.variants.len())
                .map(|idx| {
                    let fields = enums
                        .iter()
                        .filter(|(_, built)| {
                            !matches!(built, Built::Enum(variant, _) if *variant != idx)
                        })
                        .map(|(data, built)| (&data.variants[idx].fields[..], *built))
                        .collect::<Vec<_>>();
                    if fields.is_empty() {
                        return Some(first.variants[idx].clone());
                    }
                    Some(EnumVariantLayout {
                        fields: merged_built_fields(db, &fields)?.into(),
                    })
                })
                .collect::<Option<Vec<_>>>()?;
            LayoutKey::Enum(EnumLayoutKey {
                variants: variants.into(),
            })
        }
        _ => {
            let ((first, _), rest) = values.split_first()?;
            return rest.iter().try_fold(first.clone(), |merged, (class, _)| {
                merge_runtime_class(db, &merged, class)
            });
        }
    };
    Some(RuntimeClass::AggregateValue {
        layout: LayoutId::new(db, layout),
    })
}

/// The join of the fields of layouts of one shape, each given with what its
/// value builds.
fn merged_built_fields<'db>(
    db: &'db dyn MirDb,
    layouts: &[(&[RuntimeClass<'db>], &Built)],
) -> Option<Vec<RuntimeClass<'db>>> {
    let len = layouts.first()?.0.len();
    if layouts.iter().any(|(fields, _)| fields.len() != len) {
        return None;
    }
    (0..len)
        .map(|idx| {
            let values = layouts
                .iter()
                .map(|(fields, built)| (fields[idx].clone(), built.field(idx, len)))
                .collect::<Vec<_>>();
            merged_built_class(db, &values)
        })
        .collect()
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
            semantic::{SemanticLocalKind, root_semantic_instance_key},
            ty::ty_check::{BodyOwner, ReturnSource},
        },
        hir_def::TopLevelMod,
    };
    use url::Url;

    use crate::{
        build_runtime_package,
        runtime::{RExpr, RStmt, RTerminator, RuntimeCarrier, RuntimeClass, RuntimeExitBehavior},
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
        let key = root_semantic_instance_key(db, BodyOwner::Func(func, None))
            .unwrap_or_else(|err| panic!("failed to root semantic function instance: {err:?}"));
        SemanticInstance::new(db, key)
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
            returned.push((selected.class, Built::Unknown));
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
    var x = 1
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
    fn product_part_indices_forward_return_provenance() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///product_part_indices_forward_return_provenance.fe").unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
struct Pair {
    a: u256,
    b: u256,
}

fn at(_ (mut xs, i): (mut [Pair; 2], own usize)) -> mut Pair {
    mut xs[i]
}

fn at_fwd(_ xs: mut [Pair; 2], _ i: usize) -> mut Pair {
    mut at((mut xs, i))
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
        let sources = |name| {
            semantic_instance_for_named_func(&db, top_mod, name)
                .key(&db)
                .typed_body(&db)
                .forwarded_return_sources(&db)
        };
        // A part a product parameter's `let` binds indexes as the part the
        // call passes, and a call passing a parameter as that part forwards it.
        assert_eq!(
            sources("at"),
            vec![ReturnSource {
                result_projection: Vec::new(),
                origin: CallableInputOrigin::ValueParam(0),
                projection: vec![
                    ReturnProjectionStep::Field(0),
                    ReturnProjectionStep::ParamIndex {
                        param: 0,
                        part: Some(1),
                    },
                ],
            }]
        );
        assert_eq!(
            sources("at_fwd"),
            vec![ReturnSource {
                result_projection: Vec::new(),
                origin: CallableInputOrigin::ValueParam(0),
                projection: vec![ReturnProjectionStep::ParamIndex {
                    param: 1,
                    part: None,
                }],
            }]
        );
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

impl Copy for Pair {}

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
        let storage = RuntimeClass::opaque_raw_addr(AddressSpace::Storage);
        let transient = RuntimeClass::opaque_raw_addr(AddressSpace::Transient);

        // Return sites that disagree on a non-Memory space cannot be merged, so the
        // fold reports failure (caller falls back to the default class) regardless of
        // the order the return sites were collected in.
        assert_eq!(
            merged_return_class(
                &db,
                vec![
                    (storage.clone(), Built::Unknown),
                    (transient.clone(), Built::Unknown)
                ],
                false
            ),
            None
        );
        assert_eq!(
            merged_return_class(
                &db,
                vec![(transient, Built::Unknown), (storage, Built::Unknown)],
                false
            ),
            None
        );
    }

    #[test]
    fn merged_return_class_folds_memory_into_non_memory_regardless_of_order() {
        let db = DriverDataBase::default();
        let memory = RuntimeClass::opaque_raw_addr(AddressSpace::Memory);
        let storage = RuntimeClass::opaque_raw_addr(AddressSpace::Storage);
        let merged = RuntimeClass::opaque_raw_addr(AddressSpace::Storage);

        assert_eq!(
            merged_return_class(
                &db,
                vec![
                    (memory.clone(), Built::Unknown),
                    (storage.clone(), Built::Unknown)
                ],
                false
            ),
            Some(merged.clone())
        );
        assert_eq!(
            merged_return_class(
                &db,
                vec![(storage, Built::Unknown), (memory, Built::Unknown)],
                false
            ),
            Some(merged)
        );
    }

    #[test]
    fn projection_result_spaces_hold_the_lowered_return_class() {
        let source = r#"
fn first(_ a: mut [u256; 2]) -> mut u256 {
    mut a[0]
}
"#;
        let mut db = DriverDataBase::default();
        let file_url = Url::parse("file:///projection_result_spaces.fe").unwrap();
        db.workspace()
            .touch(&mut db, file_url.clone(), Some(source.to_string()));
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let semantic = semantic_instance_for_named_func(&db, top_mod, "first");
        let key = runtime_instance_for_semantic(&db, semantic).key(&db);
        let ret = declaration_runtime_return_class(&db, key).expect("a projection's grant class");

        assert_eq!(
            check_projection_result_spaces(&db, semantic, Some(&ret)),
            Ok(())
        );
        assert!(
            check_projection_result_spaces(
                &db,
                semantic,
                Some(&RuntimeClass::opaque_raw_addr(AddressSpace::Storage)),
            )
            .is_err(),
            "a memory grant returned as a storage address is an error"
        );
    }
}
