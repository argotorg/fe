use hir::analysis::ty::ty_def::{CapabilityKind, TyId};

use crate::{
    db::MirDb,
    runtime::{
        AddressSpaceKind, ArrayLayout, EnumLayoutKey, EnumVariantLayout, Layout, LayoutId,
        LayoutKey, PlaceRoot, RBlockId, RExpr, RLocalId, RStmt, RefKind, RefView, RuntimeClass,
        RuntimePlace, ScalarClass, ScalarRepr, ScalarRole, StructLayout, remap_ref_view_to_pointee,
    },
};

use super::type_info::{RuntimeTypeEnv, runtime_interface_ty_in_env};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct RuntimeConversionPlan<'db> {
    pub(crate) steps: Box<[RuntimeConversionStep<'db>]>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum RuntimeConversionStep<'db> {
    RetagRef {
        class: RuntimeClass<'db>,
    },
    LoadRef {
        class: RuntimeClass<'db>,
    },
    AddrOfRef {
        class: RuntimeClass<'db>,
    },
    LoadRawAddr {
        class: RuntimeClass<'db>,
        space: AddressSpaceKind,
        layout: LayoutId<'db>,
    },
    MaterializeToObject {
        class: RuntimeClass<'db>,
    },
    ProviderFromRaw {
        class: RuntimeClass<'db>,
        provider_ty: TyId<'db>,
        space: AddressSpaceKind,
        target: Option<LayoutId<'db>>,
    },
    ProviderToRaw {
        class: RuntimeClass<'db>,
    },
    WordToRawAddr {
        class: RuntimeClass<'db>,
        space: AddressSpaceKind,
        target: Option<LayoutId<'db>>,
    },
    RawAddrToWord {
        class: RuntimeClass<'db>,
        scalar: ScalarClass<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum RuntimeConversionError<'db> {
    Unsupported {
        source: RuntimeClass<'db>,
        target: RuntimeClass<'db>,
    },
    Cycle {
        source: RuntimeClass<'db>,
        target: RuntimeClass<'db>,
    },
}

pub(crate) trait RuntimeConversionEmitter<'db> {
    fn alloc_conversion_temp(
        &mut self,
        semantic_ty: TyId<'db>,
        class: RuntimeClass<'db>,
    ) -> RLocalId;

    fn push_conversion_stmt(&mut self, bb: RBlockId, stmt: RStmt<'db>);
}

pub(crate) fn emit_runtime_conversion_plan<'db>(
    emitter: &mut impl RuntimeConversionEmitter<'db>,
    bb: RBlockId,
    mut value: RLocalId,
    plan: RuntimeConversionPlan<'db>,
    semantic_ty: TyId<'db>,
) -> RLocalId {
    let RuntimeConversionPlan { steps } = plan;
    for step in steps {
        value = emit_runtime_conversion_step(emitter, bb, value, step, semantic_ty);
    }
    value
}

pub(crate) fn emit_runtime_coercion<'db>(
    emitter: &mut impl RuntimeConversionEmitter<'db>,
    db: &'db dyn MirDb,
    bb: RBlockId,
    value: RLocalId,
    source: RuntimeClass<'db>,
    target: &RuntimeClass<'db>,
    semantic_ty: TyId<'db>,
) -> Result<RLocalId, RuntimeConversionError<'db>> {
    let plan = RuntimeConversionPlanner::plan(db, source, target.clone())?;
    Ok(emit_runtime_conversion_plan(
        emitter,
        bb,
        value,
        plan,
        semantic_ty,
    ))
}

fn emit_runtime_conversion_step<'db>(
    emitter: &mut impl RuntimeConversionEmitter<'db>,
    bb: RBlockId,
    src: RLocalId,
    step: RuntimeConversionStep<'db>,
    semantic_ty: TyId<'db>,
) -> RLocalId {
    match step {
        RuntimeConversionStep::RetagRef { class } => assign_runtime_conversion_temp(
            emitter,
            bb,
            semantic_ty,
            class,
            RExpr::RetagRef { value: src },
        ),
        RuntimeConversionStep::LoadRef { class } => assign_runtime_conversion_temp(
            emitter,
            bb,
            semantic_ty,
            class,
            RExpr::Load {
                place: RuntimePlace {
                    root: PlaceRoot::Ref(src),
                    path: Box::default(),
                },
            },
        ),
        RuntimeConversionStep::AddrOfRef { class } => assign_runtime_conversion_temp(
            emitter,
            bb,
            semantic_ty,
            class,
            RExpr::AddrOf {
                place: RuntimePlace {
                    root: PlaceRoot::Ref(src),
                    path: Box::default(),
                },
            },
        ),
        RuntimeConversionStep::LoadRawAddr {
            class,
            space,
            layout,
        } => assign_runtime_conversion_temp(
            emitter,
            bb,
            semantic_ty,
            class,
            RExpr::Load {
                place: RuntimePlace {
                    root: PlaceRoot::Ptr {
                        addr: src,
                        space,
                        class: RuntimeClass::AggregateValue { layout },
                    },
                    path: Box::default(),
                },
            },
        ),
        RuntimeConversionStep::MaterializeToObject { class } => assign_runtime_conversion_temp(
            emitter,
            bb,
            semantic_ty,
            class,
            RExpr::MaterializeToObject { src },
        ),
        RuntimeConversionStep::ProviderFromRaw {
            class,
            provider_ty,
            space,
            target,
        } => assign_runtime_conversion_temp(
            emitter,
            bb,
            semantic_ty,
            class,
            RExpr::ProviderFromRaw {
                raw: src,
                provider_ty,
                space,
                target,
            },
        ),
        RuntimeConversionStep::ProviderToRaw { class } => assign_runtime_conversion_temp(
            emitter,
            bb,
            semantic_ty,
            class,
            RExpr::ProviderToRaw { value: src },
        ),
        RuntimeConversionStep::WordToRawAddr {
            class,
            space,
            target,
        } => assign_runtime_conversion_temp(
            emitter,
            bb,
            semantic_ty,
            class,
            RExpr::WordToRawAddr {
                value: src,
                space,
                target,
            },
        ),
        RuntimeConversionStep::RawAddrToWord { class, scalar } => assign_runtime_conversion_temp(
            emitter,
            bb,
            semantic_ty,
            class,
            RExpr::Cast {
                value: src,
                to: scalar,
            },
        ),
    }
}

fn assign_runtime_conversion_temp<'db>(
    emitter: &mut impl RuntimeConversionEmitter<'db>,
    bb: RBlockId,
    semantic_ty: TyId<'db>,
    class: RuntimeClass<'db>,
    expr: RExpr<'db>,
) -> RLocalId {
    let dst = emitter.alloc_conversion_temp(semantic_ty, class);
    emitter.push_conversion_stmt(bb, RStmt::Assign { dst, expr });
    dst
}

pub(crate) struct RuntimeConversionPlanner<'db> {
    db: &'db dyn MirDb,
    stack: Vec<(RuntimeClass<'db>, RuntimeClass<'db>)>,
}

impl<'db> RuntimeConversionPlanner<'db> {
    pub(crate) fn plan(
        db: &'db dyn MirDb,
        source: RuntimeClass<'db>,
        target: RuntimeClass<'db>,
    ) -> Result<RuntimeConversionPlan<'db>, RuntimeConversionError<'db>> {
        let mut planner = Self {
            db,
            stack: Vec::new(),
        };
        let mut steps = Vec::new();
        planner.convert(source, target.clone(), &mut steps)?;
        Ok(RuntimeConversionPlan {
            steps: steps.into_boxed_slice(),
        })
    }

    fn convert(
        &mut self,
        source: RuntimeClass<'db>,
        target: RuntimeClass<'db>,
        steps: &mut Vec<RuntimeConversionStep<'db>>,
    ) -> Result<(), RuntimeConversionError<'db>> {
        if source == target {
            return Ok(());
        }
        if self.stack.iter().any(|(active_source, active_target)| {
            active_source == &source && active_target == &target
        }) {
            return Err(RuntimeConversionError::Cycle { source, target });
        }

        self.stack.push((source.clone(), target.clone()));
        let result = self.convert_inner(source, target, steps);
        self.stack.pop();
        result
    }

    fn convert_inner(
        &mut self,
        source: RuntimeClass<'db>,
        target: RuntimeClass<'db>,
        steps: &mut Vec<RuntimeConversionStep<'db>>,
    ) -> Result<(), RuntimeConversionError<'db>> {
        match (&source, &target) {
            (
                RuntimeClass::AggregateValue { .. },
                RuntimeClass::AggregateValue {
                    layout: target_layout,
                },
            ) if source.can_repack_as(self.db, &target) => {
                steps.push(RuntimeConversionStep::MaterializeToObject {
                    class: RuntimeClass::object_ref(*target_layout),
                });
                steps.push(RuntimeConversionStep::LoadRef { class: target });
                Ok(())
            }
            (RuntimeClass::Ref { .. }, RuntimeClass::Ref { .. })
                if source.shares_runtime_carrier_with(self.db, &target) =>
            {
                steps.push(RuntimeConversionStep::RetagRef { class: target });
                Ok(())
            }
            (
                RuntimeClass::Ref {
                    pointee,
                    kind: RefKind::Const,
                    view: RefView::Whole,
                },
                RuntimeClass::AggregateValue { .. },
            ) if pointee.aggregate_layout().is_some() => {
                let loaded = pointee.as_ref().clone();
                let layout = loaded.aggregate_layout().expect("aggregate const layout");
                steps.push(RuntimeConversionStep::MaterializeToObject {
                    class: RuntimeClass::object_ref(layout),
                });
                steps.push(RuntimeConversionStep::LoadRef {
                    class: loaded.clone(),
                });
                self.convert(loaded, target, steps)
            }
            (
                RuntimeClass::Ref {
                    pointee,
                    kind: RefKind::Provider { space, .. },
                    view: RefView::Whole,
                },
                RuntimeClass::Scalar(scalar),
            ) if *space != AddressSpaceKind::Memory && is_plain_word_scalar(scalar) => {
                let raw = RuntimeClass::RawAddr {
                    space: *space,
                    target: pointee.aggregate_layout(),
                };
                self.convert(source, raw.clone(), steps)?;
                self.convert(raw, target, steps)
            }
            (RuntimeClass::Ref { pointee, .. }, _) if !target.is_transport() => {
                let loaded = pointee.as_ref().clone();
                steps.push(RuntimeConversionStep::LoadRef {
                    class: loaded.clone(),
                });
                self.convert(loaded, target, steps)
            }
            (
                RuntimeClass::RawAddr {
                    space,
                    target: Some(layout),
                },
                RuntimeClass::Ref {
                    pointee,
                    kind:
                        RefKind::Provider {
                            provider_ty,
                            space: provider_space,
                        },
                    view: RefView::Whole,
                },
            ) if *space != AddressSpaceKind::Memory
                && space == provider_space
                && pointee.as_ref() == &(RuntimeClass::AggregateValue { layout: *layout }) =>
            {
                steps.push(RuntimeConversionStep::ProviderFromRaw {
                    class: target.clone(),
                    provider_ty: *provider_ty,
                    space: *space,
                    target: Some(*layout),
                });
                Ok(())
            }
            (
                RuntimeClass::Ref {
                    pointee,
                    kind: RefKind::Const,
                    view: RefView::Whole,
                },
                RuntimeClass::Ref {
                    pointee: target_pointee,
                    kind: RefKind::Object,
                    view: RefView::Whole,
                },
            ) if pointee == target_pointee => {
                let layout = target_pointee.aggregate_layout().ok_or_else(|| {
                    RuntimeConversionError::Unsupported {
                        source: source.clone(),
                        target: target.clone(),
                    }
                })?;
                steps.push(RuntimeConversionStep::MaterializeToObject {
                    class: RuntimeClass::object_ref(layout),
                });
                Ok(())
            }
            (
                RuntimeClass::Ref {
                    kind:
                        RefKind::Const
                        | RefKind::Object
                        | RefKind::Provider {
                            space: AddressSpaceKind::Memory,
                            ..
                        },
                    ..
                },
                RuntimeClass::Ref {
                    kind: RefKind::Object,
                    ..
                },
            ) if source.can_repack_as(self.db, &target) => {
                steps.push(RuntimeConversionStep::MaterializeToObject { class: target });
                Ok(())
            }
            (
                RuntimeClass::Ref {
                    pointee,
                    kind: RefKind::Const,
                    view: RefView::Whole,
                },
                RuntimeClass::Ref {
                    pointee: target_pointee,
                    kind:
                        RefKind::Provider {
                            space: AddressSpaceKind::Memory,
                            ..
                        },
                    view: RefView::Whole,
                },
            ) if pointee == target_pointee => {
                let layout = pointee.aggregate_layout().ok_or_else(|| {
                    RuntimeConversionError::Unsupported {
                        source: source.clone(),
                        target: target.clone(),
                    }
                })?;
                let object = RuntimeClass::object_ref(layout);
                self.convert(source, object.clone(), steps)?;
                self.convert(object, target, steps)
            }
            (
                RuntimeClass::Ref {
                    pointee,
                    kind: RefKind::Object,
                    view: RefView::Whole,
                },
                RuntimeClass::RawAddr {
                    space,
                    target: target_layout,
                },
            ) if *space == AddressSpaceKind::Memory
                && target_layout.is_none_or(|target_layout| {
                    Some(target_layout) == pointee.aggregate_layout()
                }) =>
            {
                steps.push(RuntimeConversionStep::AddrOfRef { class: target });
                Ok(())
            }
            (
                RuntimeClass::Ref {
                    pointee,
                    kind:
                        RefKind::Provider {
                            space: AddressSpaceKind::Memory,
                            ..
                        },
                    view: RefView::Whole,
                },
                RuntimeClass::RawAddr {
                    space: AddressSpaceKind::Memory,
                    target: target_layout,
                },
            ) if target_layout
                .is_none_or(|target_layout| Some(target_layout) == pointee.aggregate_layout()) =>
            {
                steps.push(RuntimeConversionStep::AddrOfRef { class: target });
                Ok(())
            }
            (
                RuntimeClass::Ref {
                    pointee,
                    kind: RefKind::Const,
                    view: RefView::Whole,
                },
                RuntimeClass::RawAddr {
                    space: AddressSpaceKind::Memory,
                    target: target_layout,
                },
            ) if pointee.aggregate_layout().is_some()
                && target_layout.is_none_or(|target_layout| {
                    Some(target_layout) == pointee.aggregate_layout()
                }) =>
            {
                let layout = pointee
                    .aggregate_layout()
                    .expect("aggregate const ref layout");
                let object = RuntimeClass::object_ref(layout);
                self.convert(source, object.clone(), steps)?;
                self.convert(object, target, steps)
            }
            (
                RuntimeClass::RawAddr {
                    space,
                    target: Some(layout),
                },
                RuntimeClass::AggregateValue {
                    layout: target_layout,
                },
            ) if layout == target_layout => {
                steps.push(RuntimeConversionStep::LoadRawAddr {
                    class: RuntimeClass::AggregateValue {
                        layout: *target_layout,
                    },
                    space: *space,
                    layout: *layout,
                });
                Ok(())
            }
            (
                RuntimeClass::AggregateValue { layout },
                RuntimeClass::Ref {
                    pointee,
                    kind: RefKind::Object,
                    view: RefView::Whole,
                },
            ) if pointee.as_ref() == &(RuntimeClass::AggregateValue { layout: *layout }) => {
                steps.push(RuntimeConversionStep::MaterializeToObject {
                    class: RuntimeClass::object_ref(*layout),
                });
                Ok(())
            }
            (
                RuntimeClass::RawAddr {
                    space,
                    target: source_target,
                },
                RuntimeClass::Ref {
                    pointee,
                    kind:
                        RefKind::Provider {
                            provider_ty,
                            space: provider_space,
                        },
                    view: RefView::Whole,
                },
            ) if *space != AddressSpaceKind::Memory
                && space == provider_space
                && source_target.is_none_or(|source_target| {
                    Some(source_target) == pointee.aggregate_layout()
                }) =>
            {
                let target_layout = pointee.aggregate_layout();
                steps.push(RuntimeConversionStep::ProviderFromRaw {
                    class: target.clone(),
                    provider_ty: *provider_ty,
                    space: *space,
                    target: target_layout,
                });
                Ok(())
            }
            (
                RuntimeClass::Ref {
                    kind:
                        RefKind::Provider {
                            space: source_space,
                            ..
                        },
                    ..
                },
                RuntimeClass::Ref {
                    pointee,
                    kind:
                        RefKind::Provider {
                            provider_ty,
                            space: target_space,
                        },
                    view: RefView::Whole,
                },
            ) if *target_space != AddressSpaceKind::Memory
                && source_space == target_space
                && pointee.aggregate_layout().is_some() =>
            {
                let target_layout = pointee.aggregate_layout();
                let raw = RuntimeClass::RawAddr {
                    space: *target_space,
                    target: target_layout,
                };
                self.convert(source, raw, steps)?;
                self.convert(
                    RuntimeClass::RawAddr {
                        space: *target_space,
                        target: target_layout,
                    },
                    RuntimeClass::Ref {
                        pointee: pointee.clone(),
                        kind: RefKind::Provider {
                            provider_ty: *provider_ty,
                            space: *target_space,
                        },
                        view: RefView::Whole,
                    },
                    steps,
                )
            }
            (
                RuntimeClass::AggregateValue { layout },
                RuntimeClass::Ref {
                    pointee,
                    kind: RefKind::Provider { provider_ty, space },
                    view: RefView::Whole,
                },
            ) if (RuntimeClass::AggregateValue { layout: *layout })
                .shares_runtime_rep_with(self.db, pointee) =>
            {
                let target_layout = pointee
                    .aggregate_layout()
                    .expect("aggregate provider ref layout");
                self.convert(source, pointee.as_ref().clone(), steps)?;
                self.convert(
                    pointee.as_ref().clone(),
                    RuntimeClass::object_ref(target_layout),
                    steps,
                )?;
                self.convert(
                    RuntimeClass::object_ref(target_layout),
                    RuntimeClass::provider_ref(target_layout, *provider_ty, *space),
                    steps,
                )
            }
            (
                RuntimeClass::Scalar(scalar),
                RuntimeClass::Ref {
                    pointee,
                    kind: RefKind::Provider { space, .. },
                    view: RefView::Whole,
                },
            ) if *space != AddressSpaceKind::Memory && is_plain_word_scalar(scalar) => {
                let target_layout = pointee.aggregate_layout();
                let raw = RuntimeClass::RawAddr {
                    space: *space,
                    target: target_layout,
                };
                steps.push(RuntimeConversionStep::WordToRawAddr {
                    class: raw.clone(),
                    space: *space,
                    target: target_layout,
                });
                self.convert(raw, target, steps)
            }
            (
                RuntimeClass::Ref {
                    pointee,
                    kind: RefKind::Provider { space, .. },
                    view: RefView::Whole,
                    ..
                },
                RuntimeClass::RawAddr {
                    space: target_space,
                    target: target_layout,
                },
            ) if *space != AddressSpaceKind::Memory
                && space == target_space
                && target_layout.is_none_or(|target_layout| {
                    Some(target_layout) == pointee.aggregate_layout()
                }) =>
            {
                steps.push(RuntimeConversionStep::ProviderToRaw { class: target });
                Ok(())
            }
            (RuntimeClass::RawAddr { .. }, RuntimeClass::Scalar(scalar))
                if matches!(
                    scalar.repr,
                    ScalarRepr::Int {
                        bits: 256,
                        signed: false
                    }
                ) =>
            {
                steps.push(RuntimeConversionStep::RawAddrToWord {
                    class: target.clone(),
                    scalar: scalar.clone(),
                });
                Ok(())
            }
            (
                RuntimeClass::Scalar(scalar),
                RuntimeClass::RawAddr {
                    space,
                    target: target_layout,
                },
            ) if is_plain_word_scalar(scalar) => {
                steps.push(RuntimeConversionStep::WordToRawAddr {
                    class: target.clone(),
                    space: *space,
                    target: *target_layout,
                });
                Ok(())
            }
            (
                _,
                RuntimeClass::Ref {
                    pointee,
                    kind: RefKind::Object,
                    view: RefView::Whole,
                },
            ) if pointee.aggregate_layout().is_some() => {
                let layout = pointee
                    .aggregate_layout()
                    .expect("aggregate object ref layout");
                self.convert(source, pointee.as_ref().clone(), steps)?;
                steps.push(RuntimeConversionStep::MaterializeToObject {
                    class: RuntimeClass::object_ref(layout),
                });
                Ok(())
            }
            _ => Err(RuntimeConversionError::Unsupported { source, target }),
        }
    }
}

/// Canonicalize a stored aggregate field when its declared type is a read-only
/// view. A view may snapshot immutable constant storage into an object without
/// changing observable aliasing, while an existing object remains the same
/// object whenever its representation is already canonical.
pub(crate) fn canonical_aggregate_field_class<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    field_ty: TyId<'db>,
    class: RuntimeClass<'db>,
) -> RuntimeClass<'db> {
    if matches!(
        runtime_interface_ty_in_env(db, env, field_ty).as_capability(db),
        Some((CapabilityKind::View, _))
    ) {
        canonical_memory_class(db, class, false)
    } else {
        class
    }
}

/// Closure environments are nominal values, so immutable constant references
/// are materialized into their canonical owned-memory representation.
/// Existing objects and memory providers retain their identity unless the
/// canonical class is representation-compatible and can be reached by a
/// no-copy retag. Non-memory providers and raw addresses always retain their
/// address-space-specific semantics.
pub(crate) fn canonical_closure_capture_class<'db>(
    db: &'db dyn MirDb,
    class: RuntimeClass<'db>,
) -> RuntimeClass<'db> {
    canonical_memory_class(db, class, true)
}

fn canonical_memory_class<'db>(
    db: &'db dyn MirDb,
    class: RuntimeClass<'db>,
    preserve_memory_identity: bool,
) -> RuntimeClass<'db> {
    match class {
        RuntimeClass::Scalar(_) | RuntimeClass::RawAddr { .. } => class,
        RuntimeClass::AggregateValue { layout } => RuntimeClass::AggregateValue {
            layout: canonical_memory_layout(db, layout, preserve_memory_identity),
        },
        RuntimeClass::Ref {
            pointee,
            kind,
            view,
        } => {
            let source = RuntimeClass::Ref {
                pointee: pointee.clone(),
                kind: kind.clone(),
                view: view.clone(),
            };
            let memory_object = matches!(
                &kind,
                RefKind::Const
                    | RefKind::Object
                    | RefKind::Provider {
                        space: AddressSpaceKind::Memory,
                        ..
                    }
            ) && pointee.aggregate_layout().is_some();
            if !memory_object {
                return RuntimeClass::Ref {
                    pointee,
                    kind,
                    view,
                };
            }
            let pointee = canonical_memory_class(db, *pointee, preserve_memory_identity);
            let view = remap_ref_view_to_pointee(&view, &pointee);
            let canonical = RuntimeClass::Ref {
                pointee: Box::new(pointee),
                kind: RefKind::Object,
                view,
            };
            if !preserve_memory_identity
                || matches!(kind, RefKind::Const)
                || source.shares_runtime_rep_with(db, &canonical)
            {
                canonical
            } else {
                source
            }
        }
    }
}

fn canonical_memory_layout<'db>(
    db: &'db dyn MirDb,
    layout: LayoutId<'db>,
    preserve_memory_identity: bool,
) -> LayoutId<'db> {
    match layout.data(db) {
        Layout::Struct(data) => LayoutId::new(
            db,
            LayoutKey::Struct(StructLayout {
                fields: data
                    .fields
                    .iter()
                    .cloned()
                    .map(|field| canonical_memory_class(db, field, preserve_memory_identity))
                    .collect(),
            }),
        ),
        Layout::Array(data) => LayoutId::new(
            db,
            LayoutKey::Array(ArrayLayout {
                elem: canonical_memory_class(db, data.elem.clone(), preserve_memory_identity),
                len: data.len,
            }),
        ),
        Layout::Enum(data) => LayoutId::new(
            db,
            LayoutKey::Enum(EnumLayoutKey {
                variants: data
                    .variants
                    .iter()
                    .map(|variant| EnumVariantLayout {
                        fields: variant
                            .fields
                            .iter()
                            .cloned()
                            .map(|field| {
                                canonical_memory_class(db, field, preserve_memory_identity)
                            })
                            .collect(),
                    })
                    .collect(),
            }),
        ),
    }
}

fn is_plain_word_scalar(scalar: &ScalarClass<'_>) -> bool {
    matches!(
        scalar,
        ScalarClass {
            repr: ScalarRepr::Int {
                bits: 256,
                signed: false
            },
            role: ScalarRole::Plain
        }
    )
}

#[cfg(test)]
mod tests {
    use driver::DriverDataBase;
    use hir::analysis::ty::trait_resolution::PredicateListId;

    use super::*;
    use crate::runtime::{EnumLayoutKey, EnumVariantLayout, LayoutKey, StructLayout, VariantId};

    fn word_class<'db>() -> RuntimeClass<'db> {
        RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int {
                bits: 256,
                signed: false,
            },
            role: ScalarRole::Plain,
        })
    }

    fn test_struct_layout<'db>(db: &'db dyn MirDb) -> LayoutId<'db> {
        LayoutId::new(
            db,
            LayoutKey::Struct(StructLayout {
                fields: vec![word_class()].into(),
            }),
        )
    }

    fn test_enum_layout<'db>(db: &'db dyn MirDb) -> LayoutId<'db> {
        LayoutId::new(
            db,
            LayoutKey::Enum(EnumLayoutKey {
                variants: vec![EnumVariantLayout {
                    fields: vec![word_class()].into(),
                }]
                .into(),
            }),
        )
    }

    #[test]
    fn identity_conversion_has_no_steps() {
        let db = DriverDataBase::default();
        let source = RuntimeClass::RawAddr {
            space: AddressSpaceKind::Memory,
            target: None,
        };

        let plan = RuntimeConversionPlanner::plan(&db, source.clone(), source).unwrap();

        assert!(plan.steps.is_empty());
    }

    #[test]
    fn word_and_raw_address_conversions_are_explicit_steps() {
        let db = DriverDataBase::default();
        let raw = RuntimeClass::RawAddr {
            space: AddressSpaceKind::Storage,
            target: None,
        };

        let to_raw = RuntimeConversionPlanner::plan(&db, word_class(), raw.clone()).unwrap();
        assert_eq!(
            to_raw.steps.as_ref(),
            &[RuntimeConversionStep::WordToRawAddr {
                class: raw.clone(),
                space: AddressSpaceKind::Storage,
                target: None,
            }]
        );

        let to_word = RuntimeConversionPlanner::plan(&db, raw, word_class()).unwrap();
        assert_eq!(
            to_word.steps.as_ref(),
            &[RuntimeConversionStep::RawAddrToWord {
                class: word_class(),
                scalar: match word_class() {
                    RuntimeClass::Scalar(scalar) => scalar,
                    _ => unreachable!(),
                },
            }]
        );
    }

    #[test]
    fn raw_address_to_provider_requires_matching_space() {
        let db = DriverDataBase::default();
        let provider_ty = TyId::unit(&db);
        let pointee = Box::new(word_class());
        let storage_provider = RuntimeClass::Ref {
            pointee: pointee.clone(),
            kind: RefKind::Provider {
                provider_ty,
                space: AddressSpaceKind::Storage,
            },
            view: RefView::Whole,
        };
        let storage_raw = RuntimeClass::RawAddr {
            space: AddressSpaceKind::Storage,
            target: None,
        };

        let plan =
            RuntimeConversionPlanner::plan(&db, storage_raw.clone(), storage_provider.clone())
                .unwrap();
        assert_eq!(
            plan.steps.as_ref(),
            &[RuntimeConversionStep::ProviderFromRaw {
                class: storage_provider,
                provider_ty,
                space: AddressSpaceKind::Storage,
                target: None,
            }]
        );

        let memory_provider = RuntimeClass::Ref {
            pointee,
            kind: RefKind::Provider {
                provider_ty,
                space: AddressSpaceKind::Memory,
            },
            view: RefView::Whole,
        };
        assert!(matches!(
            RuntimeConversionPlanner::plan(&db, storage_raw, memory_provider),
            Err(RuntimeConversionError::Unsupported { .. })
        ));
    }

    #[test]
    fn typed_raw_address_to_provider_requires_matching_target_layout() {
        let db = DriverDataBase::default();
        let source_layout = test_struct_layout(&db);
        let target_layout = LayoutId::new(
            &db,
            LayoutKey::Struct(StructLayout {
                fields: vec![RuntimeClass::Scalar(ScalarClass {
                    repr: ScalarRepr::Bool,
                    role: ScalarRole::Plain,
                })]
                .into(),
            }),
        );
        let source = RuntimeClass::RawAddr {
            space: AddressSpaceKind::Storage,
            target: Some(source_layout),
        };
        let target =
            RuntimeClass::provider_ref(target_layout, TyId::unit(&db), AddressSpaceKind::Storage);

        assert!(matches!(
            RuntimeConversionPlanner::plan(&db, source, target),
            Err(RuntimeConversionError::Unsupported { .. })
        ));
    }

    #[test]
    fn raw_memory_address_does_not_reconstruct_memory_provider() {
        let db = DriverDataBase::default();
        let provider_ty = TyId::unit(&db);
        let memory_provider = RuntimeClass::Ref {
            pointee: Box::new(word_class()),
            kind: RefKind::Provider {
                provider_ty,
                space: AddressSpaceKind::Memory,
            },
            view: RefView::Whole,
        };
        let memory_raw = RuntimeClass::RawAddr {
            space: AddressSpaceKind::Memory,
            target: None,
        };

        assert!(matches!(
            RuntimeConversionPlanner::plan(&db, memory_raw, memory_provider.clone()),
            Err(RuntimeConversionError::Unsupported { .. })
        ));
        assert!(matches!(
            RuntimeConversionPlanner::plan(&db, word_class(), memory_provider),
            Err(RuntimeConversionError::Unsupported { .. })
        ));
    }

    #[test]
    fn provider_to_raw_address_preserves_its_address_space() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let provider =
            RuntimeClass::provider_ref(layout, TyId::unit(&db), AddressSpaceKind::Storage);
        let storage_raw = RuntimeClass::RawAddr {
            space: AddressSpaceKind::Storage,
            target: Some(layout),
        };
        let calldata_raw = RuntimeClass::RawAddr {
            space: AddressSpaceKind::Calldata,
            target: Some(layout),
        };

        let plan =
            RuntimeConversionPlanner::plan(&db, provider.clone(), storage_raw.clone()).unwrap();
        assert_eq!(
            plan.steps.as_ref(),
            &[RuntimeConversionStep::ProviderToRaw { class: storage_raw }]
        );
        assert!(matches!(
            RuntimeConversionPlanner::plan(&db, provider, calldata_raw),
            Err(RuntimeConversionError::Unsupported { .. })
        ));
    }

    #[test]
    fn distinct_non_memory_provider_types_round_trip_through_raw() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let source = RuntimeClass::provider_ref(layout, TyId::unit(&db), AddressSpaceKind::Storage);
        let target = RuntimeClass::provider_ref(layout, TyId::u256(&db), AddressSpaceKind::Storage);
        let raw = RuntimeClass::RawAddr {
            space: AddressSpaceKind::Storage,
            target: Some(layout),
        };

        assert!(!source.shares_runtime_rep_with(&db, &target));
        let plan = RuntimeConversionPlanner::plan(&db, source, target.clone()).unwrap();
        assert_eq!(
            plan.steps.as_ref(),
            &[
                RuntimeConversionStep::ProviderToRaw { class: raw },
                RuntimeConversionStep::ProviderFromRaw {
                    class: target,
                    provider_ty: TyId::u256(&db),
                    space: AddressSpaceKind::Storage,
                    target: Some(layout),
                },
            ]
        );
    }

    #[test]
    fn memory_provider_materializes_before_becoming_a_raw_address() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let provider =
            RuntimeClass::provider_ref(layout, TyId::unit(&db), AddressSpaceKind::Memory);
        let target = RuntimeClass::RawAddr {
            space: AddressSpaceKind::Memory,
            target: Some(layout),
        };

        let plan = RuntimeConversionPlanner::plan(&db, provider, target.clone()).unwrap();
        assert_eq!(
            plan.steps.as_ref(),
            &[RuntimeConversionStep::AddrOfRef { class: target }]
        );
    }

    #[test]
    fn scalar_object_ref_to_untyped_memory_address_materializes_its_address() {
        let db = DriverDataBase::default();
        let source = RuntimeClass::Ref {
            pointee: Box::new(word_class()),
            kind: RefKind::Object,
            view: RefView::Whole,
        };
        let target = RuntimeClass::RawAddr {
            space: AddressSpaceKind::Memory,
            target: None,
        };

        let plan = RuntimeConversionPlanner::plan(&db, source, target.clone()).unwrap();
        assert_eq!(
            plan.steps.as_ref(),
            &[RuntimeConversionStep::AddrOfRef { class: target }]
        );
    }

    #[test]
    fn scalar_const_ref_to_untyped_memory_address_is_unsupported() {
        let db = DriverDataBase::default();
        let source = RuntimeClass::Ref {
            pointee: Box::new(word_class()),
            kind: RefKind::Const,
            view: RefView::Whole,
        };
        let target = RuntimeClass::RawAddr {
            space: AddressSpaceKind::Memory,
            target: None,
        };

        assert!(matches!(
            RuntimeConversionPlanner::plan(&db, source, target),
            Err(RuntimeConversionError::Unsupported { .. })
        ));
    }

    #[test]
    fn aggregate_const_ref_materializes_before_becoming_a_memory_address() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let source = RuntimeClass::const_ref(layout);
        let target = RuntimeClass::RawAddr {
            space: AddressSpaceKind::Memory,
            target: Some(layout),
        };

        let plan = RuntimeConversionPlanner::plan(&db, source, target.clone()).unwrap();
        assert_eq!(
            plan.steps.as_ref(),
            &[
                RuntimeConversionStep::MaterializeToObject {
                    class: RuntimeClass::object_ref(layout),
                },
                RuntimeConversionStep::AddrOfRef { class: target },
            ]
        );
    }

    #[test]
    fn aggregate_const_ref_cannot_become_a_non_memory_address() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let source = RuntimeClass::const_ref(layout);
        let target = RuntimeClass::RawAddr {
            space: AddressSpaceKind::Storage,
            target: Some(layout),
        };

        assert!(matches!(
            RuntimeConversionPlanner::plan(&db, source, target),
            Err(RuntimeConversionError::Unsupported { .. })
        ));
    }

    #[test]
    fn aggregate_to_object_ref_materializes_without_policy() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let source = RuntimeClass::AggregateValue { layout };
        let target = RuntimeClass::object_ref(layout);

        let plan = RuntimeConversionPlanner::plan(&db, source, target.clone()).unwrap();

        assert_eq!(
            plan.steps.as_ref(),
            &[RuntimeConversionStep::MaterializeToObject { class: target }]
        );
    }

    #[test]
    fn const_ref_to_object_ref_materializes_without_retagging() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let target = RuntimeClass::object_ref(layout);

        let plan =
            RuntimeConversionPlanner::plan(&db, RuntimeClass::const_ref(layout), target.clone())
                .unwrap();

        assert_eq!(
            plan.steps.as_ref(),
            &[RuntimeConversionStep::MaterializeToObject { class: target }]
        );
    }

    #[test]
    fn scalar_const_ref_to_object_ref_is_unsupported_without_panicking() {
        let db = DriverDataBase::default();
        let source = RuntimeClass::Ref {
            pointee: Box::new(word_class()),
            kind: RefKind::Const,
            view: RefView::Whole,
        };
        let target = RuntimeClass::Ref {
            pointee: Box::new(word_class()),
            kind: RefKind::Object,
            view: RefView::Whole,
        };

        assert!(matches!(
            RuntimeConversionPlanner::plan(&db, source, target),
            Err(RuntimeConversionError::Unsupported { .. })
        ));
    }

    #[test]
    fn object_ref_cannot_become_a_non_memory_provider() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let provider_ty = TyId::unit(&db);
        let target = RuntimeClass::provider_ref(layout, provider_ty, AddressSpaceKind::Storage);

        assert!(matches!(
            RuntimeConversionPlanner::plan(&db, RuntimeClass::object_ref(layout), target),
            Err(RuntimeConversionError::Unsupported { .. })
        ));
    }

    #[test]
    fn const_ref_materializes_before_becoming_a_memory_provider() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let target = RuntimeClass::provider_ref(layout, TyId::unit(&db), AddressSpaceKind::Memory);

        let plan =
            RuntimeConversionPlanner::plan(&db, RuntimeClass::const_ref(layout), target.clone())
                .unwrap();
        assert_eq!(
            plan.steps.as_ref(),
            &[
                RuntimeConversionStep::MaterializeToObject {
                    class: RuntimeClass::object_ref(layout),
                },
                RuntimeConversionStep::RetagRef { class: target },
            ]
        );
    }

    #[test]
    fn const_ref_cannot_become_a_non_memory_provider() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let target = RuntimeClass::provider_ref(layout, TyId::unit(&db), AddressSpaceKind::Storage);

        assert!(matches!(
            RuntimeConversionPlanner::plan(&db, RuntimeClass::const_ref(layout), target),
            Err(RuntimeConversionError::Unsupported { .. })
        ));
    }

    #[test]
    fn raw_aggregate_address_to_value_loads_from_pointer() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let target = RuntimeClass::AggregateValue { layout };

        let plan = RuntimeConversionPlanner::plan(
            &db,
            RuntimeClass::RawAddr {
                space: AddressSpaceKind::Storage,
                target: Some(layout),
            },
            target.clone(),
        )
        .unwrap();

        assert_eq!(
            plan.steps.as_ref(),
            &[RuntimeConversionStep::LoadRawAddr {
                class: target,
                space: AddressSpaceKind::Storage,
                layout,
            }]
        );
    }

    #[test]
    fn structurally_identical_enum_values_need_no_conversion() {
        let db = DriverDataBase::default();
        let source_layout = test_enum_layout(&db);
        let target_layout = test_enum_layout(&db);
        let source = RuntimeClass::AggregateValue {
            layout: source_layout,
        };
        let target = RuntimeClass::AggregateValue {
            layout: target_layout,
        };

        assert_eq!(source_layout, target_layout);
        let plan = RuntimeConversionPlanner::plan(&db, source, target).unwrap();
        assert!(plan.steps.is_empty());
    }

    #[test]
    fn nested_const_references_repack_into_canonical_closure_capture_layouts() {
        let db = DriverDataBase::default();
        let inner = test_struct_layout(&db);
        let source_layout = LayoutId::new(
            &db,
            LayoutKey::Struct(StructLayout {
                fields: vec![RuntimeClass::const_ref(inner)].into(),
            }),
        );
        let source = RuntimeClass::AggregateValue {
            layout: source_layout,
        };
        let target = canonical_closure_capture_class(&db, source.clone());
        let RuntimeClass::AggregateValue {
            layout: target_layout,
        } = target.clone()
        else {
            panic!("canonical aggregate capture must remain aggregate-valued");
        };
        let Layout::Struct(target_data) = target_layout.data(&db) else {
            panic!("canonical struct capture must retain its layout kind");
        };

        assert_eq!(
            target_data.fields.as_ref(),
            &[RuntimeClass::object_ref(inner)]
        );
        assert!(source.can_repack_as(&db, &target));
        let plan = RuntimeConversionPlanner::plan(&db, source, target.clone()).unwrap();
        assert_eq!(
            plan.steps.as_ref(),
            &[
                RuntimeConversionStep::MaterializeToObject {
                    class: RuntimeClass::object_ref(target_layout),
                },
                RuntimeConversionStep::LoadRef { class: target },
            ]
        );
    }

    #[test]
    fn readonly_view_fields_canonicalize_nested_const_storage_before_aliasing() {
        let db = DriverDataBase::default();
        let assumptions = PredicateListId::new(&db, Vec::new());
        let inner = test_struct_layout(&db);
        let source_layout = LayoutId::new(
            &db,
            LayoutKey::Struct(StructLayout {
                fields: vec![RuntimeClass::const_ref(inner)].into(),
            }),
        );
        let source = RuntimeClass::object_ref(source_layout);
        let target = canonical_aggregate_field_class(
            &db,
            RuntimeTypeEnv::new(None, assumptions),
            TyId::view_of(&db, TyId::unit(&db)),
            source.clone(),
        );
        let RuntimeClass::Ref {
            pointee,
            kind: RefKind::Object,
            ..
        } = &target
        else {
            panic!("canonical read-only view must remain object-backed");
        };
        let target_layout = pointee.aggregate_layout().expect("canonical view layout");
        let Layout::Struct(target_data) = target_layout.data(&db) else {
            panic!("canonical view must retain its struct layout");
        };

        assert_eq!(
            target_data.fields.as_ref(),
            &[RuntimeClass::object_ref(inner)]
        );
        let plan = RuntimeConversionPlanner::plan(&db, source, target.clone()).unwrap();
        assert_eq!(
            plan.steps.as_ref(),
            &[RuntimeConversionStep::MaterializeToObject { class: target }]
        );
    }

    #[test]
    fn memory_provider_capture_normalization_preserves_the_reference() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let source = RuntimeClass::provider_ref(layout, TyId::unit(&db), AddressSpaceKind::Memory);
        let target = canonical_closure_capture_class(&db, source.clone());

        assert_eq!(target, RuntimeClass::object_ref(layout));
        let plan = RuntimeConversionPlanner::plan(&db, source, target.clone()).unwrap();
        assert_eq!(
            plan.steps.as_ref(),
            &[RuntimeConversionStep::RetagRef { class: target }]
        );
    }

    #[test]
    fn enum_variant_capture_normalization_remaps_the_asserted_layout() {
        let db = DriverDataBase::default();
        let payload_layout = test_struct_layout(&db);
        let source_layout = LayoutId::new(
            &db,
            LayoutKey::Enum(EnumLayoutKey {
                variants: vec![EnumVariantLayout {
                    fields: vec![RuntimeClass::const_ref(payload_layout)].into(),
                }]
                .into(),
            }),
        );
        let source = RuntimeClass::Ref {
            pointee: Box::new(RuntimeClass::AggregateValue {
                layout: source_layout,
            }),
            kind: RefKind::Const,
            view: RefView::EnumVariant(VariantId {
                enum_layout: source_layout,
                index: 0,
            }),
        };
        let target = canonical_closure_capture_class(&db, source.clone());
        let RuntimeClass::Ref {
            pointee,
            view: RefView::EnumVariant(target_variant),
            ..
        } = &target
        else {
            panic!("canonical enum capture must retain its variant view");
        };
        let target_layout = pointee.aggregate_layout().expect("canonical enum layout");

        assert_ne!(source_layout, target_layout);
        assert_eq!(target_variant.enum_layout, target_layout);
        let plan = RuntimeConversionPlanner::plan(&db, source, target.clone()).unwrap();
        assert_eq!(
            plan.steps.as_ref(),
            &[RuntimeConversionStep::MaterializeToObject { class: target }]
        );
    }
}
