use common::compilation::Target;
use hir::analysis::ty::AddressSpace;
use hir::analysis::ty::ty_def::TyId;

use crate::{
    db::MirDb,
    runtime::{
        LayoutId, PlaceRoot, RBlockId, RExpr, RLocalId, RStmt, RefKind, RefView, RuntimeClass,
        RuntimePlace, ScalarClass, ScalarRepr, ScalarRole,
        lower::type_info::{address_int_class, pointer_class},
    },
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct RuntimeConversionPlan<'db> {
    pub(crate) steps: Box<[RuntimeConversionStep<'db>]>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum RuntimeConversionStep<'db> {
    UseAs {
        class: RuntimeClass<'db>,
    },
    NativeRef {
        class: RuntimeClass<'db>,
    },
    RetagRef {
        class: RuntimeClass<'db>,
    },
    LoadRef {
        class: RuntimeClass<'db>,
    },
    LoadRawAddr {
        class: RuntimeClass<'db>,
        space: AddressSpace,
    },
    MaterializeToObject {
        class: RuntimeClass<'db>,
    },
    AllocObjectCopy {
        class: RuntimeClass<'db>,
        layout: LayoutId<'db>,
    },
    ProviderRefFromRaw {
        class: RuntimeClass<'db>,
        provider_ty: TyId<'db>,
        space: AddressSpace,
    },
    ProviderRefToRaw {
        class: RuntimeClass<'db>,
    },
    ObjectFromRaw {
        class: RuntimeClass<'db>,
    },
    WordToRawAddr {
        class: RuntimeClass<'db>,
        space: AddressSpace,
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
        RuntimeConversionStep::UseAs { class } => {
            assign_runtime_conversion_temp(emitter, bb, semantic_ty, class, RExpr::Use(src))
        }
        RuntimeConversionStep::NativeRef { class } => assign_runtime_conversion_temp(
            emitter,
            bb,
            semantic_ty,
            class,
            RExpr::NativeRef { value: src },
        ),
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
        RuntimeConversionStep::LoadRawAddr { class, space } => {
            let pointee = class.clone();
            assign_runtime_conversion_temp(
                emitter,
                bb,
                semantic_ty,
                class,
                RExpr::Load {
                    place: RuntimePlace {
                        root: PlaceRoot::Ptr {
                            addr: src,
                            space,
                            class: pointee,
                        },
                        path: Box::default(),
                    },
                },
            )
        }
        RuntimeConversionStep::MaterializeToObject { class } => assign_runtime_conversion_temp(
            emitter,
            bb,
            semantic_ty,
            class,
            RExpr::MaterializeToObject { src },
        ),
        RuntimeConversionStep::AllocObjectCopy { class, layout } => {
            let dst = assign_runtime_conversion_temp(
                emitter,
                bb,
                semantic_ty,
                class,
                RExpr::AllocObject { layout },
            );
            emitter.push_conversion_stmt(
                bb,
                RStmt::CopyInto {
                    dst: RuntimePlace {
                        root: PlaceRoot::Ref(dst),
                        path: Box::default(),
                    },
                    src,
                },
            );
            dst
        }
        RuntimeConversionStep::ProviderRefFromRaw {
            class,
            provider_ty,
            space,
        } => assign_runtime_conversion_temp(
            emitter,
            bb,
            semantic_ty,
            class,
            RExpr::ProviderRefFromRaw {
                raw: src,
                provider_ty,
                space,
            },
        ),
        RuntimeConversionStep::ProviderRefToRaw { class } => assign_runtime_conversion_temp(
            emitter,
            bb,
            semantic_ty,
            class,
            RExpr::ProviderRefToRaw { value: src },
        ),
        RuntimeConversionStep::ObjectFromRaw { class } => assign_runtime_conversion_temp(
            emitter,
            bb,
            semantic_ty,
            class,
            RExpr::ObjectFromRaw { raw: src },
        ),
        RuntimeConversionStep::WordToRawAddr { class, space } => assign_runtime_conversion_temp(
            emitter,
            bb,
            semantic_ty,
            class,
            RExpr::WordToRawAddr { value: src, space },
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
            ) if source.shares_runtime_rep_with(self.db, &target) => {
                steps.push(RuntimeConversionStep::AllocObjectCopy {
                    class: RuntimeClass::object_ref(*target_layout),
                    layout: *target_layout,
                });
                steps.push(RuntimeConversionStep::LoadRef { class: target });
                Ok(())
            }
            (
                RuntimeClass::Ref {
                    view: actual_view, ..
                },
                RuntimeClass::Ref {
                    view: desired_view, ..
                },
            ) if actual_view == desired_view
                && source.shares_runtime_rep_with(self.db, &target) =>
            {
                steps.push(RuntimeConversionStep::RetagRef { class: target });
                Ok(())
            }
            (
                RuntimeClass::Ref {
                    view: RefView::Whole,
                    ..
                }
                | RuntimeClass::RawAddr {
                    pointee: Some(_), ..
                },
                RuntimeClass::Ref {
                    pointee: desired,
                    kind: RefKind::Native,
                    view: RefView::Whole,
                },
            ) if source.targets(self.db, desired) => {
                steps.push(RuntimeConversionStep::NativeRef { class: target });
                Ok(())
            }
            (RuntimeClass::RawAddr { .. }, RuntimeClass::RawAddr { .. })
                if source.shares_runtime_rep_with(self.db, &target) =>
            {
                steps.push(RuntimeConversionStep::UseAs { class: target });
                Ok(())
            }
            // Native raw memory has the object layout, so the object at an
            // address is that memory, not a copy of it.
            (
                RuntimeClass::RawAddr {
                    space: AddressSpace::Memory,
                    pointee: Some(raw_pointee),
                },
                RuntimeClass::Ref {
                    pointee,
                    kind: RefKind::Object,
                    view: RefView::Whole,
                },
            ) if self.db.compilation_settings().target(self.db) == Target::Native
                && raw_pointee.target(self.db) == &**pointee =>
            {
                steps.push(RuntimeConversionStep::ObjectFromRaw { class: target });
                Ok(())
            }
            (
                RuntimeClass::Scalar(scalar),
                RuntimeClass::Ref {
                    pointee,
                    kind: RefKind::Object,
                    view: RefView::Whole,
                },
            ) if self.db.compilation_settings().target(self.db) == Target::Native
                && *scalar == pointer_class(self.db, AddressSpace::Memory) =>
            {
                let raw =
                    RuntimeClass::raw_addr(self.db, AddressSpace::Memory, (**pointee).clone());
                self.convert(source, raw.clone(), steps)?;
                self.convert(raw, target, steps)
            }
            (
                RuntimeClass::Ref {
                    pointee,
                    kind: RefKind::Provider { space, .. },
                    view: RefView::Whole,
                },
                RuntimeClass::Scalar(scalar),
            ) if *space != AddressSpace::Memory && is_plain_word_scalar(scalar) => {
                let raw = RuntimeClass::raw_addr(self.db, *space, pointee.as_ref().clone());
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
                    pointee: raw_pointee,
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
            ) if *space != AddressSpace::Memory
                && space == provider_space
                && raw_pointee
                    .is_none_or(|raw_pointee| raw_pointee.target(self.db) == &**pointee) =>
            {
                steps.push(RuntimeConversionStep::ProviderRefFromRaw {
                    class: target.clone(),
                    provider_ty: *provider_ty,
                    space: *space,
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
                let layout = target_pointee
                    .aggregate_layout()
                    .expect("aggregate ref layout");
                steps.push(RuntimeConversionStep::MaterializeToObject {
                    class: RuntimeClass::object_ref(layout),
                });
                Ok(())
            }
            (
                RuntimeClass::RawAddr {
                    space,
                    pointee: Some(pointee),
                },
                RuntimeClass::AggregateValue {
                    layout: target_layout,
                },
            ) if *pointee.target(self.db)
                == (RuntimeClass::AggregateValue {
                    layout: *target_layout,
                }) =>
            {
                steps.push(RuntimeConversionStep::LoadRawAddr {
                    class: RuntimeClass::AggregateValue {
                        layout: *target_layout,
                    },
                    space: *space,
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
            ) if *target_space != AddressSpace::Memory
                && source_space == target_space
                && pointee.aggregate_layout().is_some() =>
            {
                let raw = RuntimeClass::raw_addr(self.db, *target_space, pointee.as_ref().clone());
                self.convert(source, raw.clone(), steps)?;
                self.convert(
                    raw,
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
            ) if *space != AddressSpace::Memory && is_plain_word_scalar(scalar) => {
                let raw = RuntimeClass::raw_addr(self.db, *space, pointee.as_ref().clone());
                steps.push(RuntimeConversionStep::WordToRawAddr {
                    class: raw.clone(),
                    space: *space,
                });
                self.convert(raw, target, steps)
            }
            (
                RuntimeClass::Ref {
                    pointee,
                    kind:
                        RefKind::Provider {
                            space: provider_space,
                            ..
                        },
                    view: RefView::Whole,
                },
                RuntimeClass::RawAddr {
                    space: raw_space,
                    pointee: raw_pointee,
                },
            ) if *provider_space != AddressSpace::Memory
                && provider_space == raw_space
                && raw_pointee.is_none_or(|target| target.target(self.db) == &**pointee) =>
            {
                steps.push(RuntimeConversionStep::ProviderRefToRaw { class: target });
                Ok(())
            }
            (RuntimeClass::RawAddr { space, .. }, RuntimeClass::Scalar(scalar))
                if scalar.repr == address_int_class(self.db, *space).repr =>
            {
                steps.push(RuntimeConversionStep::RawAddrToWord {
                    class: target.clone(),
                    scalar: scalar.clone(),
                });
                Ok(())
            }
            (RuntimeClass::Scalar(scalar), RuntimeClass::RawAddr { space, .. })
                if *scalar == pointer_class(self.db, *space) =>
            {
                steps.push(RuntimeConversionStep::WordToRawAddr {
                    class: target.clone(),
                    space: *space,
                });
                Ok(())
            }
            (
                RuntimeClass::Ref {
                    kind: RefKind::Native,
                    ..
                },
                RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. },
            ) => Err(RuntimeConversionError::Unsupported { source, target }),
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
                steps.push(RuntimeConversionStep::AllocObjectCopy {
                    class: RuntimeClass::object_ref(layout),
                    layout,
                });
                Ok(())
            }
            _ => Err(RuntimeConversionError::Unsupported { source, target }),
        }
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
    use common::InputDb;
    use driver::DriverDataBase;
    use salsa::Setter;

    use super::*;
    use crate::runtime::{EnumLayoutKey, EnumVariantLayout, LayoutKey, RawPointeeId, StructLayout};

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
                pinned: false,
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
        let source = RuntimeClass::opaque_raw_addr(AddressSpace::Memory);

        let plan = RuntimeConversionPlanner::plan(&db, source.clone(), source).unwrap();

        assert!(plan.steps.is_empty());
    }

    #[test]
    fn word_and_raw_address_conversions_are_explicit_steps() {
        let db = DriverDataBase::default();
        let raw = RuntimeClass::opaque_raw_addr(AddressSpace::Storage);

        let to_raw = RuntimeConversionPlanner::plan(&db, word_class(), raw.clone()).unwrap();
        assert_eq!(
            to_raw.steps.as_ref(),
            &[RuntimeConversionStep::WordToRawAddr {
                class: raw.clone(),
                space: AddressSpace::Storage,
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
                space: AddressSpace::Storage,
            },
            view: RefView::Whole,
        };
        let storage_raw = RuntimeClass::raw_addr(&db, AddressSpace::Storage, word_class());

        let plan =
            RuntimeConversionPlanner::plan(&db, storage_raw.clone(), storage_provider.clone())
                .unwrap();
        assert_eq!(
            plan.steps.as_ref(),
            &[RuntimeConversionStep::ProviderRefFromRaw {
                class: storage_provider,
                provider_ty,
                space: AddressSpace::Storage,
            }]
        );

        let memory_provider = RuntimeClass::Ref {
            pointee,
            kind: RefKind::Provider {
                provider_ty,
                space: AddressSpace::Memory,
            },
            view: RefView::Whole,
        };
        assert!(matches!(
            RuntimeConversionPlanner::plan(&db, storage_raw, memory_provider),
            Err(RuntimeConversionError::Unsupported { .. })
        ));
    }

    #[test]
    fn memory_provider_cannot_be_reinterpreted_as_raw_memory() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        for pointee in [word_class(), RuntimeClass::AggregateValue { layout }] {
            let provider = RuntimeClass::Ref {
                pointee: Box::new(pointee.clone()),
                kind: RefKind::Provider {
                    provider_ty: TyId::unit(&db),
                    space: AddressSpace::Memory,
                },
                view: RefView::Whole,
            };
            for raw in [
                RuntimeClass::raw_addr(&db, AddressSpace::Memory, pointee),
                RuntimeClass::opaque_raw_addr(AddressSpace::Memory),
            ] {
                assert!(matches!(
                    RuntimeConversionPlanner::plan(&db, provider.clone(), raw.clone()),
                    Err(RuntimeConversionError::Unsupported { .. })
                ));
                assert!(matches!(
                    RuntimeConversionPlanner::plan(&db, raw, provider.clone()),
                    Err(RuntimeConversionError::Unsupported { .. })
                ));
            }
        }
    }

    #[test]
    fn provider_to_raw_address_requires_matching_space_and_target() {
        let db = DriverDataBase::default();
        let provider_ty = TyId::unit(&db);
        let storage_provider = RuntimeClass::Ref {
            pointee: Box::new(word_class()),
            kind: RefKind::Provider {
                provider_ty,
                space: AddressSpace::Storage,
            },
            view: RefView::Whole,
        };
        let storage_raw = RuntimeClass::RawAddr {
            space: AddressSpace::Storage,
            pointee: None,
        };

        let plan =
            RuntimeConversionPlanner::plan(&db, storage_provider.clone(), storage_raw.clone())
                .unwrap();
        assert_eq!(
            plan.steps.as_ref(),
            &[RuntimeConversionStep::ProviderRefToRaw { class: storage_raw }]
        );

        let memory_raw = RuntimeClass::RawAddr {
            space: AddressSpace::Memory,
            pointee: None,
        };
        assert!(matches!(
            RuntimeConversionPlanner::plan(&db, storage_provider.clone(), memory_raw),
            Err(RuntimeConversionError::Unsupported { .. })
        ));

        let layout = test_struct_layout(&db);
        let typed_raw = RuntimeClass::RawAddr {
            space: AddressSpace::Storage,
            pointee: Some(RawPointeeId::new(
                &db,
                RuntimeClass::AggregateValue { layout },
            )),
        };
        assert!(matches!(
            RuntimeConversionPlanner::plan(&db, storage_provider, typed_raw),
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
                space: AddressSpace::Memory,
            },
            view: RefView::Whole,
        };
        let memory_raw = RuntimeClass::raw_addr(&db, AddressSpace::Memory, word_class());

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
    fn scalar_ref_to_untyped_raw_addr_is_unsupported() {
        let db = DriverDataBase::default();
        let source = RuntimeClass::Ref {
            pointee: Box::new(word_class()),
            kind: RefKind::Object,
            view: RefView::Whole,
        };
        let target = RuntimeClass::opaque_raw_addr(AddressSpace::Memory);

        assert!(matches!(
            RuntimeConversionPlanner::plan(&db, source, target),
            Err(RuntimeConversionError::Unsupported { .. })
        ));
    }

    #[test]
    fn raw_memory_is_an_object_in_place_only_natively() {
        for target_kind in [Target::Evm, Target::Native] {
            let mut db = DriverDataBase::default();
            db.compilation_settings()
                .set_target(&mut db)
                .to(target_kind);
            let layout = test_struct_layout(&db);
            let source = RuntimeClass::raw_addr(
                &db,
                AddressSpace::Memory,
                RuntimeClass::AggregateValue { layout },
            );
            let target = RuntimeClass::object_ref(layout);
            let plan = RuntimeConversionPlanner::plan(&db, source, target.clone()).unwrap();
            if target_kind == Target::Native {
                assert_eq!(
                    plan.steps.as_ref(),
                    &[RuntimeConversionStep::ObjectFromRaw { class: target }]
                );
            } else {
                // EVM objects and raw memory lay a struct out differently: the
                // object is a copy.
                assert!(matches!(
                    plan.steps.last(),
                    Some(RuntimeConversionStep::AllocObjectCopy { .. })
                ));
            }
        }
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
    fn const_ref_to_aggregate_value_loads_the_whole_value() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let target = RuntimeClass::AggregateValue { layout };

        let plan =
            RuntimeConversionPlanner::plan(&db, RuntimeClass::const_ref(layout), target.clone())
                .unwrap();

        assert_eq!(
            plan.steps.as_ref(),
            &[RuntimeConversionStep::LoadRef { class: target }]
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
    fn native_reference_conversion_preserves_static_and_raw_referents() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let native = RuntimeClass::Ref {
            pointee: Box::new(RuntimeClass::AggregateValue { layout }),
            kind: RefKind::Native,
            view: RefView::Whole,
        };
        let mut sources = vec![
            RuntimeClass::object_ref(layout),
            RuntimeClass::const_ref(layout),
        ];
        for space in [
            AddressSpace::Memory,
            AddressSpace::Storage,
            AddressSpace::Transient,
            AddressSpace::Calldata,
            AddressSpace::Code,
        ] {
            sources.push(RuntimeClass::raw_addr(
                &db,
                space,
                RuntimeClass::AggregateValue { layout },
            ));
            sources.push(RuntimeClass::provider_ref(layout, TyId::unit(&db), space));
        }
        for source in sources {
            assert!(!source.shares_runtime_rep_with(&db, &native));
            let plan = RuntimeConversionPlanner::plan(&db, source.clone(), native.clone()).unwrap();
            assert_eq!(
                plan.steps.as_ref(),
                &[RuntimeConversionStep::NativeRef {
                    class: native.clone()
                }]
            );
            assert!(
                RuntimeConversionPlanner::plan(&db, native.clone(), source).is_err(),
                "an erased native layout cannot become a static view"
            );
        }
        assert!(
            RuntimeConversionPlanner::plan(
                &db,
                RuntimeClass::opaque_raw_addr(AddressSpace::Memory),
                native
            )
            .is_err()
        );
    }

    #[test]
    fn object_ref_cannot_change_to_a_storage_provider() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let provider_ty = TyId::unit(&db);
        let target = RuntimeClass::provider_ref(layout, provider_ty, AddressSpace::Storage);

        assert!(matches!(
            RuntimeConversionPlanner::plan(&db, RuntimeClass::object_ref(layout), target),
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
            RuntimeClass::raw_addr(
                &db,
                AddressSpace::Storage,
                RuntimeClass::AggregateValue { layout },
            ),
            target.clone(),
        )
        .unwrap();

        assert_eq!(
            plan.steps.as_ref(),
            &[RuntimeConversionStep::LoadRawAddr {
                class: target,
                space: AddressSpace::Storage,
            }]
        );
    }

    #[test]
    fn raw_address_retag_preserves_same_space_pointer_value() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let target = RuntimeClass::opaque_raw_addr(AddressSpace::Memory);

        let plan = RuntimeConversionPlanner::plan(
            &db,
            RuntimeClass::raw_addr(
                &db,
                AddressSpace::Memory,
                RuntimeClass::AggregateValue { layout },
            ),
            target.clone(),
        )
        .unwrap();

        assert_eq!(
            plan.steps.as_ref(),
            &[RuntimeConversionStep::UseAs { class: target }]
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
}
