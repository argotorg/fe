use hir::analysis::{
    semantic::{NOperand, NSPlace, SLocalId},
    ty::ty_def::TyId,
};

use crate::runtime::{
    PlaceRoot, RBlockId, RLocalId, RuntimeBoundarySpec, RuntimeClass, RuntimeParamPlan,
    RuntimePlace,
};

use super::boundary::{
    BoundaryMatcher, RuntimeValueAddress, RuntimeValueMaterialization, RuntimeValueSource,
    RuntimeValueUsePlan, RuntimeValueUsePlanner,
};

#[derive(Clone, Debug)]
pub(super) struct SelectedRuntimeArg<'db> {
    pub(super) class: RuntimeClass<'db>,
    pub(super) source: RuntimeArgSource<'db>,
    pub(super) use_plan: RuntimeValueUsePlan<'db>,
}

#[derive(Clone, Debug)]
pub(super) enum RuntimeArgSource<'db> {
    SemanticOperand(NOperand),
    DirectValueMaterialization {
        local: SLocalId,
        materialized_class: RuntimeClass<'db>,
    },
    RuntimeValue(SLocalId),
    HandleLikeValue(SLocalId),
    PlaceAddress(NSPlace<'db>, TyId<'db>),
    PlaceValue(NSPlace<'db>, TyId<'db>),
    ValueExtract {
        place: NSPlace<'db>,
        semantic_ty: TyId<'db>,
        value_class: RuntimeClass<'db>,
    },
    SemanticPlaceAddress(SLocalId, TyId<'db>),
    AggregateFromRuntimeSource(SLocalId),
    Placeholder(TyId<'db>),
}

#[derive(Clone, Debug)]
pub(crate) struct SelectedRuntimeValueArg<'db> {
    pub(crate) source: RLocalId,
    pub(crate) semantic_ty: TyId<'db>,
    pub(crate) class: RuntimeClass<'db>,
    pub(crate) use_plan: RuntimeValueUsePlan<'db>,
}

impl<'db> SelectedRuntimeValueArg<'db> {
    fn use_value(source: RLocalId, semantic_ty: TyId<'db>, class: RuntimeClass<'db>) -> Self {
        Self {
            source,
            semantic_ty,
            class,
            use_plan: RuntimeValueUsePlan::UseValue,
        }
    }
}

pub(crate) trait RuntimeValueArgSelectionCx<'db> {
    fn runtime_value_class(&self, value: RLocalId) -> Option<RuntimeClass<'db>>;

    fn runtime_value_source(&self, value: RLocalId) -> Option<RuntimeValueSource<'db>>;

    fn promote_runtime_value_address(
        &mut self,
        value: RLocalId,
        boundary: &RuntimeBoundarySpec<'db>,
    ) -> Option<RuntimeValueAddress<'db>>;
}

pub(crate) struct RuntimeValueArgSelector<'cx, Cx> {
    cx: &'cx mut Cx,
}

impl<'cx, Cx> RuntimeValueArgSelector<'cx, Cx> {
    pub(crate) fn new(cx: &'cx mut Cx) -> Self {
        Self { cx }
    }
}

impl<'cx, 'db, Cx> RuntimeValueArgSelector<'cx, Cx>
where
    Cx: RuntimeValueArgSelectionCx<'db>,
{
    pub(crate) fn selected_arg_for_param_plan(
        &mut self,
        source: RLocalId,
        plan: &RuntimeParamPlan<'db>,
        semantic_ty: TyId<'db>,
    ) -> SelectedRuntimeValueArg<'db> {
        match plan {
            RuntimeParamPlan::Erased => {
                panic!("erased runtime param should not have a runtime arg")
            }
            RuntimeParamPlan::PassActual => {
                SelectedRuntimeValueArg::use_value(source, semantic_ty, self.source_class(source))
            }
            RuntimeParamPlan::ReadOnlyView { value, borrow } => {
                let source_class = self.source_class(source);
                let use_plan = self.select_read_only_view_use_plan(source, value, borrow);
                SelectedRuntimeValueArg {
                    source,
                    semantic_ty,
                    class: use_plan.class(&source_class),
                    use_plan,
                }
            }
            RuntimeParamPlan::Boundary(boundary) => {
                let source_class = self.source_class(source);
                let use_plan = self.select_boundary_use_plan(source, boundary);
                SelectedRuntimeValueArg {
                    source,
                    semantic_ty,
                    class: use_plan.class(&source_class),
                    use_plan,
                }
            }
        }
    }

    fn select_boundary_use_plan(
        &mut self,
        source: RLocalId,
        boundary: &RuntimeBoundarySpec<'db>,
    ) -> RuntimeValueUsePlan<'db> {
        let mut value_source = self.cx.runtime_value_source(source).unwrap_or_else(|| {
            panic!("cannot realize erased runtime value {source:?} for boundary {boundary:?}")
        });
        if let Some(address) = self.cx.promote_runtime_value_address(source, boundary) {
            value_source.address = Some(address);
        }
        RuntimeValueUsePlanner::select(value_source, boundary).unwrap_or_else(|| {
            let source = self.source_class(source);
            panic!(
                "runtime boundary has no realizable materialization: source={source:?} boundary={boundary:?}"
            )
        })
    }

    fn select_read_only_view_use_plan(
        &mut self,
        source: RLocalId,
        value: &RuntimeClass<'db>,
        borrow: &RuntimeBoundarySpec<'db>,
    ) -> RuntimeValueUsePlan<'db> {
        let source_class = self.source_class(source);
        let mut value_source = self.cx.runtime_value_source(source).unwrap_or_else(|| {
            panic!("cannot realize erased runtime value {source:?} for read-only view")
        });
        if let Some(address) = self.cx.promote_runtime_value_address(source, borrow) {
            value_source.address = Some(address);
        }
        if BoundaryMatcher::class_satisfies_boundary(&source_class, borrow) {
            return RuntimeValueUsePlan::UseValue;
        }
        if let Some(address) = value_source
            .address
            .filter(|address| BoundaryMatcher::class_satisfies_boundary(&address.class, borrow))
        {
            return RuntimeValueUsePlan::AddrOfRuntimePlace {
                place: address.place,
                class: address.class,
            };
        }
        if &source_class == value {
            return RuntimeValueUsePlan::CoerceValue(value.clone());
        }
        self.select_boundary_use_plan(source, borrow)
    }

    fn source_class(&self, source: RLocalId) -> RuntimeClass<'db> {
        self.cx
            .runtime_value_class(source)
            .unwrap_or_else(|| panic!("cannot pass erased runtime arg {source:?}"))
    }
}

pub(crate) trait RuntimeValueUseEmitter<'db> {
    fn value_class_for_use(&self, value: RLocalId) -> Option<RuntimeClass<'db>>;

    fn coerce_value_for_use(
        &mut self,
        bb: RBlockId,
        src: RLocalId,
        target: &RuntimeClass<'db>,
        semantic_ty: TyId<'db>,
    ) -> RLocalId;

    fn emit_addr_of_place_for_use(
        &mut self,
        bb: RBlockId,
        place: RuntimePlace<'db>,
        class: RuntimeClass<'db>,
        semantic_ty: TyId<'db>,
    ) -> RLocalId;

    fn alloc_value_slot(&mut self, semantic_ty: TyId<'db>, class: RuntimeClass<'db>) -> RLocalId;

    fn push_value_use(&mut self, bb: RBlockId, dst: RLocalId, src: RLocalId);
}

pub(crate) fn emit_runtime_value_use_plan<'db>(
    emitter: &mut impl RuntimeValueUseEmitter<'db>,
    bb: RBlockId,
    src: RLocalId,
    use_plan: RuntimeValueUsePlan<'db>,
    semantic_ty: TyId<'db>,
) -> RLocalId {
    match use_plan {
        RuntimeValueUsePlan::UseValue => src,
        RuntimeValueUsePlan::AddrOfRuntimePlace { place, class } => {
            emitter.emit_addr_of_place_for_use(bb, place, class, semantic_ty)
        }
        RuntimeValueUsePlan::CoerceValue(target) => {
            emitter.coerce_value_for_use(bb, src, &target, semantic_ty)
        }
        RuntimeValueUsePlan::MaterializeValue(materialization) => {
            emit_runtime_value_materialization(emitter, bb, src, materialization, semantic_ty)
        }
    }
}

pub(crate) fn emit_selected_runtime_value_arg<'db>(
    emitter: &mut impl RuntimeValueUseEmitter<'db>,
    bb: RBlockId,
    arg: &SelectedRuntimeValueArg<'db>,
) -> RLocalId {
    let value = emit_runtime_value_use_plan(
        emitter,
        bb,
        arg.source,
        arg.use_plan.clone(),
        arg.semantic_ty,
    );
    let Some(class) = emitter.value_class_for_use(value) else {
        panic!(
            "selected runtime value arg lowered without a runtime class: arg={arg:?}; value={value:?}"
        );
    };
    assert_eq!(
        class, arg.class,
        "selected runtime value arg class mismatch: arg={arg:?}; value={value:?}",
    );
    value
}

pub(crate) fn emit_selected_runtime_value_args<'db>(
    emitter: &mut impl RuntimeValueUseEmitter<'db>,
    bb: RBlockId,
    args: &[SelectedRuntimeValueArg<'db>],
) -> Vec<RLocalId> {
    args.iter()
        .map(|arg| emit_selected_runtime_value_arg(emitter, bb, arg))
        .collect()
}

fn emit_runtime_value_materialization<'db>(
    emitter: &mut impl RuntimeValueUseEmitter<'db>,
    bb: RBlockId,
    src: RLocalId,
    materialization: RuntimeValueMaterialization<'db>,
    semantic_ty: TyId<'db>,
) -> RLocalId {
    match materialization {
        RuntimeValueMaterialization::ObjectRef(layout) => {
            emitter.coerce_value_for_use(bb, src, &RuntimeClass::object_ref(layout), semantic_ty)
        }
        RuntimeValueMaterialization::RawAddrSlot(pointee) => {
            let source = emitter
                .value_class_for_use(src)
                .unwrap_or_else(|| panic!("cannot materialize erased runtime value {src:?}"));
            let stored = if source == pointee {
                src
            } else {
                emitter.coerce_value_for_use(bb, src, &pointee, semantic_ty)
            };
            let slot = emitter.alloc_value_slot(semantic_ty, pointee.clone());
            emitter.push_value_use(bb, slot, stored);
            emitter.emit_addr_of_place_for_use(
                bb,
                RuntimePlace {
                    root: PlaceRoot::Slot(slot),
                    path: Box::default(),
                },
                RuntimeValueMaterialization::RawAddrSlot(pointee).class(),
                semantic_ty,
            )
        }
    }
}

impl<'db> SelectedRuntimeArg<'db> {
    fn coerce(source: RuntimeArgSource<'db>, class: RuntimeClass<'db>) -> Self {
        Self {
            use_plan: RuntimeValueUsePlan::CoerceValue(class.clone()),
            class,
            source,
        }
    }

    fn materialize(
        source: RuntimeArgSource<'db>,
        materialization: RuntimeValueMaterialization<'db>,
    ) -> Self {
        Self {
            class: materialization.class(),
            source,
            use_plan: RuntimeValueUsePlan::MaterializeValue(materialization),
        }
    }

    pub(super) fn local_value(local: SLocalId, class: RuntimeClass<'db>) -> Self {
        let source = if class.is_transport() {
            RuntimeArgSource::HandleLikeValue(local)
        } else {
            RuntimeArgSource::RuntimeValue(local)
        };
        Self::coerce(source, class)
    }

    pub(super) fn runtime_value(local: SLocalId, class: RuntimeClass<'db>) -> Self {
        Self::coerce(RuntimeArgSource::RuntimeValue(local), class)
    }

    pub(super) fn handle_like_value(local: SLocalId, class: RuntimeClass<'db>) -> Self {
        Self::coerce(RuntimeArgSource::HandleLikeValue(local), class)
    }

    pub(super) fn semantic_operand(arg: NOperand, class: RuntimeClass<'db>) -> Self {
        Self::coerce(RuntimeArgSource::SemanticOperand(arg), class)
    }

    pub(super) fn direct_value_materialization(
        local: SLocalId,
        materialized_class: RuntimeClass<'db>,
        class: RuntimeClass<'db>,
    ) -> Self {
        Self::coerce(
            RuntimeArgSource::DirectValueMaterialization {
                local,
                materialized_class,
            },
            class,
        )
    }

    pub(super) fn placeholder(semantic_ty: TyId<'db>, class: RuntimeClass<'db>) -> Self {
        Self {
            class,
            source: RuntimeArgSource::Placeholder(semantic_ty),
            use_plan: RuntimeValueUsePlan::UseValue,
        }
    }

    pub(super) fn place_addr(
        place: NSPlace<'db>,
        semantic_ty: TyId<'db>,
        class: RuntimeClass<'db>,
    ) -> Self {
        Self {
            class,
            source: RuntimeArgSource::PlaceAddress(place, semantic_ty),
            use_plan: RuntimeValueUsePlan::UseValue,
        }
    }

    pub(super) fn place_load(
        place: NSPlace<'db>,
        semantic_ty: TyId<'db>,
        class: RuntimeClass<'db>,
    ) -> Self {
        Self::coerce(RuntimeArgSource::PlaceValue(place, semantic_ty), class)
    }

    pub(super) fn value_extract(
        place: NSPlace<'db>,
        semantic_ty: TyId<'db>,
        class: RuntimeClass<'db>,
    ) -> Self {
        let value_class = class.clone();
        Self::coerce(
            RuntimeArgSource::ValueExtract {
                place,
                semantic_ty,
                value_class,
            },
            class,
        )
    }

    pub(super) fn semantic_place_addr(
        local: SLocalId,
        semantic_ty: TyId<'db>,
        class: RuntimeClass<'db>,
    ) -> Self {
        Self {
            class,
            source: RuntimeArgSource::SemanticPlaceAddress(local, semantic_ty),
            use_plan: RuntimeValueUsePlan::UseValue,
        }
    }

    pub(super) fn aggregate_from_runtime_source(local: SLocalId, class: RuntimeClass<'db>) -> Self {
        Self::coerce(RuntimeArgSource::AggregateFromRuntimeSource(local), class)
    }

    pub(super) fn materialized_place(
        place: NSPlace<'db>,
        semantic_ty: TyId<'db>,
        materialization: RuntimeValueMaterialization<'db>,
    ) -> Self {
        Self::materialize(
            RuntimeArgSource::PlaceValue(place, semantic_ty),
            materialization,
        )
    }

    pub(super) fn materialized_value_extract(
        place: NSPlace<'db>,
        semantic_ty: TyId<'db>,
        value_class: RuntimeClass<'db>,
        materialization: RuntimeValueMaterialization<'db>,
    ) -> Self {
        Self::materialize(
            RuntimeArgSource::ValueExtract {
                place,
                semantic_ty,
                value_class,
            },
            materialization,
        )
    }

    pub(super) fn materialized_semantic_operand(
        arg: NOperand,
        materialization: RuntimeValueMaterialization<'db>,
    ) -> Self {
        Self::materialize(RuntimeArgSource::SemanticOperand(arg), materialization)
    }
}

#[cfg(test)]
mod tests {
    use cranelift_entity::EntityRef;
    use driver::DriverDataBase;
    use hir::analysis::ty::ty_def::TyId;

    use crate::runtime::{
        AddressSpaceKind, RBlockId, RLocalId, RuntimeClass, RuntimePlace, ScalarClass, ScalarRepr,
        ScalarRole,
    };

    use super::super::boundary::{RuntimeValueMaterialization, RuntimeValueUsePlan};
    use super::{
        RuntimeValueUseEmitter, SelectedRuntimeValueArg, emit_runtime_value_use_plan,
        emit_selected_runtime_value_args,
    };

    fn word_class<'db>() -> RuntimeClass<'db> {
        RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int {
                bits: 256,
                signed: false,
            },
            role: ScalarRole::Plain,
        })
    }

    fn bool_class<'db>() -> RuntimeClass<'db> {
        RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Bool,
            role: ScalarRole::Plain,
        })
    }

    fn raw_addr_class<'db>(space: AddressSpaceKind) -> RuntimeClass<'db> {
        RuntimeClass::raw_addr(space, word_class())
    }

    #[derive(Debug, PartialEq, Eq)]
    enum FakeEmitOp<'db> {
        Coerce {
            src: RLocalId,
            dst: RLocalId,
            target: RuntimeClass<'db>,
        },
        AddrOf {
            dst: RLocalId,
            class: RuntimeClass<'db>,
        },
        Assign {
            dst: RLocalId,
            src: RLocalId,
        },
    }

    struct FakeBoundaryEmitter<'db> {
        classes: Vec<Option<RuntimeClass<'db>>>,
        roots: Vec<Option<RuntimeClass<'db>>>,
        ops: Vec<FakeEmitOp<'db>>,
    }

    impl<'db> FakeBoundaryEmitter<'db> {
        fn new(classes: Vec<Option<RuntimeClass<'db>>>) -> Self {
            let roots = vec![None; classes.len()];
            Self {
                classes,
                roots,
                ops: Vec::new(),
            }
        }

        fn push_value(&mut self, class: RuntimeClass<'db>) -> RLocalId {
            let id = RLocalId::new(self.classes.len());
            self.classes.push(Some(class));
            self.roots.push(None);
            id
        }
    }

    impl<'db> RuntimeValueUseEmitter<'db> for FakeBoundaryEmitter<'db> {
        fn value_class_for_use(&self, value: RLocalId) -> Option<RuntimeClass<'db>> {
            self.classes.get(value.index())?.clone()
        }

        fn coerce_value_for_use(
            &mut self,
            bb: RBlockId,
            src: RLocalId,
            target: &RuntimeClass<'db>,
            semantic_ty: TyId<'db>,
        ) -> RLocalId {
            let _ = (bb, semantic_ty);
            let dst = self.push_value(target.clone());
            self.ops.push(FakeEmitOp::Coerce {
                src,
                dst,
                target: target.clone(),
            });
            dst
        }

        fn emit_addr_of_place_for_use(
            &mut self,
            bb: RBlockId,
            place: RuntimePlace<'db>,
            class: RuntimeClass<'db>,
            semantic_ty: TyId<'db>,
        ) -> RLocalId {
            let _ = (bb, place, semantic_ty);
            let dst = self.push_value(class.clone());
            self.ops.push(FakeEmitOp::AddrOf { dst, class });
            dst
        }

        fn alloc_value_slot(
            &mut self,
            semantic_ty: TyId<'db>,
            class: RuntimeClass<'db>,
        ) -> RLocalId {
            let _ = semantic_ty;
            let dst = self.push_value(class.clone());
            self.roots[dst.index()] = Some(class);
            dst
        }

        fn push_value_use(&mut self, bb: RBlockId, dst: RLocalId, src: RLocalId) {
            let _ = bb;
            self.ops.push(FakeEmitOp::Assign { dst, src });
        }
    }

    #[test]
    fn raw_addr_slot_materialization_coerces_stores_and_addresses_slot() {
        let db = DriverDataBase::default();
        let semantic_ty = TyId::unit(&db);
        let mut emitter = FakeBoundaryEmitter::new(vec![Some(bool_class())]);

        let result = emit_runtime_value_use_plan(
            &mut emitter,
            RBlockId::new(0),
            RLocalId::new(0),
            RuntimeValueUsePlan::MaterializeValue(RuntimeValueMaterialization::RawAddrSlot(
                word_class(),
            )),
            semantic_ty,
        );

        assert_eq!(result, RLocalId::new(3));
        assert_eq!(
            emitter.classes[result.index()],
            Some(raw_addr_class(AddressSpaceKind::Memory))
        );
        assert_eq!(emitter.roots[RLocalId::new(2).index()], Some(word_class()));
        assert_eq!(
            emitter.ops,
            vec![
                FakeEmitOp::Coerce {
                    src: RLocalId::new(0),
                    dst: RLocalId::new(1),
                    target: word_class(),
                },
                FakeEmitOp::Assign {
                    dst: RLocalId::new(2),
                    src: RLocalId::new(1),
                },
                FakeEmitOp::AddrOf {
                    dst: RLocalId::new(3),
                    class: raw_addr_class(AddressSpaceKind::Memory),
                },
            ]
        );
    }

    #[test]
    fn selected_runtime_value_args_share_use_plan_lowering() {
        let db = DriverDataBase::default();
        let semantic_ty = TyId::unit(&db);
        let mut emitter = FakeBoundaryEmitter::new(vec![Some(word_class())]);

        let values = emit_selected_runtime_value_args(
            &mut emitter,
            RBlockId::new(0),
            &[SelectedRuntimeValueArg {
                source: RLocalId::new(0),
                semantic_ty,
                class: word_class(),
                use_plan: RuntimeValueUsePlan::UseValue,
            }],
        );

        assert_eq!(values, vec![RLocalId::new(0)]);
        assert!(emitter.ops.is_empty());
    }
}
