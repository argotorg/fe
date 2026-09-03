//! Canonical resolution of runtime places and value classes.
//!
//! `resolve_runtime_place` is the single walker from a [`RuntimePlace`] to
//! the classes along its projection path; every consumer — body lowering,
//! the verifier, and codegen — resolves places here. The projection
//! arithmetic (`project_field` and friends) likewise has exactly one
//! implementation, with panicking wrappers for lowering-internal callers
//! whose places are already known well-formed.

use cranelift_entity::EntityRef;
use hir::analysis::semantic::FieldIndex;
use hir::projection::IndexSource;

use crate::{
    db::MirDb,
    runtime::{
        AddressSpaceKind, ConstScalar, Layout, LayoutId, PlaceElem, PlaceRoot, RLocalId, RefKind,
        RefView, ResolvedPlaceElem, ResolvedPlaceRootKind, ResolvedRuntimePlace, RuntimeBody,
        RuntimeClass, RuntimeLocalRoot, RuntimeProgramView, RuntimeProviderBinding,
        RuntimeProviderBindingId, ScalarClass, ScalarRepr, ScalarRole, VariantId,
    },
    verify::VerifyError,
};

/// The class environment a place resolves against: the finished
/// [`RuntimeBody`], or the in-flight lowering state before a body exists.
pub trait PlaceClassEnv<'db> {
    fn place_local_root(&self, local: RLocalId) -> Option<&RuntimeLocalRoot<'db>>;
    fn place_value_class(&self, value: crate::runtime::RValueId) -> Option<&RuntimeClass<'db>>;
    fn place_provider_binding(
        &self,
        binding: RuntimeProviderBindingId,
    ) -> Option<&RuntimeProviderBinding<'db>>;
}

impl<'db> PlaceClassEnv<'db> for RuntimeBody<'db> {
    fn place_local_root(&self, local: RLocalId) -> Option<&RuntimeLocalRoot<'db>> {
        self.local(local).map(|local| &local.root)
    }

    fn place_value_class(&self, value: crate::runtime::RValueId) -> Option<&RuntimeClass<'db>> {
        self.value_class(value)
    }

    fn place_provider_binding(
        &self,
        binding: RuntimeProviderBindingId,
    ) -> Option<&RuntimeProviderBinding<'db>> {
        self.provider_bindings.get(binding.index())
    }
}

pub fn resolve_runtime_place<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    body: &impl PlaceClassEnv<'db>,
    place: &crate::runtime::RuntimePlace<'db>,
) -> Result<ResolvedRuntimePlace<'db>, VerifyError<'db>> {
    let mut current = match &place.root {
        PlaceRoot::Slot(local) => match body
            .place_local_root(*local)
            .ok_or(VerifyError::MissingRuntimeLocal(*local))?
        {
            RuntimeLocalRoot::None | RuntimeLocalRoot::Ref(_) | RuntimeLocalRoot::Ptr { .. } => {
                return Err(VerifyError::InvalidPlace(RuntimeClass::opaque_raw_addr(
                    crate::runtime::AddressSpaceKind::Memory,
                )));
            }
            RuntimeLocalRoot::Slot(class) => class.clone(),
        },
        PlaceRoot::Ref(value) => match body
            .place_value_class(*value)
            .cloned()
            .ok_or(VerifyError::ErasedRuntimeValue(*value))?
        {
            RuntimeClass::Ref { pointee, .. } => *pointee,
            class => class,
        },
        PlaceRoot::Provider(binding) => body
            .place_provider_binding(*binding)
            .map(|binding| binding.place_class.clone())
            .ok_or(VerifyError::InvalidPlace(RuntimeClass::opaque_raw_addr(
                crate::runtime::AddressSpaceKind::Memory,
            )))?,
        PlaceRoot::Ptr { addr, space, class } => {
            match body
                .place_value_class(*addr)
                .ok_or(VerifyError::ErasedRuntimeValue(*addr))?
            {
                RuntimeClass::RawAddr {
                    space: actual_space,
                    ..
                } if *actual_space == *space => {}
                RuntimeClass::Ref {
                    kind:
                        crate::runtime::RefKind::Provider {
                            space: actual_space,
                            ..
                        },
                    ..
                } if *actual_space == *space => {}
                value_class => return Err(VerifyError::InvalidPlace(value_class.clone())),
            }
            class.clone()
        }
    };

    let root_kind = match &place.root {
        PlaceRoot::Slot(local) => ResolvedPlaceRootKind::Slot {
            local: *local,
            class: current.clone(),
        },
        PlaceRoot::Ref(value) => ResolvedPlaceRootKind::Ref {
            value: *value,
            class: current.clone(),
        },
        PlaceRoot::Provider(binding) => {
            let provider =
                body.place_provider_binding(*binding)
                    .ok_or(VerifyError::InvalidPlace(RuntimeClass::opaque_raw_addr(
                        crate::runtime::AddressSpaceKind::Memory,
                    )))?;
            ResolvedPlaceRootKind::Provider {
                binding: *binding,
                value: provider.value,
                provider_class: provider.provider_class.clone(),
                class: current.clone(),
            }
        }
        PlaceRoot::Ptr { addr, space, .. } => ResolvedPlaceRootKind::Ptr {
            addr: *addr,
            space: *space,
            class: current.clone(),
        },
    };

    let mut path = Vec::with_capacity(place.path.len());
    for elem in place.path.iter() {
        match elem {
            PlaceElem::Field(field) => {
                current = project_field(program, current, *field)?;
                path.push(ResolvedPlaceElem::Field {
                    field: *field,
                    class: current.clone(),
                });
            }
            PlaceElem::Index(index) => {
                if let IndexSource::Dynamic(index) = index {
                    let _ = body
                        .place_value_class(*index)
                        .ok_or(VerifyError::ErasedRuntimeValue(*index))?;
                }
                current = project_index(program, current)?;
                path.push(ResolvedPlaceElem::Index {
                    index: *index,
                    class: current.clone(),
                });
            }
            PlaceElem::VariantField { variant, field } => {
                current = project_variant_field(db, current, *variant, *field)?;
                path.push(ResolvedPlaceElem::VariantField {
                    variant: *variant,
                    field: *field,
                    class: current.clone(),
                });
            }
            PlaceElem::Deref => {
                let carrier_class = current;
                current = carrier_class
                    .deref_target()
                    .ok_or_else(|| VerifyError::InvalidPlace(carrier_class.clone()))?;
                path.push(ResolvedPlaceElem::Deref {
                    carrier_class,
                    class: current.clone(),
                });
            }
        }
    }

    if place.path.is_empty()
        && let PlaceRoot::Ref(_) | PlaceRoot::Provider(_) = &place.root
        && let RuntimeClass::Ref { ref pointee, .. } = current
        && let RuntimeClass::AggregateValue { layout } = **pointee
    {
        current = RuntimeClass::AggregateValue { layout };
    }

    Ok(ResolvedRuntimePlace {
        root_kind,
        result_class: current,
        path: path.into_boxed_slice(),
    })
}

pub fn resolve_runtime_place_address_class<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    body: &impl PlaceClassEnv<'db>,
    place: &crate::runtime::RuntimePlace<'db>,
) -> Result<RuntimeClass<'db>, VerifyError<'db>> {
    let resolved = resolve_runtime_place(db, program, body, place)?;
    let (mut root_class, mut root_space, mut force_raw) =
        runtime_place_transport_root(body, place)?;
    for elem in resolved.path.iter() {
        if let ResolvedPlaceElem::Deref { carrier_class, .. } = elem {
            root_class = carrier_class.clone();
            root_space = root_class.address_space().unwrap_or(root_space);
            force_raw = matches!(root_class, RuntimeClass::RawAddr { .. });
        }
    }
    Ok(ref_class_for_place_result(
        &root_class,
        &resolved.result_class,
        root_space,
        force_raw,
    ))
}

pub(crate) fn project_place<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    body: &impl PlaceClassEnv<'db>,
    place: &crate::runtime::RuntimePlace<'db>,
) -> Result<RuntimeClass<'db>, VerifyError<'db>> {
    Ok(resolve_runtime_place(db, program, body, place)?.result_class)
}

/// The reference (or raw-address) class produced by taking the address of a
/// place whose transport root has class `root_class` and whose projected
/// value has class `value_class`.
pub(crate) fn ref_class_for_place_result<'db>(
    root_class: &RuntimeClass<'db>,
    value_class: &RuntimeClass<'db>,
    root_space: AddressSpaceKind,
    force_raw: bool,
) -> RuntimeClass<'db> {
    if !force_raw {
        match root_class {
            RuntimeClass::Ref { kind, .. } => {
                return RuntimeClass::Ref {
                    pointee: Box::new(value_class.clone()),
                    kind: kind.clone(),
                    view: RefView::Whole,
                };
            }
            RuntimeClass::AggregateValue { .. } => {
                return RuntimeClass::Ref {
                    pointee: Box::new(value_class.clone()),
                    kind: RefKind::Object,
                    view: RefView::Whole,
                };
            }
            RuntimeClass::Scalar(_) | RuntimeClass::RawAddr { .. } => {}
        }
    }
    RuntimeClass::raw_addr(
        root_class.address_space().unwrap_or(root_space),
        value_class.clone(),
    )
}

fn runtime_place_transport_root<'db>(
    body: &impl PlaceClassEnv<'db>,
    place: &crate::runtime::RuntimePlace<'db>,
) -> Result<(RuntimeClass<'db>, crate::runtime::AddressSpaceKind, bool), VerifyError<'db>> {
    Ok(match &place.root {
        PlaceRoot::Slot(local) => (
            match body
                .place_local_root(*local)
                .ok_or(VerifyError::MissingRuntimeLocal(*local))?
            {
                RuntimeLocalRoot::Slot(class) => class.clone(),
                RuntimeLocalRoot::None
                | RuntimeLocalRoot::Ref(_)
                | RuntimeLocalRoot::Ptr { .. } => {
                    return Err(VerifyError::InvalidPlace(RuntimeClass::opaque_raw_addr(
                        crate::runtime::AddressSpaceKind::Memory,
                    )));
                }
            },
            crate::runtime::AddressSpaceKind::Memory,
            false,
        ),
        PlaceRoot::Ref(value) => (
            body.place_value_class(*value)
                .ok_or(VerifyError::ErasedRuntimeValue(*value))?
                .clone(),
            crate::runtime::AddressSpaceKind::Memory,
            false,
        ),
        PlaceRoot::Provider(binding) => {
            let class = body
                .place_provider_binding(*binding)
                .map(|binding| binding.provider_class.clone())
                .ok_or(VerifyError::InvalidPlace(RuntimeClass::opaque_raw_addr(
                    crate::runtime::AddressSpaceKind::Memory,
                )))?;
            (
                class.clone(),
                class
                    .address_space()
                    .unwrap_or(crate::runtime::AddressSpaceKind::Memory),
                false,
            )
        }
        PlaceRoot::Ptr { space, class, .. } => {
            (RuntimeClass::raw_addr(*space, class.clone()), *space, true)
        }
    })
}

pub(crate) fn runtime_value_class<'a, 'db>(
    body: &'a RuntimeBody<'db>,
    value: crate::runtime::RValueId,
) -> Result<&'a RuntimeClass<'db>, VerifyError<'db>> {
    body.value_class(value)
        .ok_or(VerifyError::ErasedRuntimeValue(value))
}

pub(crate) fn scalar_class_from_const<'db>(value: &ConstScalar) -> ScalarClass<'db> {
    match value {
        ConstScalar::Bool(_) => ScalarClass {
            repr: ScalarRepr::Bool,
            role: ScalarRole::Plain,
        },
        ConstScalar::Int { bits, signed, .. } => ScalarClass {
            repr: ScalarRepr::Int {
                bits: *bits,
                signed: *signed,
            },
            role: ScalarRole::Plain,
        },
        ConstScalar::FixedBytes(bytes) => ScalarClass {
            repr: ScalarRepr::FixedBytes {
                len: bytes.len() as u16,
            },
            role: ScalarRole::Plain,
        },
        ConstScalar::Address { bits, .. } => ScalarClass {
            repr: ScalarRepr::Address { bits: *bits },
            role: ScalarRole::Plain,
        },
    }
}

pub(crate) fn enum_tag_class<'db>(
    enum_layout: LayoutId<'db>,
    program: &impl RuntimeProgramView<'db>,
) -> ScalarClass<'db> {
    let Layout::Enum(layout) = program.layout(enum_layout) else {
        unreachable!();
    };
    layout.tag
}

pub(crate) fn enum_tag_class_from_value<'db>(
    db: &'db dyn MirDb,
    body: &RuntimeBody<'db>,
    value: crate::runtime::RValueId,
) -> Result<RuntimeClass<'db>, VerifyError<'db>> {
    let class = runtime_value_class(body, value)?.clone();
    let Some(enum_layout) = class.aggregate_layout() else {
        return Err(VerifyError::InvalidPlace(class));
    };
    Ok(RuntimeClass::Scalar(ScalarClass {
        repr: match enum_layout.data(db) {
            Layout::Enum(layout) => layout.tag.repr,
            Layout::Struct(_) | Layout::Array(_) => {
                return Err(VerifyError::InvalidEnumTag(enum_layout));
            }
        },
        role: ScalarRole::EnumTag { enum_layout },
    }))
}

pub(crate) fn verify_enum_handle<'db>(
    body: &RuntimeBody<'db>,
    root: crate::runtime::RValueId,
    variant: VariantId<'db>,
    program: &impl RuntimeProgramView<'db>,
) -> Result<RuntimeClass<'db>, VerifyError<'db>> {
    let class = runtime_value_class(body, root)?.clone();
    let (layout, result) = match class {
        RuntimeClass::Ref {
            pointee,
            kind,
            view: RefView::Whole,
        } => {
            let Some(layout) = pointee.aggregate_layout() else {
                return Err(VerifyError::InvalidVariantPlace(RuntimeClass::Ref {
                    pointee,
                    kind,
                    view: RefView::Whole,
                }));
            };
            (
                layout,
                RuntimeClass::Ref {
                    pointee,
                    kind,
                    view: RefView::Whole,
                },
            )
        }
        class => return Err(VerifyError::InvalidVariantPlace(class)),
    };
    if layout != variant.enum_layout || !matches!(program.layout(layout), Layout::Enum(_)) {
        return Err(VerifyError::InvalidVariant(layout, variant.index));
    }
    Ok(result)
}

pub(crate) fn verify_enum_write_variant<'db>(
    program: &impl RuntimeProgramView<'db>,
    body: &RuntimeBody<'db>,
    root: crate::runtime::RValueId,
    variant: VariantId<'db>,
    fields: &[crate::runtime::RValueId],
) -> Result<(), VerifyError<'db>> {
    let RuntimeClass::Ref { pointee, .. } = verify_enum_handle(body, root, variant, program)?
    else {
        unreachable!();
    };
    let RuntimeClass::AggregateValue { layout } = *pointee else {
        unreachable!();
    };
    let Layout::Enum(enum_layout) = program.layout(layout) else {
        return Err(VerifyError::InvalidEnumTag(layout));
    };
    let Some(variant_layout) = enum_layout.variants.get(variant.index as usize) else {
        return Err(VerifyError::InvalidVariant(layout, variant.index));
    };
    if variant_layout.fields.len() != fields.len() {
        return Err(VerifyError::InvalidVariant(layout, variant.index));
    }
    for (field, expected) in fields.iter().zip(variant_layout.fields.iter()) {
        if runtime_value_class(body, *field)? != expected {
            return Err(VerifyError::InvalidVariant(layout, variant.index));
        }
    }
    Ok(())
}

pub(crate) fn verify_value_enum_variant<'db>(
    program: &impl RuntimeProgramView<'db>,
    body: &RuntimeBody<'db>,
    value_class: RuntimeClass<'db>,
    variant: VariantId<'db>,
    fields: &[crate::runtime::RValueId],
) -> Result<(), VerifyError<'db>> {
    let variant_layout = verify_value_enum_variant_ref(program, value_class, variant)?;
    if variant_layout.fields.len() != fields.len() {
        return Err(VerifyError::InvalidVariant(
            variant.enum_layout,
            variant.index,
        ));
    }
    for (field, expected) in fields.iter().zip(variant_layout.fields.iter()) {
        if runtime_value_class(body, *field)? != expected {
            return Err(VerifyError::InvalidVariant(
                variant.enum_layout,
                variant.index,
            ));
        }
    }
    Ok(())
}

pub(crate) fn verify_value_enum_variant_ref<'db>(
    program: &impl RuntimeProgramView<'db>,
    value_class: RuntimeClass<'db>,
    variant: VariantId<'db>,
) -> Result<crate::runtime::EnumVariantLayout<'db>, VerifyError<'db>> {
    let RuntimeClass::AggregateValue { layout } = value_class else {
        return Err(VerifyError::InvalidVariantPlace(value_class));
    };
    if layout != variant.enum_layout {
        return Err(VerifyError::InvalidVariant(layout, variant.index));
    }
    let Layout::Enum(enum_layout) = program.layout(layout) else {
        return Err(VerifyError::InvalidEnumTag(layout));
    };
    enum_layout
        .variants
        .get(variant.index as usize)
        .cloned()
        .ok_or(VerifyError::InvalidVariant(layout, variant.index))
}

pub(crate) fn enum_extract_class<'db>(
    db: &'db dyn MirDb,
    body: &RuntimeBody<'db>,
    value: crate::runtime::RValueId,
    variant: VariantId<'db>,
    field: FieldIndex,
) -> Result<RuntimeClass<'db>, VerifyError<'db>> {
    let RuntimeClass::AggregateValue { layout } = runtime_value_class(body, value)?.clone() else {
        return Err(VerifyError::InvalidVariantPlace(
            runtime_value_class(body, value)?.clone(),
        ));
    };
    if layout != variant.enum_layout {
        return Err(VerifyError::InvalidVariant(layout, variant.index));
    }
    let enum_layout = variant
        .layout(db)
        .ok_or(VerifyError::InvalidVariant(layout, variant.index))?;
    enum_layout
        .variants
        .get(variant.index as usize)
        .and_then(|variant| variant.fields.get(field.0 as usize))
        .cloned()
        .ok_or(VerifyError::InvalidVariant(layout, variant.index))
}

fn project_field<'db>(
    program: &impl RuntimeProgramView<'db>,
    current: RuntimeClass<'db>,
    field: FieldIndex,
) -> Result<RuntimeClass<'db>, VerifyError<'db>> {
    let layout_id =
        layout_for_projection(current.clone()).ok_or(VerifyError::InvalidPlace(current))?;
    match program.layout(layout_id) {
        Layout::Struct(layout) => {
            layout
                .fields
                .get(field.0 as usize)
                .cloned()
                .ok_or(VerifyError::InvalidPlace(RuntimeClass::AggregateValue {
                    layout: layout_id,
                }))
        }
        Layout::Array(_) | Layout::Enum(_) => {
            Err(VerifyError::InvalidPlace(RuntimeClass::AggregateValue {
                layout: layout_id,
            }))
        }
    }
}

fn project_index<'db>(
    program: &impl RuntimeProgramView<'db>,
    current: RuntimeClass<'db>,
) -> Result<RuntimeClass<'db>, VerifyError<'db>> {
    let layout =
        layout_for_projection(current.clone()).ok_or(VerifyError::InvalidPlace(current))?;
    match program.layout(layout) {
        Layout::Array(layout) => Ok(layout.elem),
        Layout::Struct(_) | Layout::Enum(_) => {
            Err(VerifyError::InvalidPlace(RuntimeClass::AggregateValue {
                layout,
            }))
        }
    }
}

fn project_variant_field<'db>(
    db: &'db dyn MirDb,
    current: RuntimeClass<'db>,
    variant: VariantId<'db>,
    field: FieldIndex,
) -> Result<RuntimeClass<'db>, VerifyError<'db>> {
    let current_clone = current.clone();
    let Some(layout) = current.aggregate_layout() else {
        return Err(VerifyError::InvalidVariantPlace(current_clone));
    };
    if layout != variant.enum_layout {
        return Err(VerifyError::InvalidVariantPlace(current_clone));
    }
    let enum_layout = variant
        .layout(db)
        .ok_or(VerifyError::InvalidVariant(layout, variant.index))?;
    let variant_layout = enum_layout
        .variants
        .get(variant.index as usize)
        .ok_or(VerifyError::InvalidVariant(layout, variant.index))?;
    variant_layout
        .fields
        .get(field.0 as usize)
        .cloned()
        .ok_or(VerifyError::InvalidVariant(layout, variant.index))
}

fn layout_for_projection<'db>(class: RuntimeClass<'db>) -> Option<LayoutId<'db>> {
    class.aggregate_layout()
}

/// Panicking wrappers over the canonical projections, for lowering-internal
/// callers whose places are already known well-formed.
pub(crate) fn project_field_class<'db>(
    db: &'db dyn MirDb,
    class: RuntimeClass<'db>,
    field: FieldIndex,
) -> RuntimeClass<'db> {
    let program: &dyn MirDb = db;
    project_field(&program, class.clone(), field).unwrap_or_else(|_| {
        match class.aggregate_layout().map(|layout| layout.data(db)) {
            Some(crate::runtime::Layout::Struct(layout)) => panic!(
                "invalid field projection: field={field:?} fields={:?} class={class:?}",
                layout.fields,
            ),
            _ => panic!("invalid field projection class: {class:?}"),
        }
    })
}

pub(crate) fn project_index_class<'db>(
    db: &'db dyn MirDb,
    class: RuntimeClass<'db>,
) -> RuntimeClass<'db> {
    let program: &dyn MirDb = db;
    project_index(&program, class.clone())
        .unwrap_or_else(|_| panic!("invalid index projection class: {class:?}"))
}

pub(crate) fn project_variant_field_class<'db>(
    db: &'db dyn MirDb,
    class: RuntimeClass<'db>,
    variant: VariantId<'db>,
    field: FieldIndex,
) -> RuntimeClass<'db> {
    project_variant_field(db, class.clone(), variant, field)
        .unwrap_or_else(|_| panic!("invalid variant-field projection class: {class:?}"))
}
