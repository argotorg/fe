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
use hir::analysis::ty::AddressSpace;
use hir::projection::IndexSource;

use crate::{
    db::MirDb,
    runtime::{
        ConstScalar, Layout, LayoutId, PlaceElem, PlaceRoot, RLocalId, RefKind, RefView,
        ResolvedPlaceElem, ResolvedPlaceRootKind, ResolvedRuntimePlace, RuntimeBody, RuntimeClass,
        RuntimeLocalRoot, RuntimeMemoryLayout, RuntimeProgramView, RuntimeProviderBinding,
        RuntimeProviderBindingId, ScalarClass, ScalarRepr, ScalarRole, StorageLaneView, VariantId,
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
                    AddressSpace::Memory,
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
                AddressSpace::Memory,
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
                        AddressSpace::Memory,
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
                    .deref_target(db)
                    .ok_or_else(|| VerifyError::InvalidPlace(carrier_class.clone()))?;
                path.push(ResolvedPlaceElem::Deref {
                    carrier_class,
                    class: current.clone(),
                });
            }
        }
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
    let (root_class, root_space, force_raw) =
        runtime_place_address_root(db, body, &resolved, place)?;
    if runtime_place_lane(db, program, body, place)?.is_some() {
        return Err(VerifyError::InvalidPlace(resolved.result_class));
    }
    Ok(ref_class_for_place_result(
        db,
        &root_class,
        &resolved.result_class,
        root_space,
        force_raw,
    ))
}

/// The transport root an address of `place` is taken from: the class of its
/// last dereferenced carrier, or of its root, with its space and whether it
/// is a raw address.
fn runtime_place_address_root<'db>(
    db: &'db dyn MirDb,
    body: &impl PlaceClassEnv<'db>,
    resolved: &ResolvedRuntimePlace<'db>,
    place: &crate::runtime::RuntimePlace<'db>,
) -> Result<(RuntimeClass<'db>, AddressSpace, bool), VerifyError<'db>> {
    let (mut root_class, mut root_space, mut force_raw) =
        runtime_place_transport_root(db, body, place)?;
    for elem in resolved.path.iter() {
        if let ResolvedPlaceElem::Deref { carrier_class, .. } = elem {
            root_class = carrier_class.clone();
            root_space = root_class.address_space().unwrap_or(root_space);
            force_raw = matches!(root_class, RuntimeClass::RawAddr { .. });
        }
    }
    Ok((root_class, root_space, force_raw))
}

/// The lane `place` ends at: a field packed into a storage word, which no
/// reference can point at (see `storage_field_lane`).
pub(crate) fn runtime_place_lane<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    body: &impl PlaceClassEnv<'db>,
    place: &crate::runtime::RuntimePlace<'db>,
) -> Result<Option<StorageLaneView>, VerifyError<'db>> {
    let resolved = resolve_runtime_place(db, program, body, place)?;
    let (Some((PlaceElem::Field(field), parent_path)), Some(ResolvedPlaceElem::Field { .. })) =
        (place.path.split_last(), resolved.path.last())
    else {
        return Ok(None);
    };
    let (_, root_space, _) = runtime_place_address_root(db, body, &resolved, place)?;
    let parent = crate::runtime::RuntimePlace {
        root: place.root.clone(),
        path: parent_path.to_vec().into(),
    };
    let parent_class = resolve_runtime_place(db, program, body, &parent)?.result_class;
    Ok(storage_field_lane(db, &parent_class, *field, root_space))
}

/// The lane field `field` of a value of class `parent` in `space` occupies:
/// a scalar packed into a storage word with other fields, or placed anywhere
/// but at its word's start. A packed scalar alone at the start of its word is
/// the whole word.
pub(crate) fn storage_field_lane<'db>(
    db: &'db dyn MirDb,
    parent: &RuntimeClass<'db>,
    field: FieldIndex,
    space: AddressSpace,
) -> Option<StorageLaneView> {
    if space.is_byte_addressed() {
        return None;
    }
    let placement = RuntimeMemoryLayout::for_space(db, space)
        .field_placement(parent, field)
        .ok()?;
    let lane = placement.lane?;
    (placement.shared || lane.byte_offset != 0).then_some(StorageLaneView {
        byte_offset: lane.byte_offset as u8,
        byte_width: lane.byte_width as u8,
        shared: placement.shared,
    })
}

/// The class of a reference to a place, which is a lane when `lane`: a lane
/// has no address, so a reference to it is a reference to a memory copy of
/// its value, which a write-back stores into the lane. Any other place is
/// referenced at its address (`ref_class_for_place_result`).
pub(crate) fn place_reference_class<'db>(
    db: &'db dyn MirDb,
    root_class: &RuntimeClass<'db>,
    value_class: &RuntimeClass<'db>,
    root_space: AddressSpace,
    force_raw: bool,
    lane: bool,
) -> RuntimeClass<'db> {
    if lane {
        return RuntimeClass::Ref {
            pointee: Box::new(value_class.clone()),
            kind: RefKind::Object,
            view: RefView::Whole,
        };
    }
    ref_class_for_place_result(db, root_class, value_class, root_space, force_raw)
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
    db: &'db dyn MirDb,
    root_class: &RuntimeClass<'db>,
    value_class: &RuntimeClass<'db>,
    root_space: AddressSpace,
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
            RuntimeClass::Scalar(_) | RuntimeClass::AggregateValue { .. } => {
                return RuntimeClass::Ref {
                    pointee: Box::new(value_class.clone()),
                    kind: RefKind::Object,
                    view: RefView::Whole,
                };
            }
            RuntimeClass::RawAddr { .. } => {}
        }
    }
    RuntimeClass::raw_addr(
        db,
        root_class.address_space().unwrap_or(root_space),
        value_class.clone(),
    )
}

fn runtime_place_transport_root<'db>(
    db: &'db dyn MirDb,
    body: &impl PlaceClassEnv<'db>,
    place: &crate::runtime::RuntimePlace<'db>,
) -> Result<(RuntimeClass<'db>, AddressSpace, bool), VerifyError<'db>> {
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
                        AddressSpace::Memory,
                    )));
                }
            },
            AddressSpace::Memory,
            false,
        ),
        PlaceRoot::Ref(value) => {
            let class = body
                .place_value_class(*value)
                .ok_or(VerifyError::ErasedRuntimeValue(*value))?
                .clone();
            // A reference root addresses its referent's space, like a
            // dereferenced carrier: a storage reference projects storage lanes.
            let space = class.address_space().unwrap_or(AddressSpace::Memory);
            (class, space, false)
        }
        PlaceRoot::Provider(binding) => {
            let class = body
                .place_provider_binding(*binding)
                .map(|binding| binding.provider_class.clone())
                .ok_or(VerifyError::InvalidPlace(RuntimeClass::opaque_raw_addr(
                    AddressSpace::Memory,
                )))?;
            (
                class.clone(),
                class.address_space().unwrap_or(AddressSpace::Memory),
                false,
            )
        }
        PlaceRoot::Ptr { space, class, .. } => (
            RuntimeClass::raw_addr(db, *space, class.clone()),
            *space,
            true,
        ),
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
    let RuntimeClass::AggregateValue {
        layout: enum_layout,
    } = class
    else {
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

#[cfg(test)]
mod tests {
    use driver::DriverDataBase;
    use hir::analysis::ty::AddressSpace;

    use super::{
        PlaceClassEnv, ref_class_for_place_result, resolve_runtime_place,
        resolve_runtime_place_address_class,
    };
    use crate::{
        db::MirDb,
        runtime::{
            LayoutId, LayoutKey, PlaceElem, PlaceRoot, RLocalId, RefKind, RefView, RuntimeClass,
            RuntimeLocalRoot, RuntimePlace, RuntimeProviderBinding, RuntimeProviderBindingId,
            ScalarClass, ScalarRepr, ScalarRole, StructLayout,
        },
    };

    struct ReferenceSlot<'db>(RuntimeClass<'db>);

    impl<'db> PlaceClassEnv<'db> for ReferenceSlot<'db> {
        fn place_local_root(&self, _: RLocalId) -> Option<&RuntimeLocalRoot<'db>> {
            None
        }
        fn place_value_class(&self, _: RLocalId) -> Option<&RuntimeClass<'db>> {
            Some(&self.0)
        }
        fn place_provider_binding(
            &self,
            _: RuntimeProviderBindingId,
        ) -> Option<&RuntimeProviderBinding<'db>> {
            None
        }
    }

    #[test]
    fn native_scalar_addresses_preserve_object_layout() {
        let db = DriverDataBase::default();
        let scalar = RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int {
                bits: 8,
                signed: false,
            },
            role: ScalarRole::Plain,
        });
        let object = RuntimeClass::Ref {
            pointee: Box::new(scalar.clone()),
            kind: RefKind::Object,
            view: RefView::Whole,
        };
        assert_eq!(
            ref_class_for_place_result(&db, &scalar, &scalar, AddressSpace::Memory, false),
            object
        );
        let pointer = RuntimeClass::raw_addr(&db, AddressSpace::Memory, scalar.clone());
        assert_eq!(
            ref_class_for_place_result(&db, &pointer, &scalar, AddressSpace::Memory, true),
            pointer
        );
    }

    #[test]
    fn loading_a_reference_slot_preserves_the_stored_reference() {
        let db = DriverDataBase::default();
        let program: &dyn MirDb = &db;
        let layout = LayoutId::new(
            &db,
            LayoutKey::Struct(StructLayout {
                fields: Box::new([]),
                pinned: false,
            }),
        );
        for stored in [
            RuntimeClass::const_ref(layout),
            RuntimeClass::object_ref(layout),
        ] {
            let address = RuntimeClass::Ref {
                pointee: Box::new(stored.clone()),
                kind: RefKind::Object,
                view: RefView::Whole,
            };
            let environment = ReferenceSlot(address.clone());
            let mut place = RuntimePlace {
                root: PlaceRoot::Ref(RLocalId::from_u32(0)),
                path: Box::new([]),
            };
            assert_eq!(
                resolve_runtime_place(&db, &program, &environment, &place)
                    .unwrap()
                    .result_class,
                stored
            );
            assert_eq!(
                resolve_runtime_place_address_class(&db, &program, &environment, &place).unwrap(),
                address
            );
            place.path = Box::new([PlaceElem::Deref]);
            assert_eq!(
                resolve_runtime_place(&db, &program, &environment, &place)
                    .unwrap()
                    .result_class,
                RuntimeClass::AggregateValue { layout }
            );
        }
    }
}
