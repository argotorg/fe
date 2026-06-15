use hir::analysis::semantic::SLocalId;
use hir::analysis::semantic::{
    FieldIndex, NEffectArg, NEffectArgValue, NLocalOrigin, NSPlaceRoot, NormalizedSemanticBody,
    resolved_provider_binding_for_instance_effect,
};
use hir::analysis::ty::ProviderAddressSpace;

use crate::{
    db::MirDb,
    runtime::{AddressSpaceKind, RuntimeClass, VariantId},
};

pub(super) fn project_field_class<'db>(
    db: &'db dyn MirDb,
    class: RuntimeClass<'db>,
    field: FieldIndex,
) -> RuntimeClass<'db> {
    let layout = class
        .aggregate_layout()
        .unwrap_or_else(|| panic!("invalid field projection class"));
    match layout.data(db) {
        crate::runtime::Layout::Struct(layout) => layout
            .fields
            .get(field.0 as usize)
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "invalid field projection: field={field:?} source_ty={} fields={:?} class={class:?}",
                    layout.source_ty.pretty_print(db),
                    layout.fields,
                )
            }),
        _ => panic!("invalid field projection layout"),
    }
}

pub(super) fn project_index_class<'db>(
    db: &'db dyn MirDb,
    class: RuntimeClass<'db>,
) -> RuntimeClass<'db> {
    let layout = class
        .aggregate_layout()
        .unwrap_or_else(|| panic!("invalid index projection class"));
    match layout.data(db) {
        crate::runtime::Layout::Array(layout) => layout.elem,
        _ => panic!("invalid index projection layout"),
    }
}

pub(super) fn project_variant_field_class<'db>(
    db: &'db dyn MirDb,
    class: RuntimeClass<'db>,
    variant: VariantId<'db>,
    field: FieldIndex,
) -> RuntimeClass<'db> {
    let layout = class
        .aggregate_layout()
        .unwrap_or_else(|| panic!("invalid variant-field projection class"));
    if layout != variant.enum_layout {
        panic!("invalid variant-field projection class");
    }
    variant.layout(db).expect("variant layout").variants[variant.index as usize].fields
        [field.0 as usize]
        .clone()
}

pub(super) fn address_space_from_provider(provider: ProviderAddressSpace) -> AddressSpaceKind {
    match provider {
        ProviderAddressSpace::Memory => AddressSpaceKind::Memory,
        ProviderAddressSpace::Storage => AddressSpaceKind::Storage,
        ProviderAddressSpace::Transient => AddressSpaceKind::Transient,
        ProviderAddressSpace::Calldata => AddressSpaceKind::Calldata,
        ProviderAddressSpace::Code => AddressSpaceKind::Code,
    }
}

fn local_provider_address_space<'db>(
    db: &'db dyn MirDb,
    body: &NormalizedSemanticBody<'db>,
    local: SLocalId,
) -> Option<ProviderAddressSpace> {
    let local = body.local(local)?;
    match &local.facts.origin {
        NLocalOrigin::RootProvider(provider) => provider.semantics.address_space,
        NLocalOrigin::SelfRooted | NLocalOrigin::AliasedPlace => {
            let binding = local.source?;
            resolved_provider_binding_for_instance_effect(db, body.owner, binding)?
                .semantics
                .address_space
        }
    }
}

pub(super) fn resolved_effect_arg_address_space<'db>(
    db: &'db dyn MirDb,
    body: &NormalizedSemanticBody<'db>,
    arg: &NEffectArg<'db>,
) -> AddressSpaceKind {
    let provider = arg.provider.or_else(|| match &arg.arg {
        NEffectArgValue::Value(value) => local_provider_address_space(db, body, value.local),
        NEffectArgValue::Place(place) => match place.root {
            NSPlaceRoot::Root(root) => match body.root(root)? {
                hir::analysis::semantic::borrowck::NBorrowRoot::Param { local, .. }
                | hir::analysis::semantic::borrowck::NBorrowRoot::LocalSlot { local } => {
                    local_provider_address_space(db, body, *local)
                }
                hir::analysis::semantic::borrowck::NBorrowRoot::Provider { binding } => {
                    binding.semantics.address_space
                }
            },
            NSPlaceRoot::CarrierDerefLocal(local) => local_provider_address_space(db, body, local),
        },
    });
    address_space_from_provider(provider.unwrap_or_else(|| {
        let owner = body.owner.key(db).owner(db);
        let context = match &arg.arg {
            NEffectArgValue::Value(value) => format!("local={:?}", body.local(value.local)),
            NEffectArgValue::Place(place) => match place.root {
                NSPlaceRoot::Root(root) => format!("root={:?}", body.root(root)),
                NSPlaceRoot::CarrierDerefLocal(local) => {
                    format!("carrier_local={:?}", body.local(local))
                }
            },
        };
        panic!(
            "effect/provider args must carry an explicit resolved address space before rMIR lowering: owner={owner:?}; arg={arg:?}; {context}",
        )
    }))
}
