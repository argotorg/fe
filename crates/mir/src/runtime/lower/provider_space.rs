use hir::analysis::semantic::{
    SLocalId,
    normalized::{NEffectArg, NEffectArgValue, NPlaceBase},
    resolved_provider_binding_for_instance_effect,
};
use hir::analysis::ty::ProviderAddressSpace;

use crate::{db::MirDb, runtime::AddressSpaceKind};

use super::semantic_body::RuntimeSemanticBody;

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
    body: &RuntimeSemanticBody<'db>,
    local: SLocalId,
) -> Option<ProviderAddressSpace> {
    let local = body.local(local)?;
    local
        .role
        .root_provider(&body.source.locals)
        .and_then(|provider| provider.semantics.address_space)
        .or_else(|| {
            let binding = local.source?;
            resolved_provider_binding_for_instance_effect(db, body.owner(), binding)?
                .semantics
                .address_space
        })
}

pub(super) fn resolved_effect_arg_address_space<'db>(
    db: &'db dyn MirDb,
    body: &RuntimeSemanticBody<'db>,
    arg: &NEffectArg<'db>,
) -> AddressSpaceKind {
    let provider = arg.provider.or_else(|| match &arg.arg {
        NEffectArgValue::Value(value) => {
            local_provider_address_space(db, body, body.operand_source(*value)?)
        }
        NEffectArgValue::Place(place) => match place.base {
            NPlaceBase::Root(root) => body.normalized.root(root)?.address_space.into(),
            NPlaceBase::CapabilityTarget { carrier } => {
                local_provider_address_space(db, body, body.value_source(carrier)?)
            }
        },
    });
    address_space_from_provider(provider.unwrap_or_else(|| {
        let owner = body.owner().key(db).owner(db);
        let context = match &arg.arg {
            NEffectArgValue::Value(value) => format!("value={:?}", body.normalized.value(value.value)),
            NEffectArgValue::Place(place) => format!("place={place:?}"),
        };
        panic!(
            "effect/provider args must carry an explicit resolved address space before rMIR lowering: owner={owner:?}; arg={arg:?}; {context}",
        )
    }))
}
