use hir::analysis::semantic::{
    SLocalId,
    normalized::{NEffectArg, NEffectArgValue, NPlaceBase},
    resolved_provider_binding_for_instance_effect,
};
use hir::analysis::ty::AddressSpace;

use crate::db::MirDb;

use super::semantic_body::RuntimeSemanticBody;

fn local_provider_address_space<'db>(
    db: &'db dyn MirDb,
    body: &RuntimeSemanticBody<'db>,
    local: SLocalId,
) -> Option<AddressSpace> {
    let local = body.local(local)?;
    local
        .role
        .root_provider(&body.locals)
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
) -> AddressSpace {
    let provider = arg.provider.or_else(|| match &arg.arg {
        NEffectArgValue::Value(value) => {
            local_provider_address_space(db, body, body.operand_local(*value)?)
        }
        NEffectArgValue::Place(place) => match place.base {
            NPlaceBase::Root(root) => body.normalized.root(root)?.address_space.into(),
            NPlaceBase::CapabilityTarget { carrier } => {
                local_provider_address_space(db, body, body.value_local(carrier)?)
            }
            NPlaceBase::Deref { pointer } => Some(body.normalized.value(pointer)?.ty.as_ptr(db)?.1),
        },
    });
    provider.unwrap_or_else(|| {
        let owner = body.owner().key(db).owner(db);
        let context = match &arg.arg {
            NEffectArgValue::Value(value) => format!("value={:?}", body.normalized.value(value.value)),
            NEffectArgValue::Place(place) => format!("place={place:?}"),
        };
        panic!(
            "effect/provider args must carry an explicit resolved address space before rMIR lowering: owner={owner:?}; arg={arg:?}; {context}",
        )
    })
}
