use cranelift_entity::EntityRef;

use crate::{
    db::MirDb,
    instance::{RuntimeInstanceKey, RuntimeInstanceSource},
    runtime::{
        RLocalId, RuntimeClass, RuntimeInterfaceSignature, RuntimeParam,
        synthetic::runtime_synthetic_interface_signature,
    },
};

use super::{
    interface::runtime_param_locals,
    returns::{declaration_runtime_return_class, declared_return_class_admits},
    semantic_body::RuntimeSemanticBody,
};

pub(crate) fn runtime_declaration_signature<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
) -> RuntimeInterfaceSignature<'db> {
    let RuntimeInstanceSource::Semantic(_) = key.source(db) else {
        let RuntimeInstanceSource::Synthetic(synthetic) = key.source(db) else {
            unreachable!()
        };
        return runtime_synthetic_interface_signature(synthetic.spec(db).clone());
    };
    semantic_runtime_signature(db, key, declaration_runtime_return_class(db, key))
}

fn semantic_runtime_signature<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
    ret: Option<RuntimeClass<'db>>,
) -> RuntimeInterfaceSignature<'db> {
    let params = key
        .params(db)
        .iter()
        .enumerate()
        .map(|(index, class)| RuntimeParam {
            local: RLocalId::from_u32(index as u32),
            class: class.clone(),
        })
        .collect();
    RuntimeInterfaceSignature { params, ret }
}

/// The body implements its declaration contract. The declaration is computed
/// without the body, so it may be wider than the class the body returns, which
/// return lowering then adapts; it must never be narrower.
pub(crate) fn runtime_body_signature<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
    body: &RuntimeSemanticBody<'db>,
    returned: Option<&RuntimeClass<'db>>,
) -> RuntimeInterfaceSignature<'db> {
    let semantic = key
        .semantic(db)
        .expect("runtime body ABI requires a semantic instance");
    assert_eq!(
        body.owner(),
        semantic,
        "runtime ABI body must belong to its semantic instance"
    );
    let declared = declaration_runtime_return_class(db, key);
    if let Some(returned) = returned {
        let returned_layout = returned.aggregate_layout().map(|layout| layout.data(db));
        let declared_layout = declared
            .as_ref()
            .and_then(RuntimeClass::aggregate_layout)
            .map(|layout| layout.data(db));
        assert!(
            declared.as_ref().is_some_and(|declared| {
                declared_return_class_admits(db, semantic, declared, returned)
            }),
            "admitted runtime body returns must conform to their declaration contract: semantic={:?}, params={:?}, returned={returned:?}, declared={declared:?}, returned_layout={returned_layout:#?}, declared_layout={declared_layout:#?}",
            semantic.key(db),
            key.params(db),
        );
    }
    let mut signature = semantic_runtime_signature(db, key, declared);
    for (param, local) in signature.params.iter_mut().zip(runtime_param_locals(
        db,
        semantic,
        &body.source,
        key.params(db),
    )) {
        param.local = RLocalId::from_u32(local.index() as u32);
    }
    signature
}
