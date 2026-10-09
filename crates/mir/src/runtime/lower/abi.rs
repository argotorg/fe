use cranelift_entity::EntityRef;
use hir::analysis::ty::{CallableLayoutParamPort, LayoutBundleComponentId, ty_def::TyId};

use crate::{
    db::MirDb,
    instance::{RuntimeInstanceKey, RuntimeInstanceSource},
    runtime::{
        LayoutId, LayoutKey, RLocalId, RuntimeClass, RuntimeInterfaceSignature, RuntimeParam,
        StructLayout, synthetic::runtime_synthetic_interface_signature,
    },
};

use super::{
    interface::runtime_param_locals,
    layout_evidence::layout_root_scalar_class,
    returns::{declaration_runtime_return_class, declared_return_class_admits},
    semantic_body::RuntimeSemanticBody,
    type_info::RuntimeTypeEnv,
};

#[derive(Clone, Debug)]
pub(crate) struct RuntimeAbiEvidenceParam<'db> {
    pub source: CallableLayoutParamPort,
    /// The layout root's const type.
    pub ty: TyId<'db>,
    pub param: RuntimeParam<'db>,
}

#[derive(Clone, Debug)]
pub(crate) struct RuntimeAbiEvidenceResult<'db> {
    pub component_id: LayoutBundleComponentId,
    /// The layout root's const type.
    pub ty: TyId<'db>,
    pub class: RuntimeClass<'db>,
}

#[derive(Clone, Debug)]
pub(crate) struct RuntimeAbiReturn<'db> {
    pub visible: Option<RuntimeClass<'db>>,
    pub evidence: Vec<RuntimeAbiEvidenceResult<'db>>,
    pub class: Option<RuntimeClass<'db>>,
    pub layout: Option<LayoutId<'db>>,
}

#[derive(Clone, Debug)]
pub(crate) struct RuntimeAbiPlan<'db> {
    pub visible_params: Vec<RuntimeParam<'db>>,
    pub evidence_params: Vec<RuntimeAbiEvidenceParam<'db>>,
    pub returns: RuntimeAbiReturn<'db>,
}

impl<'db> RuntimeAbiPlan<'db> {
    pub fn signature(&self) -> RuntimeInterfaceSignature<'db> {
        RuntimeInterfaceSignature {
            params: self
                .visible_params
                .iter()
                .cloned()
                .chain(self.evidence_params.iter().map(|param| param.param.clone()))
                .collect(),
            ret: self.returns.class.clone(),
        }
    }
}

pub(crate) fn runtime_declaration_abi_plan<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
) -> RuntimeAbiPlan<'db> {
    let RuntimeInstanceSource::Semantic(semantic) = key.source(db) else {
        let RuntimeInstanceSource::Synthetic(synthetic) = key.source(db) else {
            unreachable!()
        };
        let signature = runtime_synthetic_interface_signature(synthetic.spec(db).clone());
        return RuntimeAbiPlan {
            visible_params: signature.params,
            evidence_params: Vec::new(),
            returns: RuntimeAbiReturn {
                visible: signature.ret.clone(),
                evidence: Vec::new(),
                class: signature.ret,
                layout: None,
            },
        };
    };

    let visible = declaration_runtime_return_class(db, key);
    semantic_runtime_abi_plan(db, key, semantic, visible)
}

fn semantic_runtime_abi_plan<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
    semantic: hir::analysis::semantic::SemanticInstance<'db>,
    visible: Option<RuntimeClass<'db>>,
) -> RuntimeAbiPlan<'db> {
    let visible_params = key
        .params(db)
        .iter()
        .enumerate()
        .map(|(index, class)| RuntimeParam {
            local: RLocalId::from_u32(index as u32),
            class: class.clone(),
        })
        .collect::<Vec<_>>();
    let signature = semantic.key(db).layout_bundle_signature(db);
    let env = RuntimeTypeEnv::for_semantic(db, semantic);
    let first_local = visible_params.len();
    let evidence_params = signature
        .runtime_params()
        .enumerate()
        .map(|(index, param)| RuntimeAbiEvidenceParam {
            source: param.source,
            ty: param.component.ty,
            param: RuntimeParam {
                local: RLocalId::from_u32(first_local as u32 + index as u32),
                class: RuntimeClass::Scalar(layout_root_scalar_class(db, env, param.component.ty)),
            },
        })
        .collect::<Vec<_>>();

    let evidence = signature
        .runtime_results()
        .map(|result| RuntimeAbiEvidenceResult {
            component_id: result.component_id,
            ty: result.component.ty,
            class: RuntimeClass::Scalar(layout_root_scalar_class(db, env, result.component.ty)),
        })
        .collect::<Vec<_>>();
    let (class, layout) = if evidence.is_empty() {
        (visible.clone(), None)
    } else {
        let fields = visible
            .iter()
            .cloned()
            .chain(evidence.iter().map(|result| result.class.clone()))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let layout = LayoutId::new(db, LayoutKey::Struct(StructLayout { fields }));
        (Some(RuntimeClass::AggregateValue { layout }), Some(layout))
    };

    RuntimeAbiPlan {
        visible_params,
        evidence_params,
        returns: RuntimeAbiReturn {
            visible,
            evidence,
            class,
            layout,
        },
    }
}

/// The body implements its declaration contract. The declaration is computed
/// without the body, so it may be wider than the class the body returns, which
/// return lowering then adapts; it must never be narrower.
pub(crate) fn runtime_body_abi_plan<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
    body: &RuntimeSemanticBody<'db>,
    returned: Option<&RuntimeClass<'db>>,
) -> RuntimeAbiPlan<'db> {
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
    let mut plan = semantic_runtime_abi_plan(db, key, semantic, declared);
    for (param, local) in plan.visible_params.iter_mut().zip(runtime_param_locals(
        db,
        semantic,
        &body.source,
        key.params(db),
    )) {
        param.local = RLocalId::from_u32(local.index() as u32);
    }
    for (index, evidence) in plan.evidence_params.iter_mut().enumerate() {
        evidence.param.local = RLocalId::from_u32(body.locals.len() as u32 + index as u32);
    }
    plan
}
