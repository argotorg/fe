use cranelift_entity::EntityRef;
use hir::analysis::{
    semantic::borrowck::NormalizedSemanticBody,
    ty::{CallableLayoutParamPort, LayoutBundleComponentId, LayoutMapTy},
};

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
    layout_evidence::runtime_layout_map_for_map_ty,
    returns::{declaration_runtime_return_class, runtime_return_class_for_body},
    type_info::RuntimeTypeEnv,
};

#[derive(Clone, Debug)]
pub(crate) struct RuntimeAbiEvidenceParam<'db> {
    pub source: CallableLayoutParamPort,
    pub map_ty: LayoutMapTy<'db>,
    pub param: RuntimeParam<'db>,
}

#[derive(Clone, Debug)]
pub(crate) struct RuntimeAbiEvidenceResult<'db> {
    pub component_id: LayoutBundleComponentId,
    pub map_ty: LayoutMapTy<'db>,
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
    let evidence_param_specs = signature
        .runtime_params()
        .map(|param| (param.source, param.component.map_ty()))
        .collect::<Vec<_>>();
    let evidence_params = evidence_param_specs
        .into_iter()
        .enumerate()
        .map(|(index, (source, map_ty))| {
            let param = RuntimeParam {
                local: RLocalId::from_u32(first_local as u32 + index as u32),
                class: runtime_layout_map_for_map_ty(db, env, &map_ty).class(),
            };
            RuntimeAbiEvidenceParam {
                source,
                map_ty,
                param,
            }
        })
        .collect::<Vec<_>>();

    let evidence = signature
        .runtime_results()
        .map(|result| {
            let map_ty = result.component.map_ty();
            RuntimeAbiEvidenceResult {
                component_id: result.component_id,
                class: runtime_layout_map_for_map_ty(db, env, &map_ty).class(),
                map_ty,
            }
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

pub(crate) fn runtime_body_abi_plan<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
    body: &NormalizedSemanticBody<'db>,
) -> RuntimeAbiPlan<'db> {
    let semantic = key
        .semantic(db)
        .expect("runtime body ABI requires a semantic instance");
    assert_eq!(
        body.owner, semantic,
        "runtime ABI body must belong to its semantic instance"
    );
    let visible = runtime_return_class_for_body(db, key, body);
    let declaration_visible = declaration_runtime_return_class(db, key);
    let body_layout = visible
        .as_ref()
        .and_then(RuntimeClass::aggregate_layout)
        .map(|layout| layout.data(db));
    let declaration_layout = declaration_visible
        .as_ref()
        .and_then(RuntimeClass::aggregate_layout)
        .map(|layout| layout.data(db));
    assert_eq!(
        visible,
        declaration_visible,
        "admitted runtime body return ABI must match its declaration contract: semantic={:?}, params={:?}, body_layout={body_layout:#?}, declaration_layout={declaration_layout:#?}",
        semantic.key(db),
        key.params(db),
    );
    let mut plan = semantic_runtime_abi_plan(db, key, semantic, visible);
    for (param, local) in
        plan.visible_params
            .iter_mut()
            .zip(runtime_param_locals(db, semantic, body, key.params(db)))
    {
        param.local = RLocalId::from_u32(local.index() as u32);
    }
    for (index, evidence) in plan.evidence_params.iter_mut().enumerate() {
        evidence.param.local = RLocalId::from_u32(body.locals.len() as u32 + index as u32);
    }
    plan
}
