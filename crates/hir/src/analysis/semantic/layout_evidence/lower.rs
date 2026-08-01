use std::collections::VecDeque;

use cranelift_entity::EntityRef;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        NBorrowRoot, NEffectArg, NEffectArgValue, NExpr, NOperand, NSPlace, NSPlaceRoot,
        NSStmtKind, NSTerminatorKind, NormalizedSemanticBody, SConst, SLocalId, SemConstId,
        SemOrigin, SemanticCalleeRef, SemanticInstance,
        borrowck::normalize_semantic_body_for_layout_evidence, get_or_build_semantic_instance,
        identity_semantic_instance_key,
    },
    ty::{
        CallableLayoutParamPort, CallableLayoutPort, LayoutBundleComponent,
        LayoutBundleComponentId, LayoutBundleComponentKey, LayoutBundleComponentTransport,
        LayoutBundleInterface, LayoutBundleSchema, LayoutBundleViewMapping, LayoutEvidencePath,
        LayoutEvidencePathStep, LayoutMapTy, LayoutPortKey,
        adt_def::instantiate_adt_field_shape,
        const_ty::CallableInputLayoutHoleOrigin,
        provider::{EffectHandleResolution, ProviderLayoutEvidence, resolve_effect_handle},
        ty_check::{EffectArgLayoutView, LocalBinding},
        ty_def::TyId,
        ty_lower::layout_bundle_schema_for_semantic_value,
    },
};
use crate::projection::{IndexSource, Projection};
use crate::semantic::{AssignedRootValue, LayoutProjection, LayoutViewKind, ProviderBinding};

use super::{
    LayoutEvidenceAssignment, LayoutEvidenceBase, LayoutEvidenceBody, LayoutEvidenceCall,
    LayoutEvidenceCallArg, LayoutEvidenceComponentValue, LayoutEvidenceConstBinding,
    LayoutEvidenceConstant, LayoutEvidenceError, LayoutEvidenceExpr, LayoutEvidenceIndex,
    LayoutEvidenceLocal, LayoutEvidenceLocalId, LayoutEvidenceOperand, LayoutEvidenceReturn,
    LayoutEvidenceStatement, LayoutEvidenceTerminator, LayoutEvidenceValue,
    layout_const_param_uses, verify_layout_evidence_body,
};

#[derive(Clone, PartialEq, Eq)]
struct ComponentExpr<'db> {
    expr: LayoutEvidenceExpr<'db>,
    map_ty: LayoutMapTy<'db>,
    port: LayoutPortKey,
}

#[derive(Clone)]
struct LayoutBundleValue<'db> {
    components: Vec<ComponentExpr<'db>>,
    by_port: FxHashMap<LayoutPortKey, usize>,
}

impl<'db> LayoutBundleValue<'db> {
    fn new(components: Vec<ComponentExpr<'db>>) -> Result<Self, LayoutEvidenceError<'db>> {
        let mut by_port = FxHashMap::default();
        for (idx, component) in components.iter().enumerate() {
            if by_port.insert(component.port.clone(), idx).is_some() {
                return Err(LayoutEvidenceError::InvalidPlace);
            }
        }
        Ok(Self {
            components,
            by_port,
        })
    }

    fn component(&self, port: &LayoutPortKey) -> Option<&ComponentExpr<'db>> {
        self.by_port
            .get(port)
            .and_then(|idx| self.components.get(*idx))
    }

    fn len(&self) -> usize {
        self.components.len()
    }
}

impl<'db> IntoIterator for LayoutBundleValue<'db> {
    type Item = ComponentExpr<'db>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.components.into_iter()
    }
}

/// Concrete layout maps keyed by their structural bundle port at an assigned
/// provider boundary.
///
/// The inner value bundle owns the uniqueness index, so entry lowering cannot
/// accidentally pair parallel vectors or rediscover components with ad hoc
/// searches.
pub struct AssignedProviderLayoutEvidence<'db> {
    value: LayoutBundleValue<'db>,
}

impl<'db> AssignedProviderLayoutEvidence<'db> {
    pub fn component(&self, port: &LayoutPortKey) -> Option<&LayoutEvidenceConstant<'db>> {
        match &self.value.component(port)?.expr {
            LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Constant(value)) => Some(value),
            _ => unreachable!("assigned provider evidence must be an affine constant"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ContextStrength {
    /// An existing place can supply a landing when the producer has no
    /// stronger expected layout.
    Fallback,
    /// The enclosing expression's result must carry this exact layout.
    Required,
}

#[derive(Clone, PartialEq, Eq)]
struct ContextualComponentExpr<'db> {
    value: ComponentExpr<'db>,
    strength: ContextStrength,
}

#[derive(Clone)]
struct DeclaredComponentSource<'db> {
    component: LayoutBundleComponent<'db>,
    source: CallableLayoutParamPort,
    value: ComponentExpr<'db>,
}

#[derive(Clone, Default)]
struct EvidenceProjection {
    path: LayoutEvidencePath,
    indices: Vec<LayoutEvidenceIndex>,
}

#[derive(Clone)]
struct LayoutTransferSource {
    local: SLocalId,
    component: LayoutBundleComponentId,
    indices: Vec<LayoutEvidenceIndex>,
}

#[derive(Clone)]
enum LayoutTransferExpr<'db> {
    Source(LayoutTransferSource),
    Fixed(LayoutEvidenceExpr<'db>),
    Ambient {
        local: SLocalId,
        component: LayoutBundleComponentId,
    },
    Array {
        elements: Box<[LayoutTransferSource]>,
    },
    Repeat {
        len: usize,
        element: LayoutTransferSource,
    },
    Zero,
    CallResult {
        component: LayoutBundleComponentId,
    },
    Update {
        base: LayoutTransferSource,
        indices: Box<[LayoutEvidenceIndex]>,
        value: LayoutTransferSource,
    },
}

#[derive(Clone)]
struct LayoutTransferComponent<'db> {
    target_id: LayoutBundleComponentId,
    target: LayoutBundleComponentShape<'db>,
    expr: LayoutTransferExpr<'db>,
}

impl<'db> LayoutTransferComponent<'db> {
    fn new(
        target_id: LayoutBundleComponentId,
        target: &LayoutBundleComponent<'db>,
        expr: LayoutTransferExpr<'db>,
    ) -> Self {
        Self {
            target_id,
            target: LayoutBundleComponentShape {
                map_ty: target.map_ty(),
                port: target.port.clone(),
            },
            expr,
        }
    }
}

#[derive(Clone)]
struct LayoutBundleComponentShape<'db> {
    map_ty: LayoutMapTy<'db>,
    port: LayoutPortKey,
}

impl<'db> LayoutBundleComponentShape<'db> {
    fn map_ty(&self) -> LayoutMapTy<'db> {
        self.map_ty.clone()
    }
}

#[derive(Clone, Default)]
struct LayoutTransferBundle<'db> {
    components: Vec<LayoutTransferComponent<'db>>,
    by_id: FxHashMap<LayoutBundleComponentId, usize>,
    by_port: FxHashMap<LayoutPortKey, usize>,
}

impl<'db> LayoutTransferBundle<'db> {
    fn new(
        components: Vec<LayoutTransferComponent<'db>>,
    ) -> Result<Self, LayoutEvidenceError<'db>> {
        let mut by_id = FxHashMap::default();
        let mut by_port = FxHashMap::default();
        for (idx, component) in components.iter().enumerate() {
            if by_id.insert(component.target_id, idx).is_some()
                || by_port.insert(component.target.port.clone(), idx).is_some()
            {
                return Err(LayoutEvidenceError::InvalidPlace);
            }
        }
        Ok(Self {
            components,
            by_id,
            by_port,
        })
    }

    fn component(&self, id: LayoutBundleComponentId) -> Option<&LayoutTransferComponent<'db>> {
        self.by_id
            .get(&id)
            .and_then(|idx| self.components.get(*idx))
    }

    fn component_by_port(&self, port: &LayoutPortKey) -> Option<&LayoutTransferComponent<'db>> {
        self.by_port
            .get(port)
            .and_then(|idx| self.components.get(*idx))
    }
}

#[derive(Clone)]
struct LayoutTransferCallArg<'db> {
    target: CallableLayoutParamPort,
    value: LayoutTransferComponent<'db>,
}

#[derive(Clone)]
struct LayoutTransferCall<'db> {
    callee: SemanticCalleeRef<'db>,
    args: Vec<LayoutTransferCallArg<'db>>,
}

/// The canonical layout semantics of one normalized statement.
///
/// Construction is the only operation that inspects [`NExpr`]. Context solving
/// traverses this graph backward, and evidence emission evaluates the same graph
/// forward. Keeping both passes over one representation prevents their handling
/// of a semantic operation from drifting apart.
#[derive(Clone)]
enum LayoutTransfer<'db> {
    Assign {
        dst: SLocalId,
        value: LayoutTransferBundle<'db>,
        call: Option<LayoutTransferCall<'db>>,
        const_binding: Option<(SemConstId<'db>, SemOrigin<'db>)>,
    },
    Store {
        dst: Option<SLocalId>,
        value: LayoutTransferBundle<'db>,
        fallback: Option<(SLocalId, LayoutTransferBundle<'db>)>,
    },
}

fn layout_projections<'db>(
    path: &[LayoutEvidencePathStep],
) -> Result<Vec<LayoutProjection>, LayoutEvidenceError<'db>> {
    let mut projections = Vec::new();
    let mut idx = 0;
    while idx < path.len() {
        match path[idx] {
            LayoutEvidencePathStep::Field(field) => {
                projections.push(LayoutProjection::Field(field));
                idx += 1;
            }
            LayoutEvidencePathStep::Variant(variant) => {
                let Some(LayoutEvidencePathStep::Field(field)) = path.get(idx + 1).copied() else {
                    return Err(LayoutEvidenceError::InvalidPlace);
                };
                projections.push(LayoutProjection::VariantField { variant, field });
                idx += 2;
            }
            LayoutEvidencePathStep::Index => {
                projections.push(LayoutProjection::Index(None));
                idx += 1;
            }
            LayoutEvidencePathStep::EffectTarget => {
                projections.push(LayoutProjection::EffectTarget);
                idx += 1;
            }
        }
    }
    Ok(projections)
}

fn assigned_component<'db>(
    value: AssignedRootValue<'db>,
    projection: &EvidenceProjection,
    map_ty: LayoutMapTy<'db>,
    port: LayoutPortKey,
) -> Result<ComponentExpr<'db>, LayoutEvidenceError<'db>> {
    match value {
        AssignedRootValue::Literal { slot, .. } => Ok(ComponentExpr {
            expr: LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Constant(
                LayoutEvidenceConstant {
                    map_ty: map_ty.clone(),
                    base: LayoutEvidenceBase::Slot(slot),
                    strides: vec![0; map_ty.rank()].into_boxed_slice(),
                },
            )),
            map_ty,
            port,
        }),
        AssignedRootValue::Indexed {
            base,
            dimensions,
            strides,
            ..
        } => {
            let consumed = projection.indices.len();
            if consumed + map_ty.rank() != strides.len() || dimensions.len() != strides.len() {
                return Err(LayoutEvidenceError::InvalidPlace);
            }
            let source_ty = LayoutMapTy {
                scalar_ty: map_ty.scalar_ty,
                dimensions: dimensions.iter().map(|dimension| dimension.len).collect(),
            };
            let source = LayoutEvidenceOperand::Constant(LayoutEvidenceConstant {
                map_ty: source_ty,
                base: LayoutEvidenceBase::Slot(base),
                strides: strides.into_boxed_slice(),
            });
            let indices = projection.indices.clone().into_boxed_slice();
            Ok(ComponentExpr {
                expr: if indices.is_empty() {
                    LayoutEvidenceExpr::Use(source)
                } else {
                    LayoutEvidenceExpr::Project { source, indices }
                },
                map_ty,
                port,
            })
        }
    }
}

fn assigned_provider_components<'db>(
    db: &'db dyn HirAnalysisDb,
    provider: &ProviderBinding<'db>,
    projection: &EvidenceProjection,
    expected: &LayoutBundleSchema<'db>,
) -> Result<LayoutBundleValue<'db>, LayoutEvidenceError<'db>> {
    let Some((field, default_view)) = provider.assigned_field_layout(db) else {
        return Err(LayoutEvidenceError::ProviderPlace);
    };
    let base_projections = layout_projections(&projection.path)?;
    let mut values = Vec::with_capacity(expected.components.len());
    for component in &expected.components {
        if let Some(LayoutBundleComponentKey::Static(base)) = &component.representative {
            values.push(ComponentExpr {
                expr: LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Constant(
                    LayoutEvidenceConstant {
                        map_ty: component.map_ty(),
                        base: LayoutEvidenceBase::Root(*base),
                        strides: vec![0; component.rank()].into_boxed_slice(),
                    },
                )),
                map_ty: component.map_ty(),
                port: component.port.clone(),
            });
            continue;
        }
        let (view, component_path) = match component.port.value_path.split_first() {
            Some((LayoutEvidencePathStep::EffectTarget, path)) => (LayoutViewKind::Target, path),
            _ if matches!(
                provider.semantics.evidence,
                ProviderLayoutEvidence::ResolvedHandle(_)
                    | ProviderLayoutEvidence::TraitBoundHandle(_)
            ) =>
            {
                (
                    LayoutViewKind::Declared,
                    component.port.value_path.as_slice(),
                )
            }
            _ => (default_view, component.port.value_path.as_slice()),
        };
        let mut projections = base_projections.clone();
        projections.extend(layout_projections(component_path)?);
        let value = match &component.representative {
            Some(LayoutBundleComponentKey::Root(root)) => field
                .root_value_for_evidence_projections(view, *root, component.ty, &projections)
                .ok()
                .or_else(|| {
                    field.unique_root_value_for_evidence_projections(
                        view,
                        component.ty,
                        &projections,
                    )
                }),
            None
            | Some(LayoutBundleComponentKey::Param(_) | LayoutBundleComponentKey::Static(_)) => {
                field.unique_root_value_for_evidence_projections(view, component.ty, &projections)
            }
        }
        .ok_or(LayoutEvidenceError::ProviderPlace)?;
        let source = assigned_component(
            value,
            projection,
            component.map_ty(),
            component.port.clone(),
        )?;
        if source.map_ty != component.map_ty() {
            return Err(LayoutEvidenceError::ProviderPlace);
        }
        values.push(ComponentExpr {
            expr: source.expr,
            map_ty: source.map_ty,
            port: component.port.clone(),
        });
    }
    LayoutBundleValue::new(values)
}

/// Resolves the concrete layout maps supplied by an assigned provider at a
/// semantic entry boundary.
///
/// Entry providers are whole values, so their evidence is always an affine
/// constant. Dynamic projections are introduced only inside the callee body.
pub fn assigned_provider_layout_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    provider: &ProviderBinding<'db>,
    expected: &LayoutBundleSchema<'db>,
) -> Result<AssignedProviderLayoutEvidence<'db>, LayoutEvidenceError<'db>> {
    let projection = EvidenceProjection {
        path: Vec::new(),
        indices: Vec::new(),
    };
    let value = assigned_provider_components(db, provider, &projection, expected)?;
    if value.components.iter().any(|component| {
        !matches!(
            component.expr,
            LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Constant(_))
        )
    }) {
        return Err(LayoutEvidenceError::ProviderPlace);
    }
    Ok(AssignedProviderLayoutEvidence { value })
}

enum EvidencePlaceRoot<'db> {
    Local(SLocalId),
    Provider(ProviderBinding<'db>),
}

struct LayoutEvidenceBuilder<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    normalized: &'a NormalizedSemanticBody<'db>,
    locals: Vec<LayoutEvidenceLocal<'db>>,
    semantic_values: Vec<LayoutEvidenceValue<'db>>,
    declared_sources: Vec<DeclaredComponentSource<'db>>,
    contextual_sources: Vec<Vec<ContextualComponentExpr<'db>>>,
}

impl<'a, 'db> LayoutEvidenceBuilder<'a, 'db> {
    fn alloc_local(
        &mut self,
        semantic_local: Option<SLocalId>,
        component_id: LayoutBundleComponentId,
        component: &LayoutBundleComponent<'db>,
        param: Option<CallableLayoutParamPort>,
    ) -> LayoutEvidenceLocalId {
        let id = LayoutEvidenceLocalId::from_u32(self.locals.len() as u32);
        self.locals.push(LayoutEvidenceLocal {
            semantic_local,
            component: component_id,
            map_ty: component.map_ty(),
            param,
        });
        id
    }

    fn alloc_value(
        &mut self,
        semantic_local: SLocalId,
        interface: LayoutBundleInterface<'db>,
        input_origin: Option<CallableInputLayoutHoleOrigin>,
    ) -> Result<LayoutEvidenceValue<'db>, LayoutEvidenceError<'db>> {
        let LayoutBundleInterface { schema, transport } = interface;
        let mut components = Vec::with_capacity(schema.components.len());
        for (component_id, component) in schema.indexed_components() {
            let value = match (transport.component(component_id), &component.representative) {
                (
                    Some(LayoutBundleComponentTransport::CompileTime),
                    Some(LayoutBundleComponentKey::Static(base)),
                ) => LayoutEvidenceComponentValue::Known(LayoutEvidenceConstant {
                    map_ty: component.map_ty(),
                    base: LayoutEvidenceBase::Root(*base),
                    strides: vec![0; component.rank()].into_boxed_slice(),
                }),
                (Some(LayoutBundleComponentTransport::Runtime), _) => {
                    LayoutEvidenceComponentValue::Dynamic(self.alloc_local(
                        Some(semantic_local),
                        component_id,
                        component,
                        input_origin.map(|origin| {
                            CallableLayoutParamPort::Input(CallableLayoutPort {
                                origin,
                                component: component.port.clone(),
                            })
                        }),
                    ))
                }
                (
                    Some(LayoutBundleComponentTransport::CompileTime),
                    None
                    | Some(LayoutBundleComponentKey::Root(_))
                    | Some(LayoutBundleComponentKey::Param(_)),
                ) => {
                    unreachable!("compile-time layout component must have a static key")
                }
                (None, _) => return Err(LayoutEvidenceError::InvalidPlace),
            };
            components.push(value);
        }
        Ok(LayoutEvidenceValue {
            schema,
            components: components.into_boxed_slice(),
        })
    }

    fn component_expr(
        &self,
        local: SLocalId,
        component_idx: usize,
    ) -> Result<ComponentExpr<'db>, LayoutEvidenceError<'db>> {
        let value = self
            .semantic_values
            .get(local.index())
            .ok_or(LayoutEvidenceError::InvalidPlace)?;
        let schema = &value.schema.components[component_idx];
        let operand = match &value.components[component_idx] {
            LayoutEvidenceComponentValue::Known(value) => {
                LayoutEvidenceOperand::Constant(value.clone())
            }
            LayoutEvidenceComponentValue::Dynamic(local) => LayoutEvidenceOperand::Local(*local),
        };
        Ok(ComponentExpr {
            expr: LayoutEvidenceExpr::Use(operand),
            map_ty: schema.map_ty(),
            port: schema.port.clone(),
        })
    }

    fn known_component_expr(
        &self,
        local: SLocalId,
        component_idx: usize,
    ) -> Option<ComponentExpr<'db>> {
        match self
            .semantic_values
            .get(local.index())?
            .components
            .get(component_idx)?
        {
            LayoutEvidenceComponentValue::Known(_) => {
                self.component_expr(local, component_idx).ok()
            }
            LayoutEvidenceComponentValue::Dynamic(_) => None,
        }
    }

    fn operand(expr: &LayoutEvidenceExpr<'db>) -> Option<LayoutEvidenceOperand<'db>> {
        match expr {
            LayoutEvidenceExpr::Use(operand) => Some(operand.clone()),
            LayoutEvidenceExpr::Project { .. }
            | LayoutEvidenceExpr::Array { .. }
            | LayoutEvidenceExpr::Repeat { .. }
            | LayoutEvidenceExpr::Update { .. }
            | LayoutEvidenceExpr::CallResult { .. } => None,
        }
    }

    fn retarget_component(
        dst: SLocalId,
        component_id: LayoutBundleComponentId,
        component: &LayoutBundleComponentShape<'db>,
        source: ComponentExpr<'db>,
    ) -> Result<ComponentExpr<'db>, LayoutEvidenceError<'db>> {
        if source.map_ty != component.map_ty() {
            return Err(LayoutEvidenceError::MapTypeMismatch {
                dst,
                component: component_id,
            });
        }
        Ok(ComponentExpr {
            expr: source.expr,
            map_ty: source.map_ty,
            port: component.port.clone(),
        })
    }

    fn projected_component(
        source: ComponentExpr<'db>,
        projection: &EvidenceProjection,
        target_ty: LayoutMapTy<'db>,
        port: LayoutPortKey,
    ) -> Result<ComponentExpr<'db>, LayoutEvidenceError<'db>> {
        let operand = Self::operand(&source.expr).ok_or(LayoutEvidenceError::InvalidPlace)?;
        let Some(projected_ty) = source.map_ty.projected(projection.indices.len()) else {
            return Err(LayoutEvidenceError::InvalidPlace);
        };
        if projected_ty != target_ty {
            return Err(LayoutEvidenceError::InvalidPlace);
        }
        let indices = projection.indices.clone().into_boxed_slice();
        let expr = if indices.is_empty() {
            LayoutEvidenceExpr::Use(operand)
        } else {
            LayoutEvidenceExpr::Project {
                source: operand,
                indices,
            }
        };
        Ok(ComponentExpr {
            expr,
            map_ty: target_ty,
            port,
        })
    }

    fn project_value(
        &self,
        local: SLocalId,
        projection: &EvidenceProjection,
    ) -> Result<LayoutBundleValue<'db>, LayoutEvidenceError<'db>> {
        let value = self
            .semantic_values
            .get(local.index())
            .ok_or(LayoutEvidenceError::InvalidPlace)?;
        let consumed = projection.indices.len();
        let mut projected = Vec::new();
        for (component_idx, component) in value.schema.components.iter().enumerate() {
            let Some(port) = value
                .schema
                .projected_port(&component.port, &projection.path)
            else {
                continue;
            };
            let target_ty = component
                .map_ty()
                .projected(consumed)
                .ok_or(LayoutEvidenceError::InvalidPlace)?;
            projected.push(Self::projected_component(
                self.component_expr(local, component_idx)?,
                projection,
                target_ty,
                port,
            )?);
        }
        LayoutBundleValue::new(projected)
    }

    fn whole_value(
        &self,
        local: SLocalId,
    ) -> Result<LayoutBundleValue<'db>, LayoutEvidenceError<'db>> {
        self.project_value(
            local,
            &EvidenceProjection {
                path: Vec::new(),
                indices: Vec::new(),
            },
        )
    }

    fn transfer_source_for_port(
        &self,
        local: SLocalId,
        port: &LayoutPortKey,
        indices: Vec<LayoutEvidenceIndex>,
        map_ty: &LayoutMapTy<'db>,
    ) -> Result<LayoutTransferSource, LayoutEvidenceError<'db>> {
        let value = self
            .semantic_values
            .get(local.index())
            .ok_or(LayoutEvidenceError::InvalidPlace)?;
        let port = value.schema.canonicalize_port(port);
        let (component, source) = value.schema.component_by_port(&port).ok_or_else(|| {
            LayoutEvidenceError::MissingPort {
                local,
                port: port.clone(),
            }
        })?;
        if source.map_ty().projected(indices.len()).as_ref() != Some(map_ty) {
            return Err(LayoutEvidenceError::MapTypeMismatch {
                dst: local,
                component,
            });
        }
        Ok(LayoutTransferSource {
            local,
            component,
            indices,
        })
    }

    fn projected_transfer_source(
        &self,
        local: SLocalId,
        projection: &EvidenceProjection,
        target: &LayoutBundleComponent<'db>,
    ) -> Result<LayoutTransferSource, LayoutEvidenceError<'db>> {
        let value = self
            .semantic_values
            .get(local.index())
            .ok_or(LayoutEvidenceError::InvalidPlace)?;
        let candidates = value
            .schema
            .indexed_components()
            .filter(|(_, component)| {
                value
                    .schema
                    .projected_port(&component.port, &projection.path)
                    .is_some_and(|port| port == target.port)
                    && component
                        .map_ty()
                        .projected(projection.indices.len())
                        .as_ref()
                        == Some(&target.map_ty())
            })
            .collect::<Vec<_>>();
        let [(source, _)] = candidates.as_slice() else {
            return Err(LayoutEvidenceError::ShapeMismatch {
                dst: local,
                expected: 1,
                actual: candidates.len(),
            });
        };
        Ok(LayoutTransferSource {
            local,
            component: *source,
            indices: projection.indices.clone(),
        })
    }

    fn source_transfer_bundle(
        &self,
        local: SLocalId,
        projection: &EvidenceProjection,
        target: &LayoutBundleSchema<'db>,
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        target
            .indexed_components()
            .map(|(target_id, component)| {
                Ok(LayoutTransferComponent::new(
                    target_id,
                    component,
                    LayoutTransferExpr::Source(
                        self.projected_transfer_source(local, projection, component)?,
                    ),
                ))
            })
            .collect::<Result<Vec<_>, _>>()
            .and_then(LayoutTransferBundle::new)
    }

    fn mapped_transfer_bundle(
        &self,
        local: SLocalId,
        projection: &EvidenceProjection,
        target: &LayoutBundleInterface<'db>,
        mapping: &LayoutBundleViewMapping,
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        let source = &self.semantic_values[local.index()].schema;
        target
            .runtime_components()
            .map(|(target_id, component)| {
                let source_id =
                    mapping
                        .source(target_id)
                        .ok_or(LayoutEvidenceError::MissingComponent {
                            local,
                            component: target_id,
                        })?;
                let source_component =
                    source
                        .component(source_id)
                        .ok_or(LayoutEvidenceError::MissingComponent {
                            local,
                            component: source_id,
                        })?;
                if source
                    .projected_port(&source_component.port, &projection.path)
                    .as_ref()
                    != Some(&component.port)
                    || source_component
                        .map_ty()
                        .projected(projection.indices.len())
                        .as_ref()
                        != Some(&component.map_ty())
                {
                    return Err(LayoutEvidenceError::MapTypeMismatch {
                        dst: local,
                        component: source_id,
                    });
                }
                Ok(LayoutTransferComponent::new(
                    target_id,
                    component,
                    LayoutTransferExpr::Source(LayoutTransferSource {
                        local,
                        component: source_id,
                        indices: projection.indices.clone(),
                    }),
                ))
            })
            .collect::<Result<Vec<_>, _>>()
            .and_then(LayoutTransferBundle::new)
    }

    fn fixed_transfer_bundle(
        &self,
        target: &LayoutBundleSchema<'db>,
        sources: LayoutBundleValue<'db>,
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        if target.components.len() != sources.len() {
            return Err(LayoutEvidenceError::InvalidPlace);
        }
        target
            .indexed_components()
            .map(|(target_id, component)| {
                let source = sources
                    .component(&component.port)
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                if source.map_ty != component.map_ty() {
                    return Err(LayoutEvidenceError::InvalidPlace);
                }
                Ok(LayoutTransferComponent::new(
                    target_id,
                    component,
                    LayoutTransferExpr::Fixed(source.expr.clone()),
                ))
            })
            .collect::<Result<Vec<_>, _>>()
            .and_then(LayoutTransferBundle::new)
    }

    fn place_transfer_bundle(
        &self,
        place: &NSPlace<'db>,
        target: &LayoutBundleSchema<'db>,
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        if target.components.is_empty() {
            return Ok(LayoutTransferBundle::default());
        }
        let (root, projection) = self.place_projection(place)?;
        match root {
            EvidencePlaceRoot::Local(local) => {
                self.source_transfer_bundle(local, &projection, target)
            }
            EvidencePlaceRoot::Provider(provider) => self.fixed_transfer_bundle(
                target,
                self.provider_value(&provider, &projection, target)?,
            ),
        }
    }

    fn ambient_component(
        &self,
        local: SLocalId,
        component: LayoutBundleComponentId,
        target: &LayoutBundleComponentShape<'db>,
    ) -> Result<ComponentExpr<'db>, LayoutEvidenceError<'db>> {
        let value = &self.semantic_values[local.index()];
        let destination = value
            .schema
            .component(component)
            .ok_or(LayoutEvidenceError::MissingComponent { local, component })?;
        if destination.port != target.port || destination.map_ty() != target.map_ty() {
            return Err(LayoutEvidenceError::MapTypeMismatch {
                dst: local,
                component,
            });
        }
        if let Some(source) = self.contextual_component_source(local, destination)? {
            return Self::retarget_component(local, component, target, source);
        }
        let is_runtime = matches!(
            value.components.get(component.index()),
            Some(LayoutEvidenceComponentValue::Dynamic(_))
        );
        let source = match (is_runtime, &destination.representative) {
            (false, Some(LayoutBundleComponentKey::Static(_))) => {
                Self::static_component(destination)
            }
            (true, Some(LayoutBundleComponentKey::Static(_))) => self
                .declared_component_source(local, component, destination)?
                .or_else(|| Self::static_component(destination)),
            (
                true,
                None
                | Some(LayoutBundleComponentKey::Root(_))
                | Some(LayoutBundleComponentKey::Param(_)),
            ) => self.declared_component_source(local, component, destination)?,
            (
                false,
                None
                | Some(LayoutBundleComponentKey::Root(_))
                | Some(LayoutBundleComponentKey::Param(_)),
            ) => unreachable!("compile-time layout component must have a static key"),
        }
        .ok_or(LayoutEvidenceError::MissingComponent { local, component })?;
        Self::retarget_component(local, component, target, source)
    }

    fn transfer_source_component(
        &self,
        source: &LayoutTransferSource,
    ) -> Result<&LayoutBundleComponent<'db>, LayoutEvidenceError<'db>> {
        let component = self.semantic_values[source.local.index()]
            .schema
            .component(source.component)
            .ok_or(LayoutEvidenceError::MissingComponent {
                local: source.local,
                component: source.component,
            })?;
        Ok(component)
    }

    fn evaluate_transfer_source(
        &self,
        source: &LayoutTransferSource,
        map_ty: LayoutMapTy<'db>,
        port: LayoutPortKey,
    ) -> Result<ComponentExpr<'db>, LayoutEvidenceError<'db>> {
        Self::projected_component(
            self.component_expr(source.local, source.component.index())?,
            &EvidenceProjection {
                path: Vec::new(),
                indices: source.indices.clone(),
            },
            map_ty,
            port,
        )
    }

    fn evaluate_transfer_component(
        &self,
        transfer: &LayoutTransferComponent<'db>,
    ) -> Result<ComponentExpr<'db>, LayoutEvidenceError<'db>> {
        let target = &transfer.target;
        match &transfer.expr {
            LayoutTransferExpr::Source(source) => {
                self.evaluate_transfer_source(source, target.map_ty(), target.port.clone())
            }
            LayoutTransferExpr::Fixed(expr) => Ok(ComponentExpr {
                expr: expr.clone(),
                map_ty: target.map_ty(),
                port: target.port.clone(),
            }),
            LayoutTransferExpr::Ambient { local, component } => {
                self.ambient_component(*local, *component, target)
            }
            LayoutTransferExpr::Array { elements } => {
                let child_ty = target
                    .map_ty()
                    .projected(1)
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                let child_port = target
                    .port
                    .projected(&[LayoutEvidencePathStep::Index])
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                let elements = elements
                    .iter()
                    .map(|source| {
                        self.evaluate_transfer_source(source, child_ty.clone(), child_port.clone())
                            .map(|source| source.expr)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(ComponentExpr {
                    expr: LayoutEvidenceExpr::Array {
                        elements: elements.into_boxed_slice(),
                    },
                    map_ty: target.map_ty(),
                    port: target.port.clone(),
                })
            }
            LayoutTransferExpr::Repeat { len, element } => {
                let child_ty = target
                    .map_ty()
                    .projected(1)
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                let child_port = target
                    .port
                    .projected(&[LayoutEvidencePathStep::Index])
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                let element = self.evaluate_transfer_source(element, child_ty, child_port)?;
                Ok(ComponentExpr {
                    expr: LayoutEvidenceExpr::Repeat {
                        len: *len,
                        element: Box::new(element.expr),
                    },
                    map_ty: target.map_ty(),
                    port: target.port.clone(),
                })
            }
            LayoutTransferExpr::Zero => Ok(Self::zero_component(target)),
            LayoutTransferExpr::CallResult { component } => Ok(ComponentExpr {
                expr: LayoutEvidenceExpr::CallResult {
                    component: *component,
                },
                map_ty: target.map_ty(),
                port: target.port.clone(),
            }),
            LayoutTransferExpr::Update {
                base,
                indices,
                value,
            } => {
                let base =
                    self.evaluate_transfer_source(base, target.map_ty(), target.port.clone())?;
                let base = Self::operand(&base.expr).ok_or(LayoutEvidenceError::InvalidPlace)?;
                let value_ty = target
                    .map_ty()
                    .projected(indices.len())
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                let value_port = self.transfer_source_component(value)?.port.clone();
                let value = self.evaluate_transfer_source(value, value_ty, value_port)?;
                Ok(ComponentExpr {
                    expr: if indices.is_empty() {
                        value.expr
                    } else {
                        LayoutEvidenceExpr::Update {
                            source: base,
                            indices: indices.clone(),
                            value: Box::new(value.expr),
                        }
                    },
                    map_ty: target.map_ty(),
                    port: target.port.clone(),
                })
            }
        }
    }

    fn evaluate_transfer_bundle(
        &self,
        bundle: &LayoutTransferBundle<'db>,
    ) -> Result<LayoutBundleValue<'db>, LayoutEvidenceError<'db>> {
        let components = bundle
            .components
            .iter()
            .map(|component| self.evaluate_transfer_component(component))
            .collect::<Result<Vec<_>, _>>()?;
        LayoutBundleValue::new(components)
    }

    fn ambient_transfer_bundle(
        &self,
        local: SLocalId,
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        LayoutTransferBundle::new(
            self.semantic_values[local.index()]
                .schema
                .indexed_components()
                .map(|(target_id, component)| {
                    LayoutTransferComponent::new(
                        target_id,
                        component,
                        LayoutTransferExpr::Ambient {
                            local,
                            component: target_id,
                        },
                    )
                })
                .collect(),
        )
    }

    fn aggregate_transfer_bundle(
        &self,
        dst: SLocalId,
        fields: &[NOperand],
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        let target = &self.semantic_values[dst.index()].schema;
        target
            .indexed_components()
            .map(|(target_id, component)| {
                let expr = if let Some(source) = self.known_component_expr(dst, target_id.index()) {
                    LayoutTransferExpr::Fixed(source.expr)
                } else if let Some(LayoutEvidencePathStep::Field(field_idx)) =
                    component.port.value_path.first()
                {
                    let field = fields
                        .get(*field_idx as usize)
                        .ok_or(LayoutEvidenceError::InvalidPlace)?;
                    let port = component
                        .port
                        .projected(&[LayoutEvidencePathStep::Field(*field_idx)])
                        .ok_or(LayoutEvidenceError::InvalidPlace)?;
                    LayoutTransferExpr::Source(self.transfer_source_for_port(
                        field.local,
                        &port,
                        Vec::new(),
                        &component.map_ty(),
                    )?)
                } else {
                    LayoutTransferExpr::Ambient {
                        local: dst,
                        component: target_id,
                    }
                };
                Ok(LayoutTransferComponent::new(target_id, component, expr))
            })
            .collect::<Result<Vec<_>, _>>()
            .and_then(LayoutTransferBundle::new)
    }

    fn array_transfer_bundle(
        &self,
        dst: SLocalId,
        fields: &[NOperand],
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        let target = &self.semantic_values[dst.index()].schema;
        target
            .indexed_components()
            .map(|(target_id, component)| {
                if component.rank() == 0
                    || component.port.value_path.first() != Some(&LayoutEvidencePathStep::Index)
                {
                    return Err(LayoutEvidenceError::IncompatibleComponent {
                        dst,
                        component: target_id,
                    });
                }
                let port = component
                    .port
                    .projected(&[LayoutEvidencePathStep::Index])
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                let child_ty = component
                    .map_ty()
                    .projected(1)
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                let elements = fields
                    .iter()
                    .map(|field| {
                        self.transfer_source_for_port(field.local, &port, Vec::new(), &child_ty)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(LayoutTransferComponent::new(
                    target_id,
                    component,
                    LayoutTransferExpr::Array {
                        elements: elements.into_boxed_slice(),
                    },
                ))
            })
            .collect::<Result<Vec<_>, _>>()
            .and_then(LayoutTransferBundle::new)
    }

    fn repeat_transfer_bundle(
        &self,
        dst: SLocalId,
        value: SLocalId,
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        let target = &self.semantic_values[dst.index()].schema;
        target
            .indexed_components()
            .map(|(target_id, component)| {
                if component.port.value_path.first() != Some(&LayoutEvidencePathStep::Index) {
                    return Err(LayoutEvidenceError::IncompatibleComponent {
                        dst,
                        component: target_id,
                    });
                }
                let port = component
                    .port
                    .projected(&[LayoutEvidencePathStep::Index])
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                let child_ty = component
                    .map_ty()
                    .projected(1)
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                Ok(LayoutTransferComponent::new(
                    target_id,
                    component,
                    LayoutTransferExpr::Repeat {
                        len: component.dimensions[0],
                        element: self.transfer_source_for_port(
                            value,
                            &port,
                            Vec::new(),
                            &child_ty,
                        )?,
                    },
                ))
            })
            .collect::<Result<Vec<_>, _>>()
            .and_then(LayoutTransferBundle::new)
    }

    fn enum_transfer_bundle(
        &self,
        dst: SLocalId,
        variant: u16,
        fields: &[NOperand],
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        let target = &self.semantic_values[dst.index()].schema;
        target
            .indexed_components()
            .map(|(target_id, component)| {
                let expr = match component.port.value_path.as_slice() {
                    [
                        LayoutEvidencePathStep::Variant(candidate),
                        LayoutEvidencePathStep::Field(field_idx),
                        ..,
                    ] if *candidate == variant => {
                        let field = fields
                            .get(*field_idx as usize)
                            .ok_or(LayoutEvidenceError::InvalidPlace)?;
                        let prefix = [
                            LayoutEvidencePathStep::Variant(*candidate),
                            LayoutEvidencePathStep::Field(*field_idx),
                        ];
                        let port = component
                            .port
                            .projected(&prefix)
                            .ok_or(LayoutEvidenceError::InvalidPlace)?;
                        LayoutTransferExpr::Source(self.transfer_source_for_port(
                            field.local,
                            &port,
                            Vec::new(),
                            &component.map_ty(),
                        )?)
                    }
                    [] => self.known_component_expr(dst, target_id.index()).map_or(
                        LayoutTransferExpr::Ambient {
                            local: dst,
                            component: target_id,
                        },
                        |source| LayoutTransferExpr::Fixed(source.expr),
                    ),
                    _ => LayoutTransferExpr::Zero,
                };
                Ok(LayoutTransferComponent::new(target_id, component, expr))
            })
            .collect::<Result<Vec<_>, _>>()
            .and_then(LayoutTransferBundle::new)
    }

    fn effect_arg_transfer_bundle(
        &self,
        local: SLocalId,
        provider_target_ty: TyId<'db>,
        expected: &LayoutBundleInterface<'db>,
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        let local_data = self
            .normalized
            .local(local)
            .ok_or(LayoutEvidenceError::InvalidPlace)?;
        let source_schema = &self.semantic_values[local.index()].schema;
        let mut path = Vec::new();
        let mut source_ty = local_data
            .ty
            .as_capability(self.db)
            .map_or(local_data.ty, |(_, inner)| inner);
        let provider_target_ty = self
            .normalized
            .owner
            .normalized_ty(self.db, provider_target_ty);
        source_ty = self.normalized.owner.normalized_ty(self.db, source_ty);
        let mut seen = FxHashSet::default();
        while source_ty != provider_target_ty {
            if !seen.insert(source_ty) {
                return Err(LayoutEvidenceError::InvalidPlace);
            }
            let EffectHandleResolution::Resolved {
                target_ty: next_ty, ..
            } = resolve_effect_handle(
                self.db,
                self.normalized.owner.key(self.db).owner(self.db).scope(),
                self.normalized.owner.assumptions(self.db),
                source_ty,
            )
            else {
                return Err(LayoutEvidenceError::InvalidPlace);
            };
            path.push(LayoutEvidencePathStep::EffectTarget);
            source_ty = self.normalized.owner.normalized_ty(self.db, next_ty);
        }
        let mapping = expected
            .runtime_call_mapping(source_schema, &path)
            .ok_or(LayoutEvidenceError::InvalidPlace)?;
        self.mapped_transfer_bundle(
            local,
            &EvidenceProjection {
                path,
                indices: Vec::new(),
            },
            expected,
            &mapping,
        )
    }

    fn call_input_transfer_bundle(
        &self,
        origin: CallableInputLayoutHoleOrigin,
        interface: &LayoutBundleInterface<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        let direct = |local: SLocalId| {
            let projection = EvidenceProjection::default();
            let source = &self.semantic_values[local.index()].schema;
            let mapping = interface
                .runtime_call_mapping(source, &[])
                .ok_or(LayoutEvidenceError::InvalidPlace)?;
            self.mapped_transfer_bundle(local, &projection, interface, &mapping)
        };
        match origin {
            CallableInputLayoutHoleOrigin::Receiver => args
                .first()
                .ok_or(LayoutEvidenceError::InvalidPlace)
                .and_then(|value| direct(value.local)),
            CallableInputLayoutHoleOrigin::ValueParam(idx) => args
                .get(idx)
                .ok_or(LayoutEvidenceError::InvalidPlace)
                .and_then(|value| direct(value.local)),
            CallableInputLayoutHoleOrigin::Effect(idx) => {
                let arg = effect_args
                    .iter()
                    .find(|arg| arg.binding_idx as usize == idx)
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                match &arg.arg {
                    NEffectArgValue::Value(value) => match arg.layout_view {
                        EffectArgLayoutView::Direct => direct(value.local),
                        EffectArgLayoutView::ProviderTarget => self.effect_arg_transfer_bundle(
                            value.local,
                            arg.provider_target_ty
                                .ok_or(LayoutEvidenceError::InvalidPlace)?,
                            interface,
                        ),
                    },
                    NEffectArgValue::Place(place) => LayoutTransferBundle::new(
                        self.place_transfer_bundle(place, &interface.schema)?
                            .components
                            .into_iter()
                            .filter(|component| interface.is_runtime(component.target_id))
                            .collect(),
                    ),
                }
            }
        }
    }

    fn call_result_transfer_bundle(
        &self,
        dst: SLocalId,
        output: &LayoutBundleInterface<'db>,
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        let value = &self.semantic_values[dst.index()];
        if value.schema.components.len() != output.schema.components.len() {
            return Err(LayoutEvidenceError::ShapeMismatch {
                dst,
                expected: value.schema.components.len(),
                actual: output.schema.components.len(),
            });
        }
        value
            .schema
            .indexed_components()
            .map(|(target_id, target)| {
                let (output_id, output_component) =
                    output.schema.component_by_port(&target.port).ok_or(
                        LayoutEvidenceError::MissingComponent {
                            local: dst,
                            component: target_id,
                        },
                    )?;
                if output_component.map_ty() != target.map_ty() {
                    return Err(LayoutEvidenceError::MapTypeMismatch {
                        dst,
                        component: output_id,
                    });
                }
                let expr = if output.is_runtime(output_id) {
                    LayoutTransferExpr::CallResult {
                        component: output_id,
                    }
                } else {
                    LayoutTransferExpr::Fixed(
                        Self::static_component(output_component)
                            .expect("compile-time layout output must have a static key")
                            .expr,
                    )
                };
                Ok(LayoutTransferComponent::new(target_id, target, expr))
            })
            .collect::<Result<Vec<_>, _>>()
            .and_then(LayoutTransferBundle::new)
    }

    fn call_transfer(
        &self,
        dst: SLocalId,
        callee: SemanticCalleeRef<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
    ) -> Result<
        (LayoutTransferBundle<'db>, Option<LayoutTransferCall<'db>>),
        LayoutEvidenceError<'db>,
    > {
        let signature = callee.key.layout_bundle_signature(self.db);
        let mut inputs = FxHashMap::default();
        for input in &signature.inputs {
            let bundle =
                self.call_input_transfer_bundle(input.origin, &input.interface, args, effect_args)?;
            if inputs.insert(input.origin, bundle).is_some() {
                return Err(LayoutEvidenceError::DuplicateInput(input.origin));
            }
        }
        let output_witnesses = LayoutTransferBundle::new(
            signature
                .output_witnesses
                .runtime_components()
                .map(|(target_id, component)| {
                    let (destination_id, destination) = self.semantic_values[dst.index()]
                        .schema
                        .component_by_port(&component.port)
                        .ok_or(LayoutEvidenceError::MissingComponent {
                            local: dst,
                            component: target_id,
                        })?;
                    if destination.map_ty() != component.map_ty() {
                        return Err(LayoutEvidenceError::MapTypeMismatch {
                            dst,
                            component: target_id,
                        });
                    }
                    Ok(LayoutTransferComponent::new(
                        target_id,
                        component,
                        LayoutTransferExpr::Ambient {
                            local: dst,
                            component: destination_id,
                        },
                    ))
                })
                .collect::<Result<Vec<_>, _>>()?,
        )?;
        let call_args = signature
            .runtime_params()
            .map(|param| {
                let value = match &param.source {
                    CallableLayoutParamPort::Input(port) => inputs
                        .get(&port.origin)
                        .and_then(|bundle| bundle.component(param.component_id)),
                    CallableLayoutParamPort::OutputWitness(_) => {
                        output_witnesses.component(param.component_id)
                    }
                }
                .filter(|value| {
                    value.target.port == param.component.port
                        && value.target.map_ty() == param.component.map_ty()
                })
                .cloned()
                .ok_or(LayoutEvidenceError::MissingComponent {
                    local: dst,
                    component: param.component_id,
                })?;
                Ok(LayoutTransferCallArg {
                    target: param.source,
                    value,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let value = self.call_result_transfer_bundle(dst, &signature.output)?;
        Ok((
            value,
            signature
                .has_runtime_evidence()
                .then_some(LayoutTransferCall {
                    callee,
                    args: call_args,
                }),
        ))
    }

    fn store_transfer_bundle(
        &self,
        dst: SLocalId,
        projection: &EvidenceProjection,
        src: SLocalId,
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        let target = &self.semantic_values[dst.index()].schema;
        if projection.path.is_empty() {
            return self.source_transfer_bundle(src, &EvidenceProjection::default(), target);
        }
        target
            .indexed_components()
            .filter_map(|(target_id, component)| {
                let port = target.projected_port(&component.port, &projection.path)?;
                Some((target_id, component, port))
            })
            .map(|(target_id, component, port)| {
                let value_ty = component
                    .map_ty()
                    .projected(projection.indices.len())
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                Ok(LayoutTransferComponent::new(
                    target_id,
                    component,
                    LayoutTransferExpr::Update {
                        base: self.transfer_source_for_port(
                            dst,
                            &component.port,
                            Vec::new(),
                            &component.map_ty(),
                        )?,
                        indices: projection.indices.clone().into_boxed_slice(),
                        value: self.transfer_source_for_port(src, &port, Vec::new(), &value_ty)?,
                    },
                ))
            })
            .collect::<Result<Vec<_>, _>>()
            .and_then(LayoutTransferBundle::new)
    }

    fn build_layout_transfer(
        &self,
        statement: &crate::analysis::semantic::NSStmt<'db>,
    ) -> Result<LayoutTransfer<'db>, LayoutEvidenceError<'db>> {
        match &statement.kind {
            NSStmtKind::Assign { dst, expr } => {
                let mut const_value = None;
                let (value, call) = match expr {
                    NExpr::Use(value) => (
                        self.source_transfer_bundle(
                            value.local,
                            &EvidenceProjection::default(),
                            &self.semantic_values[dst.index()].schema,
                        )?,
                        None,
                    ),
                    NExpr::ReadPlace { place, .. } | NExpr::Borrow { place, .. } => (
                        self.place_transfer_bundle(
                            place,
                            &self.semantic_values[dst.index()].schema,
                        )?,
                        None,
                    ),
                    NExpr::ExtractEnumField {
                        value,
                        variant,
                        field,
                    } => (
                        self.source_transfer_bundle(
                            value.local,
                            &EvidenceProjection {
                                path: vec![
                                    LayoutEvidencePathStep::Variant(variant.0),
                                    LayoutEvidencePathStep::Field(field.0),
                                ],
                                indices: Vec::new(),
                            },
                            &self.semantic_values[dst.index()].schema,
                        )?,
                        None,
                    ),
                    NExpr::AggregateMake { ty, fields } if ty.is_array(self.db) => {
                        (self.array_transfer_bundle(*dst, fields)?, None)
                    }
                    NExpr::AggregateMake { fields, .. } => {
                        (self.aggregate_transfer_bundle(*dst, fields)?, None)
                    }
                    NExpr::ArrayRepeat { value, .. } => {
                        (self.repeat_transfer_bundle(*dst, value.local)?, None)
                    }
                    NExpr::EnumMake {
                        variant, fields, ..
                    } => (self.enum_transfer_bundle(*dst, variant.0, fields)?, None),
                    NExpr::Const(SConst::Value(value)) => {
                        const_value = Some(*value);
                        (self.ambient_transfer_bundle(*dst)?, None)
                    }
                    NExpr::Const(SConst::Ref(_)) => (self.ambient_transfer_bundle(*dst)?, None),
                    NExpr::CodeRegionRef { .. }
                    | NExpr::Unary { .. }
                    | NExpr::Binary { .. }
                    | NExpr::Cast { .. }
                    | NExpr::GetEnumTag { .. }
                    | NExpr::IsEnumVariant { .. }
                    | NExpr::CodeRegionOffset { .. }
                    | NExpr::CodeRegionLen { .. } => (LayoutTransferBundle::default(), None),
                    NExpr::Call {
                        callee,
                        args,
                        effect_args,
                        ..
                    } => {
                        let (value, call) = self.call_transfer(*dst, *callee, args, effect_args)?;
                        (value, call)
                    }
                };
                Ok(LayoutTransfer::Assign {
                    dst: *dst,
                    value,
                    call,
                    const_binding: const_value.map(|value| (value, statement.origin)),
                })
            }
            NSStmtKind::Store { dst, src } => {
                let (root, projection) = self.place_projection(dst)?;
                let fallback = (!projection.path.is_empty()
                    || matches!(&root, EvidencePlaceRoot::Provider(_)))
                .then(|| {
                    self.place_transfer_bundle(dst, &self.semantic_values[src.local.index()].schema)
                        .map(|bundle| (src.local, bundle))
                })
                .transpose()?;
                let (dst, value) = match root {
                    EvidencePlaceRoot::Local(dst) => (
                        Some(dst),
                        self.store_transfer_bundle(dst, &projection, src.local)?,
                    ),
                    EvidencePlaceRoot::Provider(_) => (None, LayoutTransferBundle::default()),
                };
                Ok(LayoutTransfer::Store {
                    dst,
                    value,
                    fallback,
                })
            }
        }
    }

    fn index_declared_sources(&mut self) -> Result<(), LayoutEvidenceError<'db>> {
        let mut sources = Vec::new();
        for (local_idx, local) in self.normalized.locals.iter().enumerate() {
            let Some(origin) = local
                .source
                .and_then(|source| source.callable_input_origin(self.db))
            else {
                continue;
            };
            let local = SLocalId::from_u32(local_idx as u32);
            let value = &self.semantic_values[local_idx];
            for (component_idx, component) in value.schema.components.iter().enumerate() {
                let value = self.component_expr(local, component_idx)?;
                sources.push(DeclaredComponentSource {
                    component: component.clone(),
                    source: CallableLayoutParamPort::Input(CallableLayoutPort {
                        origin,
                        component: component.port.clone(),
                    }),
                    value,
                });
            }
        }
        self.declared_sources = sources;
        Ok(())
    }

    fn declared_component_source(
        &self,
        local: SLocalId,
        component_id: LayoutBundleComponentId,
        component: &LayoutBundleComponent<'db>,
    ) -> Result<Option<ComponentExpr<'db>>, LayoutEvidenceError<'db>> {
        let compatible = self
            .declared_sources
            .iter()
            .filter(|source| component.map_ty() == source.component.map_ty())
            .collect::<Vec<_>>();
        let exact = compatible
            .iter()
            .copied()
            .filter(|source| {
                component.port == source.component.port
                    && component.formally_derivable_from(&source.component)
            })
            .collect::<Vec<_>>();
        let selected = if exact.is_empty() {
            compatible
                .into_iter()
                .filter(|source| component.formally_derivable_from(&source.component))
                .collect()
        } else {
            exact
        };
        match selected.as_slice() {
            [] => Ok(None),
            [source] => Ok(Some(source.value.clone())),
            sources => Err(LayoutEvidenceError::AmbiguousComponentBinding {
                local,
                component: component_id,
                sources: sources
                    .iter()
                    .map(|source| source.source.clone())
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            }),
        }
    }

    fn contextual_component_source(
        &self,
        local: SLocalId,
        component: &LayoutBundleComponent<'db>,
    ) -> Result<Option<ComponentExpr<'db>>, LayoutEvidenceError<'db>> {
        let sources = self
            .contextual_sources
            .get(local.index())
            .ok_or(LayoutEvidenceError::InvalidPlace)?
            .iter()
            .filter(|source| {
                source.value.port == component.port && source.value.map_ty == component.map_ty()
            })
            .collect::<Vec<_>>();
        let required = sources
            .iter()
            .copied()
            .filter(|source| matches!(source.strength, ContextStrength::Required))
            .collect::<Vec<_>>();
        let selected = if required.is_empty() {
            sources
        } else {
            required
        };
        match selected.as_slice() {
            [] => Ok(None),
            [source] => Ok(Some(source.value.clone())),
            [_, _, ..] => Err(LayoutEvidenceError::ConflictingContextualSource {
                local,
                port: component.port.clone(),
            }),
        }
    }

    fn add_contextual_source(
        &mut self,
        local: SLocalId,
        source: ContextualComponentExpr<'db>,
    ) -> Result<bool, LayoutEvidenceError<'db>> {
        let value = self
            .semantic_values
            .get(local.index())
            .ok_or(LayoutEvidenceError::InvalidPlace)?;
        let (component_id, component) = value
            .schema
            .component_by_port(&source.value.port)
            .ok_or_else(|| LayoutEvidenceError::MissingPort {
                local,
                port: source.value.port.clone(),
            })?;
        if component.map_ty() != source.value.map_ty {
            return Err(LayoutEvidenceError::MapTypeMismatch {
                dst: local,
                component: component_id,
            });
        }
        let sources = self
            .contextual_sources
            .get_mut(local.index())
            .ok_or(LayoutEvidenceError::InvalidPlace)?;
        if sources.contains(&source) {
            return Ok(false);
        }
        sources.push(source);
        Ok(true)
    }

    fn enqueue_contextual_source(
        &mut self,
        local: SLocalId,
        source: ContextualComponentExpr<'db>,
        queue: &mut VecDeque<SLocalId>,
        queued: &mut FxHashSet<SLocalId>,
    ) -> Result<(), LayoutEvidenceError<'db>> {
        if self.add_contextual_source(local, source)? && queued.insert(local) {
            queue.push_back(local);
        }
        Ok(())
    }

    fn project_contextual_component(
        source: ComponentExpr<'db>,
        projection: &EvidenceProjection,
        target_ty: LayoutMapTy<'db>,
        port: LayoutPortKey,
    ) -> Result<ComponentExpr<'db>, LayoutEvidenceError<'db>> {
        let projected_ty = source
            .map_ty
            .projected(projection.indices.len())
            .ok_or(LayoutEvidenceError::InvalidPlace)?;
        if projected_ty != target_ty {
            return Err(LayoutEvidenceError::InvalidPlace);
        }
        let indices = projection.indices.clone();
        let expr = if indices.is_empty() {
            source.expr
        } else {
            match source.expr {
                LayoutEvidenceExpr::Use(source) => LayoutEvidenceExpr::Project {
                    source,
                    indices: indices.into_boxed_slice(),
                },
                LayoutEvidenceExpr::Project {
                    source,
                    indices: existing,
                } => {
                    let mut combined = existing.into_vec();
                    combined.extend(indices);
                    LayoutEvidenceExpr::Project {
                        source,
                        indices: combined.into_boxed_slice(),
                    }
                }
                LayoutEvidenceExpr::Array { .. }
                | LayoutEvidenceExpr::Repeat { .. }
                | LayoutEvidenceExpr::Update { .. }
                | LayoutEvidenceExpr::CallResult { .. } => {
                    return Err(LayoutEvidenceError::InvalidPlace);
                }
            }
        };
        Ok(ComponentExpr {
            expr,
            map_ty: target_ty,
            port,
        })
    }

    fn propagate_transfer_source(
        &mut self,
        source: &LayoutTransferSource,
        expected: ContextualComponentExpr<'db>,
        queue: &mut VecDeque<SLocalId>,
        queued: &mut FxHashSet<SLocalId>,
    ) -> Result<(), LayoutEvidenceError<'db>> {
        // A scalar projection cannot define the unobserved members of its
        // source map. Array construction and updates explicitly project their
        // destination context before reaching this leaf.
        if !source.indices.is_empty() {
            return Ok(());
        }
        let component = self.transfer_source_component(source)?;
        let map_ty = component.map_ty();
        let port = component.port.clone();
        if map_ty != expected.value.map_ty {
            return Err(LayoutEvidenceError::MapTypeMismatch {
                dst: source.local,
                component: source.component,
            });
        }
        self.enqueue_contextual_source(
            source.local,
            ContextualComponentExpr {
                value: ComponentExpr {
                    expr: expected.value.expr,
                    map_ty: expected.value.map_ty,
                    port,
                },
                strength: expected.strength,
            },
            queue,
            queued,
        )
    }

    fn propagate_projected_transfer_source(
        &mut self,
        source: &LayoutTransferSource,
        indices: Vec<LayoutEvidenceIndex>,
        expected: ContextualComponentExpr<'db>,
        queue: &mut VecDeque<SLocalId>,
        queued: &mut FxHashSet<SLocalId>,
    ) -> Result<(), LayoutEvidenceError<'db>> {
        let target_ty = expected
            .value
            .map_ty
            .projected(indices.len())
            .ok_or(LayoutEvidenceError::InvalidPlace)?;
        let port = self.transfer_source_component(source)?.port.clone();
        let value = Self::project_contextual_component(
            expected.value,
            &EvidenceProjection {
                path: Vec::new(),
                indices,
            },
            target_ty,
            port,
        )?;
        self.propagate_transfer_source(
            source,
            ContextualComponentExpr {
                value,
                strength: expected.strength,
            },
            queue,
            queued,
        )
    }

    fn propagate_transfer_component(
        &mut self,
        transfer: &LayoutTransferComponent<'db>,
        expected: ContextualComponentExpr<'db>,
        queue: &mut VecDeque<SLocalId>,
        queued: &mut FxHashSet<SLocalId>,
    ) -> Result<(), LayoutEvidenceError<'db>> {
        match &transfer.expr {
            LayoutTransferExpr::Source(source) => {
                self.propagate_transfer_source(source, expected, queue, queued)
            }
            LayoutTransferExpr::Array { elements } => {
                for (index, source) in elements.iter().enumerate() {
                    self.propagate_projected_transfer_source(
                        source,
                        vec![LayoutEvidenceIndex::Constant(index)],
                        expected.clone(),
                        queue,
                        queued,
                    )?;
                }
                Ok(())
            }
            LayoutTransferExpr::Repeat { len: 1, element } => self
                .propagate_projected_transfer_source(
                    element,
                    vec![LayoutEvidenceIndex::Constant(0)],
                    expected,
                    queue,
                    queued,
                ),
            LayoutTransferExpr::Update { indices, value, .. } => self
                .propagate_projected_transfer_source(
                    value,
                    indices.to_vec(),
                    expected,
                    queue,
                    queued,
                ),
            LayoutTransferExpr::Fixed(_)
            | LayoutTransferExpr::Ambient { .. }
            | LayoutTransferExpr::Repeat { .. }
            | LayoutTransferExpr::Zero
            | LayoutTransferExpr::CallResult { .. } => Ok(()),
        }
    }

    fn propagate_transfer_bundle(
        &mut self,
        bundle: &LayoutTransferBundle<'db>,
        expected: &[ContextualComponentExpr<'db>],
        queue: &mut VecDeque<SLocalId>,
        queued: &mut FxHashSet<SLocalId>,
    ) -> Result<(), LayoutEvidenceError<'db>> {
        for expected in expected {
            let Some(transfer) = bundle.component_by_port(&expected.value.port) else {
                continue;
            };
            if transfer.target.map_ty() != expected.value.map_ty {
                return Err(LayoutEvidenceError::InvalidPlace);
            }
            self.propagate_transfer_component(transfer, expected.clone(), queue, queued)?;
        }
        Ok(())
    }

    fn propagate_layout_context(
        &mut self,
        transfers: &[LayoutTransfer<'db>],
    ) -> Result<(), LayoutEvidenceError<'db>> {
        let witnesses = self
            .declared_sources
            .iter()
            .filter(|source| matches!(source.source, CallableLayoutParamPort::OutputWitness(_)))
            .map(|source| source.value.clone())
            .collect::<Vec<_>>();
        let mut sites = vec![Vec::new(); self.normalized.locals.len()];
        let mut store_seeds = Vec::new();
        for transfer in transfers {
            match transfer {
                LayoutTransfer::Assign { dst, value, .. } => {
                    sites[dst.index()].push(value.clone());
                }
                LayoutTransfer::Store {
                    dst,
                    value,
                    fallback,
                } => {
                    if let Some(dst) = dst {
                        sites[dst.index()].push(value.clone());
                    }
                    if let Some(fallback) = fallback {
                        store_seeds.push(fallback.clone());
                    }
                }
            }
        }
        let mut queue = VecDeque::new();
        let mut queued = FxHashSet::default();
        for block in &self.normalized.blocks {
            if let NSTerminatorKind::Return(Some(value)) = block.terminator.kind {
                for witness in &witnesses {
                    self.enqueue_contextual_source(
                        value.local,
                        ContextualComponentExpr {
                            value: witness.clone(),
                            strength: ContextStrength::Required,
                        },
                        &mut queue,
                        &mut queued,
                    )?;
                }
            }
        }
        for (source_local, fallback) in store_seeds {
            for source in self.evaluate_transfer_bundle(&fallback)? {
                self.enqueue_contextual_source(
                    source_local,
                    ContextualComponentExpr {
                        value: source,
                        strength: ContextStrength::Fallback,
                    },
                    &mut queue,
                    &mut queued,
                )?;
            }
        }
        while let Some(local) = queue.pop_front() {
            queued.remove(&local);
            let expected = self.contextual_sources[local.index()].clone();
            for site in &sites[local.index()] {
                self.propagate_transfer_bundle(site, &expected, &mut queue, &mut queued)?;
            }
        }
        Ok(())
    }

    fn const_bindings(
        &self,
        value: SemConstId<'db>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<Box<[LayoutEvidenceConstBinding<'db>]>, LayoutEvidenceError<'db>> {
        layout_const_param_uses(self.db, value)
            .into_iter()
            .filter_map(|param| {
                let candidates = self
                    .declared_sources
                    .iter()
                    .filter(|source| source.component.supplied_const_params.contains(&param))
                    .filter_map(|source| {
                        Some(LayoutEvidenceConstBinding {
                            param,
                            source: source.source.clone(),
                            value: Self::operand(&source.value.expr)?,
                        })
                    })
                    .collect::<Vec<_>>();
                let is_layout_dependency = self
                    .declared_sources
                    .iter()
                    .any(|source| source.component.dependent_const_params.contains(&param));
                match candidates.as_slice() {
                    [binding] => {
                        let map_ty = match &binding.value {
                            LayoutEvidenceOperand::Local(local) => {
                                &self.locals[local.index()].map_ty
                            }
                            LayoutEvidenceOperand::Constant(value) => &value.map_ty,
                        };
                        if map_ty.rank() == 0 {
                            Some(Ok(binding.clone()))
                        } else {
                            Some(Err(LayoutEvidenceError::UnprojectedConstBinding {
                                param,
                                origin,
                                source: binding.source.clone(),
                            }))
                        }
                    }
                    [] if is_layout_dependency => {
                        Some(Err(LayoutEvidenceError::MissingConstBinding {
                            param,
                            origin,
                        }))
                    }
                    [] => None,
                    _ => Some(Err(LayoutEvidenceError::AmbiguousConstBinding {
                        param,
                        origin,
                        sources: candidates
                            .iter()
                            .map(|candidate| candidate.source.clone())
                            .collect::<Vec<_>>()
                            .into_boxed_slice(),
                    })),
                }
            })
            .collect::<Result<Vec<_>, _>>()
            .map(Vec::into_boxed_slice)
    }

    fn zero_component(component: &LayoutBundleComponentShape<'db>) -> ComponentExpr<'db> {
        ComponentExpr {
            expr: LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Constant(
                LayoutEvidenceConstant {
                    map_ty: component.map_ty(),
                    base: LayoutEvidenceBase::Slot(0),
                    strides: vec![0; component.map_ty.rank()].into_boxed_slice(),
                },
            )),
            map_ty: component.map_ty(),
            port: component.port.clone(),
        }
    }

    fn static_component(component: &LayoutBundleComponent<'db>) -> Option<ComponentExpr<'db>> {
        let Some(LayoutBundleComponentKey::Static(base)) = component.representative else {
            return None;
        };
        Some(ComponentExpr {
            expr: LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Constant(
                LayoutEvidenceConstant {
                    map_ty: component.map_ty(),
                    base: LayoutEvidenceBase::Root(base),
                    strides: vec![0; component.rank()].into_boxed_slice(),
                },
            )),
            map_ty: component.map_ty(),
            port: component.port.clone(),
        })
    }

    fn place_projection(
        &self,
        place: &NSPlace<'db>,
    ) -> Result<(EvidencePlaceRoot<'db>, EvidenceProjection), LayoutEvidenceError<'db>> {
        let mut ty = self
            .normalized
            .place_root_ty(&place.root)
            .ok_or(LayoutEvidenceError::InvalidPlace)?;
        let root = match place.root {
            NSPlaceRoot::CarrierDerefLocal(local) => self
                .normalized
                .local(local)
                .and_then(|local| local.facts.origin.root_provider())
                .cloned()
                .map(EvidencePlaceRoot::Provider)
                .unwrap_or(EvidencePlaceRoot::Local(local)),
            NSPlaceRoot::Root(root) => match self.normalized.root(root) {
                Some(NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local }) => {
                    EvidencePlaceRoot::Local(*local)
                }
                Some(NBorrowRoot::Provider { binding, .. }) => {
                    EvidencePlaceRoot::Provider(binding.clone())
                }
                None => return Err(LayoutEvidenceError::InvalidPlace),
            },
        };
        let mut path = Vec::new();
        let mut indices = Vec::new();
        for step in place.path.iter() {
            match step {
                Projection::Field(field) => {
                    path.push(LayoutEvidencePathStep::Field(
                        u16::try_from(*field).map_err(|_| LayoutEvidenceError::InvalidPlace)?,
                    ));
                    ty = if ty.is_tuple(self.db) {
                        ty.field_types(self.db)
                            .get(*field)
                            .copied()
                            .ok_or(LayoutEvidenceError::InvalidPlace)?
                    } else {
                        let adt = ty
                            .adt_def(self.db)
                            .ok_or(LayoutEvidenceError::InvalidPlace)?;
                        instantiate_adt_field_shape(
                            self.db,
                            adt,
                            0,
                            *field,
                            ty.generic_args(self.db),
                        )
                    };
                }
                Projection::VariantField {
                    variant, field_idx, ..
                } => {
                    path.push(LayoutEvidencePathStep::Variant(variant.0));
                    path.push(LayoutEvidencePathStep::Field(
                        u16::try_from(*field_idx).map_err(|_| LayoutEvidenceError::InvalidPlace)?,
                    ));
                    let adt = ty
                        .adt_def(self.db)
                        .ok_or(LayoutEvidenceError::InvalidPlace)?;
                    ty = instantiate_adt_field_shape(
                        self.db,
                        adt,
                        variant.0 as usize,
                        *field_idx,
                        ty.generic_args(self.db),
                    );
                }
                Projection::Index(index) => {
                    path.push(LayoutEvidencePathStep::Index);
                    indices.push(match index {
                        IndexSource::Constant(index) => LayoutEvidenceIndex::Constant(*index),
                        IndexSource::Dynamic(index) => LayoutEvidenceIndex::Dynamic(*index),
                        IndexSource::Any => return Err(LayoutEvidenceError::InvalidPlace),
                    });
                    ty = ty
                        .generic_args(self.db)
                        .first()
                        .copied()
                        .ok_or(LayoutEvidenceError::InvalidPlace)?;
                }
                Projection::Deref => {
                    if let Some((_, inner)) = ty.as_capability(self.db) {
                        ty = inner;
                    } else if let EffectHandleResolution::Resolved { target_ty, .. } =
                        resolve_effect_handle(
                            self.db,
                            self.normalized.owner.key(self.db).owner(self.db).scope(),
                            self.normalized.owner.assumptions(self.db),
                            ty,
                        )
                    {
                        path.push(LayoutEvidencePathStep::EffectTarget);
                        ty = target_ty;
                    }
                }
                Projection::Discriminant => return Err(LayoutEvidenceError::InvalidPlace),
            }
        }
        Ok((root, EvidenceProjection { path, indices }))
    }

    fn provider_value(
        &self,
        provider: &ProviderBinding<'db>,
        projection: &EvidenceProjection,
        expected: &LayoutBundleSchema<'db>,
    ) -> Result<LayoutBundleValue<'db>, LayoutEvidenceError<'db>> {
        if provider.assigned_field_layout(self.db).is_none() {
            let local = self
                .normalized
                .locals
                .iter()
                .position(|local| {
                    local
                        .source
                        .and_then(|source| source.callable_input_origin(self.db))
                        .is_some()
                        && local.facts.origin.root_provider() == Some(provider)
                })
                .map(|local| SLocalId::from_u32(local as u32))
                .ok_or(LayoutEvidenceError::ProviderPlace)?;
            return self.project_value(local, projection);
        }
        assigned_provider_components(self.db, provider, projection, expected)
    }

    fn transfer_assignments(
        &self,
        dst: SLocalId,
        bundle: &LayoutTransferBundle<'db>,
        complete: bool,
    ) -> Result<Box<[LayoutEvidenceAssignment<'db>]>, LayoutEvidenceError<'db>> {
        let destination = &self.semantic_values[dst.index()];
        if complete && destination.schema.components.len() != bundle.components.len() {
            return Err(LayoutEvidenceError::ShapeMismatch {
                dst,
                expected: destination.schema.components.len(),
                actual: bundle.components.len(),
            });
        }
        let values = self.evaluate_transfer_bundle(bundle)?;
        let mut assignments = Vec::new();
        for (transfer, source) in bundle.components.iter().zip(values) {
            let component_idx = transfer.target_id.index();
            let Some(component) = destination.schema.component(transfer.target_id) else {
                return Err(LayoutEvidenceError::MissingComponent {
                    local: dst,
                    component: transfer.target_id,
                });
            };
            if component.port != transfer.target.port
                || component.map_ty() != transfer.target.map_ty()
                || source.map_ty != component.map_ty()
            {
                return Err(LayoutEvidenceError::MapTypeMismatch {
                    dst,
                    component: transfer.target_id,
                });
            }
            if let LayoutEvidenceComponentValue::Dynamic(local) =
                destination.components[component_idx]
            {
                assignments.push(LayoutEvidenceAssignment {
                    dst: local,
                    expr: source.expr,
                });
            }
        }
        Ok(assignments.into_boxed_slice())
    }

    fn lower_transfer_call(
        &self,
        dst: SLocalId,
        value: &LayoutTransferBundle<'db>,
        call: &LayoutTransferCall<'db>,
    ) -> Result<LayoutEvidenceStatement<'db>, LayoutEvidenceError<'db>> {
        let args = call
            .args
            .iter()
            .map(|arg| {
                Ok(LayoutEvidenceCallArg {
                    target: arg.target.clone(),
                    value: self.evaluate_transfer_component(&arg.value)?.expr,
                })
            })
            .collect::<Result<Vec<_>, LayoutEvidenceError<'db>>>()?;
        Ok(LayoutEvidenceStatement {
            assignments: self.transfer_assignments(dst, value, true)?,
            call: Some(LayoutEvidenceCall {
                callee: call.callee,
                args: args.into_boxed_slice(),
            }),
            const_bindings: Box::new([]),
        })
    }

    fn lower_layout_transfer(
        &self,
        transfer: &LayoutTransfer<'db>,
    ) -> Result<LayoutEvidenceStatement<'db>, LayoutEvidenceError<'db>> {
        match transfer {
            LayoutTransfer::Assign {
                dst,
                value,
                call: Some(call),
                ..
            } => self.lower_transfer_call(*dst, value, call),
            LayoutTransfer::Assign {
                dst,
                value,
                call: None,
                const_binding,
            } => Ok(LayoutEvidenceStatement {
                assignments: self.transfer_assignments(*dst, value, true)?,
                call: None,
                const_bindings: match const_binding {
                    Some((value, origin)) => self.const_bindings(*value, *origin)?,
                    None => Box::new([]),
                },
            }),
            LayoutTransfer::Store {
                dst: Some(dst),
                value,
                ..
            } => Ok(LayoutEvidenceStatement {
                assignments: self.transfer_assignments(*dst, value, false)?,
                call: None,
                const_bindings: Box::new([]),
            }),
            LayoutTransfer::Store { dst: None, .. } => Ok(LayoutEvidenceStatement::default()),
        }
    }

    fn return_operands(
        &self,
        value: SLocalId,
        output: &LayoutBundleInterface<'db>,
    ) -> Result<Box<[LayoutEvidenceReturn<'db>]>, LayoutEvidenceError<'db>> {
        let source = self.whole_value(value)?;
        if source.len() != output.schema.components.len() {
            return Err(LayoutEvidenceError::ShapeMismatch {
                dst: value,
                expected: output.schema.components.len(),
                actual: source.len(),
            });
        }
        let mut operands = Vec::new();
        for (component_id, schema) in output.runtime_components() {
            let source =
                source
                    .component(&schema.port)
                    .ok_or(LayoutEvidenceError::MissingComponent {
                        local: value,
                        component: component_id,
                    })?;
            operands.push(LayoutEvidenceReturn {
                component: component_id,
                value: Self::operand(&source.expr).ok_or(LayoutEvidenceError::InvalidPlace)?,
            });
        }
        Ok(operands.into_boxed_slice())
    }
}

#[salsa::tracked(return_ref)]
fn layout_evidence_body_query<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: SemanticInstance<'db>,
) -> Result<LayoutEvidenceBody<'db>, LayoutEvidenceError<'db>> {
    let normalized = normalize_semantic_body_for_layout_evidence(db, owner)
        .map_err(LayoutEvidenceError::Normalize)?;
    let template_owner = normalized.template_owner;
    let body = template_owner.body(db);
    let identity_key = identity_semantic_instance_key(db, template_owner);
    let template_normalized = (owner.key(db) != identity_key)
        .then(|| {
            normalize_semantic_body_for_layout_evidence(
                db,
                get_or_build_semantic_instance(db, identity_key),
            )
            .map_err(LayoutEvidenceError::Normalize)
        })
        .transpose()?;
    if let Some(template) = &template_normalized
        && template.locals.len() != normalized.locals.len()
    {
        return Err(LayoutEvidenceError::TemplateLocalCountMismatch {
            expected: template.locals.len(),
            actual: normalized.locals.len(),
        });
    }
    let signature = owner.key(db).layout_bundle_signature(db);
    signature
        .output
        .validate()
        .map_err(|error| LayoutEvidenceError::InvalidInterface { local: None, error })?;
    signature
        .output_witnesses
        .validate()
        .map_err(|error| LayoutEvidenceError::InvalidInterface { local: None, error })?;
    let mut builder = LayoutEvidenceBuilder {
        db,
        normalized: &normalized,
        locals: Vec::new(),
        semantic_values: Vec::with_capacity(normalized.locals.len()),
        declared_sources: Vec::new(),
        contextual_sources: vec![Vec::new(); normalized.locals.len()],
    };
    let mut input_values = FxHashMap::default();
    for (idx, local) in normalized.locals.iter().enumerate() {
        let semantic_local = SLocalId::from_u32(idx as u32);
        let layout_ty = local.ty;
        let template_ty = template_normalized
            .as_ref()
            .map_or(layout_ty, |template| template.locals[idx].ty);
        let origin = local
            .source
            .and_then(|source| source.callable_input_origin(db));
        let interface = if let Some(interface) = origin
            .and_then(|origin| signature.input(origin))
            .map(|input| input.interface.clone())
        {
            interface
        } else {
            let schema = layout_bundle_schema_for_semantic_value(
                db,
                body.ok_or(LayoutEvidenceError::MissingBody(template_owner))?,
                idx as u32,
                layout_ty,
                template_ty,
            );
            if matches!(local.source, None | Some(LocalBinding::Local { .. })) {
                LayoutBundleInterface::all_runtime(schema)
            } else {
                LayoutBundleInterface::inferred(schema)
            }
        };
        interface
            .validate()
            .map_err(|error| LayoutEvidenceError::InvalidInterface {
                local: Some(semantic_local),
                error,
            })?;
        let value = builder.alloc_value(semantic_local, interface, origin)?;
        if let Some(origin) = origin
            && input_values.insert(origin, semantic_local).is_some()
        {
            return Err(LayoutEvidenceError::DuplicateInput(origin));
        }
        builder.semantic_values.push(value);
    }
    builder.index_declared_sources()?;
    let mut witness_values = FxHashMap::default();
    for (component_id, component) in signature.output_witnesses.runtime_components() {
        let source = CallableLayoutParamPort::OutputWitness(component.port.clone());
        let local = builder.alloc_local(None, component_id, component, Some(source.clone()));
        let value = ComponentExpr {
            expr: LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Local(local)),
            map_ty: component.map_ty(),
            port: component.port.clone(),
        };
        builder.declared_sources.push(DeclaredComponentSource {
            component: component.clone(),
            source,
            value,
        });
        witness_values.insert(
            CallableLayoutParamPort::OutputWitness(component.port.clone()),
            local,
        );
    }
    let params = signature
        .runtime_params()
        .map(|param| match &param.source {
            CallableLayoutParamPort::Input(port) => input_values
                .get(&port.origin)
                .and_then(|local| builder.semantic_values.get(local.index()))
                .and_then(|value| value.components.get(param.component_id.index()))
                .and_then(|value| match value {
                    LayoutEvidenceComponentValue::Dynamic(local) => Some(*local),
                    LayoutEvidenceComponentValue::Known(_) => None,
                })
                .ok_or(LayoutEvidenceError::InvalidPlace),
            CallableLayoutParamPort::OutputWitness(_) => witness_values
                .get(&param.source)
                .copied()
                .ok_or(LayoutEvidenceError::InvalidPlace),
        })
        .collect::<Result<Vec<_>, _>>()?;
    let statement_count = normalized
        .blocks
        .iter()
        .map(|block| block.stmts.len())
        .sum();
    let mut transfers = vec![None; statement_count];
    for statement in normalized.blocks.iter().flat_map(|block| &block.stmts) {
        let slot = transfers
            .get_mut(statement.id.index())
            .ok_or(LayoutEvidenceError::InvalidStatementIdentity(statement.id))?;
        if slot.is_some() {
            return Err(LayoutEvidenceError::InvalidStatementIdentity(statement.id));
        }
        *slot = Some(builder.build_layout_transfer(statement)?);
    }
    let transfers = transfers
        .into_iter()
        .enumerate()
        .map(|(idx, transfer)| {
            transfer.ok_or(LayoutEvidenceError::InvalidStatementIdentity(
                crate::analysis::semantic::SStmtId::from_u32(idx as u32),
            ))
        })
        .collect::<Result<Vec<_>, _>>()?;
    builder.propagate_layout_context(&transfers)?;
    let statements = transfers
        .iter()
        .map(|transfer| builder.lower_layout_transfer(transfer))
        .collect::<Result<Vec<_>, _>>()?;
    let terminators = normalized
        .blocks
        .iter()
        .map(|block| {
            let returns = match block.terminator.kind {
                NSTerminatorKind::Return(Some(value)) => {
                    builder.return_operands(value.local, &signature.output)?
                }
                NSTerminatorKind::Goto(_)
                | NSTerminatorKind::Branch { .. }
                | NSTerminatorKind::MatchEnum { .. }
                | NSTerminatorKind::Assert { .. }
                | NSTerminatorKind::Return(None) => Box::new([]),
            };
            Ok(LayoutEvidenceTerminator { returns })
        })
        .collect::<Result<Vec<_>, LayoutEvidenceError<'db>>>()?;
    let evidence = LayoutEvidenceBody {
        owner,
        template_owner,
        locals: builder.locals,
        semantic_values: builder.semantic_values,
        params,
        output: signature.output,
        statements,
        terminators,
    };
    verify_layout_evidence_body(db, &normalized, &evidence).map_err(LayoutEvidenceError::Verify)?;
    Ok(evidence)
}

pub fn layout_evidence_body<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: SemanticInstance<'db>,
) -> Result<&'db LayoutEvidenceBody<'db>, LayoutEvidenceError<'db>> {
    layout_evidence_body_query(db, owner)
        .as_ref()
        .map_err(Clone::clone)
}
