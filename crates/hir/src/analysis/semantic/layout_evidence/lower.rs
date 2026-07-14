use std::collections::VecDeque;

use cranelift_entity::EntityRef;
use rustc_hash::FxHashSet;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        NBorrowRoot, NEffectArg, NEffectArgValue, NExpr, NOperand, NSPlace, NSPlaceRoot,
        NSStmtKind, NSTerminatorKind, NormalizedSemanticBody, ReadMode, SConst, SLocalId,
        SemConstId, SemOrigin, SemanticCalleeRef, SemanticInstance,
        borrowck::normalize_semantic_body_for_layout_evidence, get_or_build_semantic_instance,
        identity_semantic_instance_key,
    },
    ty::{
        CallableLayoutBundleSignature, CallableLayoutParamPort, CallableLayoutPort,
        LayoutBundleComponent, LayoutBundleComponentId, LayoutBundleComponentKey,
        LayoutBundleSchema, LayoutBundleTransport, LayoutEvidencePath, LayoutEvidencePathStep,
        LayoutMapTy, LayoutPortKey,
        adt_def::instantiate_adt_field_shape,
        const_ty::CallableInputLayoutHoleOrigin,
        provider::{
            EffectHandleTargetResolution, ProviderLayoutEvidence, resolve_effect_handle_target,
        },
        ty_check::LocalBinding,
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
    LayoutEvidenceLocal, LayoutEvidenceLocalId, LayoutEvidenceOperand,
    LayoutEvidenceProjectionTerm, LayoutEvidenceReturn, LayoutEvidenceStatement,
    LayoutEvidenceTerminator, LayoutEvidenceValue, layout_const_param_uses,
    verify_layout_evidence_body,
};

#[derive(Clone, PartialEq, Eq)]
struct ComponentExpr<'db> {
    expr: LayoutEvidenceExpr<'db>,
    map_ty: LayoutMapTy<'db>,
    port: LayoutPortKey,
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
    target: LayoutBundleComponent<'db>,
    expr: LayoutTransferExpr<'db>,
}

#[derive(Clone, Default)]
struct LayoutTransferBundle<'db> {
    components: Vec<LayoutTransferComponent<'db>>,
}

#[derive(Clone)]
struct LayoutTransferInput<'db> {
    origin: CallableInputLayoutHoleOrigin,
    bundle: LayoutTransferBundle<'db>,
}

#[derive(Clone)]
struct LayoutTransferCall<'db> {
    callee: SemanticCalleeRef<'db>,
    inputs: Vec<LayoutTransferInput<'db>>,
    output_witnesses: LayoutTransferBundle<'db>,
    has_runtime_evidence: bool,
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
            let terms = projection
                .indices
                .iter()
                .copied()
                .zip(dimensions.iter().map(|dimension| dimension.len))
                .map(|(index, len)| LayoutEvidenceProjectionTerm { index, len })
                .collect::<Vec<_>>();
            Ok(ComponentExpr {
                expr: if terms.is_empty() {
                    LayoutEvidenceExpr::Use(source)
                } else {
                    LayoutEvidenceExpr::Project {
                        source,
                        terms: terms.into_boxed_slice(),
                    }
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
) -> Result<Vec<ComponentExpr<'db>>, LayoutEvidenceError<'db>> {
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
    Ok(values)
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
) -> Result<Vec<(LayoutPortKey, LayoutEvidenceConstant<'db>)>, LayoutEvidenceError<'db>> {
    let projection = EvidenceProjection {
        path: Vec::new(),
        indices: Vec::new(),
    };
    assigned_provider_components(db, provider, &projection, expected)?
        .into_iter()
        .map(|component| {
            let LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Constant(value)) = component.expr
            else {
                return Err(LayoutEvidenceError::ProviderPlace);
            };
            Ok((component.port, value))
        })
        .collect()
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
        component: &LayoutBundleComponent<'db>,
        param: Option<CallableLayoutParamPort>,
    ) -> LayoutEvidenceLocalId {
        let id = LayoutEvidenceLocalId::from_u32(self.locals.len() as u32);
        self.locals.push(LayoutEvidenceLocal {
            semantic_local,
            component: component.id,
            map_ty: component.map_ty(),
            param,
        });
        id
    }

    fn alloc_value(
        &mut self,
        semantic_local: SLocalId,
        schema: LayoutBundleSchema<'db>,
        input_origin: Option<CallableInputLayoutHoleOrigin>,
    ) -> Result<LayoutEvidenceValue<'db>, LayoutEvidenceError<'db>> {
        let mut components = Vec::with_capacity(schema.components.len());
        for component in &schema.components {
            let value = match (component.is_runtime(), &component.representative) {
                (false, Some(LayoutBundleComponentKey::Static(base))) => {
                    LayoutEvidenceComponentValue::Known(LayoutEvidenceConstant {
                        map_ty: component.map_ty(),
                        base: LayoutEvidenceBase::Root(*base),
                        strides: vec![0; component.rank()].into_boxed_slice(),
                    })
                }
                (true, _) => LayoutEvidenceComponentValue::Dynamic(self.alloc_local(
                    Some(semantic_local),
                    component,
                    input_origin.map(|origin| {
                        CallableLayoutParamPort::Input(CallableLayoutPort {
                            origin,
                            component: component.port.clone(),
                        })
                    }),
                )),
                (
                    false,
                    None
                    | Some(LayoutBundleComponentKey::Root(_))
                    | Some(LayoutBundleComponentKey::Param(_)),
                ) => {
                    unreachable!("compile-time layout component must have a static key")
                }
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
        component: &LayoutBundleComponent<'db>,
        source: ComponentExpr<'db>,
    ) -> Result<ComponentExpr<'db>, LayoutEvidenceError<'db>> {
        if source.map_ty != component.map_ty() {
            return Err(LayoutEvidenceError::MapTypeMismatch {
                dst,
                component: component.id,
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
        let terms = projection
            .indices
            .iter()
            .copied()
            .zip(source.map_ty.dimensions.iter().copied())
            .map(|(index, len)| LayoutEvidenceProjectionTerm { index, len })
            .collect::<Vec<_>>();
        let expr = if terms.is_empty() {
            LayoutEvidenceExpr::Use(operand)
        } else {
            LayoutEvidenceExpr::Project {
                source: operand,
                terms: terms.into_boxed_slice(),
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
    ) -> Result<Vec<ComponentExpr<'db>>, LayoutEvidenceError<'db>> {
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
        Ok(projected)
    }

    fn whole_value(
        &self,
        local: SLocalId,
    ) -> Result<Vec<ComponentExpr<'db>>, LayoutEvidenceError<'db>> {
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
        let source = value
            .schema
            .components
            .iter()
            .find(|component| component.port == port)
            .ok_or_else(|| LayoutEvidenceError::MissingPort {
                local,
                port: port.clone(),
            })?;
        if source.map_ty().projected(indices.len()).as_ref() != Some(map_ty) {
            return Err(LayoutEvidenceError::MapTypeMismatch {
                dst: local,
                component: source.id,
            });
        }
        Ok(LayoutTransferSource {
            local,
            component: source.id,
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
            .components
            .iter()
            .filter(|component| {
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
        let [source] = candidates.as_slice() else {
            return Err(LayoutEvidenceError::ShapeMismatch {
                dst: local,
                expected: 1,
                actual: candidates.len(),
            });
        };
        Ok(LayoutTransferSource {
            local,
            component: source.id,
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
            .components
            .iter()
            .map(|component| {
                Ok(LayoutTransferComponent {
                    target: component.clone(),
                    expr: LayoutTransferExpr::Source(
                        self.projected_transfer_source(local, projection, component)?,
                    ),
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|components| LayoutTransferBundle { components })
    }

    fn fixed_transfer_bundle(
        &self,
        target: &LayoutBundleSchema<'db>,
        sources: Vec<ComponentExpr<'db>>,
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        if target.components.len() != sources.len() {
            return Err(LayoutEvidenceError::InvalidPlace);
        }
        target
            .components
            .iter()
            .map(|component| {
                let source = sources
                    .iter()
                    .find(|source| source.port == component.port)
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                if source.map_ty != component.map_ty() {
                    return Err(LayoutEvidenceError::InvalidPlace);
                }
                Ok(LayoutTransferComponent {
                    target: component.clone(),
                    expr: LayoutTransferExpr::Fixed(source.expr.clone()),
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|components| LayoutTransferBundle { components })
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
        target: &LayoutBundleComponent<'db>,
    ) -> Result<ComponentExpr<'db>, LayoutEvidenceError<'db>> {
        let destination = self.semantic_values[local.index()]
            .schema
            .components
            .get(component.index())
            .ok_or(LayoutEvidenceError::MissingComponent { local, component })?;
        if destination.id != component
            || destination.port != target.port
            || destination.map_ty() != target.map_ty()
        {
            return Err(LayoutEvidenceError::MapTypeMismatch {
                dst: local,
                component,
            });
        }
        if let Some(source) = self.contextual_component_source(local, destination)? {
            return Self::retarget_component(local, target, source);
        }
        let source = match (destination.is_runtime(), &destination.representative) {
            (false, Some(LayoutBundleComponentKey::Static(_))) => {
                Self::static_component(destination)
            }
            (true, Some(LayoutBundleComponentKey::Static(_))) => self
                .declared_component_source(local, destination)?
                .or_else(|| Self::static_component(destination)),
            (
                true,
                None
                | Some(LayoutBundleComponentKey::Root(_))
                | Some(LayoutBundleComponentKey::Param(_)),
            ) => self.declared_component_source(local, destination)?,
            (
                false,
                None
                | Some(LayoutBundleComponentKey::Root(_))
                | Some(LayoutBundleComponentKey::Param(_)),
            ) => unreachable!("compile-time layout component must have a static key"),
        }
        .ok_or(LayoutEvidenceError::MissingComponent { local, component })?;
        Self::retarget_component(local, target, source)
    }

    fn transfer_source_component(
        &self,
        source: &LayoutTransferSource,
    ) -> Result<&LayoutBundleComponent<'db>, LayoutEvidenceError<'db>> {
        let component = self.semantic_values[source.local.index()]
            .schema
            .components
            .get(source.component.index())
            .ok_or(LayoutEvidenceError::MissingComponent {
                local: source.local,
                component: source.component,
            })?;
        if component.id != source.component {
            return Err(LayoutEvidenceError::InvalidPlace);
        }
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
                let terms = indices
                    .iter()
                    .copied()
                    .zip(target.dimensions.iter().copied())
                    .map(|(index, len)| LayoutEvidenceProjectionTerm { index, len })
                    .collect::<Vec<_>>();
                Ok(ComponentExpr {
                    expr: if terms.is_empty() {
                        value.expr
                    } else {
                        LayoutEvidenceExpr::Update {
                            source: base,
                            terms: terms.into_boxed_slice(),
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
    ) -> Result<Vec<ComponentExpr<'db>>, LayoutEvidenceError<'db>> {
        bundle
            .components
            .iter()
            .map(|component| self.evaluate_transfer_component(component))
            .collect()
    }

    fn ambient_transfer_bundle(&self, local: SLocalId) -> LayoutTransferBundle<'db> {
        LayoutTransferBundle {
            components: self.semantic_values[local.index()]
                .schema
                .components
                .iter()
                .map(|component| LayoutTransferComponent {
                    target: component.clone(),
                    expr: LayoutTransferExpr::Ambient {
                        local,
                        component: component.id,
                    },
                })
                .collect(),
        }
    }

    fn aggregate_transfer_bundle(
        &self,
        dst: SLocalId,
        fields: &[NOperand],
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        let target = &self.semantic_values[dst.index()].schema;
        target
            .components
            .iter()
            .enumerate()
            .map(|(component_idx, component)| {
                let expr = if let Some(source) = self.known_component_expr(dst, component_idx) {
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
                        component: component.id,
                    }
                };
                Ok(LayoutTransferComponent {
                    target: component.clone(),
                    expr,
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|components| LayoutTransferBundle { components })
    }

    fn array_transfer_bundle(
        &self,
        dst: SLocalId,
        fields: &[NOperand],
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        let target = &self.semantic_values[dst.index()].schema;
        target
            .components
            .iter()
            .map(|component| {
                if component.rank() == 0
                    || component.port.value_path.first() != Some(&LayoutEvidencePathStep::Index)
                {
                    return Err(LayoutEvidenceError::IncompatibleComponent {
                        dst,
                        component: component.id,
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
                Ok(LayoutTransferComponent {
                    target: component.clone(),
                    expr: LayoutTransferExpr::Array {
                        elements: elements.into_boxed_slice(),
                    },
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|components| LayoutTransferBundle { components })
    }

    fn repeat_transfer_bundle(
        &self,
        dst: SLocalId,
        value: SLocalId,
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        let target = &self.semantic_values[dst.index()].schema;
        target
            .components
            .iter()
            .map(|component| {
                if component.port.value_path.first() != Some(&LayoutEvidencePathStep::Index) {
                    return Err(LayoutEvidenceError::IncompatibleComponent {
                        dst,
                        component: component.id,
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
                Ok(LayoutTransferComponent {
                    target: component.clone(),
                    expr: LayoutTransferExpr::Repeat {
                        len: component.dimensions[0],
                        element: self.transfer_source_for_port(
                            value,
                            &port,
                            Vec::new(),
                            &child_ty,
                        )?,
                    },
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|components| LayoutTransferBundle { components })
    }

    fn enum_transfer_bundle(
        &self,
        dst: SLocalId,
        variant: u16,
        fields: &[NOperand],
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        let target = &self.semantic_values[dst.index()].schema;
        target
            .components
            .iter()
            .enumerate()
            .map(|(component_idx, component)| {
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
                    [] => self.known_component_expr(dst, component_idx).map_or(
                        LayoutTransferExpr::Ambient {
                            local: dst,
                            component: component.id,
                        },
                        |source| LayoutTransferExpr::Fixed(source.expr),
                    ),
                    _ => LayoutTransferExpr::Zero,
                };
                Ok(LayoutTransferComponent {
                    target: component.clone(),
                    expr,
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|components| LayoutTransferBundle { components })
    }

    fn effect_arg_transfer_bundle(
        &self,
        local: SLocalId,
        target_ty: TyId<'db>,
        expected: &LayoutBundleSchema<'db>,
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        let local_data = self
            .normalized
            .local(local)
            .ok_or(LayoutEvidenceError::InvalidPlace)?;
        let source_schema = &self.semantic_values[local.index()].schema;
        let mut path = Vec::new();
        if expected.runtime_components_supplied_by_view(source_schema, &path) {
            return self.source_transfer_bundle(local, &EvidenceProjection::default(), expected);
        }
        let mut source_ty = local_data
            .ty
            .as_capability(self.db)
            .map_or(local_data.ty, |(_, inner)| inner);
        let target_ty = self.normalized.owner.normalized_ty(self.db, target_ty);
        source_ty = self.normalized.owner.normalized_ty(self.db, source_ty);
        let mut seen = FxHashSet::default();
        while source_ty != target_ty {
            if !seen.insert(source_ty) {
                return Err(LayoutEvidenceError::InvalidPlace);
            }
            let EffectHandleTargetResolution::Resolved {
                target_ty: next_ty, ..
            } = resolve_effect_handle_target(
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
            if expected.runtime_components_supplied_by_view(source_schema, &path) {
                return self.source_transfer_bundle(
                    local,
                    &EvidenceProjection {
                        path,
                        indices: Vec::new(),
                    },
                    expected,
                );
            }
        }
        Err(LayoutEvidenceError::InvalidPlace)
    }

    fn call_input_transfer_bundle(
        &self,
        origin: CallableInputLayoutHoleOrigin,
        schema: &LayoutBundleSchema<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        match origin {
            CallableInputLayoutHoleOrigin::Receiver => args
                .first()
                .ok_or(LayoutEvidenceError::InvalidPlace)
                .and_then(|value| {
                    self.source_transfer_bundle(value.local, &EvidenceProjection::default(), schema)
                }),
            CallableInputLayoutHoleOrigin::ValueParam(idx) => args
                .get(idx)
                .ok_or(LayoutEvidenceError::InvalidPlace)
                .and_then(|value| {
                    self.source_transfer_bundle(value.local, &EvidenceProjection::default(), schema)
                }),
            CallableInputLayoutHoleOrigin::Effect(idx) => {
                let arg = effect_args
                    .iter()
                    .find(|arg| arg.binding_idx as usize == idx)
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                match &arg.arg {
                    NEffectArgValue::Value(value) => arg.target_ty.map_or_else(
                        || {
                            self.source_transfer_bundle(
                                value.local,
                                &EvidenceProjection::default(),
                                schema,
                            )
                        },
                        |target_ty| self.effect_arg_transfer_bundle(value.local, target_ty, schema),
                    ),
                    NEffectArgValue::Place(place) => self.place_transfer_bundle(place, schema),
                }
            }
        }
    }

    fn call_result_transfer_bundle(
        &self,
        dst: SLocalId,
        output: &LayoutBundleSchema<'db>,
    ) -> Result<LayoutTransferBundle<'db>, LayoutEvidenceError<'db>> {
        let value = &self.semantic_values[dst.index()];
        if value.schema.components.len() != output.components.len() {
            return Err(LayoutEvidenceError::ShapeMismatch {
                dst,
                expected: value.schema.components.len(),
                actual: output.components.len(),
            });
        }
        value
            .schema
            .components
            .iter()
            .map(|target| {
                let output = output
                    .components
                    .iter()
                    .find(|component| component.port == target.port)
                    .ok_or(LayoutEvidenceError::MissingComponent {
                        local: dst,
                        component: target.id,
                    })?;
                if output.map_ty() != target.map_ty() {
                    return Err(LayoutEvidenceError::MapTypeMismatch {
                        dst,
                        component: output.id,
                    });
                }
                let expr = if output.is_runtime() {
                    LayoutTransferExpr::CallResult {
                        component: output.id,
                    }
                } else {
                    LayoutTransferExpr::Fixed(
                        Self::static_component(output)
                            .expect("compile-time layout output must have a static key")
                            .expr,
                    )
                };
                Ok(LayoutTransferComponent {
                    target: target.clone(),
                    expr,
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|components| LayoutTransferBundle { components })
    }

    fn call_transfer(
        &self,
        dst: SLocalId,
        callee: SemanticCalleeRef<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
    ) -> Result<(LayoutTransferBundle<'db>, LayoutTransferCall<'db>), LayoutEvidenceError<'db>>
    {
        let signature = callee.key.layout_bundle_signature(self.db);
        let inputs = signature
            .inputs
            .iter()
            .map(|input| {
                Ok(LayoutTransferInput {
                    origin: input.origin,
                    bundle: self.call_input_transfer_bundle(
                        input.origin,
                        &input.schema,
                        args,
                        effect_args,
                    )?,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let output_witnesses = signature
            .output_witnesses
            .components
            .iter()
            .map(|component| {
                let destination = self.semantic_values[dst.index()]
                    .schema
                    .components
                    .iter()
                    .find(|destination| destination.port == component.port)
                    .ok_or(LayoutEvidenceError::MissingComponent {
                        local: dst,
                        component: component.id,
                    })?;
                if destination.map_ty() != component.map_ty() {
                    return Err(LayoutEvidenceError::MapTypeMismatch {
                        dst,
                        component: component.id,
                    });
                }
                Ok(LayoutTransferComponent {
                    target: component.clone(),
                    expr: LayoutTransferExpr::Ambient {
                        local: dst,
                        component: destination.id,
                    },
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let value = self.call_result_transfer_bundle(dst, &signature.output)?;
        Ok((
            value,
            LayoutTransferCall {
                callee,
                inputs,
                output_witnesses: LayoutTransferBundle {
                    components: output_witnesses,
                },
                has_runtime_evidence: signature.has_runtime_evidence(),
            },
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
            .components
            .iter()
            .filter_map(|component| {
                let port = target.projected_port(&component.port, &projection.path)?;
                Some((component, port))
            })
            .map(|(component, port)| {
                let value_ty = component
                    .map_ty()
                    .projected(projection.indices.len())
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                Ok(LayoutTransferComponent {
                    target: component.clone(),
                    expr: LayoutTransferExpr::Update {
                        base: self.transfer_source_for_port(
                            dst,
                            &component.port,
                            Vec::new(),
                            &component.map_ty(),
                        )?,
                        indices: projection.indices.clone().into_boxed_slice(),
                        value: self.transfer_source_for_port(src, &port, Vec::new(), &value_ty)?,
                    },
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|components| LayoutTransferBundle { components })
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
                        (self.ambient_transfer_bundle(*dst), None)
                    }
                    NExpr::Const(SConst::Ref(_)) => (self.ambient_transfer_bundle(*dst), None),
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
                        (value, Some(call))
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
                component: component.id,
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
        let component = value
            .schema
            .components
            .iter()
            .find(|component| component.port == source.value.port)
            .ok_or_else(|| LayoutEvidenceError::MissingPort {
                local,
                port: source.value.port.clone(),
            })?;
        if component.map_ty() != source.value.map_ty {
            return Err(LayoutEvidenceError::MapTypeMismatch {
                dst: local,
                component: component.id,
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
        let terms = projection
            .indices
            .iter()
            .copied()
            .zip(source.map_ty.dimensions.iter().copied())
            .map(|(index, len)| LayoutEvidenceProjectionTerm { index, len })
            .collect::<Vec<_>>();
        let expr = if terms.is_empty() {
            source.expr
        } else {
            match source.expr {
                LayoutEvidenceExpr::Use(source) => LayoutEvidenceExpr::Project {
                    source,
                    terms: terms.into_boxed_slice(),
                },
                LayoutEvidenceExpr::Project {
                    source,
                    terms: existing,
                } => {
                    let mut combined = existing.into_vec();
                    combined.extend(terms);
                    LayoutEvidenceExpr::Project {
                        source,
                        terms: combined.into_boxed_slice(),
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
        let component_id = component.id;
        let map_ty = component.map_ty();
        let port = component.port.clone();
        if map_ty != expected.value.map_ty {
            return Err(LayoutEvidenceError::MapTypeMismatch {
                dst: source.local,
                component: component_id,
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
            let Some(transfer) = bundle
                .components
                .iter()
                .find(|component| component.target.port == expected.value.port)
            else {
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

    fn zero_component(component: &LayoutBundleComponent<'db>) -> ComponentExpr<'db> {
        ComponentExpr {
            expr: LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Constant(
                LayoutEvidenceConstant {
                    map_ty: component.map_ty(),
                    base: LayoutEvidenceBase::Slot(0),
                    strides: vec![0; component.rank()].into_boxed_slice(),
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
                        IndexSource::Dynamic(index) => LayoutEvidenceIndex::Dynamic(NOperand {
                            local: *index,
                            origin: None,
                            mode: ReadMode::Copy,
                        }),
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
                    } else if let EffectHandleTargetResolution::Resolved { target_ty, .. } =
                        resolve_effect_handle_target(
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
    ) -> Result<Vec<ComponentExpr<'db>>, LayoutEvidenceError<'db>> {
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
            let component_idx = transfer.target.id.index();
            let Some(component) = destination.schema.components.get(component_idx) else {
                return Err(LayoutEvidenceError::MissingComponent {
                    local: dst,
                    component: transfer.target.id,
                });
            };
            if component.id != transfer.target.id
                || component.port != transfer.target.port
                || component.map_ty() != transfer.target.map_ty()
                || source.map_ty != component.map_ty()
            {
                return Err(LayoutEvidenceError::MapTypeMismatch {
                    dst,
                    component: component.id,
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
        let mut args = Vec::new();
        for input in &call.inputs {
            let values = self.evaluate_transfer_bundle(&input.bundle)?;
            for (component, value) in input.bundle.components.iter().zip(values) {
                if component.target.is_runtime() {
                    args.push(LayoutEvidenceCallArg {
                        target: CallableLayoutParamPort::Input(CallableLayoutPort {
                            origin: input.origin,
                            component: component.target.port.clone(),
                        }),
                        value: value.expr,
                    });
                }
            }
        }
        let witnesses = self.evaluate_transfer_bundle(&call.output_witnesses)?;
        args.extend(call.output_witnesses.components.iter().zip(witnesses).map(
            |(component, value)| LayoutEvidenceCallArg {
                target: CallableLayoutParamPort::OutputWitness(component.target.port.clone()),
                value: value.expr,
            },
        ));
        Ok(LayoutEvidenceStatement {
            assignments: self.transfer_assignments(dst, value, true)?,
            call: call.has_runtime_evidence.then(|| LayoutEvidenceCall {
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
        output: &LayoutBundleSchema<'db>,
    ) -> Result<Box<[LayoutEvidenceReturn<'db>]>, LayoutEvidenceError<'db>> {
        let source = self.whole_value(value)?;
        if source.len() != output.components.len() {
            return Err(LayoutEvidenceError::ShapeMismatch {
                dst: value,
                expected: output.components.len(),
                actual: source.len(),
            });
        }
        let mut operands = Vec::new();
        for schema in &output.components {
            if !schema.is_runtime() {
                continue;
            }
            let source = source
                .iter()
                .find(|source| source.port == schema.port)
                .ok_or(LayoutEvidenceError::MissingComponent {
                    local: value,
                    component: schema.id,
                })?;
            operands.push(LayoutEvidenceReturn {
                component: schema.id,
                value: Self::operand(&source.expr).ok_or(LayoutEvidenceError::InvalidPlace)?,
            });
        }
        Ok(operands.into_boxed_slice())
    }
}

fn input_schema<'db>(
    signature: &CallableLayoutBundleSignature<'db>,
    origin: CallableInputLayoutHoleOrigin,
) -> Option<LayoutBundleSchema<'db>> {
    signature
        .inputs
        .iter()
        .find(|input| input.origin == origin)
        .map(|input| input.schema.clone())
}

fn dynamic_component_locals(value: &LayoutEvidenceValue<'_>) -> Vec<LayoutEvidenceLocalId> {
    value
        .components
        .iter()
        .filter_map(|component| match component {
            LayoutEvidenceComponentValue::Known(_) => None,
            LayoutEvidenceComponentValue::Dynamic(local) => Some(*local),
        })
        .collect()
}

pub fn layout_evidence_body<'db>(
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
        .map_err(|error| LayoutEvidenceError::InvalidSchema { local: None, error })?;
    signature
        .output_witnesses
        .validate()
        .map_err(|error| LayoutEvidenceError::InvalidSchema { local: None, error })?;
    let mut builder = LayoutEvidenceBuilder {
        db,
        normalized: &normalized,
        locals: Vec::new(),
        semantic_values: Vec::with_capacity(normalized.locals.len()),
        declared_sources: Vec::new(),
        contextual_sources: vec![Vec::new(); normalized.locals.len()],
    };
    let mut input_values = Vec::new();
    for (idx, local) in normalized.locals.iter().enumerate() {
        let semantic_local = SLocalId::from_u32(idx as u32);
        let layout_ty = local.ty;
        let template_ty = template_normalized
            .as_ref()
            .map_or(layout_ty, |template| template.locals[idx].ty);
        let origin = local
            .source
            .and_then(|source| source.callable_input_origin(db));
        let mut schema =
            if let Some(schema) = origin.and_then(|origin| input_schema(&signature, origin)) {
                schema
            } else {
                layout_bundle_schema_for_semantic_value(
                    db,
                    body.ok_or(LayoutEvidenceError::MissingBody(template_owner))?,
                    idx as u32,
                    layout_ty,
                    template_ty,
                )
            };
        if matches!(local.source, None | Some(LocalBinding::Local { .. })) {
            for component in &mut schema.components {
                component.transport = LayoutBundleTransport::Runtime;
            }
        }
        schema
            .validate()
            .map_err(|error| LayoutEvidenceError::InvalidSchema {
                local: Some(semantic_local),
                error,
            })?;
        let value = builder.alloc_value(semantic_local, schema, origin)?;
        if let Some(origin) = origin {
            if input_values
                .iter()
                .any(|(candidate, _)| *candidate == origin)
            {
                return Err(LayoutEvidenceError::DuplicateInput(origin));
            }
            input_values.push((origin, semantic_local));
        }
        builder.semantic_values.push(value);
    }
    builder.index_declared_sources()?;
    let mut params = Vec::new();
    for input in &signature.inputs {
        let Some((_, local)) = input_values
            .iter()
            .find(|(origin, _)| *origin == input.origin)
        else {
            continue;
        };
        params.extend(dynamic_component_locals(
            &builder.semantic_values[local.index()],
        ));
    }
    for component in &signature.output_witnesses.components {
        let source = CallableLayoutParamPort::OutputWitness(component.port.clone());
        let local = builder.alloc_local(None, component, Some(source.clone()));
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
        params.push(local);
    }
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
