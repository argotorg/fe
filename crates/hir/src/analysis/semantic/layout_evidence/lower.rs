use std::{collections::VecDeque, slice};

use cranelift_entity::EntityRef;
use rustc_hash::FxHashSet;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        NBorrowRoot, NEffectArg, NEffectArgValue, NExpr, NOperand, NSPlace, NSPlaceRoot,
        NSStmtKind, NSTerminatorKind, NormalizedSemanticBody, ReadMode, SConst, SLocalId,
        SemConstId, SemanticCalleeRef, SemanticInstance,
        borrowck::normalize_semantic_body_for_layout_evidence, get_or_build_semantic_instance,
        identity_semantic_instance_key,
    },
    ty::{
        CallableLayoutBundleSignature, CallableLayoutParamPort, CallableLayoutPort,
        LayoutBundleComponent, LayoutBundleComponentKey, LayoutBundleSchema, LayoutBundleTransport,
        LayoutEvidencePath, LayoutEvidencePathStep, LayoutMapTy, LayoutPortKey,
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

#[derive(Clone)]
struct EvidenceProjection {
    path: LayoutEvidencePath,
    indices: Vec<LayoutEvidenceIndex>,
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

#[derive(Clone)]
enum ContextPropagationSite<'db> {
    Expr(NExpr<'db>),
    Store {
        src: SLocalId,
        projection: EvidenceProjection,
    },
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

    fn component_for_port(
        &self,
        local: SLocalId,
        port: &LayoutPortKey,
    ) -> Result<ComponentExpr<'db>, LayoutEvidenceError<'db>> {
        let value = self
            .semantic_values
            .get(local.index())
            .ok_or(LayoutEvidenceError::InvalidPlace)?;
        let port = value.schema.canonicalize_port(port);
        let selected = value
            .schema
            .components
            .iter()
            .enumerate()
            .find(|(_, component)| component.port == port)
            .ok_or_else(|| LayoutEvidenceError::MissingPort {
                local,
                port: port.clone(),
            })?;
        self.component_expr(local, selected.0)
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

    fn preferred_component_source(
        &self,
        local: SLocalId,
        component: &LayoutBundleComponent<'db>,
        known: Option<ComponentExpr<'db>>,
    ) -> Result<Option<ComponentExpr<'db>>, LayoutEvidenceError<'db>> {
        if known.is_some() {
            return Ok(known);
        }
        if let Some(source) = self.contextual_component_source(local, component)? {
            return Ok(Some(source));
        }
        self.declared_component_source(local, component)
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

    fn propagate_context_to_use(
        &mut self,
        source_local: SLocalId,
        expected: &[ContextualComponentExpr<'db>],
        queue: &mut VecDeque<SLocalId>,
        queued: &mut FxHashSet<SLocalId>,
    ) -> Result<(), LayoutEvidenceError<'db>> {
        self.propagate_context_to_projection(
            source_local,
            &EvidenceProjection {
                path: Vec::new(),
                indices: Vec::new(),
            },
            expected,
            queue,
            queued,
        )
    }

    fn propagate_context_to_projection(
        &mut self,
        source_local: SLocalId,
        projection: &EvidenceProjection,
        expected: &[ContextualComponentExpr<'db>],
        queue: &mut VecDeque<SLocalId>,
        queued: &mut FxHashSet<SLocalId>,
    ) -> Result<(), LayoutEvidenceError<'db>> {
        // A scalar result cannot define the unobserved members of an indexed
        // source map. Structural projections preserve the complete component
        // map and can therefore carry context back to their source value.
        if !projection.indices.is_empty() {
            return Ok(());
        }
        for expected in expected {
            let mut value_path = projection.path.clone();
            value_path.extend_from_slice(&expected.value.port.value_path);
            let port = self.semantic_values[source_local.index()]
                .schema
                .canonicalize_port(&LayoutPortKey {
                    value_path,
                    root: expected.value.port.root,
                });
            let component = self.semantic_values[source_local.index()]
                .schema
                .components
                .iter()
                .find(|component| component.port == port)
                .ok_or_else(|| LayoutEvidenceError::MissingPort {
                    local: source_local,
                    port: port.clone(),
                })?;
            if component.map_ty() != expected.value.map_ty {
                return Err(LayoutEvidenceError::MapTypeMismatch {
                    dst: source_local,
                    component: component.id,
                });
            }
            let source = ContextualComponentExpr {
                value: ComponentExpr {
                    expr: expected.value.expr.clone(),
                    map_ty: expected.value.map_ty.clone(),
                    port,
                },
                strength: expected.strength,
            };
            self.enqueue_contextual_source(source_local, source, queue, queued)?;
        }
        Ok(())
    }

    fn propagate_context_to_place(
        &mut self,
        place: &NSPlace<'db>,
        expected: &[ContextualComponentExpr<'db>],
        queue: &mut VecDeque<SLocalId>,
        queued: &mut FxHashSet<SLocalId>,
    ) -> Result<(), LayoutEvidenceError<'db>> {
        let (root, projection) = self.place_projection(place)?;
        let EvidencePlaceRoot::Local(local) = root else {
            return Ok(());
        };
        self.propagate_context_to_projection(local, &projection, expected, queue, queued)
    }

    fn propagate_context_to_aggregate(
        &mut self,
        ty: TyId<'db>,
        fields: &[NOperand],
        expected: &[ContextualComponentExpr<'db>],
        queue: &mut VecDeque<SLocalId>,
        queued: &mut FxHashSet<SLocalId>,
    ) -> Result<(), LayoutEvidenceError<'db>> {
        if ty.is_array(self.db) {
            for source in expected {
                if source.value.port.value_path.first() != Some(&LayoutEvidencePathStep::Index) {
                    continue;
                }
                let target_ty = source
                    .value
                    .map_ty
                    .projected(1)
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                let projection = EvidenceProjection {
                    path: vec![LayoutEvidencePathStep::Index],
                    indices: Vec::new(),
                };
                let port = source
                    .value
                    .port
                    .projected(&projection.path)
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                for (index, field) in fields.iter().enumerate() {
                    let mut projection = projection.clone();
                    projection
                        .indices
                        .push(LayoutEvidenceIndex::Constant(index));
                    let projected = Self::project_contextual_component(
                        source.value.clone(),
                        &projection,
                        target_ty.clone(),
                        port.clone(),
                    )?;
                    self.enqueue_contextual_source(
                        field.local,
                        ContextualComponentExpr {
                            value: projected,
                            strength: source.strength,
                        },
                        queue,
                        queued,
                    )?;
                }
            }
            return Ok(());
        }
        for source in expected {
            let Some(LayoutEvidencePathStep::Field(field_idx)) =
                source.value.port.value_path.first().copied()
            else {
                continue;
            };
            let field = fields
                .get(field_idx as usize)
                .ok_or(LayoutEvidenceError::InvalidPlace)?;
            let projection = EvidenceProjection {
                path: vec![LayoutEvidencePathStep::Field(field_idx)],
                indices: Vec::new(),
            };
            let port = source
                .value
                .port
                .projected(&projection.path)
                .ok_or(LayoutEvidenceError::InvalidPlace)?;
            let projected = Self::project_contextual_component(
                source.value.clone(),
                &projection,
                source.value.map_ty.clone(),
                port,
            )?;
            self.enqueue_contextual_source(
                field.local,
                ContextualComponentExpr {
                    value: projected,
                    strength: source.strength,
                },
                queue,
                queued,
            )?;
        }
        Ok(())
    }

    fn propagate_context_to_enum(
        &mut self,
        variant: u16,
        fields: &[NOperand],
        expected: &[ContextualComponentExpr<'db>],
        queue: &mut VecDeque<SLocalId>,
        queued: &mut FxHashSet<SLocalId>,
    ) -> Result<(), LayoutEvidenceError<'db>> {
        for source in expected {
            let [
                LayoutEvidencePathStep::Variant(candidate),
                LayoutEvidencePathStep::Field(field_idx),
                ..,
            ] = source.value.port.value_path.as_slice()
            else {
                continue;
            };
            if *candidate != variant {
                continue;
            }
            let field = fields
                .get(*field_idx as usize)
                .ok_or(LayoutEvidenceError::InvalidPlace)?;
            let projection = EvidenceProjection {
                path: vec![
                    LayoutEvidencePathStep::Variant(*candidate),
                    LayoutEvidencePathStep::Field(*field_idx),
                ],
                indices: Vec::new(),
            };
            let port = source
                .value
                .port
                .projected(&projection.path)
                .ok_or(LayoutEvidenceError::InvalidPlace)?;
            let projected = Self::project_contextual_component(
                source.value.clone(),
                &projection,
                source.value.map_ty.clone(),
                port,
            )?;
            self.enqueue_contextual_source(
                field.local,
                ContextualComponentExpr {
                    value: projected,
                    strength: source.strength,
                },
                queue,
                queued,
            )?;
        }
        Ok(())
    }

    fn propagate_context_to_expr(
        &mut self,
        expr: &NExpr<'db>,
        expected: &[ContextualComponentExpr<'db>],
        queue: &mut VecDeque<SLocalId>,
        queued: &mut FxHashSet<SLocalId>,
    ) -> Result<(), LayoutEvidenceError<'db>> {
        match expr {
            NExpr::Use(value) => {
                self.propagate_context_to_use(value.local, expected, queue, queued)
            }
            NExpr::ReadPlace { place, .. } | NExpr::Borrow { place, .. } => {
                self.propagate_context_to_place(place, expected, queue, queued)
            }
            NExpr::ExtractEnumField {
                value,
                variant,
                field,
            } => self.propagate_context_to_projection(
                value.local,
                &EvidenceProjection {
                    path: vec![
                        LayoutEvidencePathStep::Variant(variant.0),
                        LayoutEvidencePathStep::Field(field.0),
                    ],
                    indices: Vec::new(),
                },
                expected,
                queue,
                queued,
            ),
            NExpr::AggregateMake { ty, fields } => {
                self.propagate_context_to_aggregate(*ty, fields, expected, queue, queued)
            }
            NExpr::EnumMake {
                variant, fields, ..
            } => self.propagate_context_to_enum(variant.0, fields, expected, queue, queued),
            NExpr::ArrayRepeat { ty, value } if ty.array_len(self.db) == Some(1) => self
                .propagate_context_to_aggregate(
                    *ty,
                    slice::from_ref(value),
                    expected,
                    queue,
                    queued,
                ),
            NExpr::Const(_)
            | NExpr::ArrayRepeat { .. }
            | NExpr::CodeRegionRef { .. }
            | NExpr::Unary { .. }
            | NExpr::Binary { .. }
            | NExpr::Cast { .. }
            | NExpr::GetEnumTag { .. }
            | NExpr::IsEnumVariant { .. }
            | NExpr::CodeRegionOffset { .. }
            | NExpr::CodeRegionLen { .. }
            | NExpr::Call { .. } => Ok(()),
        }
    }

    fn propagate_context_to_store(
        &mut self,
        dst: SLocalId,
        src: SLocalId,
        projection: &EvidenceProjection,
        expected: &[ContextualComponentExpr<'db>],
        queue: &mut VecDeque<SLocalId>,
        queued: &mut FxHashSet<SLocalId>,
    ) -> Result<(), LayoutEvidenceError<'db>> {
        for source in expected {
            let Some(port) = self.semantic_values[dst.index()]
                .schema
                .projected_port(&source.value.port, &projection.path)
            else {
                continue;
            };
            let target_ty = source
                .value
                .map_ty
                .projected(projection.indices.len())
                .ok_or(LayoutEvidenceError::InvalidPlace)?;
            let projected = Self::project_contextual_component(
                source.value.clone(),
                projection,
                target_ty,
                port,
            )?;
            self.enqueue_contextual_source(
                src,
                ContextualComponentExpr {
                    value: projected,
                    strength: source.strength,
                },
                queue,
                queued,
            )?;
        }
        Ok(())
    }

    fn propagate_layout_context(&mut self) -> Result<(), LayoutEvidenceError<'db>> {
        let witnesses = self
            .declared_sources
            .iter()
            .filter(|source| matches!(source.source, CallableLayoutParamPort::OutputWitness(_)))
            .map(|source| source.value.clone())
            .collect::<Vec<_>>();
        let mut sites = vec![Vec::new(); self.normalized.locals.len()];
        let mut store_seeds = Vec::new();
        for block in &self.normalized.blocks {
            for statement in &block.stmts {
                match &statement.kind {
                    NSStmtKind::Assign { dst, expr } => {
                        sites[dst.index()].push(ContextPropagationSite::Expr(expr.clone()));
                    }
                    NSStmtKind::Store { dst, src } => {
                        let (root, projection) = self.place_projection(dst)?;
                        if !projection.path.is_empty()
                            || matches!(&root, EvidencePlaceRoot::Provider(_))
                        {
                            store_seeds.push((dst.clone(), src.local));
                        }
                        if let EvidencePlaceRoot::Local(root) = root {
                            sites[root.index()].push(ContextPropagationSite::Store {
                                src: src.local,
                                projection,
                            });
                        }
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
        for (place, source_local) in store_seeds {
            let schema = self.semantic_values[source_local.index()].schema.clone();
            for source in self.place_value(&place, &schema)? {
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
            for site in sites[local.index()].clone() {
                match site {
                    ContextPropagationSite::Expr(expr) => {
                        self.propagate_context_to_expr(&expr, &expected, &mut queue, &mut queued)?
                    }
                    ContextPropagationSite::Store { src, projection } => self
                        .propagate_context_to_store(
                            local,
                            src,
                            &projection,
                            &expected,
                            &mut queue,
                            &mut queued,
                        )?,
                }
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

    fn ambient_value(
        &self,
        dst: SLocalId,
    ) -> Result<Vec<ComponentExpr<'db>>, LayoutEvidenceError<'db>> {
        self.semantic_values[dst.index()]
            .schema
            .components
            .iter()
            .map(|component| {
                if let Some(source) = self.contextual_component_source(dst, component)? {
                    return Self::retarget_component(dst, component, source);
                }
                match (component.is_runtime(), &component.representative) {
                    (false, Some(LayoutBundleComponentKey::Static(base))) => Ok(ComponentExpr {
                        expr: LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Constant(
                            LayoutEvidenceConstant {
                                map_ty: component.map_ty(),
                                base: LayoutEvidenceBase::Root(*base),
                                strides: vec![0; component.rank()].into_boxed_slice(),
                            },
                        )),
                        map_ty: component.map_ty(),
                        port: component.port.clone(),
                    }),
                    (true, Some(LayoutBundleComponentKey::Static(base))) => self
                        .declared_component_source(dst, component)?
                        .map(|source| Self::retarget_component(dst, component, source))
                        .transpose()?
                        .map_or_else(
                            || {
                                Ok(ComponentExpr {
                                    expr: LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Constant(
                                        LayoutEvidenceConstant {
                                            map_ty: component.map_ty(),
                                            base: LayoutEvidenceBase::Root(*base),
                                            strides: vec![0; component.rank()].into_boxed_slice(),
                                        },
                                    )),
                                    map_ty: component.map_ty(),
                                    port: component.port.clone(),
                                })
                            },
                            Ok,
                        ),
                    (
                        true,
                        None
                        | Some(LayoutBundleComponentKey::Root(_))
                        | Some(LayoutBundleComponentKey::Param(_)),
                    ) => {
                        let source = self.declared_component_source(dst, component)?.ok_or(
                            LayoutEvidenceError::MissingComponent {
                                local: dst,
                                component: component.id,
                            },
                        )?;
                        Self::retarget_component(dst, component, source)
                    }
                    (
                        false,
                        None
                        | Some(LayoutBundleComponentKey::Root(_))
                        | Some(LayoutBundleComponentKey::Param(_)),
                    ) => {
                        unreachable!("compile-time layout component must have a static key")
                    }
                }
            })
            .collect()
    }

    fn aggregate_value(
        &self,
        dst: SLocalId,
        fields: &[NOperand],
    ) -> Result<Vec<ComponentExpr<'db>>, LayoutEvidenceError<'db>> {
        let target = &self.semantic_values[dst.index()].schema;
        target
            .components
            .iter()
            .enumerate()
            .map(|(component_idx, component)| {
                let source = if let Some(source) = self.known_component_expr(dst, component_idx) {
                    source
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
                    self.component_for_port(field.local, &port)?
                } else {
                    self.preferred_component_source(dst, component, None)?
                        .or_else(|| Self::static_component(component))
                        .ok_or(LayoutEvidenceError::MissingComponent {
                            local: dst,
                            component: component.id,
                        })?
                };
                Self::retarget_component(dst, component, source)
            })
            .collect()
    }

    fn array_value(
        &self,
        dst: SLocalId,
        fields: &[NOperand],
    ) -> Result<Vec<ComponentExpr<'db>>, LayoutEvidenceError<'db>> {
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
                let mut elements = Vec::with_capacity(fields.len());
                let child_ty = component
                    .map_ty()
                    .projected(1)
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                for field in fields {
                    let element = self.component_for_port(field.local, &port)?;
                    if element.map_ty != child_ty {
                        return Err(LayoutEvidenceError::MapTypeMismatch {
                            dst,
                            component: component.id,
                        });
                    }
                    elements.push(element.expr);
                }
                Ok(ComponentExpr {
                    expr: LayoutEvidenceExpr::Array {
                        elements: elements.into_boxed_slice(),
                    },
                    map_ty: component.map_ty(),
                    port: component.port.clone(),
                })
            })
            .collect()
    }

    fn array_repeat_value(
        &self,
        dst: SLocalId,
        value: SLocalId,
    ) -> Result<Vec<ComponentExpr<'db>>, LayoutEvidenceError<'db>> {
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
                let element = self.component_for_port(value, &port)?;
                if component.map_ty().projected(1).as_ref() != Some(&element.map_ty) {
                    return Err(LayoutEvidenceError::MapTypeMismatch {
                        dst,
                        component: component.id,
                    });
                }
                Ok(ComponentExpr {
                    expr: LayoutEvidenceExpr::Repeat {
                        len: component.dimensions[0],
                        element: Box::new(element.expr),
                    },
                    map_ty: component.map_ty(),
                    port: component.port.clone(),
                })
            })
            .collect()
    }

    fn enum_value(
        &self,
        dst: SLocalId,
        variant: u16,
        fields: &[NOperand],
    ) -> Result<Vec<ComponentExpr<'db>>, LayoutEvidenceError<'db>> {
        let target = &self.semantic_values[dst.index()].schema;
        target
            .components
            .iter()
            .enumerate()
            .map(|(component_idx, component)| {
                let source = match component.port.value_path.as_slice() {
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
                        self.component_for_port(field.local, &port)?
                    }
                    [] => self
                        .preferred_component_source(
                            dst,
                            component,
                            self.known_component_expr(dst, component_idx),
                        )?
                        .or_else(|| Self::static_component(component))
                        .ok_or(LayoutEvidenceError::MissingComponent {
                            local: dst,
                            component: component.id,
                        })?,
                    _ => Self::zero_component(component),
                };
                Self::retarget_component(dst, component, source)
            })
            .collect()
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

    fn place_value(
        &self,
        place: &NSPlace<'db>,
        expected: &LayoutBundleSchema<'db>,
    ) -> Result<Vec<ComponentExpr<'db>>, LayoutEvidenceError<'db>> {
        if expected.components.is_empty() {
            return Ok(Vec::new());
        }
        let (root, projection) = self.place_projection(place)?;
        match root {
            EvidencePlaceRoot::Local(local) => self.project_value(local, &projection),
            EvidencePlaceRoot::Provider(provider) => {
                self.provider_value(&provider, &projection, expected)
            }
        }
    }

    /// Selects the shortest carrier view whose runtime components satisfy the
    /// callee's schema. An effect may be declared as the physical handle or as
    /// its logical target, so `target_ty` alone does not determine the ABI view.
    fn effect_arg_value(
        &self,
        local: SLocalId,
        target_ty: TyId<'db>,
        expected: &LayoutBundleSchema<'db>,
    ) -> Result<Vec<ComponentExpr<'db>>, LayoutEvidenceError<'db>> {
        let local_data = self
            .normalized
            .local(local)
            .ok_or(LayoutEvidenceError::InvalidPlace)?;
        let source_schema = &self.semantic_values[local.index()].schema;
        let mut path = Vec::new();
        if expected.runtime_components_supplied_by_view(source_schema, &path) {
            return self.whole_value(local);
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
                return self.project_value(
                    local,
                    &EvidenceProjection {
                        path,
                        indices: Vec::new(),
                    },
                );
            }
        }
        Err(LayoutEvidenceError::InvalidPlace)
    }

    fn assign_value(
        &self,
        dst: SLocalId,
        source: Vec<ComponentExpr<'db>>,
    ) -> Result<Box<[LayoutEvidenceAssignment<'db>]>, LayoutEvidenceError<'db>> {
        let value = &self.semantic_values[dst.index()];
        if value.components.len() != source.len() {
            return Err(LayoutEvidenceError::ShapeMismatch {
                dst,
                expected: value.components.len(),
                actual: source.len(),
            });
        }
        let mut assignments = Vec::new();
        for (schema, dst_component) in value.schema.components.iter().zip(value.components.iter()) {
            let source = source
                .iter()
                .find(|source| source.port == schema.port)
                .ok_or(LayoutEvidenceError::MissingComponent {
                    local: dst,
                    component: schema.id,
                })?;
            if source.map_ty != schema.map_ty() {
                return Err(LayoutEvidenceError::MapTypeMismatch {
                    dst,
                    component: schema.id,
                });
            }
            if let LayoutEvidenceComponentValue::Dynamic(dst) = dst_component {
                assignments.push(LayoutEvidenceAssignment {
                    dst: *dst,
                    expr: source.expr.clone(),
                });
            }
        }
        Ok(assignments.into_boxed_slice())
    }

    fn store_local(
        &self,
        dst: SLocalId,
        projection: &EvidenceProjection,
        src: SLocalId,
    ) -> Result<Box<[LayoutEvidenceAssignment<'db>]>, LayoutEvidenceError<'db>> {
        if projection.path.is_empty() {
            return self.assign_value(dst, self.whole_value(src)?);
        }
        let target = &self.semantic_values[dst.index()];
        let mut assignments = Vec::new();
        for (component_idx, component) in target.schema.components.iter().enumerate() {
            let Some(port) = target
                .schema
                .projected_port(&component.port, &projection.path)
            else {
                continue;
            };
            let source = self.component_for_port(src, &port)?;
            let LayoutEvidenceComponentValue::Dynamic(target_local) =
                target.components[component_idx]
            else {
                continue;
            };
            let target_source = self.component_expr(dst, component_idx)?;
            let target_operand =
                Self::operand(&target_source.expr).ok_or(LayoutEvidenceError::InvalidPlace)?;
            let dimensions = &component.dimensions;
            let terms = projection
                .indices
                .iter()
                .copied()
                .zip(dimensions.iter().copied())
                .map(|(index, len)| LayoutEvidenceProjectionTerm { index, len })
                .collect::<Vec<_>>();
            assignments.push(LayoutEvidenceAssignment {
                dst: target_local,
                expr: if terms.is_empty() {
                    source.expr
                } else {
                    LayoutEvidenceExpr::Update {
                        source: target_operand,
                        terms: terms.into_boxed_slice(),
                        value: Box::new(source.expr),
                    }
                },
            });
        }
        Ok(assignments.into_boxed_slice())
    }

    fn lower_store(
        &self,
        dst: &NSPlace<'db>,
        src: SLocalId,
    ) -> Result<LayoutEvidenceStatement<'db>, LayoutEvidenceError<'db>> {
        let (root, projection) = self.place_projection(dst)?;
        let assignments = match root {
            EvidencePlaceRoot::Local(local) => self.store_local(local, &projection, src)?,
            EvidencePlaceRoot::Provider(_) => Box::new([]),
        };
        Ok(LayoutEvidenceStatement {
            assignments,
            call: None,
            const_bindings: Box::new([]),
        })
    }

    fn call_input_value(
        &self,
        origin: CallableInputLayoutHoleOrigin,
        schema: &LayoutBundleSchema<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
    ) -> Result<Vec<ComponentExpr<'db>>, LayoutEvidenceError<'db>> {
        match origin {
            CallableInputLayoutHoleOrigin::Receiver => args
                .first()
                .ok_or(LayoutEvidenceError::InvalidPlace)
                .and_then(|value| self.whole_value(value.local)),
            CallableInputLayoutHoleOrigin::ValueParam(idx) => args
                .get(idx)
                .ok_or(LayoutEvidenceError::InvalidPlace)
                .and_then(|value| self.whole_value(value.local)),
            CallableInputLayoutHoleOrigin::Effect(idx) => {
                let arg = effect_args
                    .iter()
                    .find(|arg| arg.binding_idx as usize == idx)
                    .ok_or(LayoutEvidenceError::InvalidPlace)?;
                match &arg.arg {
                    NEffectArgValue::Value(value) => arg.target_ty.map_or_else(
                        || self.whole_value(value.local),
                        |target_ty| self.effect_arg_value(value.local, target_ty, schema),
                    ),
                    NEffectArgValue::Place(place) => self.place_value(place, schema),
                }
            }
        }
    }

    fn flatten_call_input(
        &self,
        dst: SLocalId,
        origin: CallableInputLayoutHoleOrigin,
        schema: &LayoutBundleSchema<'db>,
        source: Vec<ComponentExpr<'db>>,
    ) -> Result<Vec<LayoutEvidenceCallArg<'db>>, LayoutEvidenceError<'db>> {
        let mut args = Vec::new();
        for component in &schema.components {
            if !component.is_runtime() {
                continue;
            }
            let source = source
                .iter()
                .find(|source| source.port == component.port)
                .ok_or(LayoutEvidenceError::MissingComponent {
                    local: dst,
                    component: component.id,
                })?;
            if source.map_ty != component.map_ty() {
                return Err(LayoutEvidenceError::MapTypeMismatch {
                    dst,
                    component: component.id,
                });
            }
            args.push(LayoutEvidenceCallArg {
                target: CallableLayoutParamPort::Input(CallableLayoutPort {
                    origin,
                    component: component.port.clone(),
                }),
                value: source.expr.clone(),
            });
        }
        Ok(args)
    }

    fn output_witness_call_arg(
        &self,
        dst: SLocalId,
        component: &LayoutBundleComponent<'db>,
    ) -> Result<LayoutEvidenceCallArg<'db>, LayoutEvidenceError<'db>> {
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
        let source = self
            .preferred_component_source(dst, destination, None)?
            .or_else(|| {
                let Some(LayoutBundleComponentKey::Static(base)) = &destination.representative
                else {
                    return None;
                };
                Some(ComponentExpr {
                    expr: LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Constant(
                        LayoutEvidenceConstant {
                            map_ty: destination.map_ty(),
                            base: LayoutEvidenceBase::Root(*base),
                            strides: vec![0; destination.rank()].into_boxed_slice(),
                        },
                    )),
                    map_ty: destination.map_ty(),
                    port: destination.port.clone(),
                })
            });
        let source = source.ok_or(LayoutEvidenceError::MissingComponent {
            local: dst,
            component: component.id,
        })?;
        let source = Self::retarget_component(dst, component, source)?;
        Ok(LayoutEvidenceCallArg {
            target: CallableLayoutParamPort::OutputWitness(component.port.clone()),
            value: source.expr,
        })
    }

    fn call_result_assignments(
        &self,
        dst: SLocalId,
        output: &LayoutBundleSchema<'db>,
    ) -> Result<Box<[LayoutEvidenceAssignment<'db>]>, LayoutEvidenceError<'db>> {
        let value = &self.semantic_values[dst.index()];
        let mut assignments = Vec::new();
        for (component_idx, dst_component) in value.components.iter().enumerate() {
            let LayoutEvidenceComponentValue::Dynamic(local) = dst_component else {
                continue;
            };
            let dst_schema = &value.schema.components[component_idx];
            let output = output
                .components
                .iter()
                .find(|component| component.port == dst_schema.port)
                .ok_or(LayoutEvidenceError::MissingComponent {
                    local: dst,
                    component: dst_schema.id,
                })?;
            if output.map_ty() != dst_schema.map_ty() {
                return Err(LayoutEvidenceError::MapTypeMismatch {
                    dst,
                    component: output.id,
                });
            }
            let expr = if output.is_runtime() {
                LayoutEvidenceExpr::CallResult {
                    component: output.id,
                }
            } else {
                let Some(LayoutBundleComponentKey::Static(base)) = &output.representative else {
                    unreachable!("compile-time layout output must have a static key")
                };
                LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Constant(LayoutEvidenceConstant {
                    map_ty: output.map_ty(),
                    base: LayoutEvidenceBase::Root(*base),
                    strides: vec![0; output.rank()].into_boxed_slice(),
                }))
            };
            assignments.push(LayoutEvidenceAssignment { dst: *local, expr });
        }
        if assignments.len()
            != value
                .components
                .iter()
                .filter(|component| matches!(component, LayoutEvidenceComponentValue::Dynamic(_)))
                .count()
        {
            return Err(LayoutEvidenceError::ShapeMismatch {
                dst,
                expected: value.components.len(),
                actual: output.components.len(),
            });
        }
        Ok(assignments.into_boxed_slice())
    }

    fn lower_call(
        &self,
        dst: SLocalId,
        callee: SemanticCalleeRef<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
    ) -> Result<LayoutEvidenceStatement<'db>, LayoutEvidenceError<'db>> {
        let signature = callee.key.layout_bundle_signature(self.db);
        let mut layout_args = Vec::new();
        for input in &signature.inputs {
            let source = self.call_input_value(input.origin, &input.schema, args, effect_args)?;
            layout_args.extend(self.flatten_call_input(
                dst,
                input.origin,
                &input.schema,
                source,
            )?);
        }
        for component in &signature.output_witnesses.components {
            layout_args.push(self.output_witness_call_arg(dst, component)?);
        }
        let assignments = self.call_result_assignments(dst, &signature.output)?;
        let call = signature
            .has_runtime_evidence()
            .then(|| LayoutEvidenceCall {
                callee,
                args: layout_args.into_boxed_slice(),
            });
        Ok(LayoutEvidenceStatement {
            assignments,
            call,
            const_bindings: Box::new([]),
        })
    }

    fn lower_assign(
        &self,
        dst: SLocalId,
        expr: &NExpr<'db>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<LayoutEvidenceStatement<'db>, LayoutEvidenceError<'db>> {
        if let NExpr::Call {
            callee,
            args,
            effect_args,
            ..
        } = expr
        {
            return self.lower_call(dst, *callee, args, effect_args);
        }
        let const_bindings = match expr {
            NExpr::Const(SConst::Value(value)) => self.const_bindings(*value, origin)?,
            _ => Box::new([]),
        };
        let source = match expr {
            NExpr::Use(value) => self.whole_value(value.local)?,
            NExpr::ReadPlace { place, .. } | NExpr::Borrow { place, .. } => {
                self.place_value(place, &self.semantic_values[dst.index()].schema)?
            }
            NExpr::ExtractEnumField {
                value,
                variant,
                field,
            } => self.project_value(
                value.local,
                &EvidenceProjection {
                    path: vec![
                        LayoutEvidencePathStep::Variant(variant.0),
                        LayoutEvidencePathStep::Field(field.0),
                    ],
                    indices: Vec::new(),
                },
            )?,
            NExpr::AggregateMake { ty, fields } if ty.is_array(self.db) => {
                self.array_value(dst, fields)?
            }
            NExpr::ArrayRepeat { value, .. } => self.array_repeat_value(dst, value.local)?,
            NExpr::AggregateMake { fields, .. } => self.aggregate_value(dst, fields)?,
            NExpr::EnumMake {
                variant, fields, ..
            } => self.enum_value(dst, variant.0, fields)?,
            NExpr::Const(_) => self.ambient_value(dst)?,
            NExpr::CodeRegionRef { .. }
            | NExpr::Unary { .. }
            | NExpr::Binary { .. }
            | NExpr::Cast { .. }
            | NExpr::GetEnumTag { .. }
            | NExpr::IsEnumVariant { .. }
            | NExpr::CodeRegionOffset { .. }
            | NExpr::CodeRegionLen { .. } => Vec::new(),
            NExpr::Call { .. } => unreachable!(),
        };
        Ok(LayoutEvidenceStatement {
            assignments: self.assign_value(dst, source)?,
            call: None,
            const_bindings,
        })
    }

    fn lower_statement(
        &self,
        stmt: &crate::analysis::semantic::NSStmt<'db>,
    ) -> Result<LayoutEvidenceStatement<'db>, LayoutEvidenceError<'db>> {
        match &stmt.kind {
            NSStmtKind::Assign { dst, expr } => self.lower_assign(*dst, expr, stmt.origin),
            NSStmtKind::Store { dst, src } => self.lower_store(dst, src.local),
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
    builder.propagate_layout_context()?;
    let statement_count = normalized
        .blocks
        .iter()
        .map(|block| block.stmts.len())
        .sum();
    let mut statements = vec![None; statement_count];
    for statement in normalized.blocks.iter().flat_map(|block| &block.stmts) {
        let slot = statements
            .get_mut(statement.id.index())
            .ok_or(LayoutEvidenceError::InvalidStatementIdentity(statement.id))?;
        if slot.is_some() {
            return Err(LayoutEvidenceError::InvalidStatementIdentity(statement.id));
        }
        *slot = Some(builder.lower_statement(statement)?);
    }
    let statements = statements
        .into_iter()
        .enumerate()
        .map(|(idx, statement)| {
            statement.ok_or(LayoutEvidenceError::InvalidStatementIdentity(
                crate::analysis::semantic::SStmtId::from_u32(idx as u32),
            ))
        })
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
