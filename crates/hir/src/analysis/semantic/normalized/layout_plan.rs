use cranelift_entity::EntityRef;
use rustc_hash::FxHashSet;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        LayoutBackingProjection, SLocalId, SemOrigin, SemanticBody,
        normalized::{
            NDataPath, NDataProjection, NIndex, NRootId, NRootKind, NValueId, NormalizedBody,
        },
    },
    ty::{
        adt_def::instantiate_adt_field_shape,
        normalize::normalize_ty,
        ty_def::{PrimTy, TyBase, TyData, TyId},
    },
};

/// Runtime-only representation mapping for an admitted normalized body.
///
/// Semantic borrow and capability analyses consume only `NormalizedBody`.
/// Layout evidence and rMIR lowering use this companion plan to locate the
/// representation that carries a semantic value or root.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NLayoutPlan<'db> {
    pub value_representations: Vec<NValueRepresentation>,
    pub root_representations: Vec<NRootRepresentation>,
    pub use_backings: Vec<NLayoutUseBacking<'db>>,
}

impl NLayoutPlan<'_> {
    pub fn value_source(&self, value: NValueId) -> Option<SLocalId> {
        self.value_representations
            .get(value.index())
            .filter(|representation| representation.value == value)
            .map(|representation| representation.source_local)
    }

    pub fn root_source(&self, root: NRootId) -> Option<SLocalId> {
        self.root_representations
            .get(root.index())
            .filter(|representation| representation.root == root)
            .and_then(|representation| representation.source_local)
    }

    pub fn use_backings(&self, value: NValueId) -> impl Iterator<Item = &NLayoutUseBacking<'_>> {
        self.use_backings
            .iter()
            .filter(move |backing| backing.value == value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NValueRepresentation {
    pub value: NValueId,
    pub source_local: SLocalId,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NRootRepresentation {
    pub root: NRootId,
    pub source_local: Option<SLocalId>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NLayoutUseBacking<'db> {
    pub value: NValueId,
    pub target: Box<[LayoutBackingProjection]>,
    pub source: NLayoutBackingSource,
    pub origin: SemOrigin<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NLayoutBackingSource {
    Value { value: NValueId, path: NDataPath },
    Root { root: NRootId, path: NDataPath },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NormalizedLayoutPlanVerifyError {
    ValueRepresentationCount { expected: usize, actual: usize },
    RootRepresentationCount { expected: usize, actual: usize },
    ValueRepresentationId(NValueId),
    RootRepresentationId(NRootId),
    MissingSourceLocal(SLocalId),
    InvalidRootSource(NRootId),
    MissingBackingValue(NValueId),
    MissingBackingRoot(NRootId),
    DuplicateBackingTarget(NValueId),
    InvalidBackingProjection(NValueId),
}

pub fn verify_normalized_layout_plan<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
    source: &SemanticBody<'db>,
    plan: &NLayoutPlan<'db>,
) -> Result<(), NormalizedLayoutPlanVerifyError> {
    if plan.value_representations.len() != body.values.len() {
        return Err(NormalizedLayoutPlanVerifyError::ValueRepresentationCount {
            expected: body.values.len(),
            actual: plan.value_representations.len(),
        });
    }
    for (index, representation) in plan.value_representations.iter().enumerate() {
        let value = NValueId::new(index);
        if representation.value != value {
            return Err(NormalizedLayoutPlanVerifyError::ValueRepresentationId(
                value,
            ));
        }
        verify_source_local(source, representation.source_local)?;
    }
    if plan.root_representations.len() != body.roots.len() {
        return Err(NormalizedLayoutPlanVerifyError::RootRepresentationCount {
            expected: body.roots.len(),
            actual: plan.root_representations.len(),
        });
    }
    for (index, representation) in plan.root_representations.iter().enumerate() {
        let root = NRootId::new(index);
        if representation.root != root {
            return Err(NormalizedLayoutPlanVerifyError::RootRepresentationId(root));
        }
        if let Some(local) = representation.source_local {
            verify_source_local(source, local)?;
        }
        let root_kind = &body.roots[index].kind;
        let valid_source = match root_kind {
            NRootKind::Provider { .. } => representation.source_local.is_none(),
            NRootKind::LocalSlot { .. } | NRootKind::ParamPlace { .. } => {
                representation.source_local.is_some()
            }
            NRootKind::CapabilityRepresentation { .. } => true,
        };
        if !valid_source {
            return Err(NormalizedLayoutPlanVerifyError::InvalidRootSource(root));
        }
    }

    let mut targets = FxHashSet::default();
    for backing in &plan.use_backings {
        let target_local = plan.value_source(backing.value).ok_or(
            NormalizedLayoutPlanVerifyError::MissingBackingValue(backing.value),
        )?;
        verify_source_local(source, target_local)?;
        if !targets.insert((
            backing.value,
            backing.target.clone(),
            backing.source.clone(),
        )) {
            return Err(NormalizedLayoutPlanVerifyError::DuplicateBackingTarget(
                backing.value,
            ));
        }
        let target_base_ty = body
            .value(backing.value)
            .expect("layout plan value count and identities were verified")
            .ty;
        project_layout_path_ty(db, source, target_base_ty, &backing.target).ok_or(
            NormalizedLayoutPlanVerifyError::InvalidBackingProjection(backing.value),
        )?;
        match &backing.source {
            NLayoutBackingSource::Value { value, path } => {
                let local = plan
                    .value_source(*value)
                    .ok_or(NormalizedLayoutPlanVerifyError::MissingBackingValue(*value))?;
                verify_source_local(source, local)?;
                verify_layout_data_path_indices(db, &body.values, path).ok_or(
                    NormalizedLayoutPlanVerifyError::InvalidBackingProjection(backing.value),
                )?;
            }
            NLayoutBackingSource::Root { root, path } => {
                body.root(*root)
                    .ok_or(NormalizedLayoutPlanVerifyError::MissingBackingRoot(*root))?;
                verify_layout_data_path_indices(db, &body.values, path).ok_or(
                    NormalizedLayoutPlanVerifyError::InvalidBackingProjection(backing.value),
                )?;
            }
        }
    }
    Ok(())
}

fn verify_source_local(
    source: &SemanticBody<'_>,
    local: SLocalId,
) -> Result<(), NormalizedLayoutPlanVerifyError> {
    source
        .local(local)
        .map(|_| ())
        .ok_or(NormalizedLayoutPlanVerifyError::MissingSourceLocal(local))
}

fn project_layout_path_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    source: &SemanticBody<'db>,
    mut ty: TyId<'db>,
    path: &[LayoutBackingProjection],
) -> Option<TyId<'db>> {
    for projection in path {
        ty = projection_base_ty(db, source, ty);
        ty = match *projection {
            LayoutBackingProjection::Field(field) => {
                ty.field_types(db).get(field.0 as usize).copied()?
            }
            LayoutBackingProjection::VariantField { variant, field } => {
                let adt = ty.adt_def(db)?;
                let variant = variant.0 as usize;
                let field = field.0 as usize;
                adt.fields(db)
                    .get(variant)
                    .filter(|fields| field < fields.num_types())?;
                instantiate_adt_field_shape(db, adt, variant, field, ty.generic_args(db))
            }
            LayoutBackingProjection::Index(index) => {
                if index.is_some_and(|index| ty.array_len(db).is_some_and(|len| index >= len)) {
                    return None;
                }
                let (_, args) = ty.decompose_ty_app(db);
                *args.first().filter(|_| ty.is_array(db))?
            }
        };
    }
    Some(ty)
}

fn verify_layout_data_path_indices<'db>(
    db: &'db dyn HirAnalysisDb,
    values: &[crate::analysis::semantic::normalized::NValue<'db>],
    path: &NDataPath,
) -> Option<()> {
    for projection in path.iter() {
        if let NDataProjection::Index(NIndex::Value(value)) = projection {
            let value = values.get(value.index())?;
            if !matches!(
                value.ty.data(db),
                TyData::TyBase(TyBase::Prim(PrimTy::Usize))
            ) {
                return None;
            }
        }
    }
    Some(())
}

fn projection_base_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    source: &SemanticBody<'db>,
    ty: TyId<'db>,
) -> TyId<'db> {
    let ty = normalize_ty(
        db,
        ty,
        source.template_owner.scope(),
        source.owner.assumptions(db),
    );
    ty.as_capability(db).map_or(ty, |(_, target)| target)
}
