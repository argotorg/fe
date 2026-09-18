use cranelift_entity::EntityRef;
use rustc_hash::FxHashSet;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        LayoutBackingProjection, SLocal, SLocalId, SStmtId, SemOrigin, SemanticBody,
        SemanticLocalRole, ValueProvenance,
        normalized::{
            NDataPath, NDataProjection, NExpr, NIndex, NRootId, NRootKind, NStatementKind,
            NValueDefinition, NValueId, NormalizedBody,
        },
    },
    ty::{
        adt_def::{AdtRef, instantiate_adt_field_shape},
        normalize::normalize_ty,
        ty_def::{PrimTy, TyBase, TyData, TyId},
    },
};

use super::normalize::normalized_source_local_value_ty;

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

/// Shared runtime homes for semantic values and their layout evidence.
/// Source provenance never gives a synthetic value permission to overwrite its source.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NLayoutLocals<'db> {
    pub locals: Vec<SLocal<'db>>,
    pub value_locals: Vec<SLocalId>,
}

impl<'db> NLayoutLocals<'db> {
    pub fn new(
        body: &NormalizedBody<'db>,
        plan: &NLayoutPlan<'db>,
        source: &SemanticBody<'db>,
    ) -> Self {
        let mut locals = source.locals.clone();
        let value_locals = body
            .values
            .iter()
            .enumerate()
            .map(|(index, value)| {
                if let NValueDefinition::Statement { block, statement } = value.definition
                    && body.blocks[block.index()].statements[statement as usize]
                        .source
                        .is_none()
                {
                    let local = SLocalId::new(locals.len());
                    locals.push(SLocal {
                        ty: value.ty,
                        mutability: value.mutability,
                        source: None,
                        role: SemanticLocalRole::DirectValue {
                            provenance: ValueProvenance::Ordinary,
                        },
                        snapshot_source: None,
                        layout_backing_sources: Vec::new(),
                    });
                    local
                } else {
                    plan.value_source(NValueId::new(index))
                        .expect("verified normalized value must have source metadata")
                }
            })
            .collect();
        Self {
            locals,
            value_locals,
        }
    }

    pub fn value_local(&self, value: NValueId) -> Option<SLocalId> {
        self.value_locals.get(value.index()).copied()
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
    ValueType(NValueId),
    RootType(NRootId),
    ValueMutability(NValueId),
    RootMutability(NRootId),
    MissingSourceLocal(SLocalId),
    InvalidRootSource(NRootId),
    MissingBackingValue(NValueId),
    MissingBackingRoot(NRootId),
    DuplicateBackingTarget(NValueId),
    InvalidBackingProjection(NValueId),
    MissingStatementSource(SStmtId),
    UnknownStatementSource(SStmtId),
    DuplicateStatementSource(SStmtId),
}

pub fn verify_normalized_layout_plan<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
    source: &SemanticBody<'db>,
    plan: &NLayoutPlan<'db>,
) -> Result<(), NormalizedLayoutPlanVerifyError> {
    let source_statements = source
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .map(|statement| statement.id)
        .collect::<FxHashSet<_>>();
    let mut normalized_statements = FxHashSet::default();
    for statement in body
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .filter_map(|statement| statement.source)
    {
        if !source_statements.contains(&statement) {
            return Err(NormalizedLayoutPlanVerifyError::UnknownStatementSource(
                statement,
            ));
        }
        if !normalized_statements.insert(statement) {
            return Err(NormalizedLayoutPlanVerifyError::DuplicateStatementSource(
                statement,
            ));
        }
    }
    if let Some(statement) = source_statements
        .into_iter()
        .find(|statement| !normalized_statements.contains(statement))
    {
        return Err(NormalizedLayoutPlanVerifyError::MissingStatementSource(
            statement,
        ));
    }

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
        if value_retains_source_type(body, value)
            && body.values[index].ty
                != normalized_source_value_ty(db, body, source, representation.source_local)
        {
            return Err(NormalizedLayoutPlanVerifyError::ValueType(value));
        }
        if body.values[index].mutability
            != source.locals[representation.source_local.index()].mutability
        {
            return Err(NormalizedLayoutPlanVerifyError::ValueMutability(value));
        }
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
            let expected = match body.roots[index].kind {
                NRootKind::Temporary { value } => body.values[value.index()].ty,
                NRootKind::LocalSlot { .. } | NRootKind::ParamPlace { .. } => {
                    normalized_source_value_ty(db, body, source, local)
                }
                NRootKind::Provider { .. } | NRootKind::CapabilityRepresentation { .. } => body
                    .owner
                    .normalized_ty(db, source.locals[local.index()].ty),
            };
            if body.roots[index].ty != expected {
                return Err(NormalizedLayoutPlanVerifyError::RootType(root));
            }
        }
        let root_kind = &body.roots[index].kind;
        let valid_source = match root_kind {
            NRootKind::Provider { .. } | NRootKind::Temporary { .. } => {
                representation.source_local.is_none()
            }
            NRootKind::LocalSlot { .. } | NRootKind::ParamPlace { .. } => {
                representation.source_local.is_some()
            }
            NRootKind::CapabilityRepresentation { .. } => true,
        };
        if !valid_source {
            return Err(NormalizedLayoutPlanVerifyError::InvalidRootSource(root));
        }
        if let Some(local) = representation.source_local
            && body.roots[index].mutability != source.locals[local.index()].mutability
        {
            return Err(NormalizedLayoutPlanVerifyError::RootMutability(root));
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

fn value_retains_source_type(body: &NormalizedBody<'_>, value: NValueId) -> bool {
    match body.values[value.index()].definition {
        NValueDefinition::EntryParam { .. } | NValueDefinition::BlockParam { .. } => true,
        NValueDefinition::Statement { block, statement } => body.blocks[block.index()]
            .statements
            .get(statement as usize)
            .is_some_and(|statement| {
                statement.source.is_some()
                    && matches!(
                        statement.kind,
                        NStatementKind::Define {
                            expr: NExpr::CodeRegionRef { .. }
                                | NExpr::Const(_)
                                | NExpr::Unary { .. }
                                | NExpr::Binary { .. }
                                | NExpr::ArrayRepeat { .. }
                                | NExpr::AggregateMake { .. }
                                | NExpr::MakeHandle { .. }
                                | NExpr::EnumMake { .. }
                                | NExpr::GetEnumTag { .. }
                                | NExpr::IsEnumVariant { .. }
                                | NExpr::CodeRegionOffset { .. }
                                | NExpr::CodeRegionLen { .. }
                                | NExpr::StructuralRepack { .. },
                            ..
                        }
                    )
            }),
    }
}

fn normalized_source_value_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
    source: &SemanticBody<'db>,
    local: SLocalId,
) -> TyId<'db> {
    normalized_source_local_value_ty(db, body.owner, &source.locals[local.index()])
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
                let adt = ty
                    .adt_def(db)
                    .filter(|adt| matches!(adt.adt_ref(db), AdtRef::Enum(_)))?;
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
