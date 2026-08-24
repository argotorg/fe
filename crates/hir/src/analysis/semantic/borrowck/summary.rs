use crate::analysis::{
    HirAnalysisDb,
    place::projectable_place_ty,
    semantic::{FieldIndex, VariantIndex},
    ty::{
        adt_def::{AdtRef, instantiate_adt_field_shape},
        ty_def::{BorrowKind, TyId},
    },
};

use super::{
    guard::{ExistentialId, Guard, IndexExpr, IndexSubst},
    shape::{CapabilityLeafKind, ShapeChildren, ShapeId, capability_shape},
};

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct BorrowSummary {
    leaves: Vec<BorrowSummaryLeaf>,
}

impl BorrowSummary {
    pub(crate) fn new(mut leaves: Vec<BorrowSummaryLeaf>) -> Self {
        leaves.sort_unstable();
        leaves.dedup();
        Self { leaves }
    }

    pub fn leaves(&self) -> &[BorrowSummaryLeaf] {
        &self.leaves
    }

    pub fn is_empty(&self) -> bool {
        self.leaves.is_empty()
    }

    pub fn len(&self) -> usize {
        self.leaves.len()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BorrowSummaryLeaf {
    pub kind: BorrowKind,
    pub path: SummaryPath,
    pub sources: Vec<BorrowSourceClause>,
}

impl BorrowSummaryLeaf {
    pub(crate) fn new(
        kind: BorrowKind,
        path: SummaryPath,
        sources: Vec<BorrowSourceClause>,
    ) -> Self {
        let mut sources = sources
            .into_iter()
            .map(BorrowSourceClause::alpha_normalize_existentials)
            .collect::<Vec<_>>();
        sources.sort_unstable();
        sources.dedup();
        Self {
            kind,
            path,
            sources,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BorrowSourceClause {
    pub guard: Guard,
    pub source: BorrowSource,
}

impl BorrowSourceClause {
    fn alpha_normalize_existentials(self) -> Self {
        let mut ordered = self.source.index_exprs();
        for expression in self.guard.index_exprs() {
            if !ordered.contains(&expression) {
                ordered.push(expression);
            }
        }
        let mut unique = Vec::new();
        ordered.retain(|expression| {
            matches!(expression, IndexExpr::Existential(_)) && !unique.contains(expression) && {
                unique.push(*expression);
                true
            }
        });
        let mut subst = IndexSubst::new();
        let mut next = 0;
        for expression in ordered {
            if let IndexExpr::Existential(id) = expression {
                subst.insert(
                    IndexExpr::Existential(id),
                    IndexExpr::Existential(ExistentialId(next)),
                );
                next += 1;
            }
        }
        Self {
            guard: self
                .guard
                .substitute(&subst)
                .expect("alpha-renaming preserves satisfiability"),
            source: self.source.substitute(&subst),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BorrowSource {
    ParamPlace { param: u32, path: SummaryPath },
    ParamCapability { param: u32, slot: SummaryPath },
    AnyAccessible { param: u32, class: AccessClass },
}

impl BorrowSource {
    pub fn param(&self) -> u32 {
        match self {
            Self::ParamPlace { param, .. }
            | Self::ParamCapability { param, .. }
            | Self::AnyAccessible { param, .. } => *param,
        }
    }

    fn index_exprs(&self) -> Vec<IndexExpr> {
        match self {
            Self::ParamPlace { path, .. } | Self::ParamCapability { slot: path, .. } => {
                path.index_exprs()
            }
            Self::AnyAccessible { .. } => Vec::new(),
        }
    }

    fn substitute(&self, subst: &IndexSubst) -> Self {
        match self {
            Self::ParamPlace { param, path } => Self::ParamPlace {
                param: *param,
                path: path.substitute(subst),
            },
            Self::ParamCapability { param, slot } => Self::ParamCapability {
                param: *param,
                slot: slot.substitute(subst),
            },
            Self::AnyAccessible { param, class } => Self::AnyAccessible {
                param: *param,
                class: *class,
            },
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AccessClass {
    Shared,
    Mutable,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SummaryPath(Vec<SummaryProjection>);

impl SummaryPath {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_steps(steps: impl IntoIterator<Item = SummaryProjection>) -> Self {
        Self(steps.into_iter().collect())
    }

    pub fn as_slice(&self) -> &[SummaryProjection] {
        &self.0
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub(crate) fn index_exprs(&self) -> Vec<IndexExpr> {
        self.0
            .iter()
            .filter_map(|projection| match projection {
                SummaryProjection::Index(index) => Some(*index),
                SummaryProjection::Field(_) | SummaryProjection::VariantField { .. } => None,
            })
            .collect()
    }

    pub(crate) fn substitute(&self, subst: &IndexSubst) -> Self {
        Self::from_steps(self.0.iter().map(|projection| match projection {
            SummaryProjection::Field(field) => SummaryProjection::Field(*field),
            SummaryProjection::VariantField { variant, field } => SummaryProjection::VariantField {
                variant: *variant,
                field: *field,
            },
            SummaryProjection::Index(index) => SummaryProjection::Index(subst.apply(*index)),
        }))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SummaryProjection {
    Field(FieldIndex),
    VariantField {
        variant: VariantIndex,
        field: FieldIndex,
    },
    Index(IndexExpr),
}

pub(crate) fn validate_borrow_summary<'db>(
    db: &'db dyn HirAnalysisDb,
    result_ty: TyId<'db>,
    argument_tys: &[TyId<'db>],
    summary: &BorrowSummary,
) -> Result<(), String> {
    let result_shape = capability_shape(db, result_ty);
    for leaf in summary.leaves() {
        if shape_for_summary_path(db, result_shape, &leaf.path)
            .filter(|shape| {
                matches!(
                    shape.data(db).direct,
                    Some(CapabilityLeafKind::Borrow(kind)) if kind == leaf.kind
                )
            })
            .is_none()
        {
            return Err(format!(
                "callee borrow summary contains invalid result slot {:?}",
                leaf.path
            ));
        }
        let result_params = leaf
            .path
            .index_exprs()
            .into_iter()
            .filter_map(|expression| match expression {
                IndexExpr::ResultParam(param) => Some(param),
                _ => None,
            })
            .collect::<Vec<_>>();
        if leaf.path.index_exprs().into_iter().any(|expression| {
            !matches!(expression, IndexExpr::Const(_) | IndexExpr::ResultParam(_))
        }) {
            return Err("callee borrow summary has an unbound result index".to_string());
        }
        for clause in &leaf.sources {
            let Some(argument_ty) = argument_tys.get(clause.source.param() as usize).copied()
            else {
                return Err(format!(
                    "callee borrow summary references missing input {}",
                    clause.source.param()
                ));
            };
            let source_indices = match &clause.source {
                BorrowSource::ParamPlace { path, .. }
                | BorrowSource::ParamCapability { slot: path, .. } => path.index_exprs(),
                BorrowSource::AnyAccessible { .. } => Vec::new(),
            };
            if clause
                .guard
                .index_exprs()
                .into_iter()
                .chain(source_indices)
                .any(|expression| match expression {
                    IndexExpr::Const(_) | IndexExpr::Existential(_) => false,
                    IndexExpr::ResultParam(param) => !result_params.contains(&param),
                    IndexExpr::InputParam(param) => param as usize >= argument_tys.len(),
                    IndexExpr::Runtime(_) | IndexExpr::ValueParam(_) | IndexExpr::LoanParam(_) => {
                        true
                    }
                })
            {
                return Err("callee borrow summary has an out-of-scope source index".to_string());
            }
            let valid_source = match &clause.source {
                BorrowSource::ParamPlace { path, .. } => {
                    summary_place_path_ty(db, argument_ty, path).is_some()
                }
                BorrowSource::ParamCapability { slot, .. } => {
                    shape_for_summary_path(db, capability_shape(db, argument_ty), slot).is_some_and(
                        |shape| {
                            matches!(
                                shape.data(db).direct,
                                Some(CapabilityLeafKind::Borrow(kind))
                                    if leaf.kind == BorrowKind::Ref || kind == BorrowKind::Mut
                            )
                        },
                    )
                }
                BorrowSource::AnyAccessible { class, .. } => {
                    leaf.kind == BorrowKind::Ref || *class == AccessClass::Mutable
                }
            };
            if !valid_source {
                return Err("callee borrow summary contains an invalid source slot".to_string());
            }
        }
    }
    Ok(())
}

pub(crate) fn shape_for_summary_path<'db>(
    db: &'db dyn HirAnalysisDb,
    mut shape: ShapeId<'db>,
    path: &SummaryPath,
) -> Option<ShapeId<'db>> {
    for projection in path.as_slice() {
        shape =
            match (projection, &shape.data(db).children) {
                (SummaryProjection::Field(field), ShapeChildren::Product { fields }) => fields
                    .iter()
                    .find_map(|(key, shape)| (key.index() == *field).then_some(*shape))?,
                (
                    SummaryProjection::VariantField { variant, field },
                    ShapeChildren::Sum { variants },
                ) => {
                    let variant_shape = variants.iter().find_map(|(candidate, shape)| {
                        (*candidate == *variant).then_some(*shape)
                    })?;
                    let ShapeChildren::Product { fields } = &variant_shape.data(db).children else {
                        return None;
                    };
                    fields
                        .iter()
                        .find_map(|(key, shape)| (key.index() == *field).then_some(*shape))?
                }
                (SummaryProjection::Index(_), ShapeChildren::Array { elem, .. }) => *elem,
                _ => return None,
            };
    }
    Some(shape)
}

fn summary_place_path_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    mut ty: TyId<'db>,
    path: &SummaryPath,
) -> Option<TyId<'db>> {
    for projection in path.as_slice() {
        ty = projectable_place_ty(db, ty);
        ty = match projection {
            SummaryProjection::Field(field) => *ty.field_types(db).get(field.0 as usize)?,
            SummaryProjection::VariantField { variant, field } => {
                let adt = ty.adt_def(db)?;
                if !matches!(adt.adt_ref(db), AdtRef::Enum(_)) {
                    return None;
                }
                instantiate_adt_field_shape(
                    db,
                    adt,
                    variant.0 as usize,
                    field.0 as usize,
                    ty.generic_args(db),
                )
            }
            SummaryProjection::Index(index) => {
                if !ty.is_array(db)
                    || matches!(index, IndexExpr::Const(index) if ty.array_len(db).is_some_and(|len| *index >= len))
                {
                    return None;
                }
                *ty.generic_args(db).first()?
            }
        };
    }
    Some(ty)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_existentials_are_clause_local_and_alpha_normalized() {
        let clause = |id| BorrowSourceClause {
            guard: Guard::equal(IndexExpr::Existential(id), IndexExpr::Const(1))
                .expect("valid guard"),
            source: BorrowSource::ParamCapability {
                param: 0,
                slot: SummaryPath::from_steps([SummaryProjection::Index(IndexExpr::Existential(
                    id,
                ))]),
            },
        };
        let left = BorrowSummaryLeaf::new(
            BorrowKind::Mut,
            SummaryPath::new(),
            vec![clause(ExistentialId(3))],
        );
        let right = BorrowSummaryLeaf::new(
            BorrowKind::Mut,
            SummaryPath::new(),
            vec![clause(ExistentialId(19))],
        );

        assert_eq!(left, right);
    }
}
