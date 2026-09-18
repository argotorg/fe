//! Verified type views over the same physical referent.
//!
//! A conversion is attached where it occurs. Projection preserves that anchor;
//! reads apply the view and writes apply its inverse. It never changes aliases.
use super::{
    index::{IndexExpr, IndexSubst},
    path::Projection,
    semantics::CapabilitySemantics,
    shape::{ShapeChildren, ShapeError, ShapeId, capability_shape},
    value::{IndexPayload, ValueId, ValueInterner},
};
use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{fold::TyFoldable, trait_resolution::PredicateListId, ty_def::TyId},
    },
    hir_def::scope_graph::ScopeId,
};

/// Created only for an admitted explicit structural conversion. Keeping its
/// normalization context lets nested referent views be derived lazily, including
/// for recursive types, without unfolding an infinite referent graph.
#[salsa::interned]
#[derive(Debug)]
pub struct ReferentRepackId<'db> {
    pub source_ty: TyId<'db>,
    pub target_ty: TyId<'db>,
    pub scope: ScopeId<'db>,
    pub assumptions: PredicateListId<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RepackError<'db> {
    Shape(ShapeError<'db>),
    InvalidProjection,
    SourceShape {
        expected: ShapeId<'db>,
        actual: ShapeId<'db>,
    },
}

/// Payloads with referents retain the conversion separately from value shape.
pub trait RepackPayload<'db>: IndexPayload<'db> {
    fn repack_referent(&self, db: &'db dyn HirAnalysisDb, repack: ReferentRepackId<'db>) -> Self;
}

impl<'db> ReferentRepackId<'db> {
    pub fn inverse(self, db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(
            db,
            self.target_ty(db),
            self.source_ty(db),
            self.scope(db),
            self.assumptions(db),
        )
    }

    pub fn substitute(self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        Self::new(
            db,
            self.source_ty(db).fold_with(db, &mut subst.clone()),
            self.target_ty(db).fold_with(db, &mut subst.clone()),
            self.scope(db),
            self.assumptions(db).fold_with(db, &mut subst.clone()),
        )
    }

    pub fn is_identity(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.source_ty(db) == self.target_ty(db)
    }

    pub fn apply<P: RepackPayload<'db>>(
        self,
        db: &'db dyn HirAnalysisDb,
        values: &mut ValueInterner<'db, P>,
        value: &ValueId<'db, P>,
        suffix: &[Projection<IndexExpr<'db>>],
    ) -> Result<ValueId<'db, P>, RepackError<'db>> {
        let project = |ty| {
            let shape = capability_shape(db, self.scope(db), self.assumptions(db), ty)
                .map_err(RepackError::Shape)?;
            suffix
                .iter()
                .try_fold(shape, |shape, step| project_shape(db, shape, *step))
        };
        let source = project(self.source_ty(db))?;
        let target = project(self.target_ty(db))?;
        if value.shape() != source {
            return Err(RepackError::SourceShape {
                expected: source,
                actual: value.shape(),
            });
        }
        Ok(values.repack_with(
            value,
            target,
            &mut |payload: &P,
                  source: CapabilitySemantics<'db>,
                  target: CapabilitySemantics<'db>| {
                if source.target_ty == target.target_ty {
                    payload.clone()
                } else {
                    payload.repack_referent(
                        db,
                        Self::new(
                            db,
                            source.target_ty,
                            target.target_ty,
                            self.scope(db),
                            self.assumptions(db),
                        ),
                    )
                }
            },
        ))
    }
}

fn project_shape<'db>(
    db: &'db dyn HirAnalysisDb,
    shape: ShapeId<'db>,
    projection: Projection<IndexExpr<'db>>,
) -> Result<ShapeId<'db>, RepackError<'db>> {
    match (shape.children(db), projection) {
        (ShapeChildren::Product(fields), Projection::Field(field)) => fields
            .iter()
            .find_map(|(key, shape)| (*key == field).then_some(*shape)),
        (ShapeChildren::Sum(variants), Projection::VariantField { variant, field }) => variants
            .iter()
            .find(|(key, _)| *key == variant)
            .and_then(|(_, shape)| project_shape(db, *shape, Projection::Field(field)).ok()),
        (ShapeChildren::Array { element, .. }, Projection::Index(_)) => Some(*element),
        _ => None,
    }
    .ok_or(RepackError::InvalidProjection)
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReferentView<'db> {
    pub depth: usize,
    pub repack: ReferentRepackId<'db>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReferentViews<'db>(Vec<ReferentView<'db>>);

impl<'db> ReferentViews<'db> {
    pub fn iter(&self) -> impl Iterator<Item = &ReferentView<'db>> {
        self.0.iter()
    }

    pub fn append(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        depth: usize,
        repack: ReferentRepackId<'db>,
    ) {
        if repack.is_identity(db) {
            return;
        }
        // An explicit A -> ... -> A chain at one physical path is identity.
        // Verified repacks preserve structural member paths.
        if let Some(index) = self
            .0
            .iter()
            .enumerate()
            .rev()
            .take_while(|(_, view)| view.depth == depth)
            .find_map(|(index, view)| {
                (view.repack.source_ty(db) == repack.target_ty(db)).then_some(index)
            })
        {
            self.0.truncate(index);
        } else {
            self.0.push(ReferentView { depth, repack });
        }
    }

    pub fn substitute(&self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        let mut result = Self::default();
        for view in &self.0 {
            result.append(db, view.depth, view.repack.substitute(db, subst));
        }
        result
    }

    pub fn extend_at(&mut self, db: &'db dyn HirAnalysisDb, other: &Self, offset: usize) {
        for view in &other.0 {
            self.append(db, view.depth + offset, view.repack);
        }
    }

    pub fn apply<P: RepackPayload<'db>>(
        &self,
        db: &'db dyn HirAnalysisDb,
        values: &mut ValueInterner<'db, P>,
        value: &ValueId<'db, P>,
        path: &[Projection<IndexExpr<'db>>],
        inverse: bool,
    ) -> Result<ValueId<'db, P>, RepackError<'db>> {
        let mut value = value.clone();
        let apply = |value: &ValueId<'db, P>,
                     view: &ReferentView<'db>,
                     values: &mut ValueInterner<'db, P>| {
            let suffix = path
                .get(view.depth..)
                .ok_or(RepackError::InvalidProjection)?;
            let repack = if inverse {
                view.repack.inverse(db)
            } else {
                view.repack
            };
            repack.apply(db, values, value, suffix)
        };
        if inverse {
            for view in self.0.iter().rev() {
                value = apply(&value, view, values)?;
            }
        } else {
            for view in &self.0 {
                value = apply(&value, view, values)?;
            }
        }
        Ok(value)
    }
}
