use super::index::{IndexExpr, IndexSubst};
use crate::analysis::{
    HirAnalysisDb,
    semantic::{FieldIndex, SemanticInstance, VariantIndex},
    ty::ty_def::TyId,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Projection<I> {
    Field(FieldIndex),
    VariantField {
        variant: VariantIndex,
        field: FieldIndex,
    },
    Index(I),
}

impl<I> Projection<I> {
    pub fn map_index<J>(&self, map: impl FnOnce(&I) -> J) -> Projection<J> {
        match self {
            Self::Field(field) => Projection::Field(*field),
            Self::VariantField { variant, field } => Projection::VariantField {
                variant: *variant,
                field: *field,
            },
            Self::Index(index) => Projection::Index(map(index)),
        }
    }
}

/// Slots within a semantic value. A path never implicitly dereferences a capability.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructuralPath<I>(Box<[Projection<I>]>);

/// Projections into referent storage; distinct from structural capability slots.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegionPath<I>(Box<[Projection<I>]>);

macro_rules! path_impl {
    ($path:ident) => {
        impl<I> Default for $path<I> {
            fn default() -> Self {
                Self(Box::new([]))
            }
        }
        impl<I> $path<I> {
            pub fn new(steps: impl Into<Box<[Projection<I>]>>) -> Self {
                Self(steps.into())
            }
            pub fn as_slice(&self) -> &[Projection<I>] {
                &self.0
            }
            pub fn is_empty(&self) -> bool {
                self.0.is_empty()
            }
            pub fn map_indices<J>(&self, mut map: impl FnMut(&I) -> J) -> $path<J> {
                $path(self.0.iter().map(|step| step.map_index(&mut map)).collect())
            }
        }
        impl<I: Clone> $path<I> {
            pub fn concat(&self, suffix: &Self) -> Self {
                Self(self.0.iter().chain(suffix.0.iter()).cloned().collect())
            }
            pub fn appended(&self, step: Projection<I>) -> Self {
                let mut steps = self.0.to_vec();
                steps.push(step);
                Self(steps.into())
            }
        }
        impl<'db> $path<IndexExpr<'db>> {
            pub fn substitute(&self, subst: &IndexSubst<'db>) -> Self {
                self.map_indices(|index| subst.apply(*index))
            }
            pub fn indices(&self) -> impl Iterator<Item = IndexExpr<'db>> + '_ {
                self.0.iter().filter_map(|step| match step {
                    Projection::Index(index) => Some(*index),
                    _ => None,
                })
            }
        }
    };
}
path_impl!(StructuralPath);
path_impl!(RegionPath);

/// Project a referent type through structural storage, without following capabilities.
pub fn project_referent_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    semantic: SemanticInstance<'db>,
    mut ty: TyId<'db>,
    path: &[Projection<IndexExpr<'db>>],
) -> Option<TyId<'db>> {
    for step in path {
        ty = ty.as_view(db).unwrap_or(ty);
        ty = match step {
            Projection::Field(field) => *semantic
                .normalized_field_types(db, ty)
                .get(usize::from(field.0))?,
            Projection::VariantField { variant, field } => *semantic
                .normalized_enum_variant_field_tys(db, ty, *variant)
                .get(usize::from(field.0))?,
            Projection::Index(_) if ty.is_array(db) => *ty.generic_args(db).first()?,
            Projection::Index(_) => return None,
        };
    }
    Some(ty)
}
