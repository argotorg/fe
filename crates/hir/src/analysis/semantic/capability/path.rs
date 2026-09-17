use super::index::{IndexExpr, IndexSubst};
use crate::analysis::semantic::{FieldIndex, VariantIndex};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Projection<I> {
    Field(FieldIndex),
    VariantField {
        variant: VariantIndex,
        field: FieldIndex,
    },
    Index(I),
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
                $path(
                    self.0
                        .iter()
                        .map(|step| match step {
                            Projection::Field(field) => Projection::Field(*field),
                            Projection::VariantField { variant, field } => {
                                Projection::VariantField {
                                    variant: *variant,
                                    field: *field,
                                }
                            }
                            Projection::Index(index) => Projection::Index(map(index)),
                        })
                        .collect(),
                )
            }
        }
        impl<I: Clone> $path<I> {
            pub fn appended(&self, step: Projection<I>) -> Self {
                let mut steps = self.0.to_vec();
                steps.push(step);
                Self(steps.into())
            }
        }
        impl $path<IndexExpr> {
            pub fn substitute(&self, subst: &IndexSubst) -> Self {
                self.map_indices(|index| subst.apply(*index))
            }
            pub fn indices(&self) -> impl Iterator<Item = IndexExpr> + '_ {
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
