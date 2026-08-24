use crate::analysis::{
    HirAnalysisDb,
    semantic::{FieldIndex, VariantIndex},
    ty::{
        adt_def::{AdtRef, instantiate_adt_field_shape},
        ty_def::{BorrowKind, TyId},
    },
};

use super::guard::IndexParamId;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum CapabilityLeafKind {
    Borrow(BorrowKind),
    View,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum FieldKey {
    Tuple(FieldIndex),
    Struct(FieldIndex),
    Variant(FieldIndex),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum SlotProjection<I> {
    Field(FieldKey),
    VariantField {
        variant: VariantIndex,
        field: FieldIndex,
    },
    Index(I),
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct SlotPath<I>(Vec<SlotProjection<I>>);

impl<I> SlotPath<I> {
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }

    pub(crate) fn from_steps(steps: impl IntoIterator<Item = SlotProjection<I>>) -> Self {
        Self(steps.into_iter().collect())
    }

    pub(crate) fn push(&mut self, projection: SlotProjection<I>) {
        self.0.push(projection);
    }

    pub(crate) fn pop(&mut self) -> Option<SlotProjection<I>> {
        self.0.pop()
    }

    pub(crate) fn as_slice(&self) -> &[SlotProjection<I>] {
        &self.0
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<I: Clone> SlotPath<I> {
    pub(crate) fn map_indices<J>(&self, mut map: impl FnMut(&I) -> J) -> SlotPath<J> {
        SlotPath::from_steps(self.0.iter().map(|projection| match projection {
            SlotProjection::Field(field) => SlotProjection::Field(*field),
            SlotProjection::VariantField { variant, field } => SlotProjection::VariantField {
                variant: *variant,
                field: *field,
            },
            SlotProjection::Index(index) => SlotProjection::Index(map(index)),
        }))
    }
}

impl FieldKey {
    pub(crate) fn index(self) -> FieldIndex {
        match self {
            Self::Tuple(index) | Self::Struct(index) | Self::Variant(index) => index,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct CapabilityShape<'db> {
    pub(crate) direct: Option<CapabilityLeafKind>,
    pub(crate) children: ShapeChildren<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum ShapeChildren<'db> {
    None,
    Product {
        fields: Box<[(FieldKey, ShapeId<'db>)]>,
    },
    Sum {
        variants: Box<[(VariantIndex, ShapeId<'db>)]>,
    },
    Array {
        len: usize,
        elem: ShapeId<'db>,
    },
}

#[salsa::interned]
#[derive(Debug)]
pub(crate) struct ShapeId<'db> {
    #[return_ref]
    pub(crate) data: CapabilityShape<'db>,
}

impl<'db> ShapeId<'db> {
    pub(crate) fn contains_borrow(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.contains(db, |leaf| matches!(leaf, CapabilityLeafKind::Borrow(_)))
    }

    pub(crate) fn contains_capability(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.contains(db, |_| true)
    }

    fn contains(
        self,
        db: &'db dyn HirAnalysisDb,
        predicate: impl Copy + Fn(CapabilityLeafKind) -> bool,
    ) -> bool {
        let shape = self.data(db);
        shape.direct.is_some_and(predicate)
            || match &shape.children {
                ShapeChildren::None => false,
                ShapeChildren::Product { fields } => fields
                    .iter()
                    .any(|(_, child)| child.contains(db, predicate)),
                ShapeChildren::Sum { variants } => variants
                    .iter()
                    .any(|(_, child)| child.contains(db, predicate)),
                ShapeChildren::Array { elem, .. } => elem.contains(db, predicate),
            }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct CapabilitySlot {
    pub(crate) kind: BorrowKind,
    pub(crate) path: SlotPath<IndexParamId>,
}

pub(crate) fn capability_slots<'db>(
    db: &'db dyn HirAnalysisDb,
    shape: ShapeId<'db>,
    include_views: bool,
) -> Vec<CapabilitySlot> {
    fn collect<'db>(
        db: &'db dyn HirAnalysisDb,
        shape: ShapeId<'db>,
        include_views: bool,
        path: &mut SlotPath<IndexParamId>,
        next_binder: &mut u32,
        out: &mut Vec<CapabilitySlot>,
    ) {
        match shape.data(db).direct {
            Some(CapabilityLeafKind::Borrow(kind)) => out.push(CapabilitySlot {
                kind,
                path: path.clone(),
            }),
            Some(CapabilityLeafKind::View) if include_views => out.push(CapabilitySlot {
                kind: BorrowKind::Ref,
                path: path.clone(),
            }),
            Some(CapabilityLeafKind::View) | None => {}
        }

        match &shape.data(db).children {
            ShapeChildren::None => {}
            ShapeChildren::Product { fields } => {
                for (field, child) in fields {
                    path.push(SlotProjection::Field(*field));
                    collect(db, *child, include_views, path, next_binder, out);
                    path.pop();
                }
            }
            ShapeChildren::Sum { variants } => {
                for (variant, child) in variants {
                    let ShapeChildren::Product { fields } = &child.data(db).children else {
                        continue;
                    };
                    for (field, field_shape) in fields {
                        path.push(SlotProjection::VariantField {
                            variant: *variant,
                            field: field.index(),
                        });
                        collect(db, *field_shape, include_views, path, next_binder, out);
                        path.pop();
                    }
                }
            }
            ShapeChildren::Array { elem, .. }
                if if include_views {
                    elem.contains_capability(db)
                } else {
                    elem.contains_borrow(db)
                } =>
            {
                let binder = IndexParamId(*next_binder);
                *next_binder = next_binder
                    .checked_add(1)
                    .expect("capability-slot binder space exhausted");
                path.push(SlotProjection::Index(binder));
                collect(db, *elem, include_views, path, next_binder, out);
                path.pop();
            }
            ShapeChildren::Array { .. } => {}
        }
    }

    let mut slots = Vec::new();
    collect(
        db,
        shape,
        include_views,
        &mut SlotPath::new(),
        &mut 0,
        &mut slots,
    );
    slots
}

pub(crate) fn capability_shape<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> ShapeId<'db> {
    build_shape(db, ty, &mut Vec::new())
}

fn build_shape<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    visiting: &mut Vec<TyId<'db>>,
) -> ShapeId<'db> {
    if let Some((kind, _)) = ty.as_borrow(db) {
        return intern_shape(
            db,
            Some(CapabilityLeafKind::Borrow(kind)),
            ShapeChildren::None,
        );
    }

    if let Some(inner) = ty.as_view(db) {
        if inner.as_capability(db).is_some() {
            return build_shape(db, inner, visiting);
        }
        let inner = build_shape(db, inner, visiting);
        return intern_shape(
            db,
            Some(CapabilityLeafKind::View),
            inner.data(db).children.clone(),
        );
    }

    if visiting.contains(&ty) {
        return empty_shape(db);
    }
    visiting.push(ty);

    let children = if ty.is_array(db) {
        match (ty.array_len(db), ty.generic_args(db).first().copied()) {
            (Some(0) | None, _) | (_, None) => ShapeChildren::None,
            (Some(len), Some(elem)) => ShapeChildren::Array {
                len,
                elem: build_shape(db, elem, visiting),
            },
        }
    } else if ty.is_tuple(db) {
        ShapeChildren::Product {
            fields: product_fields(db, ty.field_types(db), visiting, FieldKey::Tuple),
        }
    } else if let Some(adt) = ty.adt_def(db) {
        match adt.adt_ref(db) {
            AdtRef::Struct(_) => ShapeChildren::Product {
                fields: product_fields(db, ty.field_types(db), visiting, FieldKey::Struct),
            },
            AdtRef::Enum(_) => {
                let mut variants = Vec::new();
                for (variant_idx, variant) in adt.fields(db).iter().enumerate() {
                    let Some(variant_idx) = u16::try_from(variant_idx).ok().map(VariantIndex)
                    else {
                        continue;
                    };
                    let fields = (0..variant.num_types())
                        .filter_map(|field_idx| {
                            let field = u16::try_from(field_idx).ok().map(FieldIndex)?;
                            let field_ty = instantiate_adt_field_shape(
                                db,
                                adt,
                                variant_idx.0 as usize,
                                field_idx,
                                ty.generic_args(db),
                            );
                            Some((
                                FieldKey::Variant(field),
                                build_shape(db, field_ty, visiting),
                            ))
                        })
                        .collect::<Vec<_>>()
                        .into_boxed_slice();
                    variants.push((
                        variant_idx,
                        intern_shape(db, None, ShapeChildren::Product { fields }),
                    ));
                }
                ShapeChildren::Sum {
                    variants: variants.into_boxed_slice(),
                }
            }
        }
    } else {
        ShapeChildren::None
    };

    visiting.pop();
    intern_shape(db, None, children)
}

fn product_fields<'db>(
    db: &'db dyn HirAnalysisDb,
    fields: Vec<TyId<'db>>,
    visiting: &mut Vec<TyId<'db>>,
    key: impl Fn(FieldIndex) -> FieldKey,
) -> Box<[(FieldKey, ShapeId<'db>)]> {
    fields
        .into_iter()
        .enumerate()
        .filter_map(|(idx, ty)| {
            let index = u16::try_from(idx).ok().map(FieldIndex)?;
            Some((key(index), build_shape(db, ty, visiting)))
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn empty_shape<'db>(db: &'db dyn HirAnalysisDb) -> ShapeId<'db> {
    intern_shape(db, None, ShapeChildren::None)
}

fn intern_shape<'db>(
    db: &'db dyn HirAnalysisDb,
    direct: Option<CapabilityLeafKind>,
    children: ShapeChildren<'db>,
) -> ShapeId<'db> {
    ShapeId::new(db, CapabilityShape { direct, children })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_db::HirAnalysisTestDb;

    #[test]
    fn shape_nodes_are_structurally_interned() {
        let db = HirAnalysisTestDb::default();
        let first = intern_shape(
            &db,
            Some(CapabilityLeafKind::Borrow(BorrowKind::Mut)),
            ShapeChildren::None,
        );
        let second = intern_shape(
            &db,
            Some(CapabilityLeafKind::Borrow(BorrowKind::Mut)),
            ShapeChildren::None,
        );

        assert_eq!(first, second);
        assert!(first.contains_borrow(&db));
    }

    #[test]
    fn array_shape_size_does_not_depend_on_declared_length() {
        let db = HirAnalysisTestDb::default();
        let elem = intern_shape(
            &db,
            Some(CapabilityLeafKind::Borrow(BorrowKind::Ref)),
            ShapeChildren::None,
        );
        let array = intern_shape(
            &db,
            None,
            ShapeChildren::Array {
                len: 1_000_000,
                elem,
            },
        );

        assert!(array.contains_borrow(&db));
        assert!(matches!(
            array.data(&db).children,
            ShapeChildren::Array { len: 1_000_000, .. }
        ));
    }
}
