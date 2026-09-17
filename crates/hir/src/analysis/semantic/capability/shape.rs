use super::semantics::{CapabilityClass, CapabilitySemantics, capability_semantics};
use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{FieldIndex, VariantIndex},
        ty::{
            adt_def::{AdtRef, instantiate_adt_field_shape},
            trait_resolution::PredicateListId,
            ty_def::TyId,
        },
    },
    hir_def::scope_graph::ScopeId,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CapabilityShape<'db> {
    pub(super) direct: Option<CapabilitySemantics<'db>>,
    pub(super) children: ShapeChildren<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) enum ShapeChildren<'db> {
    None,
    Product(Box<[(FieldIndex, ShapeId<'db>)]>),
    Sum(Box<[(VariantIndex, ShapeId<'db>)]>),
    Array { len: usize, element: ShapeId<'db> },
}

#[salsa::interned]
#[derive(Debug)]
pub struct ShapeId<'db> {
    #[return_ref]
    pub(super) data: CapabilityShape<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ShapeError<'db> {
    RecursiveValue(TyId<'db>),
    UnresolvedCapability(TyId<'db>),
    UnknownArrayLength(TyId<'db>),
    TooManyFields(TyId<'db>),
}

impl<'db> ShapeId<'db> {
    pub fn direct(self, db: &'db dyn HirAnalysisDb) -> Option<CapabilitySemantics<'db>> {
        self.data(db).direct
    }

    pub fn contains_capability(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.direct(db).is_some()
            || match &self.data(db).children {
                ShapeChildren::None => false,
                ShapeChildren::Product(fields) => fields
                    .iter()
                    .any(|(_, child)| child.contains_capability(db)),
                ShapeChildren::Sum(variants) => variants
                    .iter()
                    .any(|(_, child)| child.contains_capability(db)),
                ShapeChildren::Array { len, element } => {
                    *len != 0 && element.contains_capability(db)
                }
            }
    }
}

pub fn capability_shape<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    ty: TyId<'db>,
) -> Result<ShapeId<'db>, ShapeError<'db>> {
    ShapeCx {
        db,
        scope,
        assumptions,
        visiting: Vec::new(),
    }
    .build(ty)
}

struct ShapeCx<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    visiting: Vec<TyId<'db>>,
}

impl<'db> ShapeCx<'db> {
    fn build(&mut self, ty: TyId<'db>) -> Result<ShapeId<'db>, ShapeError<'db>> {
        let direct = capability_semantics(self.db, self.scope, self.assumptions, ty)
            .map_err(|error| ShapeError::UnresolvedCapability(error.0))?;
        if direct.is_some_and(|semantics| matches!(semantics.class, CapabilityClass::Borrow(_))) {
            // A borrow's target is separate referent state, never representation fields.
            return Ok(ShapeId::new(
                self.db,
                CapabilityShape {
                    direct,
                    children: ShapeChildren::None,
                },
            ));
        }
        if self.visiting.contains(&ty) {
            return Err(ShapeError::RecursiveValue(ty));
        }
        self.visiting.push(ty);
        let children = if let Some(inner) = ty.as_view(self.db) {
            self.build(inner)?.data(self.db).children.clone()
        } else if ty.is_array(self.db) {
            let len = ty
                .array_len(self.db)
                .ok_or(ShapeError::UnknownArrayLength(ty))?;
            if len == 0 {
                ShapeChildren::None
            } else {
                let element = self.build(ty.generic_args(self.db)[0])?;
                ShapeChildren::Array { len, element }
            }
        } else if ty.is_tuple(self.db) || ty.is_struct(self.db) {
            ShapeChildren::Product(self.fields(ty, ty.field_types(self.db))?)
        } else if let Some(adt) = ty.adt_def(self.db)
            && matches!(adt.adt_ref(self.db), AdtRef::Enum(_))
        {
            let mut variants = Vec::new();
            for (index, fields) in adt.fields(self.db).iter().enumerate() {
                let variant =
                    VariantIndex(u16::try_from(index).map_err(|_| ShapeError::TooManyFields(ty))?);
                let field_types = (0..fields.num_types())
                    .map(|field| {
                        instantiate_adt_field_shape(
                            self.db,
                            adt,
                            index,
                            field,
                            ty.generic_args(self.db),
                        )
                    })
                    .collect();
                let children = ShapeChildren::Product(self.fields(ty, field_types)?);
                variants.push((
                    variant,
                    ShapeId::new(
                        self.db,
                        CapabilityShape {
                            direct: None,
                            children,
                        },
                    ),
                ));
            }
            ShapeChildren::Sum(variants.into())
        } else {
            ShapeChildren::None
        };
        self.visiting.pop();
        Ok(ShapeId::new(self.db, CapabilityShape { direct, children }))
    }

    fn fields(
        &mut self,
        ty: TyId<'db>,
        fields: Vec<TyId<'db>>,
    ) -> Result<Box<[(FieldIndex, ShapeId<'db>)]>, ShapeError<'db>> {
        fields
            .into_iter()
            .enumerate()
            .map(|(index, field)| {
                let index =
                    FieldIndex(u16::try_from(index).map_err(|_| ShapeError::TooManyFields(ty))?);
                Ok((index, self.build(field)?))
            })
            .collect()
    }
}
