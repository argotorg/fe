use super::{
    index::{IndexExpr, IndexSubst},
    semantics::{CapabilityClass, CapabilitySemantics, capability_semantics},
};
use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{FieldIndex, VariantIndex},
        ty::{
            adt_def::{AdtRef, instantiate_adt_field_shape},
            const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy},
            fold::TyFoldable,
            trait_resolution::PredicateListId,
            ty_def::{TyData, TyId},
        },
    },
    hir_def::scope_graph::ScopeId,
};
use num_traits::ToPrimitive;

/// Array lengths preserve the complete normalized const expression. They never
/// share the namespace of runtime values or lexical array-member binders.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ArrayLength<'db> {
    Known(usize),
    Symbolic(ConstTyId<'db>),
}

impl<'db> ArrayLength<'db> {
    fn from_const(db: &'db dyn HirAnalysisDb, value: ConstTyId<'db>) -> Option<Self> {
        match value.data(db) {
            ConstTyData::Evaluated(EvaluatedConstTy::LitInt(value), _) => {
                value.data(db).to_usize().map(Self::Known)
            }
            ConstTyData::TyParam(..)
            | ConstTyData::Abstract(..)
            | ConstTyData::UnEvaluated { .. } => Some(Self::Symbolic(value)),
            ConstTyData::TyVar(..) | ConstTyData::Hole(..) | ConstTyData::Evaluated(..) => None,
        }
    }

    pub fn known(self) -> Option<usize> {
        match self {
            Self::Known(len) => Some(len),
            Self::Symbolic(_) => None,
        }
    }

    pub fn index(self) -> IndexExpr<'db> {
        match self {
            Self::Known(len) => IndexExpr::Const(len),
            Self::Symbolic(value) => IndexExpr::TypeConst(value),
        }
    }

    fn substitute(self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        match subst.apply(self.index()) {
            IndexExpr::Const(len) => Self::Known(len),
            IndexExpr::TypeConst(value) => Self::from_const(db, value)
                .expect("array length specialization must remain a valid const"),
            _ => unreachable!("const substitution cannot introduce runtime or bound variables"),
        }
    }
}

impl<'db> From<ArrayLength<'db>> for IndexExpr<'db> {
    fn from(value: ArrayLength<'db>) -> Self {
        value.index()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CapabilityShape<'db> {
    pub(super) direct: Option<CapabilitySemantics<'db>>,
    pub(super) children: ShapeChildren<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ShapeChildren<'db> {
    None,
    EmptyArray,
    Product(Box<[(FieldIndex, ShapeId<'db>)]>),
    Sum(Box<[(VariantIndex, ShapeId<'db>)]>),
    Array {
        len: ArrayLength<'db>,
        element: ShapeId<'db>,
    },
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
    pub fn children(self, db: &'db dyn HirAnalysisDb) -> &'db ShapeChildren<'db> {
        &self.data(db).children
    }

    pub fn direct(self, db: &'db dyn HirAnalysisDb) -> Option<CapabilitySemantics<'db>> {
        self.data(db).direct
    }

    /// Specialize complete const-expression atoms in both shape lengths and the
    /// semantic types attached to direct capability leaves.
    pub fn substitute(self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        let direct = self.direct(db).map(|mut semantics| {
            semantics.target_ty = semantics.target_ty.fold_with(db, &mut subst.clone());
            semantics.representation_ty = semantics
                .representation_ty
                .fold_with(db, &mut subst.clone());
            semantics
        });
        let children = match self.children(db) {
            ShapeChildren::None => ShapeChildren::None,
            ShapeChildren::EmptyArray => ShapeChildren::EmptyArray,
            ShapeChildren::Product(fields) => ShapeChildren::Product(
                fields
                    .iter()
                    .map(|(field, child)| (*field, child.substitute(db, subst)))
                    .collect(),
            ),
            ShapeChildren::Sum(variants) => ShapeChildren::Sum(
                variants
                    .iter()
                    .map(|(variant, child)| (*variant, child.substitute(db, subst)))
                    .collect(),
            ),
            ShapeChildren::Array { len, element } => {
                let len = len.substitute(db, subst);
                if len == ArrayLength::Known(0) {
                    ShapeChildren::EmptyArray
                } else {
                    ShapeChildren::Array {
                        len,
                        element: element.substitute(db, subst),
                    }
                }
            }
        };
        Self::new(db, CapabilityShape { direct, children })
    }

    pub fn contains_capability(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.direct(db).is_some()
            || match &self.data(db).children {
                ShapeChildren::None | ShapeChildren::EmptyArray => false,
                ShapeChildren::Product(fields) => fields
                    .iter()
                    .any(|(_, child)| child.contains_capability(db)),
                ShapeChildren::Sum(variants) => variants
                    .iter()
                    .any(|(_, child)| child.contains_capability(db)),
                ShapeChildren::Array { len, element } => {
                    *len != ArrayLength::Known(0) && element.contains_capability(db)
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
            let TyData::ConstTy(constant) = ty.generic_args(self.db)[1].data(self.db) else {
                return Err(ShapeError::UnknownArrayLength(ty));
            };
            let len = ArrayLength::from_const(self.db, *constant)
                .ok_or(ShapeError::UnknownArrayLength(ty))?;
            if len == ArrayLength::Known(0) {
                ShapeChildren::EmptyArray
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
