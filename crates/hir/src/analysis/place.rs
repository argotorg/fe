use crate::{
    analysis::{
        HirAnalysisDb,
        ty::ty_check::{LocalBinding, TypedBody},
    },
    hir_def::{BinOp, Body, Expr, ExprId, FieldIndex, Partial},
};
use salsa::Update;

/// A "place" is an assignable location (an lvalue): a base binding plus zero or
/// more projections (field/index).
///
/// Places are used to model effect arguments as implicit references and to
/// select the correct load/store operations based on address space.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct Place<'db> {
    pub base: PlaceBase<'db>,
    pub projections: Vec<PlaceProjection<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum PlaceBase<'db> {
    Binding(LocalBinding<'db>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum PlaceProjection<'db> {
    Field(FieldIndex<'db>),
    Index { index_expr: ExprId },
}

impl<'db> Place<'db> {
    pub fn new(base: PlaceBase<'db>) -> Self {
        Self {
            base,
            projections: Vec::new(),
        }
    }

    pub fn push_projection(&mut self, proj: PlaceProjection<'db>) {
        self.projections.push(proj);
    }

    pub fn is_definitely_place_expr(
        db: &'db dyn HirAnalysisDb,
        typed_body: &TypedBody<'db>,
        expr: ExprId,
    ) -> bool {
        Self::from_expr(db, typed_body, expr).is_some()
    }

    pub fn from_expr(
        db: &'db dyn HirAnalysisDb,
        typed_body: &TypedBody<'db>,
        expr: ExprId,
    ) -> Option<Self> {
        let body = typed_body.body()?;
        Self::from_expr_in_body(db, body, expr, |expr| typed_body.expr_binding(expr))
    }

    pub fn from_expr_in_body<F>(
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: ExprId,
        mut expr_binding: F,
    ) -> Option<Self>
    where
        F: FnMut(ExprId) -> Option<LocalBinding<'db>>,
    {
        let Partial::Present(expr_data) = expr.data(db, body) else {
            return None;
        };

        match expr_data {
            Expr::Path(..) => {
                let binding = expr_binding(expr)?;
                Some(Place::new(PlaceBase::Binding(binding)))
            }
            Expr::Field(base, field) => {
                let field = field.to_opt()?;
                let mut place = Place::from_expr_in_body(db, body, *base, expr_binding)?;
                place.push_projection(PlaceProjection::Field(field));
                Some(place)
            }
            Expr::Bin(base, index, op) if *op == BinOp::Index => {
                let mut place = Place::from_expr_in_body(db, body, *base, expr_binding)?;
                place.push_projection(PlaceProjection::Index { index_expr: *index });
                Some(place)
            }
            _ => None,
        }
    }
}
