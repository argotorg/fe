use cranelift_entity::EntityRef;

use crate::{
    analysis::{
        place::{Place, PlaceBase, PlaceProjection, projectable_place_ty},
        semantic::{FieldIndex, SExpr, SOperand, SPlace, SemOrigin},
        ty::ty_def::TyId,
    },
    hir_def::{Expr, ExprId, Partial, UnOp, expr::BinOp},
};

use super::body::SmirLowerCtxt;

impl<'a, 'db> SmirLowerCtxt<'a, 'db> {
    pub(super) fn projectable_place_ty(&self, ty: TyId<'db>) -> TyId<'db> {
        projectable_place_ty(self.db, ty)
    }

    pub(super) fn lower_place(&mut self, expr: ExprId) -> SPlace<'db> {
        self.try_lower_place(expr)
            .unwrap_or_else(|| panic!("expected place expression: {expr:?}"))
    }

    pub(super) fn try_lower_place(&mut self, expr: ExprId) -> Option<SPlace<'db>> {
        if let Partial::Present(Expr::Un(inner, UnOp::Deref)) = expr.data(self.db, self.body) {
            let inner_ty = self.expr_ty(*inner);
            if let Some((_, ptr_ty)) = inner_ty.as_capability(self.db)
                && ptr_ty.as_ptr(self.db).is_some()
                && let Some(place) = self.typed_body.expr_place(*inner)
            {
                let place = self.lower_place_data(place);
                let ptr = self.emit_expr_with_origin(
                    SemOrigin::Expr(*inner),
                    ptr_ty,
                    SExpr::ReadPlace { place },
                );
                return Some(SPlace::deref(ptr));
            }
            let ptr = self.lower_expr(*inner);
            return Some(SPlace::deref(ptr));
        }
        if let Some(place) = self.typed_body.expr_place(expr) {
            return Some(self.lower_place_data(place));
        }
        // The frontend's binding-based Place cannot name a temporary pointer.
        // Retain its dereference while selecting fields/elements, so an owned
        // projection consumes the original storage rather than a read snapshot.
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Field(base, _)) => {
                let mut place = self.try_lower_place(*base)?;
                let field = self.typed_body.resolved_field_index(expr)?;
                place.push_field(FieldIndex(field));
                Some(place)
            }
            Partial::Present(Expr::Bin(base, index, BinOp::Index))
                if self.typed_body.semantic_expr_lowering(expr).is_none() =>
            {
                let mut place = self.try_lower_place(*base)?;
                let index = self.lower_expr(*index);
                place.push_dynamic_index(index);
                Some(place)
            }
            _ => None,
        }
    }

    pub(super) fn lower_place_data(&mut self, source_place: &Place<'db>) -> SPlace<'db> {
        self.lower_place_source(source_place, false)
    }

    pub(super) fn capture_place(&mut self, source_place: &Place<'db>) -> SPlace<'db> {
        self.lower_place_source(source_place, true)
    }

    fn lower_place_source(&mut self, source_place: &Place<'db>, capture: bool) -> SPlace<'db> {
        let PlaceBase::Binding(binding) = source_place.base;
        let local = *self
            .binding_locals
            .get(&binding)
            .expect("binding local should be allocated");
        let mut place = SPlace::new(local);
        let mut ty = self.locals[local.index()].ty;

        for projection in &source_place.projections {
            match *projection {
                PlaceProjection::Deref { .. } => {
                    if capture {
                        let ptr_ty = self.projectable_place_ty(ty);
                        let ptr = self.emit_expr(ptr_ty, SExpr::ReadPlace { place });
                        place = SPlace::deref(ptr);
                    } else {
                        place.push_deref();
                    }
                }
                PlaceProjection::Field { index, .. } => {
                    place.push_field(FieldIndex(index));
                }
                PlaceProjection::Index { index_expr, .. } => {
                    let value = self.lower_expr(index_expr);
                    let index = if capture {
                        self.emit_expr_with_origin(
                            SemOrigin::Expr(index_expr),
                            self.expr_ty(index_expr),
                            SExpr::UseValue(SOperand::expr(value, index_expr)),
                        )
                    } else {
                        value
                    };
                    place.push_dynamic_index(index);
                }
            }
            ty = projection.result_ty();
        }

        place
    }
}
