use cranelift_entity::EntityRef;

use crate::{
    analysis::{
        place::{Place, PlaceBase, PlaceProjection, projectable_place_ty},
        semantic::{FieldIndex, SExpr, SOperand, SPlace, SValueId, SemOrigin},
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
        self.try_lower_place_expr(expr, false)
    }

    /// Lowers the place named by `expr`, if any. A captured place snapshots
    /// its pointers and indices, so later writes cannot retarget it.
    pub(super) fn try_lower_place_expr(
        &mut self,
        expr: ExprId,
        capture: bool,
    ) -> Option<SPlace<'db>> {
        if let Partial::Present(Expr::Un(inner, UnOp::Deref)) = expr.data(self.db, self.body) {
            let ptr = self.lower_place_operand(*inner, capture);
            return Some(SPlace::deref(ptr));
        }
        if let Some(place) = self.typed_body.expr_place(expr) {
            return Some(self.lower_place_source(place, capture));
        }
        // The frontend's binding-based Place cannot name a temporary pointer.
        // Retain its dereference while selecting fields/elements, so an owned
        // projection consumes the original storage rather than a read snapshot.
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Field(base, _)) => {
                let field = self.typed_body.resolved_field_index(expr)?;
                // A field of a pointer is selected through an implicit `(*base).field`.
                let base_ty = self.projectable_place_ty(self.expr_ty(*base));
                let mut place = if base_ty.as_ptr(self.db).is_some() {
                    match self.try_lower_place_expr(*base, capture) {
                        Some(place) => self.deref_place(place, base_ty, capture),
                        None => SPlace::deref(self.lower_place_operand(*base, capture)),
                    }
                } else {
                    self.try_lower_place_expr(*base, capture)?
                };
                place.push_field(FieldIndex(field));
                Some(place)
            }
            Partial::Present(Expr::Bin(base, index, BinOp::Index))
                if self.typed_body.semantic_expr_lowering(expr).is_none() =>
            {
                let mut place = self.try_lower_place_expr(*base, capture)?;
                let index = self.lower_place_operand(*index, capture);
                if self.typed_body.is_place_entry(expr) {
                    place.push_entry(index);
                } else {
                    place.push_dynamic_index(index);
                }
                Some(place)
            }
            // A projection's result is a place in the grant its carrier names.
            _ if self.typed_body.expr_prop(self.db, expr).access().is_some() => {
                Some(SPlace::new(self.lower_access(expr)))
            }
            _ => None,
        }
    }

    pub(super) fn lower_place_data(&mut self, source_place: &Place<'db>) -> SPlace<'db> {
        self.lower_place_source(source_place, false)
    }

    fn lower_place_source(&mut self, source_place: &Place<'db>, capture: bool) -> SPlace<'db> {
        let PlaceBase::Binding(binding) = source_place.base;
        let (mut place, mut ty) = match self.capture_places.get(&binding) {
            Some((place, ty)) => (place.clone(), *ty),
            None => {
                let local = *self
                    .binding_locals
                    .get(&binding)
                    .expect("binding local should be allocated");
                (SPlace::new(local), self.locals[local.index()].ty)
            }
        };

        for projection in &source_place.projections {
            match *projection {
                PlaceProjection::Deref { .. } => {
                    let ptr_ty = self.projectable_place_ty(ty);
                    place = self.deref_place(place, ptr_ty, capture);
                }
                PlaceProjection::Field { index, .. } => {
                    place.push_field(FieldIndex(index));
                }
                PlaceProjection::Index { index_expr, .. } => {
                    let index = self.lower_place_operand(index_expr, capture);
                    place.push_dynamic_index(index);
                }
                PlaceProjection::Entry { key_expr, .. } => {
                    let key = self.lower_place_operand(key_expr, capture);
                    place.push_entry(key);
                }
            }
            ty = projection.result_ty();
        }

        place
    }

    /// Selects the target of the pointer stored at `place`.
    fn deref_place(
        &mut self,
        mut place: SPlace<'db>,
        ptr_ty: TyId<'db>,
        capture: bool,
    ) -> SPlace<'db> {
        if capture {
            let ptr = self.emit_expr(ptr_ty, SExpr::ReadPlace { place });
            SPlace::deref(ptr)
        } else {
            place.push_deref();
            place
        }
    }

    fn lower_place_operand(&mut self, expr: ExprId, capture: bool) -> SValueId {
        let value = self.lower_expr(expr);
        if capture {
            self.emit_expr_with_origin(
                SemOrigin::Expr(expr),
                self.expr_ty(expr),
                SExpr::UseValue(SOperand::expr(value, expr)),
            )
        } else {
            value
        }
    }
}
