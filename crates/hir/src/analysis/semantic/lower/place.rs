use crate::{
    analysis::{
        place::{Place, PlaceBase, PlaceProjection, projectable_place_ty},
        semantic::{FieldIndex, SPlace},
        ty::ty_def::TyId,
    },
    hir_def::ExprId,
};

use super::body::SmirLowerCtxt;

impl<'a, 'db> SmirLowerCtxt<'a, 'db> {
    pub(super) fn projectable_place_ty(&self, ty: TyId<'db>) -> TyId<'db> {
        projectable_place_ty(self.db, ty)
    }

    pub(super) fn lower_place(&mut self, expr: ExprId) -> SPlace<'db> {
        let place = self
            .typed_body
            .expr_place(expr)
            .unwrap_or_else(|| panic!("expected place expression: {expr:?}"));
        self.lower_place_data(place)
    }

    pub(super) fn lower_place_data(&mut self, source_place: &Place<'db>) -> SPlace<'db> {
        let PlaceBase::Binding(binding) = source_place.base;
        let mut place = if let Some(local) = self.binding_locals.get(&binding).copied() {
            SPlace::new(local)
        } else if let (Some(env), Some(field)) = (
            self.closure_env_local,
            self.closure_capture_fields.get(&binding).copied(),
        ) {
            if self
                .closure_capture_tys
                .get(&binding)
                .is_some_and(|capture_ty| {
                    capture_ty.as_capability(self.db).is_some()
                        && *capture_ty != self.binding_ty(binding)
                })
            {
                // A synthetic capability capture stores a carrier for the
                // original binding. Materialize that field as its own local so
                // semantic normalization can make it a `CarrierDerefLocal`
                // root. Explicit `Deref` projections are not valid in
                // normalized runtime paths, and treating the environment field
                // itself as the target would address the carrier slot instead
                // of its referent.
                SPlace::new(self.lower_effect_binding_value(binding))
            } else {
                let mut place = SPlace::new(env);
                place.push_field(field);
                place
            }
        } else {
            panic!("binding local should be allocated: {binding:?}");
        };

        for projection in &source_place.projections {
            match *projection {
                PlaceProjection::Field { index, .. } => {
                    place.push_field(FieldIndex(index));
                }
                PlaceProjection::Index { index_expr, .. } => {
                    let index = self.lower_index_operand(index_expr);
                    place.push_dynamic_index(index.value);
                }
            }
        }

        place
    }
}
