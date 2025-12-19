//! Variant lowering helpers for MIR: handles enum constructor calls and unit variant paths.

use super::*;
use hir::projection::Projection;
use num_bigint::BigUint;

impl<'db, 'a> MirBuilder<'db, 'a> {
    /// Lowers an enum variant constructor call into allocation and payload/discriminant stores.
    ///
    /// # Parameters
    /// - `expr`: Call expression id for the variant ctor.
    ///
    /// # Returns
    /// The allocated variant value when applicable.
    pub(super) fn try_lower_variant_ctor(&mut self, expr: ExprId) -> Option<ValueId> {
        let Partial::Present(Expr::Call(_, call_args)) = expr.data(self.db, self.body) else {
            return None;
        };
        let callable = self.typed_body.callable_expr(expr)?;
        let CallableDef::VariantCtor(variant) = callable.callable_def else {
            return None;
        };

        let mut lowered_args = Vec::with_capacity(call_args.len());
        for arg in call_args.iter() {
            if self.current_block().is_none() {
                return Some(self.ensure_value(expr));
            }
            lowered_args.push(self.lower_expr(arg.expr));
        }

        let enum_ty = self.typed_body.expr_ty(self.db, expr);
        if self.current_block().is_none() {
            return Some(self.ensure_value(expr));
        }

        let value_id = self.emit_alloc(expr, enum_ty);

        let addr_space = self.value_address_space(value_id);
        let mut inits = Vec::with_capacity(1 + lowered_args.len());
        let discr_value = self.synthetic_u256(BigUint::from(variant.idx as u64));
        inits.push((
            MirProjectionPath::from_projection(Projection::Discriminant),
            discr_value,
        ));
        for (field_idx, field_value) in lowered_args.iter().enumerate() {
            let projection = MirProjectionPath::from_projection(Projection::VariantField {
                variant,
                enum_ty,
                field_idx,
            });
            inits.push((projection, *field_value));
        }
        let place = Place::new(value_id, MirProjectionPath::new(), addr_space);
        self.push_inst_here(MirInst::InitAggregate { expr, place, inits });

        Some(value_id)
    }

    /// Lowers a unit variant path expression into an allocation plus discriminant store.
    ///
    /// # Parameters
    /// - `expr`: Path expression id resolving to a unit variant.
    ///
    /// # Returns
    /// The allocated variant value when applicable.
    pub(super) fn try_lower_unit_variant(&mut self, expr: ExprId) -> Option<ValueId> {
        let Partial::Present(Expr::Path(path)) = expr.data(self.db, self.body) else {
            return None;
        };
        let Some(block) = self.current_block() else {
            return Some(self.ensure_value(expr));
        };
        let path = path.to_opt()?;
        let scope = self.typed_body.body()?.scope();
        let variant = self.resolve_enum_variant(path, scope)?;

        if !matches!(variant.kind(self.db), VariantKind::Unit) {
            return None;
        }

        let enum_ty = self.typed_body.expr_ty(self.db, expr);

        self.move_to_block(block);
        let value_id = self.emit_alloc(expr, enum_ty);
        let addr_space = self.value_address_space(value_id);
        let discr_value = self.synthetic_u256(BigUint::from(variant.variant.idx as u64));
        let inits = vec![(
            MirProjectionPath::from_projection(Projection::Discriminant),
            discr_value,
        )];
        let place = Place::new(value_id, MirProjectionPath::new(), addr_space);
        self.push_inst_here(MirInst::InitAggregate { expr, place, inits });

        Some(value_id)
    }
}
