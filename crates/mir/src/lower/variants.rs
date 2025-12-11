//! Variant lowering helpers for MIR: handles enum constructor calls and unit variant paths.

use super::*;
use hir::projection::Projection;

impl<'db, 'a> MirBuilder<'db, 'a> {
    /// Lowers an enum variant constructor call into allocation and payload/discriminant stores.
    ///
    /// # Parameters
    /// - `block`: Block to start lowering in.
    /// - `expr`: Call expression id for the variant ctor.
    ///
    /// # Returns
    /// Successor block and the allocated variant value when applicable.
    pub(super) fn try_lower_variant_ctor(
        &mut self,
        block: BasicBlockId,
        expr: ExprId,
    ) -> Option<(Option<BasicBlockId>, ValueId)> {
        let Partial::Present(Expr::Call(_, call_args)) = expr.data(self.db, self.body) else {
            return None;
        };
        let callable = self.typed_body.callable_expr(expr)?;
        let CallableDef::VariantCtor(variant) = callable.callable_def else {
            return None;
        };

        let mut current = Some(block);
        let mut lowered_args = Vec::with_capacity(call_args.len());
        for arg in call_args.iter() {
            let Some(curr_block) = current else {
                break;
            };
            let (next_block, value) = self.lower_expr_in(curr_block, arg.expr);
            current = next_block;
            lowered_args.push(value);
        }

        let enum_ty = self.typed_body.expr_ty(self.db, expr);
        let total_size = self.enum_size_bytes(enum_ty).unwrap_or(64);

        let Some(curr_block) = current else {
            let value_id = self.ensure_value(expr);
            return Some((None, value_id));
        };

        let value_id = self.emit_alloc(expr, curr_block, enum_ty, total_size);
        self.emit_store_discriminant(expr, curr_block, value_id, variant);

        let addr_space = self.value_address_space(value_id);
        for (field_idx, field_value) in lowered_args.iter().enumerate() {
            let place = Place::new(
                value_id,
                MirProjectionPath::from_projection(Projection::VariantField {
                    variant,
                    enum_ty,
                    field_idx,
                }),
                addr_space,
            );
            self.push_inst(
                curr_block,
                MirInst::Store {
                    expr,
                    place,
                    value: *field_value,
                },
            );
        }

        Some((Some(curr_block), value_id))
    }

    /// Lowers a unit variant path expression into an allocation plus discriminant store.
    ///
    /// # Parameters
    /// - `block`: Current basic block.
    /// - `expr`: Path expression id resolving to a unit variant.
    ///
    /// # Returns
    /// Successor block and the allocated variant value when applicable.
    pub(super) fn try_lower_unit_variant(
        &mut self,
        block: BasicBlockId,
        expr: ExprId,
    ) -> Option<(Option<BasicBlockId>, ValueId)> {
        let Partial::Present(Expr::Path(path)) = expr.data(self.db, self.body) else {
            return None;
        };
        let path = path.to_opt()?;
        let scope = self.typed_body.body()?.scope();
        let variant = self.resolve_enum_variant(path, scope)?;

        if !matches!(variant.kind(self.db), VariantKind::Unit) {
            return None;
        }

        let enum_ty = self.typed_body.expr_ty(self.db, expr);

        let enum_size = self.enum_size_bytes(enum_ty).unwrap_or(64);

        let curr_block = block;

        let value_id = self.emit_alloc(expr, curr_block, enum_ty, enum_size);
        self.emit_store_discriminant(expr, curr_block, value_id, variant.variant);

        Some((Some(curr_block), value_id))
    }
}
