//! Variant lowering helpers for MIR: handles enum constructor calls and unit variant paths.

use crate::layout;

use super::*;
use num_bigint::BigUint;

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

        let variant_index = variant.idx as u64;

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

        let value_id = self.emit_alloc(expr, curr_block, total_size);
        self.emit_store_discriminant(expr, curr_block, value_id, variant_index);

        let mut stores = Vec::with_capacity(lowered_args.len());
        for (field_idx, arg_value) in lowered_args.iter().enumerate() {
            let arg_ty = self.typed_body.expr_ty(self.db, call_args[field_idx].expr);
            let offset = layout::variant_field_offset_bytes_or_word_aligned(
                self.db, enum_ty, variant, field_idx,
            );
            stores.push((offset, arg_ty, *arg_value));
        }
        let ptr_ty = match self.value_address_space(value_id) {
            AddressSpaceKind::Memory => self.core.helper_ty(CoreHelperTy::MemPtr),
            AddressSpaceKind::Storage => self.core.helper_ty(CoreHelperTy::StorPtr),
        };
        for (offset_bytes, field_ty, field_value) in stores {
            let callable =
                self.core
                    .make_callable(expr, CoreHelper::StoreVariantField, &[ptr_ty, field_ty]);
            let offset_value = self.synthetic_u256(BigUint::from(offset_bytes));
            let store_ret_ty = callable.ret_ty(self.db);
            let store_call = self.mir_body.alloc_value(ValueData {
                ty: store_ret_ty,
                origin: ValueOrigin::Call(CallOrigin {
                    expr,
                    callable,
                    args: vec![value_id, offset_value, field_value],
                    effect_args: Vec::new(),
                    effect_kinds: Vec::new(),
                    receiver_space: None,
                    resolved_name: None,
                }),
            });

            self.push_inst(
                curr_block,
                MirInst::EvalExpr {
                    expr,
                    value: store_call,
                    bind_value: false,
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

        let variant_index = variant.variant.idx as u64;
        let enum_ty = self.typed_body.expr_ty(self.db, expr);

        let enum_size = self.enum_size_bytes(enum_ty).unwrap_or(64);

        let curr_block = block;

        let value_id = self.emit_alloc(expr, curr_block, enum_size);
        self.emit_store_discriminant(expr, curr_block, value_id, variant_index);

        Some((Some(curr_block), value_id))
    }
}
