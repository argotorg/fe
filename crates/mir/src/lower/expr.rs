//! Expression and statement lowering for MIR: handles blocks, control flow, calls, and dispatches
//! to specialized lowering helpers.

use hir::{
    analysis::ty::{
        const_eval::{ConstValue, eval_const_expr},
        ty_check::{Callable, ForLoopSeq, ResolvedEffectArg},
        ty_def::{CapabilityKind, PrimTy, TyBase, TyData, prim_int_bits},
    },
    projection::{IndexSource, Projection},
};

use hir::analysis::ty::effects::EffectKeyKind;

use crate::{
    ir::{Place, Rvalue, SourceInfoId, try_value_address_space_in},
    layout,
};

use super::*;
use hir::analysis::{
    place::PlaceBase,
    ty::ty_check::{EffectArg, EffectPassMode},
};
use hir::hir_def::{
    EnumVariant,
    expr::{ArithBinOp, BinOp},
};

enum RootLvalue<'db> {
    Place(Place<'db>),
    Local(LocalId),
}

/// Parameters for lowering a for-loop statement.
struct ForLoopParams {
    stmt: StmtId,
    pat: PatId,
    iter_expr: ExprId,
    body_expr: ExprId,
    /// Unroll hint from attributes:
    /// - `None`: auto-unroll if < 10 iterations
    /// - `Some(true)`: #[unroll] forces unrolling
    /// - `Some(false)`: #[no_unroll] prevents unrolling
    unroll_hint: Option<bool>,
}

impl<'db, 'a> MirBuilder<'db, 'a> {
    /// Try to lower a `size_of<T>()` or `encoded_size<T>()` call to a constant.
    fn try_lower_size_intrinsic_call(&mut self, expr: ExprId) -> Option<ValueId> {
        let callable = self.typed_body.callable_expr(expr)?;
        let ingot_kind = callable.callable_def.ingot(self.db).kind(self.db);
        let name = callable.callable_def.name(self.db)?;

        // Get the type argument from the callable's generic args
        let ty = *callable.generic_args().first()?;

        let size_bytes = match (ingot_kind, name.data(self.db).as_str()) {
            (IngotKind::Core, "size_of") => layout::ty_size_bytes(self.db, ty)?,
            (IngotKind::Std, "encoded_size") => self.abi_static_size_bytes(ty)?,
            _ => return None,
        };

        let value_id = self.ensure_value(expr);
        self.builder.body.values[value_id.index()].origin =
            ValueOrigin::Synthetic(SyntheticValue::Int(BigUint::from(size_bytes)));
        Some(value_id)
    }

    fn try_lower_const_keccak_call(&mut self, expr: ExprId) -> Option<ValueId> {
        let callable = self.typed_body.callable_expr(expr)?;
        if !matches!(
            callable.callable_def.ingot(self.db).kind(self.db),
            IngotKind::Core
        ) {
            return None;
        }

        let name = callable.callable_def.name(self.db)?;
        if name.data(self.db) != "keccak" {
            return None;
        }

        let ConstValue::Int(value) =
            eval_const_expr(self.db, self.body, self.typed_body, self.generic_args, expr)
                .ok()
                .flatten()?
        else {
            return None;
        };

        let value_id = self.ensure_value(expr);
        self.builder.body.values[value_id.index()].origin =
            ValueOrigin::Synthetic(SyntheticValue::Int(value));
        Some(value_id)
    }

    fn u256_lit_from_expr(&self, expr: ExprId) -> Option<BigUint> {
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Lit(LitKind::Int(int_id))) => Some(int_id.data(self.db).clone()),
            _ => None,
        }
    }

    fn negated_int_literal_word(&self, expr: ExprId, inner: ExprId) -> Option<BigUint> {
        let value = self.u256_lit_from_expr(inner)?;
        let expr_ty = self
            .typed_body
            .expr_ty(self.db, expr)
            .as_capability(self.db)
            .map(|(_, inner)| inner)
            .unwrap_or_else(|| self.typed_body.expr_ty(self.db, expr));
        let TyData::TyBase(TyBase::Prim(prim)) = expr_ty.base_ty(self.db).data(self.db) else {
            return None;
        };
        if !matches!(
            prim,
            PrimTy::I8
                | PrimTy::I16
                | PrimTy::I32
                | PrimTy::I64
                | PrimTy::I128
                | PrimTy::I256
                | PrimTy::Isize
        ) {
            return None;
        }

        let bits = prim_int_bits(*prim)?;
        let modulus = BigUint::from(1u8) << bits;
        Some((&modulus - (&value % &modulus)) % modulus)
    }

    fn try_lower_negated_int_literal(&mut self, expr: ExprId, inner: ExprId) -> Option<ValueId> {
        let value = self.negated_int_literal_word(expr, inner)?;
        let value_id = self.ensure_value(expr);
        self.builder.body.values[value_id.index()].origin =
            ValueOrigin::Synthetic(SyntheticValue::Int(value));
        Some(value_id)
    }

    fn lower_index_source(&mut self, expr: ExprId) -> IndexSource<ValueId> {
        self.u256_lit_from_expr(expr)
            .and_then(|lit| lit.to_usize())
            .map(IndexSource::Constant)
            .unwrap_or_else(|| IndexSource::Dynamic(self.lower_expr(expr)))
    }

    fn set_expr_value_from_lowered_value(&mut self, value_id: ValueId, lowered: ValueId) {
        if value_id == lowered {
            return;
        }

        let lowered_data = self.builder.body.value(lowered).clone();
        self.builder.body.values[value_id.index()].origin = lowered_data.origin;
        self.builder.body.values[value_id.index()].repr = lowered_data.repr;
        self.refresh_value_pointer_info(value_id);
    }

    fn mark_place_root_address_taken(&mut self, place: &Place<'db>) {
        let Some((local, projection)) =
            crate::ir::resolve_local_projection_root(&self.builder.body.values, place.base)
        else {
            return;
        };
        if !projection.is_empty()
            || !matches!(
                crate::repr::repr_kind_for_ty(
                    self.db,
                    &self.core,
                    self.builder.body.local(local).ty,
                ),
                crate::repr::ReprKind::Word
            )
        {
            return;
        }
        self.address_taken_locals.insert(local);
    }

    fn alloc_place_ref_value(
        &mut self,
        ty: TyId<'db>,
        place: Place<'db>,
        repr: ValueRepr,
    ) -> ValueId {
        self.mark_place_root_address_taken(&place);
        self.alloc_value(ty, ValueOrigin::PlaceRef(place), repr)
    }

    fn projection_source_value(
        &mut self,
        expr: ExprId,
        place: Place<'db>,
        ty: TyId<'db>,
        addr_space: AddressSpaceKind,
        temp_name: &'static str,
    ) -> ValueId {
        if self.is_by_ref_ty(ty) {
            return self.alloc_place_ref_value(ty, place, ValueRepr::Ref(addr_space));
        }

        let dest = self.alloc_temp_local(ty, false, temp_name);
        let load_space = self.load_result_address_space(expr, ty, addr_space);
        self.builder.body.locals[dest.index()].address_space = load_space;
        self.assign(None, Some(dest), Rvalue::Load { place });
        self.alloc_value(
            ty,
            ValueOrigin::Local(dest),
            self.value_repr_for_ty(ty, load_space),
        )
    }

    fn coerce_capability_value_to_target_ty(
        &mut self,
        expr: ExprId,
        source_value: ValueId,
        source_ty: TyId<'db>,
        target_ty: TyId<'db>,
        target_repr: ValueRepr,
    ) -> ValueId {
        if target_ty == source_ty {
            return source_value;
        }

        let Some((kind, inner_ty)) = source_ty.as_capability(self.db) else {
            return source_value;
        };
        debug_assert_eq!(
            target_ty,
            inner_ty,
            "unexpected capability coercion from `{}` to `{}`",
            source_ty.pretty_print(self.db),
            target_ty.pretty_print(self.db),
        );
        if matches!(kind, CapabilityKind::View) {
            return self.alloc_value(
                target_ty,
                ValueOrigin::TransparentCast {
                    value: source_value,
                },
                target_repr,
            );
        }

        if let Some(place) = self.place_from_derefable_value(source_value, source_ty) {
            let place_space = self.place_address_space(&place);
            let dest = self.alloc_temp_local(target_ty, false, "coerce");
            let load_space = self.load_result_address_space(expr, target_ty, place_space);
            self.builder.body.locals[dest.index()].address_space = load_space;
            self.assign(None, Some(dest), Rvalue::Load { place });
            return self.alloc_value(
                target_ty,
                ValueOrigin::Local(dest),
                self.value_repr_for_ty(target_ty, load_space),
            );
        }

        if target_repr.address_space().is_some() {
            return self.alloc_value(
                target_ty,
                ValueOrigin::TransparentCast {
                    value: source_value,
                },
                target_repr,
            );
        }

        self.alloc_value(
            target_ty,
            ValueOrigin::TransparentCast {
                value: source_value,
            },
            target_repr,
        )
    }

    fn coerce_contextual_capability_expr_value(
        &mut self,
        expr: ExprId,
        source_value: ValueId,
        source_ty: TyId<'db>,
    ) -> ValueId {
        let expr_ty = self.typed_body.expr_ty(self.db, expr);
        let target_repr = self.value_repr_for_expr(expr, expr_ty);
        self.coerce_capability_value_to_target_ty(
            expr,
            source_value,
            source_ty,
            expr_ty,
            target_repr,
        )
    }

    fn contract_field_slot_offset(&self, contract_name: &str, field_idx: usize) -> Option<usize> {
        let top_mod = self.body.top_mod(self.db);
        let contract = top_mod
            .all_contracts(self.db)
            .iter()
            .copied()
            .find(|contract| {
                contract
                    .name(self.db)
                    .to_opt()
                    .is_some_and(|id| id.data(self.db) == contract_name)
            })?;

        contract
            .field_layout(self.db)
            .get_index(field_idx)
            .and_then(|(_, field)| field.storage_layout())
            .map(|layout| layout.slot_offset)
    }

    /// Lowers the body root expression, starting from the current block.
    ///
    /// # Parameters
    /// - `expr`: Root expression id of the body.
    pub(super) fn lower_root(&mut self, expr: ExprId) {
        let Some(block) = self.current_block() else {
            return;
        };

        self.move_to_block(block);
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Block(stmts)) => self.lower_block_expr(stmts),
            _ => {
                let value = self.lower_expr(expr);
                self.builder.body.expr_values.insert(expr, value);
            }
        }
    }

    /// Lowers a block expression by sequentially lowering its statements.
    ///
    /// # Parameters
    /// - `stmts`: Statements contained in the block.
    pub(super) fn lower_block(&mut self, stmts: &[StmtId]) {
        for &stmt_id in stmts {
            if self.current_block().is_none() {
                break;
            }
            self.lower_stmt(stmt_id);
        }
    }

    fn lower_block_expr(&mut self, stmts: &[StmtId]) {
        if stmts.is_empty() {
            return;
        }
        let (head, last) = stmts.split_at(stmts.len() - 1);
        self.lower_block(head);
        if self.current_block().is_none() {
            return;
        }
        let stmt_id = last[0];
        let Partial::Present(stmt) = stmt_id.data(self.db, self.body) else {
            return;
        };
        if let Stmt::Expr(expr) = stmt {
            let ty = self.typed_body.expr_ty(self.db, *expr);
            if self.is_unit_ty(ty) {
                self.lower_expr_stmt(stmt_id, *expr);
            } else {
                let _ = self.lower_expr(*expr);
            }
        } else {
            self.lower_stmt(stmt_id);
        }
    }

    /// Lowers an expression, emitting any required control flow and side effects.
    ///
    /// # Parameters
    /// - `expr`: Expression id to lower.
    ///
    /// # Returns
    /// The value representing the expression.
    pub(super) fn lower_expr(&mut self, expr: ExprId) -> ValueId {
        if self.current_block().is_none() {
            return self.ensure_value(expr);
        }

        if self.typed_body.is_implicit_move(expr) {
            let assumptions =
                hir::analysis::ty::trait_resolution::PredicateListId::empty_list(self.db);
            let ty = self.typed_body.expr_ty(self.db, expr);
            let move_ty = ty
                .as_capability(self.db)
                .map(|(_, inner)| inner)
                .unwrap_or(ty);
            if !hir::analysis::ty::ty_is_copy(self.db, self.core.scope, move_ty, assumptions) {
                let value_id = self.ensure_value(expr);
                if let Some(place) = self.place_for_borrow_expr(expr) {
                    if let Some((_, inner_ty)) = ty.as_capability(self.db) {
                        let repr = self.value_repr_for_ty(inner_ty, self.expr_address_space(expr));
                        self.builder.body.values[value_id.index()].ty = inner_ty;
                        self.builder.body.values[value_id.index()].repr = repr;
                    }
                    self.builder.body.values[value_id.index()].origin =
                        ValueOrigin::MoveOut { place };
                    return value_id;
                }
            }
        }

        if let Some(value) = self.try_lower_variant_ctor(expr, None) {
            return value;
        }
        if let Some(value) = self.try_lower_unit_variant(expr, None) {
            return value;
        }

        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Block(stmts)) => {
                self.lower_block_expr(stmts);
                self.ensure_value(expr)
            }
            Partial::Present(Expr::With(bindings, body_expr)) => {
                for binding in bindings {
                    if self.current_block().is_none() {
                        break;
                    }
                    let value = self.lower_expr(binding.value);
                    if self.current_block().is_some() {
                        let ty = self.typed_body.expr_ty(self.db, binding.value);
                        if self.is_unit_ty(ty) {
                            self.assign(None, None, Rvalue::Value(value));
                        } else {
                            let source = self.source_for_expr(binding.value);
                            self.push_inst_here(MirInst::BindValue { source, value });
                        }
                    }
                }

                let value = self.lower_expr(*body_expr);
                self.builder.body.expr_values.insert(expr, value);
                value
            }
            Partial::Present(Expr::RecordInit(_, fields)) => self.try_lower_record(expr, fields),
            Partial::Present(Expr::Tuple(elems)) => self.try_lower_tuple(expr, elems),
            Partial::Present(Expr::Array(elems)) => self.try_lower_array(expr, elems),
            Partial::Present(Expr::ArrayRep(elem, len)) => {
                self.try_lower_array_rep(expr, *elem, *len)
            }
            Partial::Present(Expr::Match(scrutinee, arms)) => {
                if let Partial::Present(arms) = arms {
                    // Try decision tree lowering first
                    return self.lower_match_with_decision_tree(expr, *scrutinee, arms);
                }
                self.ensure_value(expr)
            }
            Partial::Present(Expr::If(cond, then_expr, else_expr)) => {
                self.lower_if(expr, *cond, *then_expr, *else_expr)
            }
            Partial::Present(Expr::Call(callee, call_args)) => {
                let _ = callee;
                let _ = call_args;
                self.lower_call_expr(expr)
            }
            Partial::Present(Expr::MethodCall(receiver, _, _, call_args)) => {
                let _ = receiver;
                let _ = call_args;
                self.lower_call_expr(expr)
            }
            Partial::Present(Expr::Un(inner, op)) => {
                let has_callable = self.typed_body.callable_expr(expr).is_some();
                if matches!(op, hir::hir_def::expr::UnOp::Minus)
                    && let Some(value_id) = self.try_lower_negated_int_literal(expr, *inner)
                {
                    return value_id;
                }
                // Checked arithmetic: unary minus with a callable (Neg::neg trait)
                // uses call-based lowering for overflow detection.
                if matches!(op, hir::hir_def::expr::UnOp::Minus) {
                    let inner_ty = self
                        .typed_body
                        .expr_ty(self.db, *inner)
                        .as_capability(self.db)
                        .map(|(_, inner)| inner)
                        .unwrap_or_else(|| self.typed_body.expr_ty(self.db, *inner));
                    let is_const_minus = eval_const_expr(
                        self.db,
                        self.body,
                        self.typed_body,
                        self.generic_args,
                        expr,
                    )
                    .ok()
                    .flatten()
                    .is_some();
                    // When the core library is not available (standalone `.fe`
                    // files), no callable is registered — allow unchecked fallback.
                    let _ = inner_ty.is_integral(self.db) && !has_callable && !is_const_minus;
                }
                if matches!(op, hir::hir_def::expr::UnOp::Minus)
                    && let Some(intrinsic) = self.direct_checked_intrinsic(
                        expr,
                        self.typed_body.expr_ty(self.db, *inner),
                        "neg",
                        crate::ir::CheckedArithmeticOp::Neg,
                    )
                {
                    let inner_value = self.lower_expr(*inner);
                    if self.current_block().is_none() {
                        return self.ensure_value(expr);
                    }
                    let inner_value =
                        self.coerce_primitive_operand_if_copy_capability(*inner, inner_value);
                    return self.lower_checked_intrinsic_expr(expr, vec![inner_value], intrinsic);
                }
                if matches!(op, hir::hir_def::expr::UnOp::Minus) && has_callable {
                    return self.lower_call_expr(expr);
                }

                if !matches!(
                    op,
                    hir::hir_def::expr::UnOp::Mut | hir::hir_def::expr::UnOp::Ref
                ) && self.needs_op_trait_call(expr)
                {
                    return self.lower_call_expr_inner(expr, None, None);
                }

                let value_id = self.ensure_value(expr);
                if self.current_block().is_none() {
                    return value_id;
                }

                match op {
                    hir::hir_def::expr::UnOp::Mut | hir::hir_def::expr::UnOp::Ref => {
                        if let Some(place) = self.place_for_borrow_expr(*inner) {
                            let space = self.place_address_space(&place);
                            let value_ty = self.builder.body.value(value_id).ty;
                            self.mark_place_root_address_taken(&place);
                            self.builder.body.values[value_id.index()].origin =
                                ValueOrigin::PlaceRef(place);
                            self.builder.body.values[value_id.index()].repr =
                                self.value_repr_for_ty(value_ty, space);
                            self.refresh_value_pointer_info(value_id);
                        } else {
                            let _ = self.lower_expr(*inner);
                        }
                    }
                    _ => {
                        let _ = self.lower_expr(*inner);
                    }
                }
                if matches!(
                    op,
                    hir::hir_def::expr::UnOp::Mut | hir::hir_def::expr::UnOp::Ref
                ) && let Some(span) = expr.span(self.body).into_un_expr().op().resolve(self.db)
                {
                    self.builder.body.values[value_id.index()].source =
                        self.source_info_for_span(Some(span));
                }

                value_id
            }
            Partial::Present(Expr::Cast(inner, _)) => {
                let value_id = self.ensure_value(expr);
                let inner_value = self.lower_expr(*inner);
                if self.current_block().is_none() {
                    return value_id;
                }

                let inner_ty = self.typed_body.expr_ty(self.db, *inner);
                let lowered = if inner_ty.as_capability(self.db).is_some() {
                    self.coerce_contextual_capability_expr_value(expr, inner_value, inner_ty)
                } else if inner_ty == self.typed_body.expr_ty(self.db, expr) {
                    inner_value
                } else {
                    self.alloc_value(
                        self.typed_body.expr_ty(self.db, expr),
                        ValueOrigin::TransparentCast { value: inner_value },
                        self.value_repr_for_expr(expr, self.typed_body.expr_ty(self.db, expr)),
                    )
                };
                self.set_expr_value_from_lowered_value(value_id, lowered);
                value_id
            }
            Partial::Present(Expr::Bin(lhs, rhs, BinOp::Index)) => {
                self.lower_index_expr(expr, *lhs, *rhs)
            }
            Partial::Present(Expr::Bin(lhs, rhs, BinOp::Arith(ArithBinOp::Range))) => {
                // Desugar range expression `start..end` into Range struct construction
                self.lower_range_expr(expr, *lhs, *rhs)
            }
            Partial::Present(Expr::Bin(lhs, rhs, op)) => {
                if let Some((expected_name, checked_op)) = match op {
                    BinOp::Arith(ArithBinOp::Add) => {
                        Some(("add", crate::ir::CheckedArithmeticOp::Add))
                    }
                    BinOp::Arith(ArithBinOp::Sub) => {
                        Some(("sub", crate::ir::CheckedArithmeticOp::Sub))
                    }
                    BinOp::Arith(ArithBinOp::Mul) => {
                        Some(("mul", crate::ir::CheckedArithmeticOp::Mul))
                    }
                    BinOp::Arith(ArithBinOp::Div) => {
                        Some(("div", crate::ir::CheckedArithmeticOp::Div))
                    }
                    BinOp::Arith(ArithBinOp::Rem) => {
                        Some(("rem", crate::ir::CheckedArithmeticOp::Rem))
                    }
                    _ => None,
                } && let Some(intrinsic) = self.direct_checked_intrinsic(
                    expr,
                    self.typed_body.expr_ty(self.db, *lhs),
                    expected_name,
                    checked_op,
                ) {
                    let lhs_value = self.lower_expr(*lhs);
                    let rhs_value = self.lower_expr(*rhs);
                    if self.current_block().is_none() {
                        return self.ensure_value(expr);
                    }
                    let lhs_value =
                        self.coerce_primitive_operand_if_copy_capability(*lhs, lhs_value);
                    let rhs_value =
                        self.coerce_primitive_operand_if_copy_capability(*rhs, rhs_value);
                    return self.lower_checked_intrinsic_expr(
                        expr,
                        vec![lhs_value, rhs_value],
                        intrinsic,
                    );
                }

                // Remaining primitive arithmetic/comparison ops still use
                // call-based lowering when core/std provides the operator
                // implementation.
                let critical_primitive_op = matches!(
                    op,
                    BinOp::Arith(
                        ArithBinOp::Add | ArithBinOp::Sub | ArithBinOp::Mul | ArithBinOp::Pow
                    ) | BinOp::Comp(..)
                );
                let has_callable = self.typed_body.callable_expr(expr).is_some();
                if critical_primitive_op {
                    let lhs_ty = self
                        .typed_body
                        .expr_ty(self.db, *lhs)
                        .as_capability(self.db)
                        .map(|(_, inner)| inner)
                        .unwrap_or_else(|| self.typed_body.expr_ty(self.db, *lhs));
                    let rhs_ty = self
                        .typed_body
                        .expr_ty(self.db, *rhs)
                        .as_capability(self.db)
                        .map(|(_, inner)| inner)
                        .unwrap_or_else(|| self.typed_body.expr_ty(self.db, *rhs));
                    let primitive_operands = (lhs_ty.is_integral(self.db)
                        || lhs_ty.is_bool(self.db))
                        && (rhs_ty.is_integral(self.db) || rhs_ty.is_bool(self.db));
                    // When the core library is available, every primitive
                    // arithmetic / comparison op should have a callable registered
                    // (for checked-arithmetic lowering). Standalone `.fe` files
                    // without an ingot lack core, so the trait resolver returns
                    // nothing — fall through to unchecked raw-binary in that case.
                    let _ = primitive_operands;
                }
                if critical_primitive_op && has_callable {
                    self.lower_call_expr(expr)
                } else if self.needs_op_trait_call(expr) {
                    self.lower_call_expr_inner(expr, None, None)
                } else {
                    let lhs_value = self.lower_expr(*lhs);
                    let rhs_value = self.lower_expr(*rhs);
                    let coerced_lhs =
                        self.coerce_primitive_operand_if_copy_capability(*lhs, lhs_value);
                    let coerced_rhs =
                        self.coerce_primitive_operand_if_copy_capability(*rhs, rhs_value);
                    let value_id = self.ensure_value(expr);
                    if let ValueOrigin::Binary { lhs, rhs, .. } =
                        &mut self.builder.body.values[value_id.index()].origin
                    {
                        *lhs = coerced_lhs;
                        *rhs = coerced_rhs;
                    }
                    value_id
                }
            }
            Partial::Present(Expr::Field(lhs, field_index)) => {
                self.lower_field_expr(expr, *lhs, *field_index)
            }
            Partial::Present(Expr::Path(_)) => self.lower_path_expr(expr),
            Partial::Present(Expr::Assign(_, _) | Expr::AugAssign(_, _, _)) => {
                // Assignment expressions are expected to be lowered in statement position.
                self.ensure_value(expr)
            }
            _ => self.ensure_value(expr),
        }
    }

    fn lower_call_expr(&mut self, expr: ExprId) -> ValueId {
        self.lower_call_expr_inner(expr, None, None)
    }

    fn assign_checked_intrinsic_call(
        &mut self,
        stmt: Option<StmtId>,
        dest: LocalId,
        args: Vec<ValueId>,
        intrinsic: crate::ir::CheckedIntrinsic<'db>,
        expr: Option<ExprId>,
    ) {
        let call_origin = CallOrigin {
            expr,
            hir_target: None,
            args,
            effect_args: Vec::new(),
            resolved_name: None,
            checked_intrinsic: Some(intrinsic),
            builtin_terminator: None,
            receiver_space: None,
        };
        self.assign(stmt, Some(dest), Rvalue::Call(call_origin));
    }

    fn lower_checked_intrinsic_expr(
        &mut self,
        expr: ExprId,
        args: Vec<ValueId>,
        intrinsic: crate::ir::CheckedIntrinsic<'db>,
    ) -> ValueId {
        let value_id = self.ensure_value(expr);
        if self.current_block().is_none() {
            return value_id;
        }

        let dest_ty = self.typed_body.expr_ty(self.db, expr);
        let dest = self.alloc_temp_local(dest_ty, false, "checked");
        self.builder.body.locals[dest.index()].address_space = self.expr_address_space(expr);
        self.assign_checked_intrinsic_call(None, dest, args, intrinsic, Some(expr));
        self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
        value_id
    }

    fn direct_checked_intrinsic(
        &self,
        expr: ExprId,
        operand_ty: TyId<'db>,
        expected_name: &str,
        op: crate::ir::CheckedArithmeticOp,
    ) -> Option<crate::ir::CheckedIntrinsic<'db>> {
        let callable = self.typed_body.callable_expr(expr)?;
        match callable.callable_def.ingot(self.db).kind(self.db) {
            IngotKind::Core | IngotKind::Std => {}
            _ => return None,
        }

        let name = callable.callable_def.name(self.db)?;
        if name.data(self.db).as_str() != expected_name {
            return None;
        }

        let operand_ty = operand_ty
            .as_capability(self.db)
            .map(|(_, inner)| inner)
            .unwrap_or(operand_ty);
        let TyData::TyBase(TyBase::Prim(prim)) = operand_ty.base_ty(self.db).data(self.db) else {
            return None;
        };
        if !prim.is_integral() {
            return None;
        }
        if matches!(op, crate::ir::CheckedArithmeticOp::Neg)
            && !matches!(
                prim,
                PrimTy::I8
                    | PrimTy::I16
                    | PrimTy::I32
                    | PrimTy::I64
                    | PrimTy::I128
                    | PrimTy::I256
                    | PrimTy::Isize
            )
        {
            return None;
        }

        Some(crate::ir::CheckedIntrinsic { op, ty: operand_ty })
    }

    fn call_expected_arg_tys(&self, callable: &Callable<'db>) -> Vec<TyId<'db>> {
        callable
            .callable_def
            .arg_tys(self.db)
            .into_iter()
            .map(|binder| binder.instantiate(self.db, callable.generic_args()))
            .collect()
    }

    pub(super) fn deref_target_ty(&self, ty: TyId<'db>) -> Option<TyId<'db>> {
        crate::repr::deref_target_ty(self.db, &self.core, ty)
    }

    pub(super) fn direct_deref_target_ty(&self, ty: TyId<'db>) -> Option<TyId<'db>> {
        crate::repr::direct_deref_target_ty(self.db, &self.core, ty)
    }

    pub(super) fn place_base_ty(&self, ty: TyId<'db>) -> TyId<'db> {
        self.direct_deref_target_ty(ty).unwrap_or(ty)
    }

    fn transparent_field0_preserves_value(
        &self,
        lhs_ty: TyId<'db>,
        base_value: ValueId,
        field_ty: TyId<'db>,
    ) -> bool {
        let base_repr = self.builder.body.value(base_value).repr;
        if base_repr.is_ref() {
            return false;
        }

        if self.direct_deref_target_ty(lhs_ty).is_none() {
            return true;
        }

        self.deref_target_ty(field_ty).is_some() || !self.value_supports_direct_deref(base_value)
    }

    pub(super) fn value_supports_direct_deref(&self, value: ValueId) -> bool {
        let mut root = value;
        while let ValueOrigin::TransparentCast { value: inner } =
            &self.builder.body.value(root).origin
        {
            root = *inner;
        }

        let root_value = self.builder.body.value(root);
        let is_address_backed = matches!(
            root_value.origin,
            ValueOrigin::Local(_)
                | ValueOrigin::PlaceRef(_)
                | ValueOrigin::MoveOut { .. }
                | ValueOrigin::FieldPtr(_)
        );
        if !is_address_backed {
            return false;
        }

        if let Some((kind, inner_ty)) = root_value.ty.as_capability(self.db) {
            return match kind {
                CapabilityKind::Mut | CapabilityKind::Ref => true,
                // `view T` reuses `T`'s runtime representation. Only by-ref inners carry an
                // actual location that can be dereferenced directly.
                CapabilityKind::View => self.is_by_ref_ty(inner_ty),
            };
        }

        if root_value.repr.address_space().is_some()
            || self.deref_target_ty(root_value.ty).is_some()
        {
            return true;
        }

        match crate::repr::repr_kind_for_ty(self.db, &self.core, root_value.ty) {
            crate::repr::ReprKind::Zst | crate::repr::ReprKind::Word => false,
            crate::repr::ReprKind::Ptr(_) | crate::repr::ReprKind::Ref => true,
        }
    }

    fn ty_is_scalar_ref_capability(&self, ty: TyId<'db>) -> bool {
        ty.as_capability(self.db).is_some_and(|(kind, inner)| {
            matches!(kind, CapabilityKind::Ref)
                && matches!(
                    crate::repr::repr_kind_for_ty(self.db, &self.core, inner),
                    crate::repr::ReprKind::Word
                        | crate::repr::ReprKind::Zst
                        | crate::repr::ReprKind::Ptr(_)
                )
        })
    }

    pub(super) fn place_from_derefable_value(
        &mut self,
        value: ValueId,
        ty: TyId<'db>,
    ) -> Option<Place<'db>> {
        let inner_ty = self.deref_target_ty(ty)?;
        if self.value_supports_direct_deref(value) {
            return Some(Place::new(
                value,
                MirProjectionPath::from_projection(Projection::Deref),
            ));
        }

        if ty
            .as_capability(self.db)
            .is_some_and(|(kind, _)| matches!(kind, CapabilityKind::Ref))
            && self.ty_is_scalar_ref_capability(ty)
        {
            return None;
        }

        if ty.as_capability(self.db).is_some_and(|(kind, inner)| {
            matches!(kind, CapabilityKind::View) && !self.is_by_ref_ty(inner)
        }) {
            return None;
        }

        // Capability-typed rvalues can appear as immediates (e.g. integer literals coerced to
        // `view T`). Materialize storage in memory so subsequent loads don't treat the immediate
        // word as a pointer address (like `mload(10)`). Non-capability effect handles always
        // lower to concrete pointer words and should have returned through the direct-deref path.
        ty.as_capability(self.db)?;

        let inner_value = self.alloc_value(
            inner_ty,
            ValueOrigin::TransparentCast { value },
            self.value_repr_for_ty(inner_ty, AddressSpaceKind::Memory),
        );
        let temp = self.alloc_temp_local(inner_ty, false, "captmp");
        self.builder.body.locals[temp.index()].address_space = AddressSpaceKind::Memory;
        self.assign(None, Some(temp), Rvalue::Value(inner_value));
        let base = self.alloc_value(inner_ty, ValueOrigin::PlaceRoot(temp), ValueRepr::Word);
        Some(Place::new(base, MirProjectionPath::new()))
    }

    fn value_address_space_or_memory(&self, value: ValueId) -> AddressSpaceKind {
        if let Some(space) = crate::ir::try_value_address_space_in(
            &self.builder.body.values,
            &self.builder.body.locals,
            value,
        ) {
            return space;
        }

        let data = self.builder.body.value(value);
        debug_assert!(
            data.repr.address_space().is_none(),
            "missing address space for pointer-like value in lowering: repr={:?}, origin={:?}",
            data.repr,
            data.origin
        );
        AddressSpaceKind::Memory
    }

    fn coerce_call_arg_value(
        &mut self,
        arg_expr: ExprId,
        arg_value: ValueId,
        expected_ty: TyId<'db>,
    ) -> ValueId {
        let actual_ty = self.builder.body.value(arg_value).ty;
        let arg_space = self.value_address_space_or_memory(arg_value);
        if actual_ty == expected_ty {
            if let Some((_, inner_ty)) = expected_ty.as_capability(self.db) {
                let expected_repr = self.value_repr_for_ty(expected_ty, arg_space);
                if expected_repr.address_space().is_some()
                    && !self.value_supports_direct_deref(arg_value)
                    && let Some(place) = self
                        .place_for_borrow_expr(arg_expr)
                        .or_else(|| self.place_from_derefable_value(arg_value, expected_ty))
                {
                    let place_space = self.place_address_space(&place);
                    let expected_repr = self.value_repr_for_ty(expected_ty, place_space);
                    let base = self.alloc_value(
                        inner_ty,
                        ValueOrigin::PlaceRef(place.clone()),
                        self.value_repr_for_ty(inner_ty, place_space),
                    );
                    self.mark_place_root_address_taken(&place);
                    return self.alloc_value(
                        expected_ty,
                        ValueOrigin::TransparentCast { value: base },
                        expected_repr,
                    );
                }
            }
            return arg_value;
        }

        if let Some((required_cap, required_inner)) = expected_ty.as_capability(self.db) {
            let expected_repr = self.value_repr_for_ty(expected_ty, arg_space);
            if expected_repr.address_space().is_none() {
                if let Some((_, given_inner)) = actual_ty.as_capability(self.db)
                    && given_inner == required_inner
                {
                    if self.is_by_ref_ty(required_inner) {
                        return self.alloc_value(
                            expected_ty,
                            ValueOrigin::TransparentCast { value: arg_value },
                            expected_repr,
                        );
                    }

                    if let Some(place) =
                        self.place_for_capability_inner_load(arg_expr, arg_value, actual_ty)
                    {
                        let place_space = self.place_address_space(&place);
                        let expected_repr = self.value_repr_for_ty(expected_ty, place_space);
                        let loaded = self.alloc_temp_local(required_inner, false, "viewword");
                        self.builder.body.locals[loaded.index()].address_space = place_space;
                        self.assign(None, Some(loaded), Rvalue::Load { place });
                        let loaded_value = self.alloc_value(
                            required_inner,
                            ValueOrigin::Local(loaded),
                            self.value_repr_for_ty(required_inner, place_space),
                        );
                        return self.alloc_value(
                            expected_ty,
                            ValueOrigin::TransparentCast {
                                value: loaded_value,
                            },
                            expected_repr,
                        );
                    }
                    return self.alloc_value(
                        expected_ty,
                        ValueOrigin::TransparentCast { value: arg_value },
                        expected_repr,
                    );
                }

                if actual_ty == required_inner {
                    return self.alloc_value(
                        expected_ty,
                        ValueOrigin::TransparentCast { value: arg_value },
                        expected_repr,
                    );
                }

                return arg_value;
            }

            if let Some((given_cap, given_inner)) = actual_ty.as_capability(self.db)
                && given_inner == required_inner
                && given_cap.rank() >= required_cap.rank()
            {
                if self
                    .builder
                    .body
                    .value(arg_value)
                    .repr
                    .address_space()
                    .is_some()
                {
                    if let Some(place) = self.place_for_borrow_expr(arg_expr) {
                        let place_space = self.place_address_space(&place);
                        if place_space != arg_space {
                            let expected_repr = self.value_repr_for_ty(expected_ty, place_space);
                            return self.alloc_place_ref_value(expected_ty, place, expected_repr);
                        }
                    }
                    return self.alloc_value(
                        expected_ty,
                        ValueOrigin::TransparentCast { value: arg_value },
                        expected_repr,
                    );
                }

                if let Some(place) =
                    self.place_for_capability_inner_load(arg_expr, arg_value, actual_ty)
                {
                    let place_space = self.place_address_space(&place);
                    let expected_repr = self.value_repr_for_ty(expected_ty, place_space);
                    return self.alloc_place_ref_value(expected_ty, place, expected_repr);
                }
            }

            if actual_ty == required_inner
                && (self.is_by_ref_ty(actual_ty) || self.value_supports_direct_deref(arg_value))
            {
                return self.alloc_value(
                    expected_ty,
                    ValueOrigin::TransparentCast { value: arg_value },
                    expected_repr,
                );
            }

            if let Some(place) = self.place_for_borrow_expr(arg_expr) {
                let place_space = self.place_address_space(&place);
                let expected_repr = self.value_repr_for_ty(expected_ty, place_space);
                return self.alloc_place_ref_value(expected_ty, place, expected_repr);
            }

            let temp = self.alloc_temp_local(actual_ty, false, "viewtmp");
            self.builder.body.locals[temp.index()].address_space = AddressSpaceKind::Memory;
            self.assign(None, Some(temp), Rvalue::Value(arg_value));
            let base = self.alloc_value(actual_ty, ValueOrigin::PlaceRoot(temp), ValueRepr::Word);
            let place = Place::new(base, MirProjectionPath::new());
            let expected_repr = self.value_repr_for_ty(expected_ty, AddressSpaceKind::Memory);
            return self.alloc_value(expected_ty, ValueOrigin::PlaceRef(place), expected_repr);
        }

        if let Some((kind, inner_ty)) = actual_ty.as_capability(self.db)
            && expected_ty == inner_ty
        {
            let expected_repr = self.value_repr_for_ty(expected_ty, arg_space);
            if matches!(kind, CapabilityKind::View) {
                return self.alloc_value(
                    expected_ty,
                    ValueOrigin::TransparentCast { value: arg_value },
                    expected_repr,
                );
            }

            let Some(place) = self.place_for_capability_inner_load(arg_expr, arg_value, actual_ty)
            else {
                return arg_value;
            };

            let dest = self.alloc_temp_local(expected_ty, false, "copyarg");
            self.builder.body.locals[dest.index()].address_space = self.place_address_space(&place);
            self.assign(None, Some(dest), Rvalue::Load { place });
            return self.alloc_value(expected_ty, ValueOrigin::Local(dest), expected_repr);
        }

        arg_value
    }

    fn place_for_capability_inner_load(
        &mut self,
        arg_expr: ExprId,
        arg_value: ValueId,
        capability_ty: TyId<'db>,
    ) -> Option<Place<'db>> {
        self.place_from_derefable_value(arg_value, capability_ty)
            .or_else(|| self.place_for_borrow_expr(arg_expr))
    }

    fn normalize_method_receiver_call_arg(
        &mut self,
        receiver_expr: ExprId,
        receiver_value: ValueId,
        expected_receiver_ty: TyId<'db>,
        return_contains_capability: bool,
    ) -> (ValueId, Option<AddressSpaceKind>) {
        let Some(receiver_place) = self.place_for_borrow_expr(receiver_expr) else {
            let space = self.value_address_space_or_memory(receiver_value);
            return (
                receiver_value,
                (space != AddressSpaceKind::Memory).then_some(space),
            );
        };

        let receiver_space = self.place_address_space(&receiver_place);
        if receiver_space == AddressSpaceKind::Memory {
            return (receiver_value, None);
        }

        if self.value_supports_direct_deref(receiver_value) {
            return (receiver_value, Some(receiver_space));
        }

        let normalize_receiver = expected_receiver_ty
            .as_capability(self.db)
            .is_some_and(|(kind, _)| !matches!(kind, CapabilityKind::View))
            || return_contains_capability;
        if !normalize_receiver {
            return (receiver_value, Some(receiver_space));
        }
        let receiver_repr = self.value_repr_for_ty(expected_receiver_ty, receiver_space);
        (
            self.alloc_place_ref_value(expected_receiver_ty, receiver_place, receiver_repr),
            Some(receiver_space),
        )
    }

    fn callable_return_contains_capability(&self, callable: &Callable<'db>) -> bool {
        fn visit<'db>(
            builder: &MirBuilder<'db, '_>,
            ty: TyId<'db>,
            seen: &mut FxHashSet<TyId<'db>>,
        ) -> bool {
            if !seen.insert(ty) {
                return false;
            }

            if ty.as_capability(builder.db).is_some() {
                return true;
            }

            if let Some(inner) = crate::repr::transparent_newtype_field_ty(builder.db, ty)
                && visit(builder, inner, seen)
            {
                return true;
            }

            for arg in ty.generic_args(builder.db) {
                if visit(builder, *arg, seen) {
                    return true;
                }
            }

            for field_ty in ty.field_types(builder.db) {
                if visit(builder, field_ty, seen) {
                    return true;
                }
            }

            false
        }

        let return_ty = callable
            .callable_def
            .ret_ty(self.db)
            .instantiate(self.db, callable.generic_args());
        let mut seen = FxHashSet::default();
        visit(self, return_ty, &mut seen)
    }

    fn coerce_call_args_to_expected(
        &mut self,
        callable: &Callable<'db>,
        arg_exprs: &[ExprId],
        args: &mut [ValueId],
        return_contains_capability: bool,
    ) -> Option<AddressSpaceKind> {
        let expected_arg_tys = self.call_expected_arg_tys(callable);
        let expected_receiver_ty = callable
            .callable_def
            .receiver_ty(self.db)
            .map(|ty| ty.instantiate(self.db, callable.generic_args()));
        let mut receiver_space = None;

        for (idx, arg) in args.iter_mut().enumerate() {
            let Some(&arg_expr) = arg_exprs.get(idx) else {
                continue;
            };
            let expected_ty = if idx == 0 {
                expected_receiver_ty.or_else(|| expected_arg_tys.get(idx).copied())
            } else {
                expected_arg_tys.get(idx).copied()
            };
            let Some(expected_ty) = expected_ty else {
                continue;
            };
            let coerced = self.coerce_call_arg_value(arg_expr, *arg, expected_ty);
            let mut coerced = self.materialize_capability_call_arg(arg_expr, coerced, expected_ty);
            if idx == 0
                && let Some(receiver_ty) = expected_receiver_ty
            {
                let (normalized, space) = self.normalize_method_receiver_call_arg(
                    arg_expr,
                    coerced,
                    receiver_ty,
                    return_contains_capability,
                );
                coerced = normalized;
                if space.is_some() {
                    receiver_space = space;
                }
            }
            *arg = coerced;
        }

        if receiver_space.is_none()
            && expected_receiver_ty.is_some()
            && let Some(&receiver) = args.first()
            && arg_exprs
                .first()
                .and_then(|expr| self.place_for_borrow_expr(*expr))
                .is_none()
        {
            let space = self.value_address_space_or_memory(receiver);
            if space != AddressSpaceKind::Memory {
                receiver_space = Some(space);
            }
        }

        receiver_space
    }

    fn materialize_capability_call_arg(
        &mut self,
        arg_expr: ExprId,
        arg_value: ValueId,
        expected_ty: TyId<'db>,
    ) -> ValueId {
        let Some((_, _)) = expected_ty.as_capability(self.db) else {
            return arg_value;
        };

        let expected_repr =
            self.value_repr_for_ty(expected_ty, self.value_address_space_or_memory(arg_value));
        if expected_repr.address_space().is_none() || self.value_supports_direct_deref(arg_value) {
            return arg_value;
        }

        let Some(place) = self.place_for_borrow_expr(arg_expr).or_else(|| {
            let source_ty = self.builder.body.value(arg_value).ty;
            if self.deref_target_ty(source_ty).is_some() {
                return self.place_from_derefable_value(arg_value, source_ty);
            }

            let temp = self.alloc_temp_local(source_ty, false, "viewtmp");
            self.builder.body.locals[temp.index()].address_space = AddressSpaceKind::Memory;
            self.assign(None, Some(temp), Rvalue::Value(arg_value));
            let base = self.alloc_value(source_ty, ValueOrigin::PlaceRoot(temp), ValueRepr::Word);
            Some(Place::new(base, MirProjectionPath::new()))
        }) else {
            return arg_value;
        };

        let place_space = self.place_address_space(&place);
        let expected_repr = self.value_repr_for_ty(expected_ty, place_space);
        self.alloc_place_ref_value(expected_ty, place, expected_repr)
    }

    fn coerce_primitive_operand_if_copy_capability(
        &mut self,
        operand_expr: ExprId,
        operand_value: ValueId,
    ) -> ValueId {
        let operand_ty = self.builder.body.value(operand_value).ty;
        let Some((_, inner_ty)) = operand_ty.as_capability(self.db) else {
            return operand_value;
        };

        let assumptions = hir::analysis::ty::trait_resolution::PredicateListId::empty_list(self.db);
        if !hir::analysis::ty::ty_is_copy(self.db, self.core.scope, inner_ty, assumptions) {
            return operand_value;
        }

        if !self.value_supports_direct_deref(operand_value) {
            return self.alloc_value(
                inner_ty,
                ValueOrigin::TransparentCast {
                    value: operand_value,
                },
                self.value_repr_for_ty(inner_ty, AddressSpaceKind::Memory),
            );
        }

        let Some(place) =
            self.place_for_capability_inner_load(operand_expr, operand_value, operand_ty)
        else {
            return operand_value;
        };

        let dest = self.alloc_temp_local(inner_ty, false, "binload");
        self.builder.body.locals[dest.index()].address_space = AddressSpaceKind::Memory;
        self.assign(None, Some(dest), Rvalue::Load { place });
        self.alloc_value(
            inner_ty,
            ValueOrigin::Local(dest),
            self.value_repr_for_ty(inner_ty, AddressSpaceKind::Memory),
        )
    }

    fn lower_call_expr_inner(
        &mut self,
        expr: ExprId,
        dest_override: Option<LocalId>,
        stmt: Option<StmtId>,
    ) -> ValueId {
        if let Some(value_id) = self.try_lower_intrinsic_stmt(expr) {
            return value_id;
        }

        let value_id = self.ensure_value(expr);
        if self.current_block().is_none() {
            return value_id;
        }

        let ty = self.typed_body.expr_ty(self.db, expr);
        let returns_value = !self.is_unit_ty(ty) && !ty.is_never(self.db);

        if let Some(value_id) = self.try_lower_size_intrinsic_call(expr) {
            if returns_value && let Some(dest) = dest_override {
                self.assign(stmt, Some(dest), Rvalue::Value(value_id));
            }
            return value_id;
        }

        if let Some(value_id) = self.try_lower_const_keccak_call(expr) {
            if returns_value && let Some(dest) = dest_override {
                self.assign(stmt, Some(dest), Rvalue::Value(value_id));
            }
            return value_id;
        }

        let Some(mut callable) = self.typed_body.callable_expr(expr).cloned() else {
            return value_id;
        };
        let callable_def = callable.callable_def;

        let Some((mut args, arg_exprs)) = self.collect_call_args(expr) else {
            return value_id;
        };
        // Save raw args before call-coercion; intrinsics need pre-coercion values
        // to avoid double-loading capability-wrapped operands.
        let raw_args = args.clone();
        let return_contains_capability = self.callable_return_contains_capability(&callable);
        let receiver_space = self.coerce_call_args_to_expected(
            &callable,
            &arg_exprs,
            &mut args,
            return_contains_capability,
        );
        let provider_space = self.effect_provider_space_for_provider_ty(ty);
        let result_space = provider_space
            .or(receiver_space.filter(|_| ty.as_capability(self.db).is_some()))
            .unwrap_or_else(|| self.expr_address_space(expr));

        if matches!(
            callable_def.ingot(self.db).kind(self.db),
            IngotKind::Core | IngotKind::Std
        ) && callable_def
            .name(self.db)
            .is_some_and(|name| name.data(self.db) == "contract_field_slot")
            && let Some(func) = self.hir_func
            && let Some(contract_fn) = extract_contract_function(self.db, func)
            && let Some(arg_expr) = arg_exprs.first().copied()
            && let Some(field_idx) = self.u256_lit_from_expr(arg_expr)
            && let Some(field_idx) = field_idx.to_usize()
            && let Some(offset) =
                self.contract_field_slot_offset(&contract_fn.contract_name, field_idx)
        {
            self.builder.body.values[value_id.index()].origin =
                ValueOrigin::Synthetic(SyntheticValue::Int(BigUint::from(offset)));
            if returns_value && let Some(dest) = dest_override {
                self.assign(stmt, Some(dest), Rvalue::Value(value_id));
            }
            return value_id;
        }

        if self.is_cast_intrinsic(callable_def) {
            let mut cast_args = args.clone();
            if self.is_method_call(expr) && !cast_args.is_empty() {
                cast_args.remove(0);
            }
            if cast_args.len() != 1 {
                return value_id;
            }
            let arg_value = cast_args[0];
            self.builder.body.values[value_id.index()].origin =
                ValueOrigin::TransparentCast { value: arg_value };
            if returns_value && let Some(dest) = dest_override {
                self.assign(stmt, Some(dest), Rvalue::Value(value_id));
            }
            return value_id;
        }

        if let Some(op) = self.intrinsic_kind(callable_def) {
            // Use pre-coercion args: `coerce_call_args_to_expected` wraps values in
            // capability TransparentCasts which causes
            // `coerce_primitive_operand_if_copy_capability` to emit spurious
            // loads. Intrinsics operate on raw word values.
            let mut intrinsic_args = raw_args;
            let mut intrinsic_arg_exprs = arg_exprs.clone();
            if self.is_method_call(expr) && !intrinsic_args.is_empty() {
                intrinsic_args.remove(0);
                intrinsic_arg_exprs.remove(0);
            }
            for (idx, arg) in intrinsic_args.iter_mut().enumerate() {
                let Some(&arg_expr) = intrinsic_arg_exprs.get(idx) else {
                    continue;
                };
                *arg = self.coerce_primitive_operand_if_copy_capability(arg_expr, *arg);
            }
            if matches!(
                op,
                IntrinsicOp::CodeRegionOffset | IntrinsicOp::CodeRegionLen
            ) && let Some(arg) = intrinsic_args.first_mut()
                && let Some(target) =
                    self.code_region_target_from_ty(self.builder.body.value(*arg).ty)
            {
                let ty = self.builder.body.value(*arg).ty;
                let repr = self.builder.body.value(*arg).repr;
                *arg = self.alloc_value(ty, ValueOrigin::FuncItem(target), repr);
            }
            if op.returns_value() {
                // Intrinsics are word-producing operations, but some std/core APIs wrap them
                // in single-field structs (e.g. `Address { inner: caller() }`).
                //
                // When such a wrapper is lowered as an intrinsic (by name), materialize the
                // returned word into the destination aggregate so downstream code sees the
                // expected by-ref representation.
                if self.is_by_ref_ty(ty) {
                    let field_tys = ty.field_types(self.db);
                    let size_bytes = layout::ty_size_bytes(self.db, ty).unwrap_or(0);
                    if field_tys.len() != 1 || size_bytes != layout::WORD_SIZE_BYTES {
                        panic!(
                            "intrinsic `{:?}` used with unsupported by-ref type `{}`",
                            op,
                            ty.pretty_print(self.db)
                        );
                    }

                    let field_ty = field_tys[0];
                    if self.is_by_ref_ty(field_ty) {
                        panic!(
                            "intrinsic `{:?}` used with nested by-ref field type `{}`",
                            op,
                            field_ty.pretty_print(self.db)
                        );
                    }

                    let dest =
                        dest_override.unwrap_or_else(|| self.alloc_temp_local(ty, false, "intr"));
                    self.builder.body.locals[dest.index()].address_space = AddressSpaceKind::Memory;
                    self.assign(
                        stmt,
                        Some(dest),
                        Rvalue::Alloc {
                            address_space: AddressSpaceKind::Memory,
                        },
                    );

                    // Compute the intrinsic word into a temp local.
                    let word_local = self.alloc_temp_local(field_ty, false, "intrw");
                    self.assign(
                        stmt,
                        Some(word_local),
                        Rvalue::Intrinsic {
                            op,
                            args: intrinsic_args,
                        },
                    );
                    let word_value =
                        self.alloc_value(field_ty, ValueOrigin::Local(word_local), ValueRepr::Word);

                    // Store the word into the single field at offset 0.
                    self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
                    self.builder.body.values[value_id.index()].repr =
                        ValueRepr::Ref(AddressSpaceKind::Memory);
                    let place = Place::new(
                        value_id,
                        MirProjectionPath::from_projection(Projection::Field(0)),
                    );
                    let source = self.source_for_expr(expr);
                    self.push_inst_here(MirInst::Store {
                        source,
                        place,
                        value: word_value,
                    });
                    return value_id;
                }

                let dest =
                    dest_override.unwrap_or_else(|| self.alloc_temp_local(ty, false, "intr"));
                self.builder.body.locals[dest.index()].address_space = result_space;
                self.assign(
                    stmt,
                    Some(dest),
                    Rvalue::Intrinsic {
                        op,
                        args: intrinsic_args,
                    },
                );
                self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
                return value_id;
            }

            // Statement-only intrinsics are handled via `try_lower_intrinsic_stmt` above.
            self.builder.body.values[value_id.index()].origin = ValueOrigin::Unit;
            return value_id;
        }

        let mut effect_args = Vec::new();
        let mut effect_writebacks: Vec<(LocalId, Place<'db>)> = Vec::new();
        if let CallableDef::Func(func_def) = callable.callable_def
            && func_def.has_effects(self.db)
            && extract_contract_function(self.db, func_def).is_none()
            && let Some(resolved) = self.typed_body.call_effect_args(expr)
        {
            self.finalize_place_effect_provider_args_for_call(func_def, &mut callable, resolved);
            for resolved_arg in resolved {
                let value = self.lower_effect_arg(resolved_arg, &mut effect_writebacks);
                effect_args.push(value);
            }
        }

        let dest = if returns_value {
            dest_override.or_else(|| Some(self.alloc_temp_local(ty, false, "call")))
        } else {
            None
        };
        if let Some(dest) = dest {
            self.builder.body.locals[dest.index()].address_space = result_space;
        }
        let hir_target = crate::ir::HirCallTarget {
            callable_def: callable.callable_def,
            generic_args: callable.generic_args().to_vec(),
            trait_inst: callable.trait_inst(),
        };
        let checked_intrinsic = self.checked_intrinsic_kind(callable.callable_def, ty);
        let builtin_terminator = self.builtin_terminator_kind(callable.callable_def);
        let call_origin = CallOrigin {
            expr: Some(expr),
            hir_target: Some(hir_target),
            args,
            effect_args,
            resolved_name: None,
            checked_intrinsic,
            builtin_terminator,
            receiver_space,
        };
        if ty.is_never(self.db) {
            let source = self.source_for_expr(expr);
            self.set_current_terminator(Terminator::TerminatingCall {
                source,
                call: crate::ir::TerminatingCall::Call(call_origin),
            });
            self.builder.body.values[value_id.index()].origin = ValueOrigin::Unit;
            return value_id;
        }
        self.assign(stmt, dest, Rvalue::Call(call_origin));
        for (dest, place) in effect_writebacks {
            self.assign(None, Some(dest), Rvalue::Load { place });
        }
        self.builder.body.values[value_id.index()].origin =
            dest.map(ValueOrigin::Local).unwrap_or(ValueOrigin::Unit);
        value_id
    }

    fn finalize_place_effect_provider_args_for_call(
        &mut self,
        callee: Func<'db>,
        callable: &mut hir::analysis::ty::ty_check::Callable<'db>,
        resolved: &[ResolvedEffectArg<'db>],
    ) {
        let provider_arg_idx_by_effect =
            hir::analysis::ty::effects::place_effect_provider_param_index_map(self.db, callee);
        let caller_provider_arg_idx_by_effect = self.hir_func.map(|func| {
            hir::analysis::ty::effects::place_effect_provider_param_index_map(self.db, func)
        });

        for resolved_arg in resolved {
            let Some(effect_view) = callee.effect_params(self.db).nth(resolved_arg.param_idx)
            else {
                continue;
            };
            let effect_idx = effect_view.index();
            let Some(provider_arg_idx) = provider_arg_idx_by_effect
                .get(effect_idx)
                .copied()
                .flatten()
            else {
                continue;
            };

            // Don't stomp explicit provider arguments (HIR unifies those already).
            if let Some(existing) = callable.generic_args().get(provider_arg_idx).copied()
                && !matches!(existing.data(self.db), TyData::TyVar(_))
            {
                continue;
            }

            let Some(inferred) = self.infer_effect_provider_for_resolved_arg(
                resolved_arg,
                caller_provider_arg_idx_by_effect.map(|map| map.as_slice()),
            ) else {
                continue;
            };
            let Some(inferred_provider_ty) = inferred.provider_ty else {
                continue;
            };

            if let Some(slot) = callable.generic_args_mut().get_mut(provider_arg_idx) {
                *slot = inferred_provider_ty;
            }
        }
    }

    fn materialize_value_in_temp_place(
        &mut self,
        ty: TyId<'db>,
        value: ValueId,
        addr_space: AddressSpaceKind,
        hint: &str,
    ) -> (ValueId, Place<'db>) {
        let addr_local = self.alloc_temp_local(ty, false, hint);
        self.builder.body.locals[addr_local.index()].address_space = addr_space;
        self.assign(
            None,
            Some(addr_local),
            Rvalue::Alloc {
                address_space: addr_space,
            },
        );

        let addr_value = self.alloc_value(
            ty,
            ValueOrigin::Local(addr_local),
            ValueRepr::Ref(addr_space),
        );
        let place = Place::new(addr_value, crate::ir::MirProjectionPath::new());
        self.push_inst_here(MirInst::Store {
            source: SourceInfoId::SYNTHETIC,
            place: place.clone(),
            value,
        });
        (addr_value, place)
    }

    fn lower_effect_arg(
        &mut self,
        resolved_arg: &ResolvedEffectArg<'db>,
        effect_writebacks: &mut Vec<(LocalId, Place<'db>)>,
    ) -> ValueId {
        // Handle ByPlace: resolve binding and materialize as needed
        if resolved_arg.pass_mode == EffectPassMode::ByPlace {
            let EffectArg::Place(place) = &resolved_arg.arg else {
                panic!("invalid effect argument for ByPlace: {resolved_arg:?}");
            };
            let PlaceBase::Binding(binding) = place.base;

            let addr_space = self.address_space_for_binding(&binding);
            let is_non_memory = !matches!(addr_space, AddressSpaceKind::Memory);

            if matches!(
                binding,
                LocalBinding::EffectParam { .. } | LocalBinding::ContractField { .. }
            ) {
                let value = self
                    .binding_value(binding)
                    .unwrap_or_else(|| panic!("missing value for effect binding `{binding:?}`"));
                return value;
            }

            let binding_ty = match binding {
                LocalBinding::Local { pat, .. } => self.typed_body.pat_ty(self.db, pat),
                LocalBinding::Param { ty, .. } => ty,
                LocalBinding::EffectParam { .. } => self.u256_ty(),
                LocalBinding::ContractField { .. } => self.u256_ty(),
            };

            let Some(local) = self.local_for_binding(binding) else {
                panic!("missing local for effect binding `{binding:?}`");
            };

            let value_repr = self.value_repr_for_ty(binding_ty, addr_space);

            // Storage providers are addressable as handles even when their logical type is
            // word-represented (e.g. transparent newtypes around `u256`).
            if value_repr.address_space().is_some() || is_non_memory {
                let value = self.alloc_value(binding_ty, ValueOrigin::Local(local), value_repr);
                return value;
            }

            // Memory provider: materialize in temp place.
            let initial = self.alloc_value(binding_ty, ValueOrigin::Local(local), ValueRepr::Word);
            let (addr_value, addr_place) = self.materialize_value_in_temp_place(
                binding_ty,
                initial,
                AddressSpaceKind::Memory,
                "eff",
            );
            if binding.is_mut() {
                effect_writebacks.push((local, addr_place));
            }
            return addr_value;
        }

        // Handle ByTempPlace: lower value and materialize if needed
        if resolved_arg.pass_mode == EffectPassMode::ByTempPlace {
            let EffectArg::Value(expr_id) = &resolved_arg.arg else {
                panic!("invalid effect argument for ByTempPlace: {resolved_arg:?}");
            };

            let value = self.lower_expr(*expr_id);

            if self.builder.body.value(value).repr.is_ref() {
                return value;
            }

            let ty = self.typed_body.expr_ty(self.db, *expr_id);
            let (addr_value, _) = self.materialize_value_in_temp_place(
                ty,
                value,
                AddressSpaceKind::Memory,
                "eff_tmp",
            );
            return addr_value;
        }

        // Handle ByValue: lower the value directly
        if resolved_arg.pass_mode == EffectPassMode::ByValue {
            let value = match &resolved_arg.arg {
                EffectArg::Value(expr_id) => self.lower_expr(*expr_id),
                EffectArg::Binding(binding) => self
                    .binding_value(*binding)
                    .unwrap_or_else(|| panic!("missing value for effect binding `{binding:?}`")),
                EffectArg::Unknown | EffectArg::Place(_) => {
                    panic!("invalid effect argument for ByValue: {resolved_arg:?}");
                }
            };

            return value;
        }

        panic!("invalid effect argument pass mode: {resolved_arg:?}");
    }

    fn lower_expr_into_local(&mut self, stmt: StmtId, expr: ExprId, dest: LocalId) -> ValueId {
        if self.current_block().is_none() {
            return self.ensure_value(expr);
        }

        if let Some(value_id) = self.try_lower_variant_ctor(expr, Some(dest)) {
            return value_id;
        }
        if let Some(value_id) = self.try_lower_unit_variant(expr, Some(dest)) {
            return value_id;
        }

        if self.typed_body.is_implicit_move(expr) {
            let value_id = self.lower_expr(expr);
            if self.current_block().is_some() {
                self.assign(Some(stmt), Some(dest), Rvalue::Value(value_id));
            }
            return value_id;
        }

        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Call(..) | Expr::MethodCall(..)) => {
                self.lower_call_expr_inner(expr, Some(dest), Some(stmt))
            }
            Partial::Present(Expr::Field(lhs, field_index)) => {
                let value_id = self.lower_field_expr(expr, *lhs, *field_index);
                if self.current_block().is_some() {
                    self.assign(Some(stmt), Some(dest), Rvalue::Value(value_id));
                }
                value_id
            }
            Partial::Present(Expr::Bin(lhs, rhs, BinOp::Index)) => {
                let value_id = self.lower_index_expr(expr, *lhs, *rhs);
                if self.current_block().is_some() {
                    self.assign(Some(stmt), Some(dest), Rvalue::Value(value_id));
                }
                value_id
            }
            _ => {
                let value_id = self.lower_expr(expr);
                if self.current_block().is_some() {
                    self.assign(Some(stmt), Some(dest), Rvalue::Value(value_id));
                }
                value_id
            }
        }
    }

    pub(super) fn effect_param_key_kind(
        &self,
        binding: LocalBinding<'db>,
    ) -> Option<EffectKeyKind> {
        let LocalBinding::EffectParam { site, idx, .. } = binding else {
            return None;
        };
        let idx = u32::try_from(idx).ok()?;

        let bindings = match site {
            EffectParamSite::Func(func) => func.effect_bindings(self.db),
            EffectParamSite::Contract(contract) => contract.effect_bindings(self.db),
            EffectParamSite::ContractInit { contract } => contract.init_effect_bindings(self.db),
            EffectParamSite::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => contract
                .recv(self.db, recv_idx)
                .unwrap()
                .arm(self.db, arm_idx)
                .unwrap()
                .effective_effect_bindings(self.db),
        };

        bindings
            .iter()
            .find(|binding| binding.binding_idx == idx)
            .map(|binding| binding.key_kind)
    }

    pub(super) fn effect_param_key_is_trait(&self, binding: LocalBinding<'db>) -> bool {
        matches!(
            self.effect_param_key_kind(binding),
            Some(EffectKeyKind::Trait)
        )
    }

    pub(super) fn effect_param_key_is_type(&self, binding: LocalBinding<'db>) -> bool {
        matches!(
            self.effect_param_key_kind(binding),
            Some(EffectKeyKind::Type)
        )
    }

    fn lower_path_expr(&mut self, expr: ExprId) -> ValueId {
        let value_id = self.ensure_value(expr);
        if self.current_block().is_none() {
            return value_id;
        }
        let Partial::Present(Expr::Path(_)) = expr.data(self.db, self.body) else {
            return value_id;
        };

        let ty = self.typed_body.expr_ty(self.db, expr);
        let prop = self.typed_body.expr_prop(self.db, expr);
        let Some(binding) = prop.binding else {
            return value_id;
        };
        if let Some(local) = self.local_for_binding(binding)
            && self.builder.body.spill_slots.contains_key(&local)
            && let Some(place) = self.place_for_borrow_expr(expr)
        {
            let dest = self.alloc_temp_local(ty, false, "spill");
            self.assign(None, Some(dest), Rvalue::Load { place });
            self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
            self.builder.body.values[value_id.index()].repr = self.value_repr_for_expr(expr, ty);
            return value_id;
        }
        if let Some(local) = self.local_for_binding(binding)
            && self.address_taken_locals.contains(&local)
            && matches!(
                crate::repr::repr_kind_for_ty(self.db, &self.core, ty),
                crate::repr::ReprKind::Word
            )
            && let Some(place) = self.place_for_borrow_expr(expr)
        {
            let dest = self.alloc_temp_local(ty, false, "addrload");
            self.builder.body.locals[dest.index()].address_space = self.place_address_space(&place);
            self.assign(None, Some(dest), Rvalue::Load { place });
            self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
            self.builder.body.values[value_id.index()].repr = ValueRepr::Word;
            return value_id;
        }
        if let Some(local) = self.local_for_binding(binding) {
            let local_ty = self.builder.body.local(local).ty;
            let local_address_space = self.builder.body.local(local).address_space;
            if let Some((_, inner_ty)) = local_ty.as_capability(self.db)
                && ty == inner_ty
            {
                let handle = self.alloc_value(
                    local_ty,
                    ValueOrigin::Local(local),
                    self.value_repr_for_ty(local_ty, local_address_space),
                );
                let lowered = self.coerce_contextual_capability_expr_value(expr, handle, local_ty);
                self.set_expr_value_from_lowered_value(value_id, lowered);
                return value_id;
            }
        }
        let is_effect_binding = matches!(
            binding,
            LocalBinding::EffectParam { .. } | LocalBinding::ContractField { .. }
        );
        if !is_effect_binding {
            return value_id;
        }
        if matches!(binding, LocalBinding::EffectParam { .. })
            && self.effect_param_key_is_trait(binding)
        {
            return value_id;
        }

        // Effect params are passed as provider addresses/slots. When their logical type is
        // word-represented (including transparent-newtype wrappers), path expressions should load
        // the value from that place (rather than treating the provider address as the value).
        if !matches!(
            crate::repr::repr_kind_for_ty(self.db, &self.core, ty),
            crate::repr::ReprKind::Word
        ) {
            return value_id;
        }

        let Some(binding_local) = self.local_for_binding(binding) else {
            return value_id;
        };
        if !matches!(
            self.builder.body.value(value_id).origin,
            ValueOrigin::Local(local) if local == binding_local
        ) {
            return value_id;
        }

        let Some(place) = self.place_for_expr(expr) else {
            return value_id;
        };
        let dest = self.alloc_temp_local(ty, false, "load");
        self.builder.body.locals[dest.index()].address_space = self.expr_address_space(expr);
        self.assign(None, Some(dest), Rvalue::Load { place });
        self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
        self.builder.body.values[value_id.index()].repr = ValueRepr::Word;
        value_id
    }

    fn lower_field_expr(
        &mut self,
        expr: ExprId,
        lhs: ExprId,
        field_index: Partial<FieldIndex<'db>>,
    ) -> ValueId {
        let value_id = self.ensure_value(expr);
        if self.current_block().is_none() {
            return value_id;
        }
        let Some(field_index) = field_index.to_opt() else {
            return value_id;
        };

        let base_value = self.lower_expr(lhs);
        if self.current_block().is_none() {
            return value_id;
        }

        let lhs_ty = self.typed_body.expr_ty(self.db, lhs);
        let lhs_place_ty = self.place_base_ty(lhs_ty);
        let Some(info) = self.field_access_info_for_expr(expr, lhs_place_ty, field_index) else {
            return value_id;
        };

        // Transparent newtype access: field 0 is a representation-preserving cast only when we
        // can preserve the value/handle directly. Dereferenceable scalar fields must still load
        // from the pointed-to storage/memory location.
        if self.is_transparent_field0(lhs_place_ty, info.field_idx)
            && self.transparent_field0_preserves_value(lhs_ty, base_value, info.field_ty)
        {
            let space = self.value_address_space_or_memory(base_value);
            let field_value = self.alloc_value(
                info.field_ty,
                ValueOrigin::TransparentCast { value: base_value },
                self.value_repr_for_ty(info.field_ty, space),
            );
            let lowered =
                self.coerce_contextual_capability_expr_value(expr, field_value, info.field_ty);
            self.set_expr_value_from_lowered_value(value_id, lowered);
            return value_id;
        }

        let place = if self.direct_deref_target_ty(lhs_ty).is_some() {
            let Some(mut place) = self.place_from_derefable_value(base_value, lhs_ty) else {
                return value_id;
            };
            place.projection.push(Projection::Field(info.field_idx));
            place
        } else {
            Place::new(
                base_value,
                MirProjectionPath::from_projection(Projection::Field(info.field_idx)),
            )
        };
        let addr_space = self.place_address_space(&place);
        let source_value =
            self.projection_source_value(expr, place, info.field_ty, addr_space, "load");
        let lowered =
            self.coerce_contextual_capability_expr_value(expr, source_value, info.field_ty);
        self.set_expr_value_from_lowered_value(value_id, lowered);
        value_id
    }

    /// Lower a range expression `start..end` into Range struct construction.
    ///
    /// This desugars range expressions into:
    /// 1. Evaluating start and end expressions
    /// 2. Allocating a Range struct
    /// 3. Storing start and end into the struct fields
    fn lower_range_expr(&mut self, expr: ExprId, start: ExprId, end: ExprId) -> ValueId {
        let value_id = self.ensure_value(expr);
        if self.current_block().is_none() {
            return value_id;
        }

        // Get the Range type (already set by type checker)
        let range_ty = self.typed_body.expr_ty(self.db, expr);

        // Lower start and end expressions
        let start_value = self.lower_expr(start);
        if self.current_block().is_none() {
            return value_id;
        }
        let end_value = self.lower_expr(end);
        if self.current_block().is_none() {
            return value_id;
        }

        if layout::is_zero_sized_ty(self.db, range_ty) {
            let value = &mut self.builder.body.values[value_id.index()];
            value.origin = ValueOrigin::Unit;
            value.repr = ValueRepr::Word;
            return value_id;
        }

        // Allocate memory for the struct
        let alloc_value = self.emit_alloc(expr, range_ty);

        // Get field indices for start and end
        let start_ident = IdentId::new(self.db, "start".to_string());
        let end_ident = IdentId::new(self.db, "end".to_string());

        let mut inits = Vec::with_capacity(2);

        // Add start field initialization
        if let Some(info) = self.field_access_info(range_ty, FieldIndex::Ident(start_ident))
            && !layout::is_zero_sized_ty(self.db, info.field_ty)
        {
            inits.push((
                MirProjectionPath::from_projection(Projection::Field(info.field_idx)),
                start_value,
            ));
        }

        // Add end field initialization
        if let Some(info) = self.field_access_info(range_ty, FieldIndex::Ident(end_ident))
            && !layout::is_zero_sized_ty(self.db, info.field_ty)
        {
            inits.push((
                MirProjectionPath::from_projection(Projection::Field(info.field_idx)),
                end_value,
            ));
        }

        // Emit the aggregate initialization
        self.emit_init_aggregate(alloc_value, inits);

        alloc_value
    }

    fn lower_index_expr(&mut self, expr: ExprId, lhs: ExprId, rhs: ExprId) -> ValueId {
        let value_id = self.ensure_value(expr);
        if self.current_block().is_none() {
            return value_id;
        }

        let lhs_ty = self.typed_body.expr_ty(self.db, lhs);
        let lhs_place_ty = self.place_base_ty(lhs_ty);
        if !lhs_place_ty.is_array(self.db) {
            return value_id;
        }
        let Some(elem_ty) = lhs_place_ty.generic_args(self.db).first().copied() else {
            return value_id;
        };

        let base_value = self.lower_expr(lhs);
        let index_source = self.lower_index_source(rhs);
        if self.current_block().is_none() {
            return value_id;
        }

        let place = if self.direct_deref_target_ty(lhs_ty).is_some() {
            let Some(mut place) = self.place_from_derefable_value(base_value, lhs_ty) else {
                return value_id;
            };
            place.projection.push(Projection::Index(index_source));
            place
        } else {
            Place::new(
                base_value,
                MirProjectionPath::from_projection(Projection::Index(index_source)),
            )
        };
        let addr_space = self.place_address_space(&place);
        let source_value = self.projection_source_value(expr, place, elem_ty, addr_space, "load");
        let lowered = self.coerce_contextual_capability_expr_value(expr, source_value, elem_ty);
        self.set_expr_value_from_lowered_value(value_id, lowered);
        value_id
    }

    fn load_result_address_space(
        &self,
        expr: ExprId,
        ty: TyId<'db>,
        source_space: AddressSpaceKind,
    ) -> AddressSpaceKind {
        crate::repr::pointer_info_for_ty(self.db, &self.core, ty, source_space)
            .map(|info| info.address_space)
            .unwrap_or_else(|| self.expr_address_space(expr))
    }

    fn field_access_info_for_expr(
        &self,
        expr: ExprId,
        owner_ty: TyId<'db>,
        field_index: FieldIndex<'db>,
    ) -> Option<FieldAccessInfo<'db>> {
        self.field_access_info(owner_ty, field_index).or_else(|| {
            let FieldIndex::Index(integer) = field_index else {
                return None;
            };
            let field_idx = integer.data(self.db).to_usize()?;
            Some(FieldAccessInfo {
                field_ty: self.typed_body.expr_ty(self.db, expr),
                field_idx,
            })
        })
    }

    fn is_transparent_field0(&self, owner_ty: TyId<'db>, field_idx: usize) -> bool {
        crate::repr::transparent_field0_inner_ty(self.db, owner_ty, field_idx).is_some()
    }

    /// Returns true if the expression is a method call (as opposed to a regular function call).
    fn is_method_call(&self, expr: ExprId) -> bool {
        let exprs = self.body.exprs(self.db);
        matches!(&exprs[expr], Partial::Present(Expr::MethodCall(..)))
    }

    fn needs_op_trait_call(&self, expr: ExprId) -> bool {
        let operand_ty = match expr.data(self.db, self.body) {
            Partial::Present(Expr::Bin(lhs, _, _)) => self.typed_body.expr_ty(self.db, *lhs),
            Partial::Present(Expr::Un(inner, _)) => self.typed_body.expr_ty(self.db, *inner),
            _ => return false,
        };
        if operand_ty.is_integral(self.db) || operand_ty.is_bool(self.db) {
            return false;
        }
        self.typed_body.callable_expr(expr).is_some()
    }

    // NOTE: field expressions are lowered via `lower_field_expr` so scalar loads become
    // explicit `MirInst::Load` instructions.

    // NOTE: array index expressions are lowered via `lower_index_expr` so scalar loads become
    // explicit `MirInst::Load` instructions.

    pub(super) fn place_for_borrow_expr(&mut self, expr: ExprId) -> Option<Place<'db>> {
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Path(_)) => {
                let ty = self.typed_body.expr_ty(self.db, expr);
                let binding = self.typed_body.expr_prop(self.db, expr).binding?;
                if matches!(binding, LocalBinding::EffectParam { .. })
                    && self.effect_param_key_is_trait(binding)
                {
                    return None;
                }

                let local = self.local_for_binding(binding)?;
                let local_ty = self.builder.body.local(local).ty;
                if self
                    .direct_deref_target_ty(local_ty)
                    .is_some_and(|target_ty| !self.is_by_ref_ty(target_ty))
                {
                    let addr_space = self.address_space_for_binding(&binding);
                    let base_value = self.alloc_value(
                        local_ty,
                        ValueOrigin::Local(local),
                        self.value_repr_for_ty(local_ty, addr_space),
                    );
                    return self.place_from_derefable_value(base_value, local_ty);
                }

                let is_runtime_place = self.is_by_ref_ty(ty)
                    || matches!(binding, LocalBinding::ContractField { .. })
                    || matches!(binding, LocalBinding::EffectParam { .. })
                        && self.effect_param_key_is_type(binding);
                let base_value = if is_runtime_place {
                    let addr_space = self.address_space_for_binding(&binding);
                    self.alloc_value(ty, ValueOrigin::Local(local), ValueRepr::Ref(addr_space))
                } else {
                    self.alloc_value(ty, ValueOrigin::PlaceRoot(local), ValueRepr::Word)
                };
                Some(Place::new(base_value, MirProjectionPath::new()))
            }
            Partial::Present(Expr::Field(lhs, field_index)) => {
                let field_index = field_index.to_opt()?;
                let lhs_ty = self.typed_body.expr_ty(self.db, *lhs);
                let lhs_place_ty = self.place_base_ty(lhs_ty);
                let info = self.field_access_info_for_expr(expr, lhs_place_ty, field_index)?;

                if self.is_transparent_field0(lhs_place_ty, info.field_idx) {
                    if self.direct_deref_target_ty(lhs_ty).is_some() {
                        return self.project_expr_place(
                            *lhs,
                            lhs_ty,
                            Projection::Field(info.field_idx),
                            true,
                        );
                    }

                    if let Some(place) = self.place_for_borrow_expr(*lhs) {
                        return Some(place);
                    }

                    let base_value = self.lower_expr(*lhs);
                    if self.builder.body.value(base_value).repr.is_ref() {
                        return Some(Place::new(base_value, MirProjectionPath::new()));
                    }
                    return None;
                }

                self.project_expr_place(*lhs, lhs_ty, Projection::Field(info.field_idx), true)
            }
            Partial::Present(Expr::Bin(lhs, rhs, BinOp::Index)) => {
                let lhs_ty = self.typed_body.expr_ty(self.db, *lhs);
                let lhs_place_ty = self.place_base_ty(lhs_ty);
                if !lhs_place_ty.is_array(self.db) {
                    return None;
                }

                let index_source = self.lower_index_source(*rhs);
                self.project_expr_place(*lhs, lhs_ty, Projection::Index(index_source), true)
            }
            _ => None,
        }
    }

    fn project_expr_place(
        &mut self,
        lhs: ExprId,
        lhs_ty: TyId<'db>,
        proj: Projection<TyId<'db>, EnumVariant<'db>, ValueId>,
        recurse_from_lhs_place: bool,
    ) -> Option<Place<'db>> {
        if self.direct_deref_target_ty(lhs_ty).is_some() {
            let base_value = self.lower_expr(lhs);
            let mut place = self.place_from_derefable_value(base_value, lhs_ty)?;
            place.projection.push(proj);
            return Some(place);
        }

        if recurse_from_lhs_place {
            let Place {
                base,
                mut projection,
            } = self.place_for_borrow_expr(lhs)?;
            projection.push(proj);
            return Some(Place::new(base, projection));
        }

        let base_value = self.lower_expr(lhs);
        Some(Place::new(
            base_value,
            MirProjectionPath::from_projection(proj),
        ))
    }

    fn place_for_expr(&mut self, expr: ExprId) -> Option<Place<'db>> {
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Path(_)) => {
                let ty = self.typed_body.expr_ty(self.db, expr);
                let binding = self.typed_body.expr_prop(self.db, expr).binding?;
                match binding {
                    LocalBinding::EffectParam { .. } => {
                        if self.effect_param_key_is_trait(binding) {
                            return None;
                        }
                    }
                    LocalBinding::ContractField { .. } => {}
                    _ => return None,
                }

                match crate::repr::repr_kind_for_ty(self.db, &self.core, ty) {
                    crate::repr::ReprKind::Zst | crate::repr::ReprKind::Ptr(_) => return None,
                    crate::repr::ReprKind::Word | crate::repr::ReprKind::Ref => {}
                }

                let local = self.local_for_binding(binding)?;
                let addr_space = self.address_space_for_binding(&binding);
                let base_value =
                    self.alloc_value(ty, ValueOrigin::Local(local), ValueRepr::Ref(addr_space));
                Some(Place::new(base_value, MirProjectionPath::new()))
            }
            Partial::Present(Expr::Field(lhs, field_index)) => {
                let field_index = field_index.to_opt()?;
                let lhs_ty = self.typed_body.expr_ty(self.db, *lhs);
                let lhs_place_ty = self.place_base_ty(lhs_ty);
                let info = self.field_access_info_for_expr(expr, lhs_place_ty, field_index)?;

                // Transparent newtypes: treat field 0 as the same place when the base is
                // already addressable, otherwise fall back to scalar newtype semantics.
                if self.is_transparent_field0(lhs_place_ty, info.field_idx) {
                    if self.direct_deref_target_ty(lhs_ty).is_some() {
                        return self.project_expr_place(
                            *lhs,
                            lhs_ty,
                            Projection::Field(info.field_idx),
                            false,
                        );
                    }

                    if let Some(place) = self.place_for_expr(*lhs) {
                        return Some(place);
                    }
                    let base_value = self.lower_expr(*lhs);
                    if self.builder.body.value(base_value).repr.is_ref() {
                        return Some(Place::new(base_value, MirProjectionPath::new()));
                    }
                    return None;
                }

                self.project_expr_place(*lhs, lhs_ty, Projection::Field(info.field_idx), false)
            }
            Partial::Present(Expr::Bin(lhs, rhs, BinOp::Index)) => {
                let lhs_ty = self.typed_body.expr_ty(self.db, *lhs);
                let lhs_place_ty = self.place_base_ty(lhs_ty);
                if !lhs_place_ty.is_array(self.db) {
                    return None;
                }
                let index_source = self.lower_index_source(*rhs);
                self.project_expr_place(*lhs, lhs_ty, Projection::Index(index_source), false)
            }
            _ => None,
        }
    }

    fn peel_transparent_newtype_field0_lvalue(&self, mut expr: ExprId) -> ExprId {
        loop {
            let Partial::Present(Expr::Field(base, field_index)) = expr.data(self.db, self.body)
            else {
                return expr;
            };
            let Some(field_index) = field_index.to_opt() else {
                return expr;
            };
            let base_ty = self.typed_body.expr_ty(self.db, *base);
            let Some(info) = self.field_access_info(base_ty, field_index) else {
                return expr;
            };
            if self.is_transparent_field0(base_ty, info.field_idx)
                && info.field_ty.as_capability(self.db).is_none()
            {
                expr = *base;
                continue;
            }
            return expr;
        }
    }

    fn root_lvalue_for_expr(&mut self, expr: ExprId) -> Option<RootLvalue<'db>> {
        if self
            .typed_body
            .expr_ty(self.db, expr)
            .as_capability(self.db)
            .is_some()
        {
            return self.place_for_borrow_expr(expr).map(RootLvalue::Place);
        }
        if let Some(binding) = self.typed_body.expr_prop(self.db, expr).binding
            && let Some(local) = self.local_for_binding(binding)
        {
            let local_ty = self.builder.body.local(local).ty;
            if self
                .direct_deref_target_ty(local_ty)
                .is_some_and(|target_ty| !self.is_by_ref_ty(target_ty))
            {
                return self.place_for_borrow_expr(expr).map(RootLvalue::Place);
            }
        }
        if let Some(place) = self.place_for_expr(expr) {
            return Some(RootLvalue::Place(place));
        }
        let binding = self.typed_body.expr_prop(self.db, expr).binding?;
        let local = self.local_for_binding(binding)?;
        if self.builder.body.spill_slots.contains_key(&local) {
            return self.place_for_borrow_expr(expr).map(RootLvalue::Place);
        }
        Some(RootLvalue::Local(local))
    }

    fn store_to_root_lvalue(
        &mut self,
        stmt: Option<StmtId>,
        lvalue: RootLvalue<'db>,
        value: ValueId,
    ) {
        match lvalue {
            RootLvalue::Place(place) => {
                self.mark_place_root_address_taken(&place);
                let source = stmt
                    .map(|stmt| self.source_for_stmt(stmt))
                    .unwrap_or(SourceInfoId::SYNTHETIC);
                self.push_inst_here(MirInst::Store {
                    source,
                    place,
                    value,
                });
            }
            RootLvalue::Local(local) => {
                self.assign(stmt, Some(local), Rvalue::Value(value));
            }
        }
    }

    fn lower_assign_to_lvalue(&mut self, stmt_id: StmtId, target: ExprId, value: ValueId) {
        let peeled_target = self.peel_transparent_newtype_field0_lvalue(target);
        if peeled_target != target {
            let root_ty = self.typed_body.expr_ty(self.db, peeled_target);
            let wrapped = self.alloc_value(
                root_ty,
                ValueOrigin::TransparentCast { value },
                self.value_repr_for_expr(peeled_target, root_ty),
            );
            if let Some(lvalue) = self.root_lvalue_for_expr(peeled_target) {
                self.store_to_root_lvalue(Some(stmt_id), lvalue, wrapped);
                return;
            }
        }

        if let Some(lvalue) = self.root_lvalue_for_expr(target) {
            self.store_to_root_lvalue(Some(stmt_id), lvalue, value);
        }
    }

    fn lower_aug_assign_to_lvalue(
        &mut self,
        stmt_id: StmtId,
        target: ExprId,
        rhs_value: ValueId,
        op: hir::hir_def::expr::ArithBinOp,
    ) {
        let peeled_target = self.peel_transparent_newtype_field0_lvalue(target);
        let is_peeled = peeled_target != target;

        let root_expr = if is_peeled { peeled_target } else { target };
        let root_ty = self.typed_body.expr_ty(self.db, root_expr);
        let lhs_ty = self.typed_body.expr_ty(self.db, target);
        let lhs_place_ty = self.place_base_ty(lhs_ty);

        let Some(root_lvalue) = self.root_lvalue_for_expr(root_expr) else {
            return;
        };

        let lhs_value = match &root_lvalue {
            RootLvalue::Place(place) => {
                let loaded_local = self.alloc_temp_local(lhs_place_ty, false, "load");
                self.builder.body.locals[loaded_local.index()].address_space =
                    self.expr_address_space(target);
                self.assign(
                    None,
                    Some(loaded_local),
                    Rvalue::Load {
                        place: place.clone(),
                    },
                );
                self.alloc_value(
                    lhs_place_ty,
                    ValueOrigin::Local(loaded_local),
                    ValueRepr::Word,
                )
            }
            RootLvalue::Local(local) => {
                if is_peeled {
                    let base_value = self.alloc_value(
                        root_ty,
                        ValueOrigin::Local(*local),
                        self.value_repr_for_ty(
                            root_ty,
                            self.builder.body.local(*local).address_space,
                        ),
                    );
                    self.alloc_value(
                        lhs_ty,
                        ValueOrigin::TransparentCast { value: base_value },
                        ValueRepr::Word,
                    )
                } else if self.direct_deref_target_ty(lhs_ty).is_some() {
                    let base_value = self.alloc_value(
                        lhs_ty,
                        ValueOrigin::Local(*local),
                        self.value_repr_for_ty(
                            lhs_ty,
                            self.builder.body.local(*local).address_space,
                        ),
                    );
                    if let Some(place) = self.place_from_derefable_value(base_value, lhs_ty) {
                        let loaded_local = self.alloc_temp_local(lhs_place_ty, false, "load");
                        self.builder.body.locals[loaded_local.index()].address_space =
                            self.expr_address_space(target);
                        self.assign(None, Some(loaded_local), Rvalue::Load { place });
                        self.alloc_value(
                            lhs_place_ty,
                            ValueOrigin::Local(loaded_local),
                            ValueRepr::Word,
                        )
                    } else {
                        self.alloc_value(lhs_ty, ValueOrigin::Local(*local), ValueRepr::Word)
                    }
                } else {
                    self.alloc_value(lhs_ty, ValueOrigin::Local(*local), ValueRepr::Word)
                }
            }
        };

        let updated = if matches!(
            op,
            ArithBinOp::Add | ArithBinOp::Sub | ArithBinOp::Mul | ArithBinOp::Div | ArithBinOp::Rem
        ) {
            let checked_op = match op {
                ArithBinOp::Add => crate::ir::CheckedArithmeticOp::Add,
                ArithBinOp::Sub => crate::ir::CheckedArithmeticOp::Sub,
                ArithBinOp::Mul => crate::ir::CheckedArithmeticOp::Mul,
                ArithBinOp::Div => crate::ir::CheckedArithmeticOp::Div,
                ArithBinOp::Rem => crate::ir::CheckedArithmeticOp::Rem,
                _ => unreachable!(),
            };

            let dest = self.alloc_temp_local(lhs_place_ty, false, "checked");
            self.builder.body.locals[dest.index()].address_space = self.expr_address_space(target);
            self.assign_checked_intrinsic_call(
                Some(stmt_id),
                dest,
                vec![lhs_value, rhs_value],
                crate::ir::CheckedIntrinsic {
                    op: checked_op,
                    ty: lhs_place_ty,
                },
                None,
            );

            self.alloc_value(lhs_place_ty, ValueOrigin::Local(dest), ValueRepr::Word)
        } else {
            self.alloc_value(
                lhs_place_ty,
                ValueOrigin::Binary {
                    op: BinOp::Arith(op),
                    lhs: lhs_value,
                    rhs: rhs_value,
                },
                ValueRepr::Word,
            )
        };

        let stored = if is_peeled {
            self.alloc_value(
                root_ty,
                ValueOrigin::TransparentCast { value: updated },
                self.value_repr_for_expr(root_expr, root_ty),
            )
        } else {
            updated
        };

        self.store_to_root_lvalue(Some(stmt_id), root_lvalue, stored);
    }

    fn lower_arith_aug_assign_via_trait_call(
        &mut self,
        stmt_id: StmtId,
        expr: ExprId,
        target: ExprId,
        rhs_expr: ExprId,
        rhs_value: ValueId,
        op: hir::hir_def::expr::ArithBinOp,
    ) -> bool {
        // When the core library is not available (standalone `.fe` files),
        // no callable is registered — fall back to unchecked raw binary.
        let Some(callable) = self.typed_body.callable_expr(expr).cloned() else {
            return false;
        };

        if !self.can_fast_path_aug_assign(&callable, op, self.typed_body.expr_ty(self.db, target)) {
            return self
                .lower_aug_assign_trait_call(stmt_id, expr, target, rhs_expr, rhs_value, callable);
        }
        self.lower_aug_assign_to_lvalue(stmt_id, target, rhs_value, op);
        true
    }

    fn lower_aug_assign_trait_call(
        &mut self,
        stmt_id: StmtId,
        expr: ExprId,
        target: ExprId,
        rhs_expr: ExprId,
        rhs_value: ValueId,
        mut callable: Callable<'db>,
    ) -> bool {
        let target_ty = self.typed_body.expr_ty(self.db, target);
        let Some(mut receiver_place) = self.place_for_borrow_expr(target) else {
            return false;
        };
        let mut writeback_place = None;
        if matches!(
            self.builder.body.value(receiver_place.base).origin,
            ValueOrigin::PlaceRoot(_)
        ) {
            let receiver_value = self.lower_expr(target);
            let (_addr_value, temp_place) = self.materialize_value_in_temp_place(
                target_ty,
                receiver_value,
                AddressSpaceKind::Memory,
                "aug_assign_self",
            );
            receiver_place = temp_place.clone();
            writeback_place = Some(temp_place);
        }
        let Some(receiver_ty) = callable
            .callable_def
            .receiver_ty(self.db)
            .map(|ty| ty.instantiate(self.db, callable.generic_args()))
        else {
            return false;
        };

        let receiver_space = self.value_address_space(receiver_place.base);
        let receiver_repr = self.value_repr_for_ty(receiver_ty, receiver_space);
        let receiver = self.alloc_value(
            receiver_ty,
            ValueOrigin::PlaceRef(receiver_place.clone()),
            receiver_repr,
        );

        let expected_arg_tys = self.call_expected_arg_tys(&callable);
        let Some(&expected_rhs_ty) = expected_arg_tys.get(1) else {
            return false;
        };
        let mut rhs = self.coerce_call_arg_value(rhs_expr, rhs_value, expected_rhs_ty);
        rhs = self.materialize_capability_call_arg(rhs_expr, rhs, expected_rhs_ty);

        let mut effect_args = Vec::new();
        let mut effect_writebacks: Vec<(LocalId, Place<'db>)> = Vec::new();
        if let CallableDef::Func(func_def) = callable.callable_def
            && func_def.has_effects(self.db)
            && extract_contract_function(self.db, func_def).is_none()
            && let Some(resolved) = self.typed_body.call_effect_args(expr)
        {
            self.finalize_place_effect_provider_args_for_call(func_def, &mut callable, resolved);
            for resolved_arg in resolved {
                let value = self.lower_effect_arg(resolved_arg, &mut effect_writebacks);
                effect_args.push(value);
            }
        }

        let hir_target = crate::ir::HirCallTarget {
            callable_def: callable.callable_def,
            generic_args: callable.generic_args().to_vec(),
            trait_inst: callable.trait_inst(),
        };
        let call_origin = CallOrigin {
            expr: Some(expr),
            hir_target: Some(hir_target),
            args: vec![receiver, rhs],
            effect_args,
            resolved_name: None,
            checked_intrinsic: None,
            builtin_terminator: self.builtin_terminator_kind(callable.callable_def),
            receiver_space: (receiver_space != AddressSpaceKind::Memory).then_some(receiver_space),
        };
        self.assign(Some(stmt_id), None, Rvalue::Call(call_origin));
        for (writeback_local, place) in effect_writebacks {
            self.assign(None, Some(writeback_local), Rvalue::Load { place });
        }

        if let Some(place) = writeback_place {
            let updated_local = self.alloc_temp_local(target_ty, false, "aug_assign");
            self.builder.body.locals[updated_local.index()].address_space =
                self.expr_address_space(target);
            self.assign(None, Some(updated_local), Rvalue::Load { place });
            let updated = self.alloc_value(
                target_ty,
                ValueOrigin::Local(updated_local),
                self.value_repr_for_expr(target, target_ty),
            );
            self.lower_assign_to_lvalue(stmt_id, target, updated);
        }
        true
    }

    fn can_fast_path_aug_assign(
        &self,
        callable: &Callable<'db>,
        op: hir::hir_def::expr::ArithBinOp,
        lhs_ty: TyId<'db>,
    ) -> bool {
        match callable.callable_def.ingot(self.db).kind(self.db) {
            IngotKind::Core | IngotKind::Std => {}
            _ => return false,
        }

        let Some(name) = callable.callable_def.name(self.db) else {
            return false;
        };
        if !matches!(
            op,
            ArithBinOp::Add
                | ArithBinOp::Sub
                | ArithBinOp::Mul
                | ArithBinOp::LShift
                | ArithBinOp::RShift
                | ArithBinOp::BitAnd
                | ArithBinOp::BitOr
                | ArithBinOp::BitXor
        ) {
            return false;
        }
        let expected_name = match op {
            ArithBinOp::Add => "add_assign",
            ArithBinOp::Sub => "sub_assign",
            ArithBinOp::Mul => "mul_assign",
            ArithBinOp::Div => "div_assign",
            ArithBinOp::Rem => "rem_assign",
            ArithBinOp::Pow => "pow_assign",
            ArithBinOp::LShift => "shl_assign",
            ArithBinOp::RShift => "shr_assign",
            ArithBinOp::BitAnd => "bitand_assign",
            ArithBinOp::BitOr => "bitor_assign",
            ArithBinOp::BitXor => "bitxor_assign",
            ArithBinOp::Range => return false,
        };
        if name.data(self.db).as_str() != expected_name {
            return false;
        }

        let lhs_place_ty = lhs_ty
            .as_capability(self.db)
            .map(|(_, inner)| inner)
            .unwrap_or(lhs_ty);
        lhs_place_ty.is_integral(self.db) || lhs_place_ty.is_bool(self.db)
    }

    /// Lowers a statement in the current block.
    ///
    /// # Parameters
    /// - `stmt_id`: Statement to lower.
    pub(super) fn lower_stmt(&mut self, stmt_id: StmtId) {
        let Some(block) = self.current_block() else {
            return;
        };
        let Partial::Present(stmt) = stmt_id.data(self.db, self.body) else {
            return;
        };
        match stmt {
            Stmt::Let(pat, _ty, value) => {
                self.move_to_block(block);
                let Partial::Present(pat_data) = pat.data(self.db, self.body) else {
                    return;
                };
                if self.current_block().is_none() {
                    return;
                }

                match pat_data {
                    Pat::Path(..) => {
                        let binding =
                            self.typed_body
                                .pat_binding(*pat)
                                .unwrap_or(LocalBinding::Local {
                                    pat: *pat,
                                    is_mut: matches!(pat_data, Pat::Path(_, true)),
                                });
                        let Some(local) = self.local_for_binding(binding) else {
                            return;
                        };
                        if let Some(expr) = value {
                            let value_id = self.lower_expr_into_local(stmt_id, *expr, local);
                            if self.current_block().is_none() {
                                return;
                            }
                            let pat_ty = self.typed_body.pat_ty(self.db, *pat);
                            let carries_space = self
                                .value_repr_for_ty(pat_ty, AddressSpaceKind::Memory)
                                .address_space()
                                .is_some()
                                || pat_ty.as_capability(self.db).is_some();
                            let _ = value_id;
                            if carries_space {
                                let space = crate::ir::lookup_local_pointer_leaf_info(
                                    &self.builder.body.locals,
                                    local,
                                    &MirProjectionPath::new(),
                                )
                                .map(|info| info.address_space)
                                .unwrap_or(self.builder.body.local(local).address_space);
                                self.set_pat_address_space(*pat, space);
                            }
                        } else {
                            self.assign(Some(stmt_id), Some(local), Rvalue::ZeroInit);
                        }
                    }
                    Pat::WildCard | Pat::Rest => {
                        if let Some(expr) = value {
                            let value_id = self.lower_expr(*expr);
                            if self.current_block().is_some() {
                                self.assign(Some(stmt_id), None, Rvalue::Value(value_id));
                            }
                        }
                    }
                    Pat::Tuple(_) | Pat::PathTuple(_, _) | Pat::Record(_, _) => {
                        let Some(expr) = value else {
                            return;
                        };
                        let value_id = self.lower_expr(*expr);
                        if self.current_block().is_some() {
                            self.bind_pat_value(*pat, value_id);
                        }
                    }
                    _ => {
                        if let Some(expr) = value {
                            let value_id = self.lower_expr(*expr);
                            if self.current_block().is_some() {
                                self.assign(Some(stmt_id), None, Rvalue::Value(value_id));
                            }
                        }
                    }
                }
            }
            Stmt::For(pat, iter_expr, body, unroll) => {
                self.lower_for(ForLoopParams {
                    stmt: stmt_id,
                    pat: *pat,
                    iter_expr: *iter_expr,
                    body_expr: *body,
                    unroll_hint: *unroll,
                });
            }
            Stmt::While(cond, body_expr) => self.lower_while(*cond, *body_expr),
            Stmt::Continue => {
                let scope = self.loop_stack.last().expect("continue outside of loop");
                self.goto(scope.continue_target);
            }
            Stmt::Break => {
                let scope = self.loop_stack.last().expect("break outside of loop");
                self.goto(scope.break_target);
            }
            Stmt::Return(value) => {
                self.move_to_block(block);
                let source = self.source_for_stmt(stmt_id);
                if let Some(expr) = value {
                    let ret_ty = self.return_ty;
                    let returns_value = !self.is_unit_ty(ret_ty) && !ret_ty.is_never(self.db);
                    if returns_value {
                        let expr_ty = self.typed_body.expr_ty(self.db, *expr);
                        let ret_value = self.lower_expr(*expr);
                        let ret_value = self.coerce_capability_value_to_target_ty(
                            *expr,
                            ret_value,
                            expr_ty,
                            ret_ty,
                            self.value_repr_for_ty(ret_ty, AddressSpaceKind::Memory),
                        );
                        if self.current_block().is_some() {
                            self.set_current_terminator(Terminator::Return {
                                source,
                                value: Some(ret_value),
                            });
                        }
                    } else {
                        self.lower_expr_stmt(stmt_id, *expr);
                        if self.current_block().is_some() {
                            self.set_current_terminator(Terminator::Return {
                                source,
                                value: None,
                            });
                        }
                    }
                } else if self.current_block().is_some() {
                    self.set_current_terminator(Terminator::Return {
                        source,
                        value: None,
                    });
                }
            }
            Stmt::Expr(expr) => self.lower_expr_stmt(stmt_id, *expr),
        }
    }

    /// Lowers a `while` loop statement and wires its control-flow edges.
    ///
    /// # Parameters
    /// - `cond_expr`: Condition expression id.
    /// - `body_expr`: Loop body expression id.
    ///
    pub(super) fn lower_while(&mut self, cond_expr: CondId, body_expr: ExprId) {
        let Some(block) = self.current_block() else {
            return;
        };
        let cond_entry = self.alloc_block();
        let body_block = self.alloc_block();
        let exit_block = self.alloc_block();

        self.move_to_block(block);
        self.goto(cond_entry);

        self.move_to_block(cond_entry);
        self.lower_condition_branch(cond_expr, body_block, exit_block);

        self.loop_stack.push(LoopScope {
            continue_target: cond_entry,
            break_target: exit_block,
        });

        self.move_to_block(body_block);
        let _ = self.lower_expr(body_expr);
        let body_end = self.current_block();

        self.loop_stack.pop();

        let mut backedge = None;
        if let Some(body_end_block) = body_end {
            self.move_to_block(body_end_block);
            self.goto(cond_entry);
            backedge = Some(body_end_block);
        }

        self.builder.body.loop_headers.insert(
            cond_entry,
            LoopInfo {
                body: body_block,
                exit: exit_block,
                backedge,
                init_block: None,
                post_block: None,
                unroll_hint: None,
                trip_count: None,
            },
        );

        self.move_to_block(exit_block);
    }

    /// Lowers a `for` loop by desugaring into a while loop.
    ///
    /// If the type checker resolved Seq::len and Seq::get methods for this loop,
    /// uses those to implement iteration. Otherwise falls back to special-cased
    /// array lowering (legacy path for ill-typed code).
    fn lower_for(&mut self, params: ForLoopParams) {
        let Some(block) = self.current_block() else {
            return;
        };

        // Try to use resolved Seq methods from type checker
        if let Some(seq_info) = self.typed_body.for_loop_seq(params.stmt).cloned() {
            self.lower_for_seq(params, block, &seq_info);
            return;
        }

        // Fallback to special-cased lowering for arrays.
        let iter_ty = self.typed_body.expr_ty(self.db, params.iter_expr);
        if iter_ty.is_array(self.db) {
            self.lower_for_array(
                params.stmt,
                params.pat,
                params.iter_expr,
                params.body_expr,
                block,
            );
        }
    }

    /// Lower a for-loop using the generic Seq trait methods.
    ///
    /// Desugars `for x in seq` into:
    /// ```
    /// let __idx = 0
    /// let __len = seq.len()
    /// while __idx < __len {
    ///     let x = seq.get(__idx)
    ///     body
    ///     __idx = __idx + 1
    /// }
    /// ```
    fn lower_for_seq(
        &mut self,
        params: ForLoopParams,
        block: BasicBlockId,
        seq_info: &ForLoopSeq<'db>,
    ) {
        let usize_ty = TyId::new(self.db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
        let elem_ty = seq_info.elem_ty;

        // Lower the iterable expression
        self.move_to_block(block);
        let iterable_value = self.lower_expr(params.iter_expr);
        let Some(after_iter_block) = self.current_block() else {
            return;
        };

        // Create hidden index local
        let idx_local = self.alloc_temp_local(usize_ty, true, "for_idx");

        // Initialize index to 0
        self.move_to_block(after_iter_block);
        let zero = self.synthetic_u256(BigUint::from(0u64));
        self.assign(Some(params.stmt), Some(idx_local), Rvalue::Value(zero));

        // Call Seq::len to get the length
        let len_value = self.emit_seq_len_call(
            iterable_value,
            &seq_info.len_callable,
            &seq_info.len_effect_args,
        );
        let Some(after_len_block) = self.current_block() else {
            return;
        };

        // Allocate blocks for loop structure
        let cond_entry = self.alloc_block();
        let body_block = self.alloc_block();
        let inc_block = self.alloc_block();
        let exit_block = self.alloc_block();

        // Jump to condition
        self.move_to_block(after_len_block);
        self.goto(cond_entry);

        // Condition: idx < len
        self.move_to_block(cond_entry);
        let idx_value = self.alloc_value(usize_ty, ValueOrigin::Local(idx_local), ValueRepr::Word);
        let cond_value = self.alloc_value(
            TyId::bool(self.db),
            ValueOrigin::Binary {
                op: BinOp::Comp(hir::hir_def::expr::CompBinOp::Lt),
                lhs: idx_value,
                rhs: len_value,
            },
            ValueRepr::Word,
        );
        let cond_header = cond_entry;

        // Set up loop scope
        self.loop_stack.push(LoopScope {
            continue_target: inc_block,
            break_target: exit_block,
        });

        // Body block: call get, bind element, execute body
        self.move_to_block(body_block);

        // Call Seq::get to get the element
        let idx_for_get =
            self.alloc_value(usize_ty, ValueOrigin::Local(idx_local), ValueRepr::Word);
        let elem_value = self.emit_seq_get_call(
            iterable_value,
            idx_for_get,
            elem_ty,
            &seq_info.get_callable,
            &seq_info.get_effect_args,
        );

        self.bind_pat_value(params.pat, elem_value);

        // Execute the body
        let _ = self.lower_expr(params.body_expr);
        let body_end = self.current_block();

        self.loop_stack.pop();

        // Normal fallthrough from the body goes to the increment block; `continue`
        // also targets `inc_block` via the loop scope.
        if let Some(body_end_block) = body_end {
            self.move_to_block(body_end_block);
            self.goto(inc_block);
        }

        // Increment block: idx = idx + 1; goto cond
        self.move_to_block(inc_block);
        let one = self.synthetic_u256(BigUint::from(1u64));
        let current_idx =
            self.alloc_value(usize_ty, ValueOrigin::Local(idx_local), ValueRepr::Word);
        let incremented = self.alloc_value(
            usize_ty,
            ValueOrigin::Binary {
                op: BinOp::Arith(ArithBinOp::Add),
                lhs: current_idx,
                rhs: one,
            },
            ValueRepr::Word,
        );
        self.assign(None, Some(idx_local), Rvalue::Value(incremented));
        let Some(inc_end) = self.current_block() else {
            return;
        };
        self.goto(cond_entry);

        // Wire up the branch
        self.move_to_block(cond_header);
        self.branch(cond_value, body_block, exit_block);

        // Compute static trip count from the iterator type
        let trip_count = {
            let iter_ty = self.typed_body.expr_ty(self.db, params.iter_expr);
            crate::layout::array_len_with_generic_args(self.db, iter_ty, self.generic_args)
        };

        // Register loop info with post_block for proper Yul for-loop emission
        self.builder.body.loop_headers.insert(
            cond_entry,
            LoopInfo {
                body: body_block,
                exit: exit_block,
                backedge: Some(inc_end),
                init_block: None,
                post_block: Some(inc_block),
                unroll_hint: params.unroll_hint,
                trip_count,
            },
        );

        self.move_to_block(exit_block);
    }

    /// Emit a synthesized call to Seq::len(self) -> usize
    fn emit_seq_len_call(
        &mut self,
        receiver: ValueId,
        callable: &Callable<'db>,
        resolved_effect_args: &[ResolvedEffectArg<'db>],
    ) -> ValueId {
        let usize_ty = TyId::new(self.db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));

        // Create a local for the result
        let result_local = self.alloc_temp_local(usize_ty, false, "seq_len");

        let mut receiver_space = None;
        let space = self.value_address_space_or_memory(receiver);
        if space != AddressSpaceKind::Memory {
            receiver_space = Some(space);
        }

        let mut effect_args = Vec::new();
        let mut effect_writebacks: Vec<(LocalId, Place<'db>)> = Vec::new();
        if let hir::hir_def::CallableDef::Func(func_def) = callable.callable_def
            && func_def.has_effects(self.db)
            && extract_contract_function(self.db, func_def).is_none()
        {
            for resolved_arg in resolved_effect_args {
                let value = self.lower_effect_arg(resolved_arg, &mut effect_writebacks);
                effect_args.push(value);
            }
        }

        let hir_target = crate::ir::HirCallTarget {
            callable_def: callable.callable_def,
            generic_args: callable.generic_args().to_vec(),
            trait_inst: callable.trait_inst(),
        };
        let call_origin = CallOrigin {
            expr: None,
            hir_target: Some(hir_target),
            args: vec![receiver],
            effect_args,
            receiver_space,
            resolved_name: None,
            checked_intrinsic: None,
            builtin_terminator: self.builtin_terminator_kind(callable.callable_def),
        };

        self.assign(None, Some(result_local), Rvalue::Call(call_origin));
        for (dest, place) in effect_writebacks {
            self.assign(None, Some(dest), Rvalue::Load { place });
        }

        self.alloc_value(usize_ty, ValueOrigin::Local(result_local), ValueRepr::Word)
    }

    /// Emit a synthesized call to Seq::get(self, i: usize) -> T
    fn emit_seq_get_call(
        &mut self,
        receiver: ValueId,
        index: ValueId,
        elem_ty: TyId<'db>,
        callable: &Callable<'db>,
        resolved_effect_args: &[ResolvedEffectArg<'db>],
    ) -> ValueId {
        let repr = if self.is_by_ref_ty(elem_ty) {
            ValueRepr::Ref(self.value_address_space(receiver))
        } else {
            ValueRepr::Word
        };

        // Create a local for the result
        let result_local = self.alloc_temp_local(elem_ty, false, "seq_get");
        if let ValueRepr::Ref(space) = repr {
            self.builder.body.locals[result_local.index()].address_space = space;
        }

        let mut receiver_space = None;
        let space = self.value_address_space_or_memory(receiver);
        if space != AddressSpaceKind::Memory {
            receiver_space = Some(space);
        }

        let mut effect_args = Vec::new();
        let mut effect_writebacks: Vec<(LocalId, Place<'db>)> = Vec::new();
        if let hir::hir_def::CallableDef::Func(func_def) = callable.callable_def
            && func_def.has_effects(self.db)
            && extract_contract_function(self.db, func_def).is_none()
        {
            for resolved_arg in resolved_effect_args {
                let value = self.lower_effect_arg(resolved_arg, &mut effect_writebacks);
                effect_args.push(value);
            }
        }

        let hir_target = crate::ir::HirCallTarget {
            callable_def: callable.callable_def,
            generic_args: callable.generic_args().to_vec(),
            trait_inst: callable.trait_inst(),
        };
        let call_origin = CallOrigin {
            expr: None,
            hir_target: Some(hir_target),
            args: vec![receiver, index],
            effect_args,
            receiver_space,
            resolved_name: None,
            checked_intrinsic: None,
            builtin_terminator: self.builtin_terminator_kind(callable.callable_def),
        };

        self.assign(None, Some(result_local), Rvalue::Call(call_origin));
        for (dest, place) in effect_writebacks {
            self.assign(None, Some(dest), Rvalue::Load { place });
        }

        self.alloc_value(elem_ty, ValueOrigin::Local(result_local), repr)
    }

    /// Lower a for-loop over an array.
    ///
    /// Desugars `for x in arr` into:
    /// ```
    /// let mut __idx = 0
    /// while __idx < arr.len {
    ///     let x = arr[__idx]
    ///     body
    ///     __idx = __idx + 1
    /// }
    /// ```
    fn lower_for_array(
        &mut self,
        stmt: StmtId,
        pat: PatId,
        iter_expr: ExprId,
        body_expr: ExprId,
        block: BasicBlockId,
    ) {
        let usize_ty = TyId::new(self.db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));

        // Lower the array expression
        self.move_to_block(block);
        let array_value = self.lower_expr(iter_expr);
        let Some(after_array_block) = self.current_block() else {
            return;
        };

        // Get array type info
        let array_ty = self.typed_body.expr_ty(self.db, iter_expr);
        let args = array_ty.generic_args(self.db);
        let elem_ty = args.first().copied().unwrap_or(usize_ty);

        // Get array length from type
        let Some(array_len) =
            layout::array_len_with_generic_args(self.db, array_ty, self.generic_args)
        else {
            if self.deferred_error.is_none() {
                let func_name = self
                    .hir_func
                    .map(|func| func.pretty_print_signature(self.db))
                    .unwrap_or_else(|| "<body owner>".to_string());
                self.deferred_error = Some(MirLowerError::Unsupported {
                    func_name,
                    message: format!(
                        "failed to resolve array length for `{}` in for-loop lowering",
                        array_ty.pretty_print(self.db)
                    ),
                });
            }
            return;
        };
        let len_value = self.synthetic_u256(BigUint::from(array_len));

        // Create hidden index local
        let idx_local = self.alloc_temp_local(usize_ty, true, "for_idx");

        // Initialize index to 0
        self.move_to_block(after_array_block);
        let zero = self.synthetic_u256(BigUint::from(0u64));
        self.assign(Some(stmt), Some(idx_local), Rvalue::Value(zero));

        // Allocate blocks
        let cond_entry = self.alloc_block();
        let body_block = self.alloc_block();
        let inc_block = self.alloc_block();
        let exit_block = self.alloc_block();

        // Jump to condition
        self.goto(cond_entry);

        // Condition: idx < len
        self.move_to_block(cond_entry);
        let idx_value = self.alloc_value(usize_ty, ValueOrigin::Local(idx_local), ValueRepr::Word);
        let cond_value = self.alloc_value(
            TyId::bool(self.db),
            ValueOrigin::Binary {
                op: BinOp::Comp(hir::hir_def::expr::CompBinOp::Lt),
                lhs: idx_value,
                rhs: len_value,
            },
            ValueRepr::Word,
        );
        let cond_header = cond_entry;

        // Set up loop scope
        self.loop_stack.push(LoopScope {
            continue_target: inc_block,
            break_target: exit_block,
        });

        // Body block: first bind element, then execute body
        self.move_to_block(body_block);

        // Bind element: elem = arr[idx]
        let idx_for_access =
            self.alloc_value(usize_ty, ValueOrigin::Local(idx_local), ValueRepr::Word);

        let place = Place::new(
            array_value,
            MirProjectionPath::from_projection(Projection::Index(IndexSource::Dynamic(
                idx_for_access,
            ))),
        );

        let elem_value = if self.is_by_ref_ty(elem_ty) {
            let addr_space = self.value_address_space(array_value);
            self.alloc_value(
                elem_ty,
                ValueOrigin::PlaceRef(place),
                ValueRepr::Ref(addr_space),
            )
        } else {
            let dest = self.alloc_temp_local(elem_ty, false, "load");
            self.assign(None, Some(dest), Rvalue::Load { place });
            self.alloc_value(
                elem_ty,
                ValueOrigin::Local(dest),
                self.value_repr_for_ty(elem_ty, AddressSpaceKind::Memory),
            )
        };

        self.bind_pat_value(pat, elem_value);

        // Execute the body
        let _ = self.lower_expr(body_expr);
        let body_end = self.current_block();

        self.loop_stack.pop();

        // Normal fallthrough from the body goes to the increment block; `continue`
        // also targets `inc_block` via the loop scope.
        if let Some(body_end_block) = body_end {
            self.move_to_block(body_end_block);
            self.goto(inc_block);
        }

        self.move_to_block(inc_block);
        let one = self.synthetic_u256(BigUint::from(1u64));
        let current_idx =
            self.alloc_value(usize_ty, ValueOrigin::Local(idx_local), ValueRepr::Word);
        let incremented = self.alloc_value(
            usize_ty,
            ValueOrigin::Binary {
                op: BinOp::Arith(ArithBinOp::Add),
                lhs: current_idx,
                rhs: one,
            },
            ValueRepr::Word,
        );
        self.assign(None, Some(idx_local), Rvalue::Value(incremented));
        let Some(inc_end) = self.current_block() else {
            return;
        };
        self.goto(cond_entry);

        // Wire up branch
        self.move_to_block(cond_header);
        self.branch(cond_value, body_block, exit_block);

        // Register loop info with post_block for proper Yul for-loop emission
        self.builder.body.loop_headers.insert(
            cond_entry,
            LoopInfo {
                body: body_block,
                exit: exit_block,
                backedge: Some(inc_end),
                init_block: None,
                post_block: Some(inc_block),
                unroll_hint: None,
                trip_count: Some(array_len),
            },
        );

        self.move_to_block(exit_block);
    }

    fn lower_condition_branch(
        &mut self,
        cond_expr: CondId,
        true_block: BasicBlockId,
        false_block: BasicBlockId,
    ) {
        let Partial::Present(cond_data) = cond_expr.data(self.db, self.body) else {
            if self.current_block().is_some() {
                self.goto(false_block);
            }
            return;
        };

        match cond_data {
            Cond::Bin(lhs, rhs, hir::hir_def::expr::LogicalBinOp::And) => {
                let rhs_entry = self.alloc_block();
                self.lower_condition_branch(*lhs, rhs_entry, false_block);
                self.move_to_block(rhs_entry);
                self.lower_condition_branch(*rhs, true_block, false_block);
            }
            Cond::Bin(lhs, rhs, hir::hir_def::expr::LogicalBinOp::Or) => {
                let rhs_entry = self.alloc_block();
                self.lower_condition_branch(*lhs, true_block, rhs_entry);
                self.move_to_block(rhs_entry);
                self.lower_condition_branch(*rhs, true_block, false_block);
            }
            Cond::Let(pat, scrutinee) => {
                self.lower_let_condition_branch(*pat, *scrutinee, true_block, false_block);
            }
            Cond::Expr(expr) => {
                let cond_val = self.lower_expr(*expr);
                if self.current_block().is_some() {
                    self.branch(cond_val, true_block, false_block);
                }
            }
        }
    }

    fn lower_if(
        &mut self,
        if_expr: ExprId,
        cond: CondId,
        then_expr: ExprId,
        else_expr: Option<ExprId>,
    ) -> ValueId {
        let value = self.ensure_value(if_expr);
        let Some(block) = self.current_block() else {
            return value;
        };

        let if_ty = self.typed_body.expr_ty(self.db, if_expr);
        let produces_value = !self.is_unit_ty(if_ty) && !if_ty.is_never(self.db);

        let then_block = self.alloc_block();
        if produces_value {
            let Some(else_expr) = else_expr else {
                debug_assert!(
                    false,
                    "value-producing if expressions must have an else branch"
                );
                self.builder.body.values[value.index()].origin = ValueOrigin::Unit;
                return value;
            };

            let result_local = self.alloc_temp_local(if_ty, true, "if");
            self.builder.body.values[value.index()].origin = ValueOrigin::Local(result_local);
            let else_block = self.alloc_block();

            self.move_to_block(block);
            self.assign(None, Some(result_local), Rvalue::ZeroInit);
            self.lower_condition_branch(cond, then_block, else_block);
            self.move_to_block(then_block);
            let then_value = self.lower_expr(then_expr);
            let then_end = self.current_block();

            self.move_to_block(else_block);
            let else_value = self.lower_expr(else_expr);
            let else_end = self.current_block();

            let merge_block = if then_end.is_some() || else_end.is_some() {
                Some(self.alloc_block())
            } else {
                None
            };

            if let Some(merge) = merge_block {
                if let Some(end_block) = then_end {
                    self.move_to_block(end_block);
                    self.assign(None, Some(result_local), Rvalue::Value(then_value));
                    self.goto(merge);
                }
                if let Some(end_block) = else_end {
                    self.move_to_block(end_block);
                    self.assign(None, Some(result_local), Rvalue::Value(else_value));
                    self.goto(merge);
                }
            }

            if let Some(merge) = merge_block {
                self.move_to_block(merge);
            }
        } else {
            self.builder.body.values[value.index()].origin = ValueOrigin::Unit;
            let merge_block = self.alloc_block();
            let else_block = else_expr.map(|_| self.alloc_block());

            self.move_to_block(block);
            self.lower_condition_branch(cond, then_block, else_block.unwrap_or(merge_block));

            self.move_to_block(then_block);
            let _ = self.lower_expr(then_expr);
            let then_end = self.current_block();

            let else_end = if let Some(else_expr) = else_expr {
                let else_block = else_block.expect("else_block allocated");
                self.move_to_block(else_block);
                let _ = self.lower_expr(else_expr);
                self.current_block()
            } else {
                Some(merge_block)
            };

            if then_end.is_none() && else_end.is_none() {
                return value;
            }

            if let Some(end_block) = then_end {
                self.move_to_block(end_block);
                self.goto(merge_block);
            }
            if let Some(end_block) = else_end
                && end_block != merge_block
            {
                self.move_to_block(end_block);
                self.goto(merge_block);
            }

            self.move_to_block(merge_block);
        }

        value
    }

    /// Returns whether the given type is the unit tuple type.
    ///
    /// # Parameters
    /// - `ty`: Type to inspect.
    ///
    /// # Returns
    /// `true` if the type is unit.
    pub(super) fn is_unit_ty(&self, ty: TyId<'db>) -> bool {
        ty.is_tuple(self.db) && ty.field_count(self.db) == 0
    }

    /// Lowers an expression statement, emitting side-effecting instructions as needed.
    ///
    /// # Parameters
    /// - `stmt_id`: Statement id for context.
    /// - `expr`: Expression id to lower.
    pub(super) fn lower_expr_stmt(&mut self, stmt_id: StmtId, expr: ExprId) {
        let Some(block) = self.current_block() else {
            return;
        };
        if self.try_lower_intrinsic_stmt(expr).is_some() {
            return;
        }
        let exprs = self.body.exprs(self.db);
        let Partial::Present(expr_data) = &exprs[expr] else {
            return;
        };

        match expr_data {
            Expr::With(_, _) => {
                self.move_to_block(block);
                let value_id = self.lower_expr(expr);
                let ty = self.typed_body.expr_ty(self.db, expr);
                if self.current_block().is_some() && !self.is_unit_ty(ty) && !ty.is_never(self.db) {
                    self.assign(Some(stmt_id), None, Rvalue::Value(value_id));
                }
            }
            Expr::Block(stmts) => {
                self.move_to_block(block);
                if stmts.is_empty() {
                    return;
                }
                let (head, last) = stmts.split_at(stmts.len() - 1);
                self.lower_block(head);
                if self.current_block().is_none() {
                    return;
                }
                let stmt_id = last[0];
                let Partial::Present(stmt) = stmt_id.data(self.db, self.body) else {
                    return;
                };
                if let Stmt::Expr(expr) = stmt {
                    self.lower_expr_stmt(stmt_id, *expr);
                } else {
                    self.lower_stmt(stmt_id);
                }
            }
            Expr::Assign(target, value) => {
                self.move_to_block(block);
                let value_id = self.lower_expr(*value);
                if self.place_for_expr(*target).is_none()
                    && let Some(binding) = self.typed_body.expr_prop(self.db, *target).binding
                    && let LocalBinding::Local { pat, .. } = binding
                {
                    let pat_ty = self.typed_body.pat_ty(self.db, pat);
                    let carries_space = self
                        .value_repr_for_ty(pat_ty, AddressSpaceKind::Memory)
                        .address_space()
                        .is_some()
                        || pat_ty.as_capability(self.db).is_some();
                    if carries_space
                        && let Some(space) = try_value_address_space_in(
                            &self.builder.body.values,
                            &self.builder.body.locals,
                            value_id,
                        )
                    {
                        self.set_pat_address_space(pat, space);
                    }
                }

                self.lower_assign_to_lvalue(stmt_id, *target, value_id);
            }
            Expr::AugAssign(target, value, op) => {
                self.move_to_block(block);
                let value_id = self.lower_expr(*value);
                if self.place_for_expr(*target).is_none()
                    && let Some(binding) = self.typed_body.expr_prop(self.db, *target).binding
                    && let LocalBinding::Local { pat, .. } = binding
                {
                    let pat_ty = self.typed_body.pat_ty(self.db, pat);
                    let carries_space = self
                        .value_repr_for_ty(pat_ty, AddressSpaceKind::Memory)
                        .address_space()
                        .is_some()
                        || pat_ty.as_capability(self.db).is_some();
                    if carries_space
                        && let Some(space) = try_value_address_space_in(
                            &self.builder.body.values,
                            &self.builder.body.locals,
                            value_id,
                        )
                    {
                        self.set_pat_address_space(pat, space);
                    }
                }

                if !self.lower_arith_aug_assign_via_trait_call(
                    stmt_id, expr, *target, *value, value_id, *op,
                ) {
                    self.lower_aug_assign_to_lvalue(stmt_id, *target, value_id, *op);
                }
            }
            _ => {
                self.move_to_block(block);
                let value_id = self.lower_expr(expr);
                if self.current_block().is_some() {
                    self.assign(Some(stmt_id), None, Rvalue::Value(value_id));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::analysis::ty::ty_check::check_func_body;
    use hir::analysis::{
        name_resolution::{PathRes, resolve_path},
        ty::trait_resolution::PredicateListId,
    };
    use url::Url;

    use super::*;
    use crate::lower::lower_function;

    #[test]
    fn storage_field_borrow_keeps_storage_pointer_info() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///storage_field_borrow_keeps_storage_pointer_info.fe").unwrap();
        let src = r#"
struct CoinStore {
    alice: u256,
}

fn borrow_storage_field_handle() -> u256
    uses (store: mut CoinStore)
{
    let p: mut u256 = mut store.alice
    p += 1
    store.alice
}
"#;

        let file = db.workspace().touch(&mut db, url, Some(src.to_owned()));
        let top_mod = db.top_mod(file);
        let hir_func = top_mod
            .all_funcs(&db)
            .iter()
            .copied()
            .find(|func| {
                func.name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "borrow_storage_field_handle")
            })
            .expect("expected HIR `borrow_storage_field_handle`");
        let assumptions = PredicateListId::empty_list(&db);
        let key_path = hir_func
            .effect_params(&db)
            .next()
            .and_then(|effect| effect.key_path(&db))
            .expect("expected effect key path");
        match resolve_path(&db, key_path, hir_func.scope(), assumptions, false)
            .expect("effect key path should resolve")
        {
            PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => {
                assert_eq!(ty.pretty_print(&db), "CoinStore");
            }
            other => panic!("expected `CoinStore` effect key to resolve as a type, got {other:?}"),
        }
        let module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter()
            .find(|func| func.symbol_name.contains("borrow_storage_field_handle"))
            .expect("expected `borrow_storage_field_handle`");
        let place_ref_value = func
            .body
            .values
            .iter()
            .enumerate()
            .find_map(|(idx, value)| match &value.origin {
                ValueOrigin::PlaceRef(place) if place.projection.is_empty() => {
                    Some((ValueId(idx as u32), value, place))
                }
                _ => None,
            })
            .expect("expected place-ref value");
        assert_eq!(
            crate::repr::place_yields_location_value(
                &db,
                &CoreLib::new(&db, top_mod.scope()),
                &func.body.values,
                &func.body.locals,
                place_ref_value.2,
                place_ref_value.1.ty,
                place_ref_value.1.pointer_info,
            ),
            Some(true),
            "single-field storage providers should still be recognized as location-valued handles",
        );
        let p_local = func
            .body
            .locals
            .iter()
            .enumerate()
            .find_map(|(idx, local)| (local.name == "p").then_some(LocalId(idx as u32)))
            .expect("expected local `p`");
        assert_eq!(
            func.body.local(p_local).address_space,
            AddressSpaceKind::Storage,
            "borrowed storage field local should keep storage address space",
        );
        assert_eq!(
            crate::ir::lookup_local_pointer_leaf_info(
                &func.body.locals,
                p_local,
                &MirProjectionPath::new(),
            )
            .expect("borrowed storage field local should carry root pointer info")
            .address_space,
            AddressSpaceKind::Storage,
        );
        let p_value = func
            .body
            .values
            .iter()
            .enumerate()
            .find_map(|(idx, value)| match value.origin {
                ValueOrigin::Local(local) if local == p_local => Some(ValueId(idx as u32)),
                _ => None,
            })
            .expect("expected root value for local `p`");
        assert_eq!(
            func.body
                .value_pointer_info(p_value)
                .expect("borrowed storage field value should carry pointer info")
                .address_space,
            AddressSpaceKind::Storage,
        );
        for value in &func.body.values {
            if matches!(
                value.origin,
                ValueOrigin::PlaceRef(_) | ValueOrigin::MoveOut { .. }
            ) && value.repr.address_space() == Some(AddressSpaceKind::Storage)
            {
                assert_eq!(
                    value
                        .pointer_info
                        .expect("storage place values should carry pointer info")
                        .address_space,
                    AddressSpaceKind::Storage,
                    "storage place values must not retain memory pointer metadata",
                );
            }
        }
    }

    #[test]
    fn mem_ptr_binding_keeps_mem_ptr_type_and_pointer_info() {
        let mut db = DriverDataBase::default();
        let url =
            Url::parse("file:///mem_ptr_binding_keeps_mem_ptr_type_and_pointer_info.fe").unwrap();
        let src = r#"
use std::evm::{MemPtr, RawMem}

struct Foo {
    a: u256,
}

fn test() uses (mem: mut RawMem) {
    let mp: MemPtr<Foo> = mem.mem_ptr(0x100)
    with (mp) {}
}
"#;

        let file = db.workspace().touch(&mut db, url, Some(src.to_owned()));
        let top_mod = db.top_mod(file);
        let module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter()
            .find(|func| func.symbol_name.contains("test"))
            .expect("expected `test`");
        let mp_local = func
            .body
            .locals
            .iter()
            .enumerate()
            .find_map(|(idx, local)| (local.name == "mp").then_some(LocalId(idx as u32)))
            .expect("expected local `mp`");
        let mp_ty = func.body.local(mp_local).ty;
        assert!(
            crate::repr::effect_provider_space_for_ty(
                &db,
                &CoreLib::new(&db, top_mod.scope()),
                mp_ty,
            ) == Some(AddressSpaceKind::Memory),
            "expected local `mp` to keep `MemPtr<_>` type, got `{}`",
            mp_ty.pretty_print(&db),
        );
        assert_eq!(
            crate::ir::lookup_local_pointer_leaf_info(
                &func.body.locals,
                mp_local,
                &MirProjectionPath::new(),
            )
            .expect("mem ptr local should carry root pointer info")
            .address_space,
            AddressSpaceKind::Memory,
        );
    }

    #[test]
    fn mixed_effect_provider_bindings_keep_distinct_spaces() {
        let mut db = DriverDataBase::default();
        let url =
            Url::parse("file:///mixed_effect_provider_bindings_keep_distinct_spaces.fe").unwrap();
        let src = r#"
use std::evm::{MemPtr, RawMem, RawStorage, StorPtr}

struct Foo {
    a: u256,
}

fn test() uses (st: mut RawStorage, mem: mut RawMem) {
    let mp: MemPtr<Foo> = mem.mem_ptr(0x100)
    let sp: StorPtr<Foo> = st.stor_ptr(0)
    with (mp) {}
    with (sp) {}
}
"#;

        let file = db.workspace().touch(&mut db, url, Some(src.to_owned()));
        let top_mod = db.top_mod(file);
        let module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter()
            .find(|func| func.symbol_name.contains("test"))
            .expect("expected `test`");
        let local_by_name = |name: &str| {
            func.body
                .locals
                .iter()
                .enumerate()
                .find_map(|(idx, local)| (local.name == name).then_some(LocalId(idx as u32)))
                .unwrap_or_else(|| panic!("expected local `{name}`"))
        };

        let mp_local = local_by_name("mp");
        let sp_local = local_by_name("sp");
        let root = MirProjectionPath::new();

        assert_eq!(
            crate::ir::lookup_local_pointer_leaf_info(&func.body.locals, mp_local, &root)
                .expect("mem ptr local should carry root pointer info")
                .address_space,
            AddressSpaceKind::Memory,
        );
        assert_eq!(
            crate::ir::lookup_local_pointer_leaf_info(&func.body.locals, sp_local, &root)
                .expect("stor ptr local should carry root pointer info")
                .address_space,
            AddressSpaceKind::Storage,
        );
    }

    #[test]
    fn resolved_place_keeps_pointer_field_in_container_space() {
        let mut db = DriverDataBase::default();
        let url =
            Url::parse("file:///resolved_place_keeps_pointer_field_in_container_space.fe").unwrap();
        let src = r#"
use std::evm::{RawStorage, StorPtr}

struct Cell {
    value: u256,
}

struct Holder {
    ptr: StorPtr<Cell>,
    tag: u256,
}

fn extract_ptr() -> StorPtr<Cell> uses (st: mut RawStorage) {
    let holder = Holder { ptr: st.stor_ptr(0), tag: 1 }
    holder.ptr
}
"#;

        let file = db.workspace().touch(&mut db, url, Some(src.to_owned()));
        let top_mod = db.top_mod(file);
        let module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter()
            .find(|func| func.symbol_name.contains("extract_ptr"))
            .expect("expected `extract_ptr`");
        let return_value = match &func.body.blocks[0].terminator {
            crate::ir::Terminator::Return {
                value: Some(value), ..
            } => *value,
            other => panic!("expected return terminator, got {other:?}"),
        };
        let ValueOrigin::MoveOut { place } = &func.body.value(return_value).origin else {
            panic!("expected return value to move out of a place");
        };

        let resolved = crate::repr::resolve_place(
            &db,
            &CoreLib::new(&db, top_mod.scope()),
            &func.body.values,
            &func.body.locals,
            place,
        )
        .expect("place should resolve");
        let assumptions = PredicateListId::empty_list(&db);
        let final_ty = hir::analysis::ty::normalize::normalize_ty(
            &db,
            resolved.final_state().ty,
            top_mod.scope(),
            assumptions,
        );
        let return_ty = hir::analysis::ty::normalize::normalize_ty(
            &db,
            func.body.value(return_value).ty,
            top_mod.scope(),
            assumptions,
        );

        assert_eq!(resolved.segments.len(), 1);
        let segment = &resolved.segments[0];
        assert_eq!(segment.start_kind, None);
        assert_eq!(
            segment.base.location_address_space(),
            Some(AddressSpaceKind::Memory),
            "handle field location should stay in wrapper memory",
        );
        assert_eq!(segment.projections.len(), 1);
        assert!(matches!(
            segment.projections[0].projection,
            Projection::Field(0)
        ));
        assert_eq!(
            segment.terminal_state().location_address_space(),
            Some(AddressSpaceKind::Memory),
            "field access itself should remain in the container address space",
        );
        assert_eq!(
            segment
                .terminal_state()
                .pointer_info
                .expect("pointer field should keep pointee metadata")
                .address_space,
            AddressSpaceKind::Storage,
            "field value should carry the pointee address space without changing the field location",
        );
        assert_eq!(
            crate::repr::place_yields_location_value(
                &db,
                &CoreLib::new(&db, top_mod.scope()),
                &func.body.values,
                &func.body.locals,
                place,
                func.body.value(return_value).ty,
                func.body.value_pointer_info(return_value),
            ),
            Some(false),
            "moving a handle field out of a container must load the stored handle value, not reuse the field location (return_ty={}, final_ty={}, pointer_info={:?}, deref_target={:?})",
            return_ty.pretty_print(&db),
            final_ty.pretty_print(&db),
            func.body.value_pointer_info(return_value),
            crate::repr::direct_deref_target_ty(
                &db,
                &CoreLib::new(&db, top_mod.scope()),
                return_ty
            ),
        );

        for func in module
            .functions
            .iter()
            .filter(|func| func.symbol_name.contains("extract_ptr"))
        {
            let return_value = match &func.body.blocks[0].terminator {
                crate::ir::Terminator::Return {
                    value: Some(value), ..
                } => *value,
                _ => continue,
            };
            let ValueOrigin::MoveOut { place } = &func.body.value(return_value).origin else {
                continue;
            };
            assert_eq!(
                crate::repr::place_yields_location_value(
                    &db,
                    &CoreLib::new(&db, top_mod.scope()),
                    &func.body.values,
                    &func.body.locals,
                    place,
                    func.body.value(return_value).ty,
                    func.body.value_pointer_info(return_value),
                ),
                Some(false),
                "specialized `{}` must also load the handle field value instead of reusing the field location",
                func.symbol_name,
            );
        }
    }

    #[test]
    fn match_arm_scalar_payload_return_loads_the_payload_value() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///match_arm_scalar_payload_return_loads_the_payload_value.fe")
            .unwrap();
        let src = r#"
enum E {
    A(u256),
    B,
}

fn f(e: E) -> u256 {
    match e {
        E::A(v) => return v
        E::B => return 0
    }
}
"#;

        let file = db.workspace().touch(&mut db, url, Some(src.to_owned()));
        let top_mod = db.top_mod(file);
        let module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter()
            .find(|func| func.symbol_name == "f")
            .expect("expected `f`");

        let return_value = func
            .body
            .blocks
            .iter()
            .filter_map(|block| match &block.terminator {
                crate::ir::Terminator::Return {
                    value: Some(value), ..
                } => Some(*value),
                _ => None,
            })
            .find(|value| !matches!(func.body.value(*value).origin, ValueOrigin::Synthetic(_)))
            .expect("expected non-synthetic scalar return value");

        assert!(
            matches!(func.body.value(return_value).origin, ValueOrigin::Local(_)),
            "match-arm scalar payload return should materialize a scalar local after contextual return coercion",
        );

        assert_eq!(
            func.body.value(return_value).repr,
            ValueRepr::Word,
            "scalar enum payload returns must stay word-represented",
        );
        assert_eq!(
            func.body.value(return_value).runtime_shape,
            crate::ir::RuntimeShape::Word(crate::ir::RuntimeWordKind::I256),
            "scalar enum payload returns must stay word-shaped",
        );
        assert!(
            func.body.value_pointer_info(return_value).is_none(),
            "scalar enum payload returns must not retain pointer metadata",
        );
    }

    #[test]
    fn resolved_place_uses_explicit_deref_for_loaded_capability_values() {
        let mut db = DriverDataBase::default();
        let url = Url::parse(
            "file:///resolved_place_uses_explicit_deref_for_loaded_capability_values.fe",
        )
        .unwrap();
        let src = r#"
struct Cell {
    value: u256,
}

struct CellMover {
    cell: mut Cell,
    tag: u256,
}

impl CellMover {
    fn bump(mut self, by: u256) -> u256 {
        self.cell.value += by
        self.cell.value
    }
}
"#;

        let file = db.workspace().touch(&mut db, url, Some(src.to_owned()));
        let top_mod = db.top_mod(file);
        let module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter()
            .find(|func| func.symbol_name.contains("bump"))
            .expect("expected `bump`");

        let load_places = func
            .body
            .blocks
            .iter()
            .flat_map(|block| block.insts.iter())
            .filter_map(|inst| match inst {
                crate::ir::MirInst::Assign {
                    rvalue: Rvalue::Load { place },
                    ..
                } => Some(place),
                _ => None,
            })
            .collect::<Vec<_>>();

        let pointee_field_place = load_places
            .iter()
            .find(|place| matches!(func.body.value(place.base).origin, ValueOrigin::Local(_)))
            .copied()
            .expect("expected a load rooted on a loaded capability value");

        let resolved = crate::repr::resolve_place(
            &db,
            &CoreLib::new(&db, top_mod.scope()),
            &func.body.values,
            &func.body.locals,
            pointee_field_place,
        )
        .expect("place should resolve");

        assert_eq!(resolved.segments.len(), 1);
        let segment = &resolved.segments[0];
        assert_eq!(
            segment.start_kind,
            Some(crate::repr::DerefStepKind::UseBaseValue),
            "loaded handle values should cross into the pointee via an explicit deref step",
        );
        assert_eq!(
            segment.before.pointer_info.map(|info| info.address_space),
            Some(AddressSpaceKind::Memory),
            "loaded `mut Cell` handle should retain memory pointee metadata",
        );
        assert_eq!(
            segment.base.location_address_space(),
            Some(AddressSpaceKind::Memory),
            "after deref, the place should be in the pointee address space",
        );
        assert_eq!(segment.projections.len(), 1);
        assert!(matches!(
            segment.projections[0].projection,
            Projection::Field(0)
        ));
    }

    #[test]
    fn resolved_place_keeps_empty_handle_roots_as_values() {
        let mut db = DriverDataBase::default();
        let url =
            Url::parse("file:///resolved_place_keeps_empty_handle_roots_as_values.fe").unwrap();
        let src = r#"
fn forward(value: mut u256) -> mut u256 {
    value
}

fn entry() -> u256 {
    let mut local = 7
    let value = forward(mut local)
    value += 5
    value
}
"#;

        let file = db.workspace().touch(&mut db, url, Some(src.to_owned()));
        let top_mod = db.top_mod(file);
        let module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter()
            .find(|func| func.symbol_name.contains("entry"))
            .expect("expected `entry`");

        let scalar_handle_value = func
            .body
            .values
            .iter()
            .enumerate()
            .find_map(|(idx, value)| {
                value
                    .ty
                    .as_capability(&db)
                    .is_some()
                    .then_some((idx, value))
                    .filter(|(_, value)| matches!(value.origin, ValueOrigin::Local(_)))
            })
            .map(|(idx, _)| ValueId(idx as u32))
            .expect("expected a loaded scalar handle local");
        let scalar_handle_place = Place::new(scalar_handle_value, MirProjectionPath::new());

        let resolved = crate::repr::resolve_place(
            &db,
            &CoreLib::new(&db, top_mod.scope()),
            &func.body.values,
            &func.body.locals,
            &scalar_handle_place,
        )
        .expect("place should resolve");

        assert_eq!(resolved.segments.len(), 1);
        let segment = &resolved.segments[0];
        assert_eq!(
            segment.start_kind, None,
            "empty handle-root places should remain value roots until MIR inserts an explicit deref",
        );
        assert_eq!(
            segment.base.location_address_space(),
            None,
            "empty handle-root places are not locations without an explicit deref",
        );
        assert_eq!(
            segment.base.ty.pretty_print(&db),
            "mut u256",
            "empty handle-root places should preserve the handle type",
        );
        assert!(segment.projections.is_empty());
    }

    #[test]
    fn scalar_loads_from_handle_roots_use_explicit_deref() {
        let mut db = DriverDataBase::default();
        let url =
            Url::parse("file:///scalar_loads_from_handle_roots_use_explicit_deref.fe").unwrap();
        let src = r#"
fn forward(value: mut u256) -> mut u256 {
    value
}

fn entry() -> u256 {
    let mut local = 7
    let value = forward(mut local)
    value += 5
    value
}
"#;

        let file = db.workspace().touch(&mut db, url, Some(src.to_owned()));
        let top_mod = db.top_mod(file);
        let module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter()
            .find(|func| func.symbol_name.contains("entry"))
            .expect("expected `entry`");

        let (loaded_local, loaded_value) = func.body.blocks[0]
            .insts
            .iter()
            .find_map(|inst| match inst {
                crate::ir::MirInst::Assign {
                    dest: Some(local),
                    rvalue: Rvalue::Load { place },
                    ..
                } if func.body.local(*local).ty.pretty_print(&db) == "u256"
                    && func.body.value(place.base).ty.pretty_print(&db) == "mut u256"
                    && place.projection.iter().eq([Projection::Deref].iter()) =>
                {
                    let value = func
                        .body
                        .values
                        .iter()
                        .enumerate()
                        .find_map(|(idx, value)| {
                            matches!(value.origin, ValueOrigin::Local(origin) if origin == *local)
                                .then_some(ValueId(idx as u32))
                        })
                        .expect("expected value for loaded scalar local");
                    Some((*local, value))
                }
                _ => None,
            })
            .expect("expected scalar load rooted on a handle local");

        assert!(
            !func.body.blocks[0].insts.iter().any(|inst| {
                matches!(
                    inst,
                    crate::ir::MirInst::Assign {
                        rvalue: Rvalue::Load { place },
                        ..
                    } if func.body.value(place.base).ty.pretty_print(&db) == "mut u256"
                        && place.projection.is_empty()
                )
            }),
            "handle-root scalar loads must not rely on implicit base deref",
        );

        assert!(
            func.body.local(loaded_local).pointer_leaf_infos.is_empty(),
            "loading a scalar through an explicit deref must not retain handle leaf metadata",
        );
        assert!(
            func.body.value_pointer_info(loaded_value).is_none(),
            "loaded scalar value must not carry the handle pointer metadata",
        );
    }

    #[test]
    fn path_reads_from_address_taken_word_locals_use_loads() {
        let mut db = DriverDataBase::default();
        let url =
            Url::parse("file:///path_reads_from_address_taken_word_locals_use_loads.fe").unwrap();
        let src = r#"
pub fn borrow_handles_readback() -> u256 {
    let mut x: u256 = 0
    let p: mut u256 = mut x
    p = 5
    x
}
"#;

        let file = db.workspace().touch(&mut db, url, Some(src.to_owned()));
        let top_mod = db.top_mod(file);
        let module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter()
            .find(|func| func.symbol_name.contains("borrow_handles_readback"))
            .expect("expected `borrow_handles_readback`");

        let x_local = func
            .body
            .locals
            .iter()
            .enumerate()
            .find_map(|(idx, local)| (local.name == "x").then_some(LocalId(idx as u32)))
            .expect("expected local `x`");
        let canonical_local = func
            .body
            .spill_slots
            .get(&x_local)
            .copied()
            .unwrap_or(x_local);

        assert!(
            func.body.blocks[0].insts.iter().any(|inst| {
                matches!(
                    inst,
                    crate::ir::MirInst::Assign {
                        rvalue: Rvalue::Load { place },
                        ..
                    } if crate::ir::resolve_local_projection_root(&func.body.values, place.base)
                        .is_some_and(|(local, projection)| {
                            local == canonical_local && projection.is_empty()
                        })
                        && place.projection.is_empty()
                )
            }),
            "reading an address-taken word local should reload from its canonical place",
        );
    }

    #[test]
    fn repr_lowered_mem_handle_root_scalar_loads_drop_handle_pointer_metadata() {
        let mut db = DriverDataBase::default();
        let url = Url::parse(
            "file:///repr_lowered_mem_handle_root_scalar_loads_drop_handle_pointer_metadata.fe",
        )
        .unwrap();
        let file = db.workspace().touch(
            &mut db,
            url,
            Some(
                include_str!("../../../codegen/tests/fixtures/explicit_raw_boundaries.fe")
                    .to_owned(),
            ),
        );
        let top_mod = db.top_mod(file);
        let module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter()
            .find(|func| func.symbol_name.contains("bump__MemPtr_u256"))
            .expect("expected `bump__MemPtr_u256` specialization");

        let (loaded_local, loaded_value) = func.body.blocks[0]
            .insts
            .iter()
            .find_map(|inst| match inst {
                crate::ir::MirInst::Assign {
                    dest: Some(local),
                    rvalue: Rvalue::Load { place },
                    ..
                } if func.body.local(*local).ty.pretty_print(&db) == "u256"
                    && func.body.value(place.base).ty.pretty_print(&db) == "u256"
                    && place.projection.is_empty() =>
                {
                    let value = func
                        .body
                        .values
                        .iter()
                        .enumerate()
                        .find_map(|(idx, value)| {
                            matches!(value.origin, ValueOrigin::Local(origin) if origin == *local)
                                .then_some(ValueId(idx as u32))
                        })
                        .expect("expected value for loaded scalar local");
                    Some((*local, value))
                }
                _ => None,
            })
            .expect("expected scalar load rooted on the repr-lowered memory handle local");

        assert!(
            func.body.local(loaded_local).pointer_leaf_infos.is_empty(),
            "repr-lowered scalar loads from a memory handle root must not retain handle leaf metadata",
        );
        assert!(
            func.body.value_pointer_info(loaded_value).is_none(),
            "repr-lowered scalar values must not carry the memory handle pointer metadata",
        );
    }

    #[test]
    fn aug_assign_through_handle_local_stays_a_store() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///aug_assign_through_handle_local_stays_a_store.fe").unwrap();
        let src = r#"
fn forward(value: mut u256) -> mut u256 {
    value
}

fn entry() -> u256 {
    let mut local = 7
    let value = forward(mut local)
    value += 5
    value
}
"#;

        let file = db.workspace().touch(&mut db, url, Some(src.to_owned()));
        let top_mod = db.top_mod(file);
        let module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter()
            .find(|func| func.symbol_name.contains("entry"))
            .expect("expected `entry`");

        let handle_local = func.body.blocks[0]
            .insts
            .iter()
            .find_map(|inst| match inst {
                crate::ir::MirInst::Assign {
                    dest: Some(local),
                    rvalue: Rvalue::Call(_),
                    ..
                } if func.body.local(*local).ty.pretty_print(&db) == "mut u256" => Some(*local),
                _ => None,
            })
            .expect("expected call result handle local");

        assert!(
            func.body.blocks[0].insts.iter().any(|inst| {
                matches!(
                    inst,
                    crate::ir::MirInst::Store { place, value, .. }
                        if place.projection.iter().eq([Projection::Deref].iter())
                            && matches!(func.body.value(place.base).origin, ValueOrigin::Local(local) if local == handle_local)
                            && func.body.value(*value).ty.pretty_print(&db) == "u256"
                )
            }),
            "augmenting through a handle local must stay a store to the pointee location",
        );
        assert!(
            !func.body.blocks[0].insts.iter().any(|inst| {
                matches!(
                    inst,
                    crate::ir::MirInst::Assign {
                        dest: Some(local),
                        rvalue: Rvalue::Value(value),
                        ..
                    } if *local == handle_local && func.body.value(*value).ty.pretty_print(&db) == "u256"
                )
            }),
            "augmenting through a handle local must not be rewritten into a direct local assignment",
        );
    }

    #[test]
    fn place_ref_values_keep_handle_pointer_metadata() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///place_ref_values_keep_handle_pointer_metadata.fe").unwrap();
        let src = r#"
fn forward(value: mut u256) -> mut u256 {
    value
}

fn entry() -> u256 {
    let mut local = 7
    let value = forward(mut local)
    value
}
"#;

        let file = db.workspace().touch(&mut db, url, Some(src.to_owned()));
        let top_mod = db.top_mod(file);
        let module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter()
            .find(|func| func.symbol_name.contains("entry"))
            .expect("expected `entry`");

        let call_arg = func
            .body
            .blocks
            .iter()
            .flat_map(|block| block.insts.iter())
            .find_map(|inst| match inst {
                crate::ir::MirInst::Assign {
                    rvalue: Rvalue::Call(call),
                    ..
                } => call.args.first().copied(),
                _ => None,
            })
            .expect("expected call argument");

        let info = func
            .body
            .value_pointer_info(call_arg)
            .expect("place-ref handle argument should carry pointer metadata");
        assert_eq!(info.address_space, AddressSpaceKind::Memory);
        assert_eq!(
            info.target_ty
                .expect("handle metadata should preserve the pointee type")
                .pretty_print(&db),
            "u256",
        );
        assert_eq!(
            func.body.value_address_space(call_arg),
            AddressSpaceKind::Memory
        );
    }

    #[test]
    fn transparent_scalar_field_reads_do_not_retain_handle_pointer_metadata() {
        let mut db = DriverDataBase::default();
        let url = Url::parse(
            "file:///transparent_scalar_field_reads_do_not_retain_handle_pointer_metadata.fe",
        )
        .unwrap();
        let src = r#"
struct WrapU8 {
    inner: u8,
}

struct Container {
    w: WrapU8,
    pad: u256,
}

fn newtype_u8_roundtrip(x: u8) -> u8 {
    let c = Container { w: WrapU8 { inner: x }, pad: 0 }
    let w = ref c.w
    w.inner
}
"#;

        let file = db.workspace().touch(&mut db, url, Some(src.to_owned()));
        let top_mod = db.top_mod(file);
        let hir_func = top_mod
            .all_funcs(&db)
            .iter()
            .copied()
            .find(|func| {
                func.name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "newtype_u8_roundtrip")
            })
            .expect("expected `newtype_u8_roundtrip`");
        let (diags, typed_body) = check_func_body(&db, hir_func);
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:#?}");

        let lowered = lower_function(
            &db,
            hir_func,
            typed_body.clone(),
            None,
            Vec::new(),
            Vec::new(),
            Vec::new(),
        )
        .expect("function should lower");

        let has_invalid_scalar_deref = |body: &crate::MirBody<'_>| {
            body.blocks.iter().any(|block| {
                block.insts.iter().any(|inst| {
                    matches!(
                        inst,
                        crate::ir::MirInst::Assign {
                            rvalue:
                                Rvalue::Load {
                                    place: Place {
                                        base,
                                        projection,
                                    },
                                },
                            ..
                        } if projection.iter().eq([Projection::Deref].iter())
                            && body.value(*base).ty.pretty_print(&db) == "u8"
                    )
                })
            })
        };

        let mut body = lowered.body.clone();
        assert!(
            !has_invalid_scalar_deref(&body),
            "invalid scalar deref should not exist before MIR transforms"
        );

        crate::transform::canonicalize_transparent_newtypes(&db, &mut body);
        crate::transform::insert_temp_binds(&db, &mut body);
        crate::transform::lower_capability_to_repr(
            &db,
            &crate::CoreLib::new(&db, top_mod.scope()),
            &mut body,
        );
        crate::transform::canonicalize_transparent_newtypes(&db, &mut body);
        crate::transform::insert_temp_binds(&db, &mut body);
        crate::transform::canonicalize_zero_sized(&db, &mut body);

        let func = crate::ir::MirFunction { body, ..lowered };

        assert!(
            !has_invalid_scalar_deref(&func.body),
            "invalid scalar deref should not exist after repr lowering"
        );

        let func = crate::lower_module(&db, top_mod)
            .expect("module should lower")
            .functions
            .into_iter()
            .find(|func| func.symbol_name.contains("newtype_u8_roundtrip"))
            .expect("expected `newtype_u8_roundtrip`");

        let return_value = match &func.body.blocks[0].terminator {
            crate::ir::Terminator::Return {
                value: Some(value), ..
            } => *value,
            other => panic!("expected return terminator, got {other:?}"),
        };

        assert_eq!(func.body.value(return_value).ty.pretty_print(&db), "u8");
        assert!(
            func.body.value_pointer_info(return_value).is_none(),
            "scalar transparent field reads must not retain handle pointer metadata",
        );
    }

    #[test]
    fn effect_handle_field_deref_places_resolve() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///effect_handle_field_deref_places_resolve.fe").unwrap();
        let file = db.workspace().touch(
            &mut db,
            url,
            Some(
                include_str!("../../../codegen/tests/fixtures/effect_handle_field_deref.fe")
                    .to_owned(),
            ),
        );
        let top_mod = db.top_mod(file);
        let module = crate::lower_module(&db, top_mod).expect("module should lower");
        let core = crate::CoreLib::new(&db, top_mod.scope());

        for func in &module.functions {
            for (bb_idx, block) in func.body.blocks.iter().enumerate() {
                for inst in &block.insts {
                    let unresolved = match inst {
                        crate::ir::MirInst::Assign {
                            rvalue: Rvalue::Load { place },
                            ..
                        }
                        | crate::ir::MirInst::Store { place, .. }
                        | crate::ir::MirInst::InitAggregate { place, .. }
                        | crate::ir::MirInst::SetDiscriminant { place, .. } => {
                            crate::repr::resolve_place(
                                &db,
                                &core,
                                &func.body.values,
                                &func.body.locals,
                                place,
                            )
                            .is_none()
                            .then_some(place)
                        }
                        crate::ir::MirInst::Assign { .. }
                        | crate::ir::MirInst::BindValue { .. } => None,
                    };

                    if let Some(place) = unresolved {
                        let base = func.body.value(place.base);
                        panic!(
                            "unresolved place in {} bb{}: {:?} (base ty={}, repr={:?}, origin={:?}, pointer_info={:?})",
                            func.symbol_name,
                            bb_idx,
                            place,
                            base.ty.pretty_print(&db),
                            base.repr,
                            base.origin,
                            func.body.value_pointer_info(place.base),
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn method_receiver_coercion_loads_through_ref_pointer_like_newtype() {
        let mut db = DriverDataBase::default();
        let url = Url::parse(
            "file:///method_receiver_coercion_loads_through_ref_pointer_like_newtype.fe",
        )
        .unwrap();
        let file = db.workspace().touch(
            &mut db,
            url,
            Some(
                include_str!(
                    "../../../fe/tests/fixtures/fe_test/view_param_local_ref_take_reverse.fe"
                )
                .to_owned(),
            ),
        );
        let top_mod = db.top_mod(file);
        let module = crate::lower_module(&db, top_mod).expect("module should lower");

        let func = module
            .functions
            .iter()
            .find(|func| {
                func.symbol_name.contains("take_i__t__")
                    && func.symbol_name.contains("get__u256_Reverse")
            })
            .expect("expected Take<Reverse>::get specialization");

        let block = &func.body.blocks[0];
        let ref_reverse_local = block
            .insts
            .iter()
            .find_map(|inst| match inst {
                crate::ir::MirInst::Assign {
                    dest: Some(local),
                    rvalue: Rvalue::Load { .. },
                    ..
                } if func.body.local(*local).ty.pretty_print(&db)
                    == "ref Reverse<u256, [u256; 8]>" =>
                {
                    Some(*local)
                }
                _ => None,
            })
            .expect("expected loaded `ref Reverse` local");

        let reverse_call_receiver = block
            .insts
            .iter()
            .find_map(|inst| match inst {
                crate::ir::MirInst::Assign {
                    rvalue: Rvalue::Call(call),
                    ..
                } if call.resolved_name.as_deref().is_some_and(|name| {
                    name.contains("reverse_i__t__") && name.contains("get__u256__u256__8__")
                }) =>
                {
                    call.args.first().copied()
                }
                _ => None,
            })
            .expect("expected Reverse::get call result");
        let reverse_call_receiver_local = match func.body.value(reverse_call_receiver).origin {
            ValueOrigin::Local(local) => local,
            ValueOrigin::TransparentCast { value } => {
                let Some((CapabilityKind::View, inner_ty)) =
                    func.body.value(reverse_call_receiver).ty.as_capability(&db)
                else {
                    panic!(
                        "expected Reverse receiver arg to be a local or view-cast local, got {:?}",
                        func.body.value(reverse_call_receiver).origin
                    );
                };
                assert_eq!(
                    inner_ty.pretty_print(&db),
                    "Reverse<u256, [u256; 8]>",
                    "expected receiver view cast to target Reverse",
                );
                let ValueOrigin::Local(local) = func.body.value(value).origin else {
                    panic!(
                        "expected receiver view cast to wrap a local, got {:?}",
                        func.body.value(value).origin
                    );
                };
                local
            }
            _ => {
                panic!(
                    "expected Reverse receiver arg to be a local or view-cast local, got {:?}",
                    func.body.value(reverse_call_receiver).origin
                );
            }
        };

        let receiver_load_place = block
            .insts
            .iter()
            .find_map(|inst| match inst {
                crate::ir::MirInst::Assign {
                    dest: Some(local),
                    rvalue: Rvalue::Load { place },
                    ..
                } if *local == reverse_call_receiver_local => Some(place),
                _ => None,
            })
            .expect("expected Reverse receiver load");

        let ValueOrigin::Local(local) = func.body.value(receiver_load_place.base).origin else {
            panic!(
                "expected Reverse receiver load to be rooted on the loaded `ref Reverse`, got {:?}",
                func.body.value(receiver_load_place.base).origin
            );
        };
        assert_eq!(
            local, ref_reverse_local,
            "receiver coercion must load through the `ref Reverse` local instead of reloading the original field",
        );
        assert!(
            receiver_load_place
                .projection
                .iter()
                .eq([Projection::Deref].iter()),
            "receiver coercion should dereference the loaded `ref Reverse`",
        );
    }

    #[test]
    fn delegated_field_into_local_lowering_does_not_self_assign() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///delegated_field_into_local_lowering_does_not_self_assign.fe")
            .unwrap();
        let file = db.workspace().touch(
            &mut db,
            url,
            Some(
                include_str!(
                    "../../../fe/tests/fixtures/fe_test/view_param_local_ref_take_reverse.fe"
                )
                .to_owned(),
            ),
        );
        let top_mod = db.top_mod(file);
        let module = crate::lower_module(&db, top_mod).expect("module should lower");

        let func = module
            .functions
            .iter()
            .find(|func| {
                func.symbol_name.contains("take_i__t__")
                    && func.symbol_name.contains("len__u256_Reverse")
            })
            .expect("expected Take<Reverse>::len specialization");

        for block in &func.body.blocks {
            for inst in &block.insts {
                let crate::ir::MirInst::Assign {
                    dest: Some(local),
                    rvalue: Rvalue::Value(value),
                    ..
                } = inst
                else {
                    continue;
                };

                assert!(
                    !matches!(
                        func.body.value(*value).origin,
                        ValueOrigin::Local(src_local) if src_local == *local
                    ),
                    "field/index delegation must not rewrite the RHS value into the destination local",
                );
            }
        }
    }

    #[test]
    fn ref_to_by_ref_path_stays_a_place_ref_to_the_value_location() {
        let mut db = DriverDataBase::default();
        let url =
            Url::parse("file:///ref_to_by_ref_path_stays_a_place_ref_to_the_value_location.fe")
                .unwrap();
        let file = db.workspace().touch(
            &mut db,
            url,
            Some(
                include_str!(
                    "../../../fe/tests/fixtures/fe_test/view_param_local_ref_take_reverse.fe"
                )
                .to_owned(),
            ),
        );
        let top_mod = db.top_mod(file);
        let module = crate::lower_module(&db, top_mod).expect("module should lower");

        let func = module
            .functions
            .iter()
            .find(|func| func.symbol_name == "sum_last4")
            .expect("expected `sum_last4`");

        let reverse_call_arg = func.body.blocks[0]
            .insts
            .iter()
            .find_map(|inst| match inst {
                crate::ir::MirInst::Assign {
                    rvalue: Rvalue::Call(call),
                    ..
                } if call
                    .resolved_name
                    .as_deref()
                    .is_some_and(|name| name.contains("reverse_u256")) =>
                {
                    call.args.first().copied()
                }
                _ => None,
            })
            .expect("expected reverse_u256 call");

        let value = func.body.value(reverse_call_arg);
        let ValueOrigin::PlaceRef(place) = &value.origin else {
            panic!(
                "expected `ref arr` arg to lower as PlaceRef, got {:?}",
                value.origin
            );
        };
        assert_eq!(value.ty.pretty_print(&db), "ref [u256; 8]");
        let ValueOrigin::Local(local) = func.body.value(place.base).origin else {
            panic!(
                "expected `ref arr` place root to be the parameter local, got {:?}",
                func.body.value(place.base).origin
            );
        };
        assert_eq!(func.body.local(local).ty.pretty_print(&db), "[u256; 8]");
        assert!(
            place.projection.is_empty(),
            "`ref arr` should reuse the array location"
        );
        let core = crate::CoreLib::new(&db, top_mod.scope());
        let resolved =
            crate::repr::resolve_place(&db, &core, &func.body.values, &func.body.locals, place)
                .expect("`ref arr` place should resolve");
        assert_eq!(
            resolved.final_state().ty.pretty_print(&db),
            "[u256; 8]",
            "`ref arr` should resolve to the array value location",
        );
        assert_eq!(
            resolved.final_state().location_address_space(),
            Some(AddressSpaceKind::Memory),
            "`ref arr` should resolve as a memory location",
        );
        assert_eq!(
            func.body.value(place.base).repr,
            ValueRepr::Ref(AddressSpaceKind::Memory),
            "the borrowed array root should stay by-reference",
        );
        assert_eq!(
            func.body
                .value_pointer_info(reverse_call_arg)
                .expect("`ref arr` arg should preserve pointer metadata")
                .target_ty
                .expect("`ref arr` metadata should preserve pointee type")
                .pretty_print(&db),
            "[u256; 8]",
        );
        assert_eq!(
            crate::repr::place_yields_location_value(
                &db,
                &core,
                &func.body.values,
                &func.body.locals,
                place,
                value.ty,
                func.body.value_pointer_info(reverse_call_arg),
            ),
            Some(true),
            "`ref arr` should be recognized as a location value by MIR",
        );
    }

    #[test]
    fn ref_to_pointer_like_local_keeps_an_explicit_place_ref() {
        let mut db = DriverDataBase::default();
        let url =
            Url::parse("file:///ref_to_pointer_like_local_keeps_an_explicit_place_ref.fe").unwrap();
        let file = db.workspace().touch(
            &mut db,
            url,
            Some(
                include_str!(
                    "../../../fe/tests/fixtures/fe_test/view_param_local_ref_take_reverse.fe"
                )
                .to_owned(),
            ),
        );
        let top_mod = db.top_mod(file);
        let module = crate::lower_module(&db, top_mod).expect("module should lower");

        let func = module
            .functions
            .iter()
            .find(|func| func.symbol_name == "sum_last4")
            .expect("expected `sum_last4`");

        let take_call_arg = func.body.blocks[0]
            .insts
            .iter()
            .find_map(|inst| match inst {
                crate::ir::MirInst::Assign {
                    rvalue: Rvalue::Call(call),
                    ..
                } if call
                    .resolved_name
                    .as_deref()
                    .is_some_and(|name| name.contains("take_u256__Reverse")) =>
                {
                    call.args.get(1).copied()
                }
                _ => None,
            })
            .expect("expected take_u256 call");

        let value = func.body.value(take_call_arg);
        let ValueOrigin::PlaceRef(place) = &value.origin else {
            panic!(
                "expected `ref rev` arg to lower as PlaceRef, got {:?}",
                value.origin
            );
        };
        assert_eq!(value.ty.pretty_print(&db), "ref Reverse<u256, [u256; 8]>");
        let ValueOrigin::PlaceRoot(local) = func.body.value(place.base).origin else {
            panic!(
                "expected `ref rev` to borrow the local slot for `rev`, got {:?}",
                func.body.value(place.base).origin
            );
        };
        assert_eq!(
            func.body.local(local).ty.pretty_print(&db),
            "Reverse<u256, [u256; 8]>"
        );
        assert!(
            place.projection.is_empty(),
            "`ref rev` should not dereference `rev`"
        );
    }

    #[test]
    #[should_panic(expected = "invalid effect argument for ByValue")]
    fn lower_effect_arg_panics_instead_of_silently_defaulting() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///invalid_effect_arg_panics.fe").unwrap();
        let src = r#"
fn callee() uses (x: mut u256) {
    x += 1
}

pub fn entry() -> u256 {
    0
}
"#;

        let file = db.workspace().touch(&mut db, url, Some(src.to_owned()));
        let top_mod = db.top_mod(file);

        let entry = top_mod
            .all_funcs(&db)
            .iter()
            .copied()
            .find(|func| {
                func.name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "entry")
            })
            .expect("expected `entry` function");

        let callee = top_mod
            .all_funcs(&db)
            .iter()
            .copied()
            .find(|func| {
                func.name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "callee")
            })
            .expect("expected `callee` function");

        let (diags, typed_body) = check_func_body(&db, entry);
        assert!(diags.is_empty(), "expected no diagnostics, got {diags:?}");
        let body = entry.body(&db).expect("expected `entry` body");

        let mut builder = MirBuilder::new_for_func(
            &db,
            entry,
            body,
            typed_body,
            &[],
            LoweringOverrides {
                receiver_space: None,
                effect_param_space_overrides: &[],
                param_capability_space_overrides: &[],
            },
        )
        .expect("failed to create MirBuilder");

        let key = callee
            .effect_params(&db)
            .next()
            .and_then(|effect| effect.key_path(&db))
            .expect("expected a callee effect key path");

        let resolved_arg = ResolvedEffectArg {
            param_idx: 0,
            key,
            arg: EffectArg::Unknown,
            pass_mode: EffectPassMode::ByValue,
            key_kind: EffectKeyKind::Type,
            instantiated_target_ty: None,
        };

        let mut writebacks = Vec::new();
        let _ = builder.lower_effect_arg(&resolved_arg, &mut writebacks);
    }
}
