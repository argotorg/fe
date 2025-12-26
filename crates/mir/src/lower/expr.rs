//! Expression and statement lowering for MIR: handles blocks, control flow, calls, and dispatches
//! to specialized lowering helpers.

use hir::projection::{IndexSource, Projection};

use crate::layout::{self, ty_storage_slots};

use super::*;
use hir::analysis::{
    place::PlaceBase,
    ty::ty_check::{EffectArg, EffectPassMode},
};
use hir::hir_def::expr::BinOp;

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

    fn u256_lit_from_expr(&self, expr: ExprId) -> Option<BigUint> {
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Lit(LitKind::Int(int_id))) => Some(int_id.data(self.db).clone()),
            _ => None,
        }
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

        let fields = contract.hir_fields(self.db).data(self.db);
        if field_idx >= fields.len() {
            return None;
        }

        let scope = contract.scope();
        let assumptions = PredicateListId::empty_list(self.db);

        let mut offset = 0;
        for field in fields.iter().take(field_idx) {
            let field_ty = lower_opt_hir_ty(self.db, field.type_ref(), scope, assumptions);
            offset += ty_storage_slots(self.db, field_ty)?;
        }
        Some(offset)
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
                            self.push_inst_here(MirInst::Assign {
                                stmt: None,
                                dest: None,
                                rvalue: crate::ir::Rvalue::Value(value),
                            });
                        } else {
                            self.push_inst_here(MirInst::BindValue { value });
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
            Partial::Present(Expr::Un(inner, _)) => {
                let _ = self.lower_expr(*inner);
                self.ensure_value(expr)
            }
            Partial::Present(Expr::Bin(lhs, rhs, BinOp::Index)) => {
                self.lower_index_expr(expr, *lhs, *rhs)
            }
            Partial::Present(Expr::Bin(lhs, rhs, _)) => {
                let _ = self.lower_expr(*lhs);
                let _ = self.lower_expr(*rhs);
                self.ensure_value(expr)
            }
            Partial::Present(Expr::Field(lhs, field_index)) => {
                self.lower_field_expr(expr, *lhs, *field_index)
            }
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
                self.push_inst_here(MirInst::Assign {
                    stmt,
                    dest: Some(dest),
                    rvalue: crate::ir::Rvalue::Value(value_id),
                });
                self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
            }
            return value_id;
        }
        let Some(callable_def) = self.callable_def_for_call_expr(expr) else {
            return value_id;
        };

        let Some((args, arg_exprs)) = self.collect_call_args(expr) else {
            return value_id;
        };

        let result_space = self
            .effect_provider_kind_for_provider_ty(ty)
            .map(|kind| match kind {
                EffectProviderKind::Memory => AddressSpaceKind::Memory,
                EffectProviderKind::Storage => AddressSpaceKind::Storage,
                EffectProviderKind::Calldata => AddressSpaceKind::Memory,
            })
            .unwrap_or_else(|| self.expr_address_space(expr));

        if callable_def.ingot(self.db).kind(self.db) == IngotKind::Std
            && callable_def
                .name(self.db)
                .is_some_and(|name| name.data(self.db) == "at_offset")
            && self.effect_provider_kind_for_provider_ty(ty).is_some()
            && args.len() == 1
            && returns_value
        {
            let dest = dest_override.unwrap_or_else(|| self.alloc_temp_local(ty, false, "ptr"));
            self.builder.body.locals[dest.index()].address_space = result_space;
            self.push_inst_here(MirInst::Assign {
                stmt,
                dest: Some(dest),
                rvalue: crate::ir::Rvalue::Value(args[0]),
            });
            self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
            self.value_address_space.insert(value_id, result_space);
            return value_id;
        }

        if callable_def.ingot(self.db).kind(self.db) == IngotKind::Std
            && callable_def
                .name(self.db)
                .is_some_and(|name| name.data(self.db) == "contract_field_slot")
            && let Some(contract_fn) = extract_contract_function(self.db, self.func)
            && let Some(arg_expr) = arg_exprs.first().copied()
            && let Some(field_idx) = self.u256_lit_from_expr(arg_expr)
            && let Some(field_idx) = field_idx.to_usize()
            && let Some(offset) =
                self.contract_field_slot_offset(&contract_fn.contract_name, field_idx)
        {
            self.builder.body.values[value_id.index()].origin =
                ValueOrigin::Synthetic(SyntheticValue::Int(BigUint::from(offset)));
            if returns_value && let Some(dest) = dest_override {
                self.push_inst_here(MirInst::Assign {
                    stmt,
                    dest: Some(dest),
                    rvalue: crate::ir::Rvalue::Value(value_id),
                });
                self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
            }
            return value_id;
        }

        if let Some(op) = self.intrinsic_kind(callable_def) {
            let mut intrinsic_args = args.clone();
            if self.is_method_call(expr) && !intrinsic_args.is_empty() {
                intrinsic_args.remove(0);
            }
            if op.returns_value() {
                let dest =
                    dest_override.unwrap_or_else(|| self.alloc_temp_local(ty, false, "intr"));
                self.builder.body.locals[dest.index()].address_space = result_space;
                self.push_inst_here(MirInst::Assign {
                    stmt,
                    dest: Some(dest),
                    rvalue: crate::ir::Rvalue::Intrinsic {
                        op,
                        args: intrinsic_args,
                    },
                });
                self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
                self.value_address_space.insert(value_id, result_space);
                return value_id;
            }

            // Statement-only intrinsics are handled via `try_lower_intrinsic_stmt` above.
            self.builder.body.values[value_id.index()].origin = ValueOrigin::Unit;
            return value_id;
        }

        let Some(callable) = self.typed_body.callable_expr(expr) else {
            return value_id;
        };

        let mut receiver_space = None;
        if self.is_method_call(expr) && !args.is_empty() {
            let needs_space = callable
                .callable_def
                .receiver_ty(self.db)
                .is_some_and(|binder| {
                    let ty = binder.instantiate_identity();
                    ty.adt_ref(self.db)
                        .is_some_and(|adt| matches!(adt, AdtRef::Struct(_)))
                });
            if needs_space {
                receiver_space = Some(self.value_address_space(args[0]));
            }
        }

        let mut effect_args = Vec::new();
        let mut effect_kinds = Vec::new();
        if let CallableDef::Func(func_def) = callable.callable_def
            && func_def.has_effects(self.db)
            && extract_contract_function(self.db, func_def).is_none()
            && let Some(resolved) = self.typed_body.call_effect_args(expr)
        {
            for resolved_arg in resolved {
                let kind = match resolved_arg.pass_mode {
                    EffectPassMode::ByPlace => match &resolved_arg.arg {
                        EffectArg::Place(place) => match place.base {
                            PlaceBase::Binding(binding) => self
                                .effect_provider_kind_for_address_space(
                                    self.address_space_for_binding(&binding),
                                ),
                        },
                        _ => EffectProviderKind::Storage,
                    },
                    EffectPassMode::ByTempPlace => EffectProviderKind::Memory,
                    EffectPassMode::ByValue => match &resolved_arg.arg {
                        EffectArg::Value(expr_id) => self
                            .effect_provider_kind_for_provider_ty(
                                self.typed_body.expr_ty(self.db, *expr_id),
                            )
                            .unwrap_or(EffectProviderKind::Storage),
                        EffectArg::Binding(binding) => {
                            self.effect_provider_kind_for_binding(*binding)
                        }
                        _ => EffectProviderKind::Storage,
                    },
                    EffectPassMode::Unknown => EffectProviderKind::Storage,
                };

                let value = match &resolved_arg.arg {
                    EffectArg::Place(place) => match place.base {
                        PlaceBase::Binding(binding) => self
                            .binding_value(binding)
                            .unwrap_or_else(|| self.synthetic_u256(BigUint::from(0u8))),
                    },
                    EffectArg::Value(expr_id) => self.lower_expr(*expr_id),
                    EffectArg::Binding(binding) => self
                        .binding_value(*binding)
                        .unwrap_or_else(|| self.synthetic_u256(BigUint::from(0u8))),
                    EffectArg::Unknown => self.synthetic_u256(BigUint::from(0u8)),
                };
                effect_args.push(value);
                effect_kinds.push(kind);
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
        let call_origin = CallOrigin {
            expr,
            callable: callable.clone(),
            args,
            effect_args,
            effect_kinds,
            receiver_space,
            resolved_name: None,
        };
        if ty.is_never(self.db) {
            self.set_current_terminator(Terminator::TerminatingCall(
                crate::ir::TerminatingCall::Call(call_origin),
            ));
            self.builder.body.values[value_id.index()].origin = ValueOrigin::Unit;
            return value_id;
        }
        self.push_inst_here(MirInst::Assign {
            stmt,
            dest,
            rvalue: crate::ir::Rvalue::Call(call_origin),
        });
        self.builder.body.values[value_id.index()].origin =
            dest.map(ValueOrigin::Local).unwrap_or(ValueOrigin::Unit);
        self.value_address_space.insert(value_id, result_space);
        value_id
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

        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Call(..) | Expr::MethodCall(..)) => {
                self.lower_call_expr_inner(expr, Some(dest), Some(stmt))
            }
            Partial::Present(Expr::Field(lhs, field_index)) => {
                let value_id = self.ensure_value(expr);
                let Some(field_index) = field_index.to_opt() else {
                    return value_id;
                };
                let base_value = self.lower_expr(*lhs);
                if self.current_block().is_none() {
                    return value_id;
                }
                let lhs_ty = self.typed_body.expr_ty(self.db, *lhs);
                let Some(info) = self.field_access_info(lhs_ty, field_index) else {
                    return value_id;
                };
                let addr_space = self.value_address_space(base_value);
                let place = Place::new(
                    base_value,
                    MirProjectionPath::from_projection(Projection::Field(info.field_idx)),
                    addr_space,
                );

                if self.is_aggregate_ty(info.field_ty) {
                    let place_value = self.builder.body.alloc_value(ValueData {
                        ty: info.field_ty,
                        origin: ValueOrigin::PlaceRef(place),
                    });
                    self.builder.body.locals[dest.index()].address_space = addr_space;
                    self.push_inst_here(MirInst::Assign {
                        stmt: Some(stmt),
                        dest: Some(dest),
                        rvalue: crate::ir::Rvalue::Value(place_value),
                    });
                    self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
                    self.value_address_space.insert(value_id, addr_space);
                    return value_id;
                }

                self.builder.body.locals[dest.index()].address_space =
                    self.expr_address_space(expr);
                self.push_inst_here(MirInst::Assign {
                    stmt: Some(stmt),
                    dest: Some(dest),
                    rvalue: crate::ir::Rvalue::Load { place },
                });
                self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
                value_id
            }
            Partial::Present(Expr::Bin(lhs, rhs, BinOp::Index)) => {
                let value_id = self.ensure_value(expr);
                let lhs_ty = self.typed_body.expr_ty(self.db, *lhs);
                if !lhs_ty.is_array(self.db) {
                    return value_id;
                }
                let Some(elem_ty) = lhs_ty.generic_args(self.db).first().copied() else {
                    return value_id;
                };
                let base_value = self.lower_expr(*lhs);
                let index_value = self.lower_expr(*rhs);
                if self.current_block().is_none() {
                    return value_id;
                }
                let addr_space = self.value_address_space(base_value);
                let place = Place::new(
                    base_value,
                    MirProjectionPath::from_projection(Projection::Index(IndexSource::Dynamic(
                        index_value,
                    ))),
                    addr_space,
                );

                if self.is_aggregate_ty(elem_ty) {
                    let place_value = self.builder.body.alloc_value(ValueData {
                        ty: elem_ty,
                        origin: ValueOrigin::PlaceRef(place),
                    });
                    self.builder.body.locals[dest.index()].address_space = addr_space;
                    self.push_inst_here(MirInst::Assign {
                        stmt: Some(stmt),
                        dest: Some(dest),
                        rvalue: crate::ir::Rvalue::Value(place_value),
                    });
                    self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
                    self.value_address_space.insert(value_id, addr_space);
                    return value_id;
                }

                self.builder.body.locals[dest.index()].address_space =
                    self.expr_address_space(expr);
                self.push_inst_here(MirInst::Assign {
                    stmt: Some(stmt),
                    dest: Some(dest),
                    rvalue: crate::ir::Rvalue::Load { place },
                });
                self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
                value_id
            }
            _ => {
                let value_id = self.lower_expr(expr);
                if self.current_block().is_some() {
                    self.push_inst_here(MirInst::Assign {
                        stmt: Some(stmt),
                        dest: Some(dest),
                        rvalue: crate::ir::Rvalue::Value(value_id),
                    });
                }
                value_id
            }
        }
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
        let Some(info) = self.field_access_info(lhs_ty, field_index) else {
            return value_id;
        };

        let addr_space = self.value_address_space(base_value);
        let place = Place::new(
            base_value,
            MirProjectionPath::from_projection(Projection::Field(info.field_idx)),
            addr_space,
        );

        if self.is_aggregate_ty(info.field_ty) {
            self.builder.body.values[value_id.index()].origin = ValueOrigin::PlaceRef(place);
            self.value_address_space.insert(value_id, addr_space);
            return value_id;
        }

        let dest = self.alloc_temp_local(info.field_ty, false, "load");
        self.builder.body.locals[dest.index()].address_space = self.expr_address_space(expr);
        self.push_inst_here(MirInst::Assign {
            stmt: None,
            dest: Some(dest),
            rvalue: crate::ir::Rvalue::Load { place },
        });
        self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
        value_id
    }

    fn lower_index_expr(&mut self, expr: ExprId, lhs: ExprId, rhs: ExprId) -> ValueId {
        let value_id = self.ensure_value(expr);
        if self.current_block().is_none() {
            return value_id;
        }

        let lhs_ty = self.typed_body.expr_ty(self.db, lhs);
        if !lhs_ty.is_array(self.db) {
            return value_id;
        }
        let Some(elem_ty) = lhs_ty.generic_args(self.db).first().copied() else {
            return value_id;
        };

        let base_value = self.lower_expr(lhs);
        let index_value = self.lower_expr(rhs);
        if self.current_block().is_none() {
            return value_id;
        }

        let addr_space = self.value_address_space(base_value);
        let place = Place::new(
            base_value,
            MirProjectionPath::from_projection(Projection::Index(IndexSource::Dynamic(
                index_value,
            ))),
            addr_space,
        );

        if self.is_aggregate_ty(elem_ty) {
            self.builder.body.values[value_id.index()].origin = ValueOrigin::PlaceRef(place);
            self.value_address_space.insert(value_id, addr_space);
            return value_id;
        }

        let dest = self.alloc_temp_local(elem_ty, false, "load");
        self.builder.body.locals[dest.index()].address_space = self.expr_address_space(expr);
        self.push_inst_here(MirInst::Assign {
            stmt: None,
            dest: Some(dest),
            rvalue: crate::ir::Rvalue::Load { place },
        });
        self.builder.body.values[value_id.index()].origin = ValueOrigin::Local(dest);
        value_id
    }

    /// Returns true if the expression is a method call (as opposed to a regular function call).
    fn is_method_call(&self, expr: ExprId) -> bool {
        let exprs = self.body.exprs(self.db);
        matches!(&exprs[expr], Partial::Present(Expr::MethodCall(..)))
    }

    // NOTE: field expressions are lowered via `lower_field_expr` so scalar loads become
    // explicit `MirInst::Load` instructions.

    // NOTE: array index expressions are lowered via `lower_index_expr` so scalar loads become
    // explicit `MirInst::Load` instructions.

    fn place_for_expr(&mut self, expr: ExprId) -> Option<Place<'db>> {
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Field(lhs, field_index)) => {
                let field_index = field_index.to_opt()?;
                let lhs_ty = self.typed_body.expr_ty(self.db, *lhs);
                let info = self.field_access_info(lhs_ty, field_index)?;
                let addr_value = self.lower_expr(*lhs);
                let addr_space = self.value_address_space(addr_value);
                Some(Place::new(
                    addr_value,
                    MirProjectionPath::from_projection(Projection::Field(info.field_idx)),
                    addr_space,
                ))
            }
            Partial::Present(Expr::Bin(lhs, rhs, BinOp::Index)) => {
                let lhs_ty = self.typed_body.expr_ty(self.db, *lhs);
                if !lhs_ty.is_array(self.db) {
                    return None;
                }
                let addr_value = self.lower_expr(*lhs);
                let addr_space = self.value_address_space(addr_value);
                let index_value = self.lower_expr(*rhs);
                Some(Place::new(
                    addr_value,
                    MirProjectionPath::from_projection(Projection::Index(IndexSource::Dynamic(
                        index_value,
                    ))),
                    addr_space,
                ))
            }
            _ => None,
        }
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
                            let space = self.value_address_space(value_id);
                            self.set_pat_address_space(*pat, space);
                        } else {
                            self.push_inst_here(MirInst::Assign {
                                stmt: Some(stmt_id),
                                dest: Some(local),
                                rvalue: crate::ir::Rvalue::ZeroInit,
                            });
                        }
                    }
                    Pat::WildCard | Pat::Rest => {
                        if let Some(expr) = value {
                            let value_id = self.lower_expr(*expr);
                            if self.current_block().is_some() {
                                self.push_inst_here(MirInst::Assign {
                                    stmt: Some(stmt_id),
                                    dest: None,
                                    rvalue: crate::ir::Rvalue::Value(value_id),
                                });
                            }
                        }
                    }
                    Pat::Tuple(pats) => {
                        let Some(expr) = value else {
                            return;
                        };
                        let tuple_value = self.lower_expr(*expr);
                        let base_space = self.value_address_space(tuple_value);
                        for (field_idx, field_pat) in pats.iter().enumerate() {
                            let Partial::Present(field_pat_data) =
                                field_pat.data(self.db, self.body)
                            else {
                                continue;
                            };
                            if matches!(field_pat_data, Pat::WildCard | Pat::Rest) {
                                continue;
                            }
                            let binding = self.typed_body.pat_binding(*field_pat).unwrap_or(
                                LocalBinding::Local {
                                    pat: *field_pat,
                                    is_mut: matches!(field_pat_data, Pat::Path(_, true)),
                                },
                            );
                            let Some(local) = self.local_for_binding(binding) else {
                                continue;
                            };
                            let field_ty = self.typed_body.pat_ty(self.db, *field_pat);
                            let is_aggregate = self.is_aggregate_ty(field_ty);
                            let place = Place::new(
                                tuple_value,
                                MirProjectionPath::from_projection(Projection::Field(field_idx)),
                                base_space,
                            );
                            if is_aggregate {
                                let field_value = self.builder.body.alloc_value(ValueData {
                                    ty: field_ty,
                                    origin: ValueOrigin::PlaceRef(place),
                                });
                                self.push_inst_here(MirInst::Assign {
                                    stmt: Some(stmt_id),
                                    dest: Some(local),
                                    rvalue: crate::ir::Rvalue::Value(field_value),
                                });
                            } else {
                                self.push_inst_here(MirInst::Assign {
                                    stmt: Some(stmt_id),
                                    dest: Some(local),
                                    rvalue: crate::ir::Rvalue::Load { place },
                                });
                            }
                        }
                    }
                    _ => {
                        if let Some(expr) = value {
                            let value_id = self.lower_expr(*expr);
                            if self.current_block().is_some() {
                                self.push_inst_here(MirInst::Assign {
                                    stmt: Some(stmt_id),
                                    dest: None,
                                    rvalue: crate::ir::Rvalue::Value(value_id),
                                });
                            }
                        }
                    }
                }
            }
            Stmt::For(_, _, _) => {
                panic!("for loops are not supported in MIR lowering yet");
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
                if let Some(expr) = value {
                    let ret_ty = self.func.return_ty(self.db);
                    let returns_value = !self.is_unit_ty(ret_ty) && !ret_ty.is_never(self.db);
                    if returns_value {
                        let ret_value = Some(self.lower_expr(*expr));
                        if self.current_block().is_some() {
                            self.set_current_terminator(Terminator::Return(ret_value));
                        }
                    } else {
                        self.lower_expr_stmt(stmt_id, *expr);
                        if self.current_block().is_some() {
                            self.set_current_terminator(Terminator::Return(None));
                        }
                    }
                } else if self.current_block().is_some() {
                    self.set_current_terminator(Terminator::Return(None));
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
    pub(super) fn lower_while(&mut self, cond_expr: ExprId, body_expr: ExprId) {
        let Some(block) = self.current_block() else {
            return;
        };
        let cond_entry = self.alloc_block();
        let body_block = self.alloc_block();
        let exit_block = self.alloc_block();

        self.move_to_block(block);
        self.goto(cond_entry);

        self.move_to_block(cond_entry);
        let cond_val = self.lower_expr(cond_expr);
        let Some(cond_header) = self.current_block() else {
            return;
        };

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

        self.move_to_block(cond_header);
        self.branch(cond_val, body_block, exit_block);

        self.builder.body.loop_headers.insert(
            cond_entry,
            LoopInfo {
                body: body_block,
                exit: exit_block,
                backedge,
            },
        );

        self.move_to_block(exit_block);
    }

    fn lower_if(
        &mut self,
        if_expr: ExprId,
        cond: ExprId,
        then_expr: ExprId,
        else_expr: Option<ExprId>,
    ) -> ValueId {
        let value = self.ensure_value(if_expr);
        let Some(block) = self.current_block() else {
            return value;
        };

        let if_ty = self.typed_body.expr_ty(self.db, if_expr);
        let produces_value = !self.is_unit_ty(if_ty) && !if_ty.is_never(self.db);

        self.move_to_block(block);
        let cond_val = self.lower_expr(cond);
        let Some(cond_block) = self.current_block() else {
            return value;
        };

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

            self.move_to_block(cond_block);
            self.push_inst_here(MirInst::Assign {
                stmt: None,
                dest: Some(result_local),
                rvalue: crate::ir::Rvalue::ZeroInit,
            });

            let else_block = self.alloc_block();

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
                    self.push_inst_here(MirInst::Assign {
                        stmt: None,
                        dest: Some(result_local),
                        rvalue: crate::ir::Rvalue::Value(then_value),
                    });
                    self.goto(merge);
                }
                if let Some(end_block) = else_end {
                    self.move_to_block(end_block);
                    self.push_inst_here(MirInst::Assign {
                        stmt: None,
                        dest: Some(result_local),
                        rvalue: crate::ir::Rvalue::Value(else_value),
                    });
                    self.goto(merge);
                }
            }

            self.move_to_block(cond_block);
            self.switch(
                cond_val,
                vec![
                    SwitchTarget {
                        value: SwitchValue::Bool(true),
                        block: then_block,
                    },
                    SwitchTarget {
                        value: SwitchValue::Bool(false),
                        block: else_block,
                    },
                ],
                else_block,
            );

            if let Some(merge) = merge_block {
                self.move_to_block(merge);
            }
        } else {
            self.builder.body.values[value.index()].origin = ValueOrigin::Unit;
            let merge_block = self.alloc_block();
            let else_block = else_expr.map(|_| self.alloc_block());

            self.move_to_block(cond_block);
            self.branch(cond_val, then_block, else_block.unwrap_or(merge_block));

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

    fn is_aggregate_ty(&self, ty: TyId<'db>) -> bool {
        if ty.field_count(self.db) > 0 || ty.is_array(self.db) {
            return true;
        }
        ty.adt_ref(self.db)
            .is_some_and(|adt| matches!(adt, AdtRef::Enum(_)))
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
                    self.push_inst_here(MirInst::Assign {
                        stmt: Some(stmt_id),
                        dest: None,
                        rvalue: crate::ir::Rvalue::Value(value_id),
                    });
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
                if let Some(binding) = self.typed_body.expr_prop(self.db, *target).binding
                    && let LocalBinding::Local { pat, .. } = binding
                {
                    let space = self.value_address_space(value_id);
                    self.set_pat_address_space(pat, space);
                }
                if let Some(place) = self.place_for_expr(*target) {
                    self.push_inst_here(MirInst::Store {
                        place,
                        value: value_id,
                    });
                    return;
                }
                let Some(local) = self
                    .typed_body
                    .expr_prop(self.db, *target)
                    .binding
                    .and_then(|binding| self.local_for_binding(binding))
                else {
                    return;
                };
                self.push_inst_here(MirInst::Assign {
                    stmt: Some(stmt_id),
                    dest: Some(local),
                    rvalue: crate::ir::Rvalue::Value(value_id),
                });
            }
            Expr::AugAssign(target, value, op) => {
                self.move_to_block(block);
                let value_id = self.lower_expr(*value);
                if let Some(binding) = self.typed_body.expr_prop(self.db, *target).binding
                    && let LocalBinding::Local { pat, .. } = binding
                {
                    let space = self.value_address_space(value_id);
                    self.set_pat_address_space(pat, space);
                }
                let Some(local) = self
                    .typed_body
                    .expr_prop(self.db, *target)
                    .binding
                    .and_then(|binding| self.local_for_binding(binding))
                else {
                    return;
                };
                let lhs_ty = self.builder.body.local(local).ty;
                let lhs_value = self.builder.body.alloc_value(ValueData {
                    ty: lhs_ty,
                    origin: ValueOrigin::Local(local),
                });
                let updated = self.builder.body.alloc_value(ValueData {
                    ty: lhs_ty,
                    origin: ValueOrigin::Binary {
                        op: BinOp::Arith(*op),
                        lhs: lhs_value,
                        rhs: value_id,
                    },
                });
                self.push_inst_here(MirInst::Assign {
                    stmt: Some(stmt_id),
                    dest: Some(local),
                    rvalue: crate::ir::Rvalue::Value(updated),
                });
            }
            _ => {
                self.move_to_block(block);
                let value_id = self.lower_expr(expr);
                if self.current_block().is_some() {
                    self.push_inst_here(MirInst::Assign {
                        stmt: Some(stmt_id),
                        dest: None,
                        rvalue: crate::ir::Rvalue::Value(value_id),
                    });
                }
            }
        }
    }
}
