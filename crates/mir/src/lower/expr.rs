//! Expression and statement lowering for MIR: handles blocks, control flow, calls, and dispatches
//! to specialized lowering helpers.

use hir::analysis::ty::decision_tree::{Projection, ProjectionPath};

use crate::layout::{self, ty_storage_slots};

use super::*;
use hir::analysis::{
    place::PlaceBase,
    ty::ty_check::{EffectArg, EffectPassMode},
};

impl<'db, 'a> MirBuilder<'db, 'a> {
    /// Try to lower a `size_of<T>()` or `encoded_size<T>()` call to a constant.
    fn try_lower_size_intrinsic_call(&mut self, expr: ExprId) -> Option<ValueId> {
        let callable = self.typed_body.callable_expr(expr)?;
        if callable.callable_def.ingot(self.db).kind(self.db) != IngotKind::Core {
            return None;
        }

        let name = callable.callable_def.name(self.db)?;
        let is_size_of = name.data(self.db) == "size_of";
        let is_encoded_size = name.data(self.db) == "encoded_size";
        if !is_size_of && !is_encoded_size {
            return None;
        }

        // Get the type argument from the callable's generic args
        let ty = *callable.generic_args().first()?;

        let size_bytes = if is_size_of {
            layout::ty_size_bytes(self.db, ty)?
        } else {
            self.abi_static_size_bytes(ty)?
        };

        let value_ty = self.typed_body.expr_ty(self.db, expr);
        Some(self.mir_body.alloc_value(ValueData {
            ty: value_ty,
            origin: ValueOrigin::Synthetic(SyntheticValue::Int(BigUint::from(size_bytes))),
        }))
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

    /// Lowers the body root expression, starting from the provided entry block.
    ///
    /// # Parameters
    /// - `block`: Entry basic block to begin lowering.
    /// - `expr`: Root expression id of the body.
    ///
    /// # Returns
    /// The successor block after lowering the root expression.
    pub(super) fn lower_root(&mut self, block: BasicBlockId, expr: ExprId) -> Option<BasicBlockId> {
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Block(stmts)) => self.lower_block(block, expr, stmts),
            _ => {
                let (next_block, value) = self.lower_expr_in(block, expr);
                self.mir_body.expr_values.insert(expr, value);
                next_block
            }
        }
    }

    /// Lowers a block expression by sequentially lowering its statements.
    ///
    /// # Parameters
    /// - `block`: Basic block to start lowering in.
    /// - `_block_expr`: Expression id for the block (unused).
    /// - `stmts`: Statements contained in the block.
    ///
    /// # Returns
    /// The final block after lowering all statements, or `None` if terminated.
    pub(super) fn lower_block(
        &mut self,
        block: BasicBlockId,
        _block_expr: ExprId,
        stmts: &[StmtId],
    ) -> Option<BasicBlockId> {
        let mut current = Some(block);
        for &stmt_id in stmts {
            let Some(curr_block) = current else { break };
            current = self.lower_stmt(curr_block, stmt_id).0;
        }
        current
    }

    /// Lowers an expression in the context of an existing block.
    ///
    /// # Parameters
    /// - `block`: Basic block where lowering begins.
    /// - `expr`: Expression id to lower.
    ///
    /// # Returns
    /// The successor block and the resulting `ValueId`.
    pub(super) fn lower_expr_in(
        &mut self,
        block: BasicBlockId,
        expr: ExprId,
    ) -> (Option<BasicBlockId>, ValueId) {
        let (next, value, _) = self.lower_expr_core(block, expr);
        (next, value)
    }

    /// Lower an expression and indicate whether an `Eval` wrapper should be emitted.
    ///
    /// # Parameters
    /// - `block`: Entry block for lowering.
    /// - `expr`: Expression to lower.
    ///
    /// # Returns
    /// A triple of next block, resulting value, and a flag indicating whether to emit `MirInst::Eval`.
    pub(super) fn lower_expr_core(
        &mut self,
        block: BasicBlockId,
        expr: ExprId,
    ) -> (Option<BasicBlockId>, ValueId, bool) {
        if let Some((next, val)) = self.try_lower_intrinsic_stmt(block, expr) {
            return (next, val, false);
        }
        if let Some((next, val)) = self.try_lower_variant_ctor(block, expr) {
            return (next, val, true);
        }
        if let Some((next, val)) = self.try_lower_unit_variant(block, expr) {
            return (next, val, true);
        }

        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Block(stmts)) => {
                let next_block = self.lower_block(block, expr, stmts);
                let val = self.ensure_value(expr);
                (next_block, val, false)
            }
            Partial::Present(Expr::With(bindings, body_expr)) => {
                let mut current = Some(block);
                for binding in bindings {
                    let Some(curr_block) = current else { break };
                    let (next_block, value, push_eval) =
                        self.lower_expr_core(curr_block, binding.value);
                    current = next_block;
                    if push_eval && let Some(curr_block) = current {
                        let bind_value =
                            !self.is_unit_ty(self.typed_body.expr_ty(self.db, binding.value));
                        self.push_inst(
                            curr_block,
                            MirInst::EvalExpr {
                                expr: binding.value,
                                value,
                                bind_value,
                            },
                        );
                    }
                }

                let Some(curr_block) = current else {
                    let val = self.ensure_value(expr);
                    return (None, val, false);
                };

                let (next_block, value) = self.lower_expr_in(curr_block, *body_expr);
                self.mir_body.expr_values.insert(expr, value);
                (next_block, value, false)
            }
            Partial::Present(Expr::RecordInit(_, fields)) => {
                let (next, val) = self.try_lower_record(block, expr, fields);
                (next, val, true)
            }
            Partial::Present(Expr::Tuple(elems)) => {
                let (next, val) = self.try_lower_tuple(block, expr, elems);
                (next, val, true)
            }
            Partial::Present(Expr::Field(lhs, field_index)) => {
                let Some(curr_block) = self.pre_lower_if_needed(Some(block), *lhs) else {
                    let val = self.ensure_value(expr);
                    return (None, val, true);
                };

                let Some(field_index) = field_index.to_opt() else {
                    let val = self.ensure_value(expr);
                    return (Some(curr_block), val, true);
                };
                let addr_value = self.ensure_value(*lhs);
                if let Some(result) = self.lower_field_from_base(*lhs, field_index, addr_value) {
                    self.mir_body.expr_values.insert(expr, result);
                    return (Some(curr_block), result, true);
                }
                let val = self.ensure_value(expr);
                (Some(curr_block), val, true)
            }
            Partial::Present(Expr::Match(scrutinee, arms)) => {
                if let Partial::Present(arms) = arms {
                    // Try decision tree lowering first
                    let (next, val) =
                        self.lower_match_with_decision_tree(block, expr, *scrutinee, arms);
                    return (next, val, false);
                }
                let val = self.ensure_value(expr);
                (Some(block), val, true)
            }
            Partial::Present(Expr::Call(_, call_args)) => {
                // Pre-lower argument expressions that need block-aware lowering
                let mut current = Some(block);
                for arg in call_args {
                    current = self.pre_lower_if_needed(current, arg.expr);
                    if current.is_none() {
                        break;
                    }
                }
                let val = self.ensure_value(expr);
                (current, val, true)
            }
            Partial::Present(Expr::MethodCall(receiver, _, _, call_args)) => {
                // Pre-lower receiver and argument expressions that need block-aware lowering
                let mut current = self.pre_lower_if_needed(Some(block), *receiver);
                for arg in call_args {
                    current = self.pre_lower_if_needed(current, arg.expr);
                    if current.is_none() {
                        break;
                    }
                }
                let val = self.ensure_value(expr);
                (current, val, true)
            }
            Partial::Present(Expr::Bin(lhs, rhs, _)) => {
                // Pre-lower operands that need block-aware lowering
                let mut current = Some(block);
                for operand in [*lhs, *rhs] {
                    current = self.pre_lower_if_needed(current, operand);
                    if current.is_none() {
                        break;
                    }
                }
                let val = self.ensure_value(expr);
                (current, val, true)
            }
            Partial::Present(Expr::Un(inner, _)) => {
                // Pre-lower operand if it needs block-aware lowering
                let current = self.pre_lower_if_needed(Some(block), *inner);
                let val = self.ensure_value(expr);
                (current, val, true)
            }
            _ => {
                let val = self.ensure_value(expr);
                (Some(block), val, true)
            }
        }
    }

    /// Attempts to lower a function or method call into a MIR value.
    ///
    /// # Parameters
    /// - `expr`: Expression id representing the call.
    ///
    /// # Returns
    /// The allocated `ValueId` for the call result, or `None` if not a call.
    pub(super) fn try_lower_call(&mut self, expr: ExprId) -> Option<ValueId> {
        if let Some(value_id) = self.try_lower_size_intrinsic_call(expr) {
            return Some(value_id);
        }

        let callable = self.typed_body.callable_expr(expr)?;
        let (mut args, arg_exprs) = self.collect_call_args(expr)?;
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
                let receiver_value = args[0];
                receiver_space = Some(self.value_address_space(receiver_value));
            }
        }

        let ty = self.typed_body.expr_ty(self.db, expr);

        if callable.callable_def.ingot(self.db).kind(self.db) == IngotKind::Core
            && callable
                .callable_def
                .name(self.db)
                .is_some_and(|name| name.data(self.db) == "contract_field_slot")
            && let Some(contract_fn) = extract_contract_function(self.db, self.func)
            && let Some(arg_expr) = arg_exprs.first().copied()
            && let Some(field_idx) = self.u256_lit_from_expr(arg_expr)
            && let Some(field_idx) = field_idx.to_usize()
            && let Some(offset) =
                self.contract_field_slot_offset(&contract_fn.contract_name, field_idx)
        {
            let value_id = self.mir_body.alloc_value(ValueData {
                ty,
                origin: ValueOrigin::Synthetic(SyntheticValue::Int(BigUint::from(offset))),
            });
            return Some(value_id);
        }

        if let Some(kind) = self.intrinsic_kind(callable.callable_def) {
            if !kind.returns_value() {
                return None;
            }
            let mut code_region = None;
            if matches!(
                kind,
                IntrinsicOp::CodeRegionOffset | IntrinsicOp::CodeRegionLen
            ) {
                if let Some(arg_expr) = arg_exprs.first() {
                    code_region = self.code_region_target(*arg_expr);
                }
                args.clear();
            }
            let value_id = self.mir_body.alloc_value(ValueData {
                ty,
                origin: ValueOrigin::Intrinsic(IntrinsicValue {
                    op: kind,
                    args,
                    code_region,
                }),
            });
            if matches!(kind, IntrinsicOp::StorAt) {
                self.value_address_space
                    .insert(value_id, AddressSpaceKind::Storage);
            }
            return Some(value_id);
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
                    EffectArg::Value(expr_id) => self.ensure_value(*expr_id),
                    EffectArg::Binding(binding) => self
                        .binding_value(*binding)
                        .unwrap_or_else(|| self.synthetic_u256(BigUint::from(0u8))),
                    EffectArg::Unknown => self.synthetic_u256(BigUint::from(0u8)),
                };
                effect_args.push(value);
                effect_kinds.push(kind);
            }
        }
        Some(self.mir_body.alloc_value(ValueData {
            ty,
            origin: ValueOrigin::Call(CallOrigin {
                expr,
                callable: callable.clone(),
                args,
                effect_args,
                effect_kinds,
                receiver_space,
                resolved_name: None,
            }),
        }))
    }

    /// Returns true if the expression is a method call (as opposed to a regular function call).
    fn is_method_call(&self, expr: ExprId) -> bool {
        let exprs = self.body.exprs(self.db);
        matches!(&exprs[expr], Partial::Present(Expr::MethodCall(..)))
    }

    /// Rewrites a field access expression into either a `get_field` call (for primitives)
    /// or a `FieldPtr` offset computation (for nested structs).
    ///
    /// # Parameters
    /// - `expr`: Field access expression id.
    ///
    /// # Returns
    /// The lowered `ValueId` if the field can be resolved.
    pub(super) fn try_lower_field(&mut self, expr: ExprId) -> Option<ValueId> {
        let Partial::Present(Expr::Field(lhs, field_index)) = expr.data(self.db, self.body) else {
            return None;
        };
        let field_index = field_index.to_opt()?;
        if self.needs_block_aware_lowering(*lhs) {
            return None;
        }
        let addr_value = self.ensure_value(*lhs);
        self.lower_field_from_base(*lhs, field_index, addr_value)
    }

    fn lower_field_from_base(
        &mut self,
        lhs: ExprId,
        field_index: FieldIndex<'db>,
        addr_value: ValueId,
    ) -> Option<ValueId> {
        let lhs_ty = self.typed_body.expr_ty(self.db, lhs);
        let info = self.field_access_info(lhs_ty, field_index)?;
        if self.is_zero_sized_ty(info.field_ty) {
            return Some(self.synthetic_zero_for_ty(info.field_ty));
        }

        let addr_space = self.value_address_space(addr_value);
        let is_aggregate = info.field_ty.field_count(self.db) > 0;

        // Create Place with single-element projection path
        let place = Place::new(
            addr_value,
            ProjectionPath::from_projection(Projection::Field(info.field_idx)),
            addr_space,
        );

        // Use PlaceRef for aggregates (pointer only), PlaceLoad for scalars (load value)
        let origin = if is_aggregate {
            ValueOrigin::PlaceRef(place)
        } else {
            ValueOrigin::PlaceLoad(place)
        };

        let result = self.mir_body.alloc_value(ValueData {
            ty: info.field_ty,
            origin,
        });

        // Propagate address space to the result
        self.value_address_space.insert(result, addr_space);
        Some(result)
    }

    /// Lowers a statement and returns its continuation and produced value (if any).
    ///
    /// # Parameters
    /// - `block`: Current basic block.
    /// - `stmt_id`: Statement to lower.
    ///
    /// # Returns
    /// The successor block and optional produced `ValueId`.
    pub(super) fn lower_stmt(
        &mut self,
        block: BasicBlockId,
        stmt_id: StmtId,
    ) -> (Option<BasicBlockId>, Option<ValueId>) {
        let Partial::Present(stmt) = stmt_id.data(self.db, self.body) else {
            return (Some(block), None);
        };
        match stmt {
            Stmt::Let(pat, ty, value) => {
                let (next_block, value_id) = if let Some(expr) = value {
                    let (next_block, val) = self.lower_expr_in(block, *expr);
                    (next_block, Some(val))
                } else {
                    (Some(block), None)
                };
                if let Some(val) = value_id {
                    let space = self.value_address_space(val);
                    self.set_pat_address_space(*pat, space);
                }
                if let Some(curr_block) = next_block {
                    self.push_inst(
                        curr_block,
                        MirInst::Let {
                            stmt: stmt_id,
                            pat: *pat,
                            ty: *ty,
                            value: value_id,
                        },
                    );
                }
                (next_block, None)
            }
            Stmt::For(_, _, _) => {
                panic!("for loops are not supported in MIR lowering yet");
            }
            Stmt::While(cond, body_expr) => self.lower_while(block, *cond, *body_expr),
            Stmt::Continue => {
                let scope = self.loop_stack.last().expect("continue outside of loop");
                self.set_terminator(
                    block,
                    Terminator::Goto {
                        target: scope.continue_target,
                    },
                );
                (None, None)
            }
            Stmt::Break => {
                let scope = self.loop_stack.last().expect("break outside of loop");
                self.set_terminator(
                    block,
                    Terminator::Goto {
                        target: scope.break_target,
                    },
                );
                (None, None)
            }
            Stmt::Return(value) => {
                let (next_block, ret_value) = if let Some(expr) = value {
                    let (next_block, val) = self.lower_expr_in(block, *expr);
                    (next_block, Some(val))
                } else {
                    (Some(block), None)
                };
                if let Some(curr_block) = next_block {
                    self.set_terminator(curr_block, Terminator::Return(ret_value));
                }
                (None, None)
            }
            Stmt::Expr(expr) => self.lower_expr_stmt(block, stmt_id, *expr),
        }
    }

    /// Lowers a `while` loop statement and wires its control-flow edges.
    ///
    /// # Parameters
    /// - `block`: Entry block preceding the loop.
    /// - `cond_expr`: Condition expression id.
    /// - `body_expr`: Loop body expression id.
    ///
    /// # Returns
    /// The loop exit block and no produced value.
    pub(super) fn lower_while(
        &mut self,
        block: BasicBlockId,
        cond_expr: ExprId,
        body_expr: ExprId,
    ) -> (Option<BasicBlockId>, Option<ValueId>) {
        let cond_entry = self.alloc_block();
        let body_block = self.alloc_block();
        let exit_block = self.alloc_block();

        self.set_terminator(block, Terminator::Goto { target: cond_entry });

        let (cond_header_opt, cond_val) = self.lower_expr_in(cond_entry, cond_expr);
        let Some(cond_header) = cond_header_opt else {
            return (None, None);
        };

        self.loop_stack.push(LoopScope {
            continue_target: cond_entry,
            break_target: exit_block,
        });

        let body_end = self.lower_expr_in(body_block, body_expr).0;

        self.loop_stack.pop();

        let mut backedge = None;
        if let Some(body_end_block) = body_end {
            self.set_terminator(body_end_block, Terminator::Goto { target: cond_entry });
            backedge = Some(body_end_block);
        }

        self.set_terminator(
            cond_header,
            Terminator::Branch {
                cond: cond_val,
                then_bb: body_block,
                else_bb: exit_block,
            },
        );

        self.mir_body.loop_headers.insert(
            cond_entry,
            LoopInfo {
                body: body_block,
                exit: exit_block,
                backedge,
            },
        );

        (Some(exit_block), None)
    }

    fn lower_expr_as_stmt_in(&mut self, block: BasicBlockId, expr: ExprId) -> Option<BasicBlockId> {
        let expr_ty = self.typed_body.expr_ty(self.db, expr);
        if (self.is_unit_ty(expr_ty) || expr_ty.is_never(self.db))
            && let Partial::Present(Expr::If(cond, then_expr, else_expr)) =
                expr.data(self.db, self.body)
        {
            return self
                .lower_if_expr(block, expr, *cond, *then_expr, *else_expr)
                .0;
        }

        let (next_block, value, push_eval) = self.lower_expr_core(block, expr);
        if push_eval && let Some(curr_block) = next_block {
            self.push_inst(
                curr_block,
                MirInst::EvalExpr {
                    expr,
                    value,
                    bind_value: false,
                },
            );
        }
        next_block
    }

    /// Lowers an `if` expression used in statement position.
    ///
    /// # Parameters
    /// - `block`: Entry basic block.
    /// - `if_expr`: Expression id of the `if`.
    /// - `cond`: Condition expression id.
    /// - `then_expr`: Then-branch expression id.
    /// - `else_expr`: Optional else-branch expression id.
    ///
    /// # Returns
    /// The merge block (if any) and optional resulting value.
    pub(super) fn lower_if_expr(
        &mut self,
        block: BasicBlockId,
        if_expr: ExprId,
        cond: ExprId,
        then_expr: ExprId,
        else_expr: Option<ExprId>,
    ) -> (Option<BasicBlockId>, Option<ValueId>) {
        let if_ty = self.typed_body.expr_ty(self.db, if_expr);
        if !self.is_unit_ty(if_ty) && !if_ty.is_never(self.db) {
            let value = self.ensure_value(if_expr);
            return (Some(block), Some(value));
        }

        let (cond_block_opt, cond_val) = self.lower_expr_in(block, cond);
        let cond_block = match cond_block_opt {
            Some(block) => block,
            None => return (None, None),
        };

        let then_block = self.alloc_block();
        let merge_block = self.alloc_block();
        let else_block = if else_expr.is_some() {
            self.alloc_block()
        } else {
            merge_block
        };

        self.set_terminator(
            cond_block,
            Terminator::Branch {
                cond: cond_val,
                then_bb: then_block,
                else_bb: else_block,
            },
        );

        let then_end = self.lower_expr_as_stmt_in(then_block, then_expr);
        if let Some(end_block) = then_end {
            self.set_terminator(
                end_block,
                Terminator::Goto {
                    target: merge_block,
                },
            );
        }

        if let Some(else_expr) = else_expr {
            let else_end = self.lower_expr_as_stmt_in(else_block, else_expr);
            if let Some(end_block) = else_end {
                self.set_terminator(
                    end_block,
                    Terminator::Goto {
                        target: merge_block,
                    },
                );
            }
        }

        (Some(merge_block), None)
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

    /// Returns whether an expression needs block-aware lowering.
    ///
    /// Some expressions (like RecordInit) need to be lowered with access to a basic block
    /// so they can emit instructions. This method checks if an expression is one of these
    /// types, including recursively checking nested expressions.
    fn needs_block_aware_lowering(&self, expr: ExprId) -> bool {
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::RecordInit(..)) => true,
            // Tuples also need block-aware lowering (alloc + store_field)
            Partial::Present(Expr::Tuple(elems)) => !elems.is_empty(),
            // Nested calls might have RecordInit or Tuple arguments
            Partial::Present(Expr::Call(_, call_args)) => call_args
                .iter()
                .any(|arg| self.needs_block_aware_lowering(arg.expr)),
            Partial::Present(Expr::MethodCall(receiver, _, _, call_args)) => {
                self.needs_block_aware_lowering(*receiver)
                    || call_args
                        .iter()
                        .any(|arg| self.needs_block_aware_lowering(arg.expr))
            }
            Partial::Present(Expr::Field(base, _)) => self.needs_block_aware_lowering(*base),
            // Blocks might contain RecordInit expressions
            Partial::Present(Expr::Block(stmts)) => {
                stmts
                    .iter()
                    .any(|stmt_id| match stmt_id.data(self.db, self.body) {
                        Partial::Present(Stmt::Expr(e)) => self.needs_block_aware_lowering(*e),
                        _ => false,
                    })
            }
            // If expressions might contain RecordInit in their branches
            Partial::Present(Expr::If(cond, then_expr, else_expr)) => {
                self.needs_block_aware_lowering(*cond)
                    || self.needs_block_aware_lowering(*then_expr)
                    || else_expr
                        .map(|e| self.needs_block_aware_lowering(e))
                        .unwrap_or(false)
            }
            // Binary expressions might contain tuples in their operands
            Partial::Present(Expr::Bin(lhs, rhs, _)) => {
                self.needs_block_aware_lowering(*lhs) || self.needs_block_aware_lowering(*rhs)
            }
            // Unary expressions might contain tuples in their operand
            Partial::Present(Expr::Un(inner, _)) => self.needs_block_aware_lowering(*inner),
            _ => false,
        }
    }

    /// Pre-lowers a subexpression if it needs block-aware lowering.
    ///
    /// This handles the common pattern of checking whether a subexpression (like a call
    /// argument or operand) needs block-aware lowering, and if so, lowering it and
    /// emitting the appropriate `EvalExpr` instruction.
    ///
    /// # Returns
    /// The successor block after lowering, or `None` if lowering terminated the block.
    fn pre_lower_if_needed(
        &mut self,
        block: Option<BasicBlockId>,
        subexpr: ExprId,
    ) -> Option<BasicBlockId> {
        if !self.needs_block_aware_lowering(subexpr) {
            return block;
        }
        let curr_block = block?;
        let (next_block, value, push_eval) = self.lower_expr_core(curr_block, subexpr);
        if push_eval && let Some(curr_block) = next_block {
            let bind_value = !self.is_unit_ty(self.typed_body.expr_ty(self.db, subexpr));
            self.push_inst(
                curr_block,
                MirInst::EvalExpr {
                    expr: subexpr,
                    value,
                    bind_value,
                },
            );
        }
        next_block
    }

    /// Lowers an expression statement, emitting side-effecting instructions as needed.
    ///
    /// # Parameters
    /// - `block`: Current basic block.
    /// - `stmt_id`: Statement id for context.
    /// - `expr`: Expression id to lower.
    ///
    /// # Returns
    /// Successor block and optional resulting value.
    pub(super) fn lower_expr_stmt(
        &mut self,
        block: BasicBlockId,
        stmt_id: StmtId,
        expr: ExprId,
    ) -> (Option<BasicBlockId>, Option<ValueId>) {
        if let Some((next_block, value_id)) = self.try_lower_intrinsic_stmt(block, expr) {
            return (next_block, Some(value_id));
        }
        let exprs = self.body.exprs(self.db);
        let Partial::Present(expr_data) = &exprs[expr] else {
            return (Some(block), None);
        };

        match expr_data {
            Expr::Assign(target, value) => {
                let (next_block, value_id) = self.lower_expr_in(block, *value);
                if let Some(curr_block) = next_block {
                    if let Some(binding) = self.typed_body.expr_prop(self.db, *target).binding
                        && let LocalBinding::Local { pat, .. } = binding
                    {
                        let space = self.value_address_space(value_id);
                        self.set_pat_address_space(pat, space);
                    }
                    if let Some(block_after_assign) =
                        self.try_lower_field_assign(curr_block, expr, *target, value_id)
                    {
                        return (Some(block_after_assign), None);
                    }
                    self.push_inst(
                        curr_block,
                        MirInst::Assign {
                            stmt: stmt_id,
                            target: *target,
                            value: value_id,
                        },
                    );
                }
                (next_block, None)
            }
            Expr::If(cond, then_expr, else_expr) => {
                let (next_block, value_id) =
                    self.lower_if_expr(block, expr, *cond, *then_expr, *else_expr);
                if let (Some(curr_block), Some(value)) = (next_block, value_id) {
                    self.push_inst(
                        curr_block,
                        MirInst::Eval {
                            stmt: stmt_id,
                            value,
                        },
                    );
                }
                (next_block, value_id)
            }
            Expr::AugAssign(target, value, op) => {
                let (next_block, value_id) = self.lower_expr_in(block, *value);
                if let Some(curr_block) = next_block {
                    if let Some(binding) = self.typed_body.expr_prop(self.db, *target).binding
                        && let LocalBinding::Local { pat, .. } = binding
                    {
                        let space = self.value_address_space(value_id);
                        self.set_pat_address_space(pat, space);
                    }
                    self.push_inst(
                        curr_block,
                        MirInst::AugAssign {
                            stmt: stmt_id,
                            target: *target,
                            value: value_id,
                            op: *op,
                        },
                    );
                }
                (next_block, None)
            }
            _ => {
                let (next_block, value_id, push_eval) = self.lower_expr_core(block, expr);
                if push_eval && let Some(curr_block) = next_block {
                    self.push_inst(
                        curr_block,
                        MirInst::Eval {
                            stmt: stmt_id,
                            value: value_id,
                        },
                    );
                }
                (next_block, Some(value_id))
            }
        }
    }
}
