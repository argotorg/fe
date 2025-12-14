//! Expression and statement lowering for MIR: handles blocks, control flow, calls, and dispatches
//! to specialized lowering helpers.

use super::*;

impl<'db, 'a> MirBuilder<'db, 'a> {
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
            Partial::Present(Expr::RecordInit(_, fields)) => {
                let (next, val) = self.try_lower_record(block, expr, fields);
                (next, val, true)
            }
            Partial::Present(Expr::Match(scrutinee, arms)) => {
                if let Partial::Present(arms) = arms
                    && let Some(mut patterns) = self.match_arm_patterns(arms)
                {
                    let (next, val) =
                        self.lower_match_expr(block, expr, *scrutinee, arms, &mut patterns);
                    return (next, val, false);
                }
                let val = self.ensure_value(expr);
                (Some(block), val, true)
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
            return Some(self.mir_body.alloc_value(ValueData {
                ty,
                origin: ValueOrigin::Intrinsic(IntrinsicValue {
                    op: kind,
                    args,
                    code_region,
                }),
            }));
        }
        Some(self.mir_body.alloc_value(ValueData {
            ty,
            origin: ValueOrigin::Call(CallOrigin {
                expr,
                callable: callable.clone(),
                args,
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
        let lhs_ty = self.typed_body.expr_ty(self.db, *lhs);
        let info = self.field_access_info(lhs_ty, field_index)?;

        let addr_value = self.ensure_value(*lhs);
        let addr_space = self.value_address_space(addr_value);
        let is_aggregate = info.field_ty.field_count(self.db) > 0;

        // For aggregate (struct) fields, emit pointer arithmetic instead of a load
        if is_aggregate {
            // Optimization: if offset is 0, reuse the base pointer directly
            if info.offset_bytes == 0 {
                // Ensure address space is propagated even when reusing the base pointer
                self.value_address_space.insert(addr_value, addr_space);
                return Some(addr_value);
            }
            // Emit FieldPtr for non-zero offsets
            let result = self.mir_body.alloc_value(ValueData {
                ty: info.field_ty,
                origin: ValueOrigin::FieldPtr(FieldPtrOrigin {
                    base: addr_value,
                    offset_bytes: info.offset_bytes,
                }),
            });
            // Propagate address space to the result
            self.value_address_space.insert(result, addr_space);
            return Some(result);
        }

        // For primitive fields, emit a get_field call to load the value
        let ptr_ty = match addr_space {
            AddressSpaceKind::Memory => self.core.helper_ty(CoreHelperTy::MemPtr),
            AddressSpaceKind::Storage => self.core.helper_ty(CoreHelperTy::StorPtr),
        };
        let offset_value = self.synthetic_u256(BigUint::from(info.offset_bytes));
        let callable =
            self.core
                .make_callable(expr, CoreHelper::GetField, &[ptr_ty, info.field_ty]);

        Some(self.mir_body.alloc_value(ValueData {
            ty: info.field_ty,
            origin: ValueOrigin::Call(CallOrigin {
                expr,
                callable,
                args: vec![addr_value, offset_value],
                receiver_space: None,
                resolved_name: None,
            }),
        }))
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
        if !self.is_unit_ty(self.typed_body.expr_ty(self.db, if_expr)) {
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

        let then_end = self.lower_expr_in(then_block, then_expr).0;
        if let Some(end_block) = then_end {
            self.set_terminator(
                end_block,
                Terminator::Goto {
                    target: merge_block,
                },
            );
        }

        if let Some(else_expr) = else_expr {
            let else_end = self.lower_expr_in(else_block, else_expr).0;
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
