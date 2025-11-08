use std::{error::Error, fmt};

use hir::hir_def::{Body, Expr, ExprId, Func, Partial, Stmt, StmtId, TopLevelMod};
use hir_analysis::{
    HirAnalysisDb,
    ty::{
        diagnostics::FuncBodyDiag,
        ty_check::{TypedBody, check_func_body},
        ty_def::TyId,
    },
};

use crate::ir::{
    BasicBlock, BasicBlockId, CallOrigin, LoopInfo, MirBody, MirFunction, MirInst, MirModule,
    Terminator, ValueData, ValueId, ValueOrigin,
};

#[derive(Debug)]
pub enum MirLowerError {
    MissingBody { func_name: String },
}

impl fmt::Display for MirLowerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MirLowerError::MissingBody { func_name } => {
                write!(f, "function `{func_name}` is missing a body")
            }
        }
    }
}

impl Error for MirLowerError {}

pub type MirLowerResult<T> = Result<T, MirLowerError>;

pub fn lower_module<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> MirLowerResult<MirModule<'db>> {
    let mut module = MirModule::new(top_mod);

    for &func in top_mod.all_funcs(db) {
        let (diags, typed_body) = check_func_body(db, func);
        let lowered = lower_function(db, func, typed_body.clone(), diags.clone())?;
        module.functions.push(lowered);
    }

    Ok(module)
}

fn lower_function<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    typed_body: TypedBody<'db>,
    _diags: Vec<FuncBodyDiag<'db>>,
) -> MirLowerResult<MirFunction<'db>> {
    let Some(body) = func.body(db) else {
        let func_name = func
            .name(db)
            .to_opt()
            .map(|ident| ident.data(db).to_string())
            .unwrap_or_else(|| "<anonymous>".to_string());
        return Err(MirLowerError::MissingBody { func_name });
    };

    let mut builder = MirBuilder::new(db, body, &typed_body);
    let entry = builder.alloc_block();
    let fallthrough = builder.lower_root(entry, body.expr(db));
    let ret_val = builder.ensure_value(body.expr(db));
    if let Some(block) = fallthrough {
        builder.set_terminator(block, Terminator::Return(Some(ret_val)));
    }
    let mir_body = builder.finish();

    Ok(MirFunction {
        func,
        body: mir_body,
        typed_body,
    })
}

struct MirBuilder<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    typed_body: &'a TypedBody<'db>,
    mir_body: MirBody<'db>,
    loop_stack: Vec<LoopScope>,
}

#[derive(Clone, Copy)]
struct LoopScope {
    continue_target: BasicBlockId,
    break_target: BasicBlockId,
}

impl<'db, 'a> MirBuilder<'db, 'a> {
    /// Create a new MIR builder for the given HIR body and its typed info.
    fn new(db: &'db dyn HirAnalysisDb, body: Body<'db>, typed_body: &'a TypedBody<'db>) -> Self {
        Self {
            db,
            body,
            typed_body,
            mir_body: MirBody::new(),
            loop_stack: Vec::new(),
        }
    }

    /// Consume the builder and return the constructed MIR body.
    fn finish(self) -> MirBody<'db> {
        self.mir_body
    }

    /// Allocate a fresh basic block and return its identifier.
    fn alloc_block(&mut self) -> BasicBlockId {
        self.mir_body.push_block(BasicBlock::new())
    }

    /// Set the terminator for a basic block.
    fn set_terminator(&mut self, block: BasicBlockId, term: Terminator) {
        self.mir_body.block_mut(block).set_terminator(term);
    }

    /// Append an instruction to the given block.
    fn push_inst(&mut self, block: BasicBlockId, inst: MirInst<'db>) {
        self.mir_body.block_mut(block).push_inst(inst);
    }

    /// Lower the root expression of a body into MIR, starting at `block`.
    fn lower_root(&mut self, block: BasicBlockId, expr: ExprId) -> Option<BasicBlockId> {
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Block(stmts)) => self.lower_block(block, expr, stmts),
            _ => {
                let value = self.ensure_value(expr);
                self.mir_body.expr_values.insert(expr, value);
                Some(block)
            }
        }
    }

    /// Lower a block expression by sequentially lowering its statements.
    fn lower_block(
        &mut self,
        block: BasicBlockId,
        block_expr: ExprId,
        stmts: &[StmtId],
    ) -> Option<BasicBlockId> {
        let mut current = Some(block);
        let mut last_value = None;
        for &stmt_id in stmts {
            let Some(curr_block) = current else { break };
            let (next_block, value) = self.lower_stmt(curr_block, stmt_id);
            if let Some(val) = value {
                last_value = Some(val);
            }
            current = next_block;
        }

        if let Some(val) = last_value {
            self.mir_body.expr_values.insert(block_expr, val);
        }

        current
    }

    /// Ensure that the given expression has a corresponding MIR value.
    fn ensure_value(&mut self, expr: ExprId) -> ValueId {
        if let Some(&val) = self.mir_body.expr_values.get(&expr) {
            return val;
        }

        let value = match expr.data(self.db, self.body) {
            Partial::Present(Expr::Block(stmts)) => {
                let last_expr = stmts.iter().rev().find_map(|stmt_id| {
                    let Partial::Present(stmt) = stmt_id.data(self.db, self.body) else {
                        return None;
                    };
                    if let Stmt::Expr(expr_id) = stmt {
                        Some(*expr_id)
                    } else {
                        None
                    }
                });
                if let Some(inner) = last_expr {
                    let val = self.ensure_value(inner);
                    self.mir_body.expr_values.insert(expr, val);
                    return val;
                }
                self.alloc_expr_value(expr)
            }
            _ => self.alloc_expr_value(expr),
        };

        self.mir_body.expr_values.insert(expr, value);
        value
    }

    /// Lower an expression inside a concrete block, returning the exit block and value.
    fn lower_expr_in(
        &mut self,
        block: BasicBlockId,
        expr: ExprId,
    ) -> (Option<BasicBlockId>, ValueId) {
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Block(stmts)) => {
                let next_block = self.lower_block(block, expr, stmts);
                let val = self.ensure_value(expr);
                (next_block, val)
            }
            _ => {
                let val = self.ensure_value(expr);
                (Some(block), val)
            }
        }
    }

    /// Allocate the MIR value slot for an expression, handling special cases.
    fn alloc_expr_value(&mut self, expr: ExprId) -> ValueId {
        if let Some(value) = self.try_lower_call(expr) {
            return value;
        }

        let ty = self.typed_body.expr_ty(self.db, expr);
        self.mir_body.alloc_value(ValueData {
            ty,
            origin: ValueOrigin::Expr(expr),
        })
    }

    /// Attempt to lower a function or method call into a call-origin MIR value.
    fn try_lower_call(&mut self, expr: ExprId) -> Option<ValueId> {
        let (args, callable) = match expr.data(self.db, self.body) {
            Partial::Present(Expr::Call(_, call_args)) => {
                let Some(callable) = self.typed_body.callable_expr(expr) else {
                    return None;
                };
                let mut args = Vec::with_capacity(call_args.len());
                for arg in call_args.iter() {
                    args.push(self.ensure_value(arg.expr));
                }
                (args, callable)
            }
            Partial::Present(Expr::MethodCall(receiver, _, _, call_args)) => {
                let Some(callable) = self.typed_body.callable_expr(expr) else {
                    return None;
                };
                let mut args = Vec::with_capacity(call_args.len() + 1);
                args.push(self.ensure_value(*receiver));
                for arg in call_args.iter() {
                    args.push(self.ensure_value(arg.expr));
                }
                (args, callable)
            }
            _ => return None,
        };

        let ty = self.typed_body.expr_ty(self.db, expr);
        Some(self.mir_body.alloc_value(ValueData {
            ty,
            origin: ValueOrigin::Call(CallOrigin {
                expr,
                callable: callable.clone(),
                args,
            }),
        }))
    }

    /// Lower a statement, returning the successor block and (optional) produced value.
    fn lower_stmt(
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
                if let Some(curr_block) = next_block {
                    self.push_inst(
                        curr_block,
                        MirInst::Let {
                            stmt: stmt_id,
                            pat: *pat,
                            ty: ty.clone(),
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

    /// Lower a `while` loop statement and wire up its control-flow edges.
    fn lower_while(
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

        let body_end = match body_expr.data(self.db, self.body) {
            Partial::Present(Expr::Block(stmts)) => self.lower_block(body_block, body_expr, stmts),
            _ => self.lower_expr_in(body_block, body_expr).0,
        };

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

    /// Lower an `if` expression used in statement position.
    fn lower_if_stmt(
        &mut self,
        block: BasicBlockId,
        cond: ExprId,
        then_expr: ExprId,
        else_expr: Option<ExprId>,
    ) -> Option<BasicBlockId> {
        let (cond_block_opt, cond_val) = self.lower_expr_in(block, cond);
        let cond_block = match cond_block_opt {
            Some(block) => block,
            None => return None,
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
            self.set_terminator(end_block, Terminator::Goto { target: merge_block });
        }

        if let Some(else_expr) = else_expr {
            let else_end = self.lower_expr_in(else_block, else_expr).0;
            if let Some(end_block) = else_end {
                self.set_terminator(end_block, Terminator::Goto { target: merge_block });
            }
        }

        Some(merge_block)
    }

    /// Returns whether the given type represents the unit value.
    fn is_unit_ty(&self, ty: TyId<'db>) -> bool {
        ty.is_tuple(self.db) && ty.field_count(self.db) == 0
    }

    /// Lower an expression statement, producing its continuation block/value.
    fn lower_expr_stmt(
        &mut self,
        block: BasicBlockId,
        stmt_id: StmtId,
        expr: ExprId,
    ) -> (Option<BasicBlockId>, Option<ValueId>) {
        let exprs = self.body.exprs(self.db);
        let Partial::Present(expr_data) = &exprs[expr] else {
            return (Some(block), None);
        };

        match expr_data {
            Expr::Assign(target, value) => {
                let (next_block, value_id) = self.lower_expr_in(block, *value);
                if let Some(curr_block) = next_block {
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
                let expr_ty = self.typed_body.expr_ty(self.db, expr);
                if self.is_unit_ty(expr_ty) {
                    let next_block = self.lower_if_stmt(block, *cond, *then_expr, *else_expr);
                    (next_block, None)
                } else {
                    let value_id = self.ensure_value(expr);
                    self.push_inst(
                        block,
                        MirInst::Eval {
                            stmt: stmt_id,
                            value: value_id,
                        },
                    );
                    (Some(block), Some(value_id))
                }
            }
            Expr::AugAssign(target, value, op) => {
                let (next_block, value_id) = self.lower_expr_in(block, *value);
                if let Some(curr_block) = next_block {
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
            Expr::Block(stmts) => {
                let next_block = self.lower_block(block, expr, stmts);
                let value_id = self.ensure_value(expr);
                (next_block, Some(value_id))
            }
            _ => {
                let value_id = self.ensure_value(expr);
                self.push_inst(
                    block,
                    MirInst::Eval {
                        stmt: stmt_id,
                        value: value_id,
                    },
                );
                (Some(block), Some(value_id))
            }
        }
    }
}
