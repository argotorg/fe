use std::{error::Error, fmt};

use hir::hir_def::{Body, Expr, ExprId, Func, Partial, Stmt, StmtId, TopLevelMod};
use hir_analysis::{
    HirAnalysisDb,
    ty::{
        diagnostics::FuncBodyDiag,
        ty_check::{TypedBody, check_func_body},
    },
};

use crate::ir::{
    BasicBlock, BasicBlockId, LoopInfo, MirBody, MirFunction, MirInst, MirModule, Terminator,
    ValueData, ValueId, ValueOrigin,
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
    fn new(db: &'db dyn HirAnalysisDb, body: Body<'db>, typed_body: &'a TypedBody<'db>) -> Self {
        Self {
            db,
            body,
            typed_body,
            mir_body: MirBody::new(),
            loop_stack: Vec::new(),
        }
    }

    fn finish(self) -> MirBody<'db> {
        self.mir_body
    }

    fn alloc_block(&mut self) -> BasicBlockId {
        self.mir_body.push_block(BasicBlock::new())
    }

    fn set_terminator(&mut self, block: BasicBlockId, term: Terminator) {
        self.mir_body.block_mut(block).set_terminator(term);
    }

    fn push_inst(&mut self, block: BasicBlockId, inst: MirInst<'db>) {
        self.mir_body.block_mut(block).push_inst(inst);
    }

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

    fn alloc_expr_value(&mut self, expr: ExprId) -> ValueId {
        let ty = self.typed_body.expr_ty(self.db, expr);
        self.mir_body.alloc_value(ValueData {
            ty,
            origin: ValueOrigin::Expr(expr),
        })
    }

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
