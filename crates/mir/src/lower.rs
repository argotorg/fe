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
    BasicBlock, BasicBlockId, MirBody, MirFunction, MirInst, MirModule, Terminator, ValueData,
    ValueId, ValueOrigin,
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
    builder.lower_root(entry, body.expr(db));
    let (_, ret_val) = builder.lower_expr_in(entry, body.expr(db));
    builder.set_terminator(entry, Terminator::Return(Some(ret_val)));
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
}

impl<'db, 'a> MirBuilder<'db, 'a> {
    fn new(db: &'db dyn HirAnalysisDb, body: Body<'db>, typed_body: &'a TypedBody<'db>) -> Self {
        Self {
            db,
            body,
            typed_body,
            mir_body: MirBody::new(),
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

    fn lower_root(&mut self, block: BasicBlockId, expr: ExprId) {
        if let Partial::Present(Expr::Block(stmts)) = expr.data(self.db, self.body) {
            self.lower_block(block, expr, stmts);
        }
    }

    fn lower_block(&mut self, block: BasicBlockId, block_expr: ExprId, stmts: &[StmtId]) {
        let mut last_value = None;
        for &stmt_id in stmts {
            if let Some(val) = self.lower_stmt(block, stmt_id) {
                last_value = Some(val);
            }
        }

        if let Some(val) = last_value {
            self.mir_body.expr_values.insert(block_expr, val);
        }
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

    fn lower_expr_in(&mut self, block: BasicBlockId, expr: ExprId) -> (BasicBlockId, ValueId) {
        let val = self.ensure_value(expr);
        (block, val)
    }

    fn alloc_expr_value(&mut self, expr: ExprId) -> ValueId {
        let ty = self.typed_body.expr_ty(self.db, expr);
        self.mir_body.alloc_value(ValueData {
            ty,
            origin: ValueOrigin::Expr(expr),
        })
    }

    fn lower_stmt(&mut self, block: BasicBlockId, stmt_id: StmtId) -> Option<ValueId> {
        let Partial::Present(stmt) = stmt_id.data(self.db, self.body) else {
            return None;
        };
        match stmt {
            Stmt::Let(pat, ty, value) => {
                let value_id = value.map(|expr| self.lower_expr_in(block, expr).1);
                self.push_inst(
                    block,
                    MirInst::Let {
                        stmt: stmt_id,
                        pat: *pat,
                        ty: ty.clone(),
                        value: value_id,
                    },
                );
                None
            }
            Stmt::For(pat, iter, body_expr) => {
                let iter_val = self.lower_expr_in(block, *iter).1;
                self.push_inst(
                    block,
                    MirInst::ForLoop {
                        stmt: stmt_id,
                        pat: *pat,
                        iter: iter_val,
                        body: *body_expr,
                    },
                );
                None
            }
            Stmt::While(cond, body_expr) => {
                let cond_val = self.lower_expr_in(block, *cond).1;
                self.push_inst(
                    block,
                    MirInst::WhileLoop {
                        stmt: stmt_id,
                        cond: cond_val,
                        body: *body_expr,
                    },
                );
                None
            }
            Stmt::Continue => {
                self.push_inst(block, MirInst::Continue { stmt: stmt_id });
                None
            }
            Stmt::Break => {
                self.push_inst(block, MirInst::Break { stmt: stmt_id });
                None
            }
            Stmt::Return(value) => {
                let value_id = value.map(|expr| self.lower_expr_in(block, expr).1);
                self.push_inst(
                    block,
                    MirInst::Return {
                        stmt: stmt_id,
                        value: value_id,
                    },
                );
                None
            }
            Stmt::Expr(expr) => self.lower_expr_stmt(block, stmt_id, *expr),
        }
    }

    fn lower_expr_stmt(
        &mut self,
        block: BasicBlockId,
        stmt_id: StmtId,
        expr: ExprId,
    ) -> Option<ValueId> {
        let exprs = self.body.exprs(self.db);
        let Partial::Present(expr_data) = &exprs[expr] else {
            return None;
        };

        match expr_data {
            Expr::Assign(target, value) => {
                let value_id = self.lower_expr_in(block, *value).1;
                self.push_inst(
                    block,
                    MirInst::Assign {
                        stmt: stmt_id,
                        target: *target,
                        value: value_id,
                    },
                );
                None
            }
        Expr::AugAssign(target, value, op) => {
            let value_id = self.lower_expr_in(block, *value).1;
                self.push_inst(
                    block,
                    MirInst::AugAssign {
                        stmt: stmt_id,
                        target: *target,
                        value: value_id,
                        op: *op,
                    },
                );
                None
            }
            Expr::Block(stmts) => {
                self.lower_block(block, expr, stmts);
                let value_id = self.ensure_value(expr);
                Some(value_id)
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
                Some(value_id)
            }
        }
    }
}
