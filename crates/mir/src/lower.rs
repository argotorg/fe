use std::{error::Error, fmt};

use hir::hir_def::{
    Body, BodyKind, Expr, ExprId, Func, Partial, Stmt, StmtId, TopLevelMod,
};
use hir_analysis::{
    HirAnalysisDb,
    ty::{
        diagnostics::FuncBodyDiag,
        ty_check::{TypedBody, check_func_body},
    },
};

use crate::ir::{BasicBlock, MirBody, MirFunction, MirModule, MirInst, Terminator};

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

    let mut mir_body = MirBody::new();
    let mut block = BasicBlock::new(make_return_terminator(body.expr(db), body.body_kind(db)));

    lower_root_expr(db, body, body.expr(db), &mut block);

    mir_body.push_block(block);

    Ok(MirFunction {
        func,
        body: mir_body,
        typed_body,
    })
}

fn make_return_terminator(expr: hir::hir_def::ExprId, kind: BodyKind) -> Terminator {
    match kind {
        BodyKind::FuncBody => Terminator::Return(Some(expr)),
        BodyKind::Anonymous => Terminator::Return(Some(expr)),
    }
}

fn lower_root_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expr: ExprId,
    block: &mut BasicBlock<'db>,
) {
    let exprs = body.exprs(db);
    let Partial::Present(expr_data) = &exprs[expr] else {
        return;
    };

    if let Expr::Block(stmts) = expr_data {
        lower_block_stmts(db, body, stmts, block);
    }
}

fn lower_block_stmts<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    stmts: &[StmtId],
    block: &mut BasicBlock<'db>,
) {
    for &stmt_id in stmts {
        let Partial::Present(stmt) = stmt_id.data(db, body) else { continue };
        match stmt {
            Stmt::Let(pat, ty, value) => {
                block.push_inst(MirInst::Let {
                    stmt: stmt_id,
                    pat: *pat,
                    ty: ty.clone(),
                    value: *value,
                });
            }
            Stmt::For(pat, iter, body_expr) => block.push_inst(MirInst::ForLoop {
                stmt: stmt_id,
                pat: *pat,
                iter: *iter,
                body: *body_expr,
            }),
            Stmt::While(cond, body_expr) => block.push_inst(MirInst::WhileLoop {
                stmt: stmt_id,
                cond: *cond,
                body: *body_expr,
            }),
            Stmt::Continue => block.push_inst(MirInst::Continue { stmt: stmt_id }),
            Stmt::Break => block.push_inst(MirInst::Break { stmt: stmt_id }),
            Stmt::Return(value) => block.push_inst(MirInst::Return {
                stmt: stmt_id,
                value: *value,
            }),
            Stmt::Expr(expr) => {
                lower_expr_stmt(db, body, stmt_id, *expr, block);
            }
        }
    }
}

fn lower_expr_stmt<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    stmt_id: StmtId,
    expr: ExprId,
    block: &mut BasicBlock<'db>,
) {
    let exprs = body.exprs(db);
    let Partial::Present(expr_data) = &exprs[expr] else {
        return;
    };

    match expr_data {
        Expr::Assign(target, value) => block.push_inst(MirInst::Assign {
            stmt: stmt_id,
            target: *target,
            value: *value,
        }),
        Expr::AugAssign(target, value, op) => block.push_inst(MirInst::AugAssign {
            stmt: stmt_id,
            target: *target,
            value: *value,
            op: *op,
        }),
        _ => block.push_inst(MirInst::Expr {
            stmt: stmt_id,
            expr,
        }),
    }
}
