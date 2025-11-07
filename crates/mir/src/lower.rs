use std::{error::Error, fmt};

use hir::hir_def::{BodyKind, Func, TopLevelMod};
use hir_analysis::{
    HirAnalysisDb,
    ty::{
        diagnostics::FuncBodyDiag,
        ty_check::{TypedBody, check_func_body},
    },
};

use crate::ir::{BasicBlock, MirBody, MirFunction, MirModule, Terminator};

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
        let lowered = lower_function(
            db,
            func,
            typed_body.clone(),
            diags.clone(),
        )?;
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
    let terminator = make_return_terminator(body.expr(db), body.body_kind(db));
    mir_body.push_block(BasicBlock::new(terminator));

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
