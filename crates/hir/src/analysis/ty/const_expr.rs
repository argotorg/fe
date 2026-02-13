use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::trait_def::TraitInstId;
use crate::analysis::ty::ty_check::LocalBinding;
use crate::analysis::ty::ty_def::TyId;
use crate::hir_def::{ArithBinOp, Func, IdentId, UnOp};
use salsa::Update;

#[salsa::interned]
#[derive(Debug)]
pub struct ConstExprId<'db> {
    #[return_ref]
    pub data: ConstExpr<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum ConstExpr<'db> {
    ExternConstFnCall {
        func: Func<'db>,
        generic_args: Vec<TyId<'db>>,
        args: Vec<TyId<'db>>,
    },
    UserConstFnCall {
        func: Func<'db>,
        generic_args: Vec<TyId<'db>>,
        args: Vec<TyId<'db>>,
    },
    ArithBinOp {
        op: ArithBinOp,
        lhs: TyId<'db>,
        rhs: TyId<'db>,
    },
    UnOp {
        op: UnOp,
        expr: TyId<'db>,
    },
    Cast {
        expr: TyId<'db>,
        to: TyId<'db>,
    },
    TraitConst {
        inst: TraitInstId<'db>,
        name: IdentId<'db>,
    },
    LocalBinding(LocalBinding<'db>),
}

impl<'db> ConstExprId<'db> {
    pub fn pretty_print(self, db: &'db dyn HirAnalysisDb) -> String {
        match self.data(db) {
            ConstExpr::ExternConstFnCall {
                func,
                generic_args,
                args,
            } => {
                let name = func
                    .name(db)
                    .to_opt()
                    .map(|n| n.data(db).as_str())
                    .unwrap_or("<unknown>");

                let generic_args = if generic_args.is_empty() {
                    String::new()
                } else {
                    let generic_args = generic_args
                        .iter()
                        .map(|a| a.pretty_print(db).as_str())
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("<{generic_args}>")
                };

                let args = args
                    .iter()
                    .map(|a| a.pretty_print(db).as_str())
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{name}{generic_args}({args})")
            }
            ConstExpr::UserConstFnCall {
                func,
                generic_args,
                args,
            } => {
                let name = func
                    .name(db)
                    .to_opt()
                    .map(|n| n.data(db).as_str())
                    .unwrap_or("<unknown>");

                let generic_args = if generic_args.is_empty() {
                    String::new()
                } else {
                    let generic_args = generic_args
                        .iter()
                        .map(|a| a.pretty_print(db).as_str())
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("<{generic_args}>")
                };

                let args = args
                    .iter()
                    .map(|a| a.pretty_print(db).as_str())
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{name}{generic_args}({args})")
            }
            ConstExpr::ArithBinOp { op, lhs, rhs } => {
                let op_str = match op {
                    ArithBinOp::Add => "+",
                    ArithBinOp::Sub => "-",
                    ArithBinOp::Mul => "*",
                    ArithBinOp::Div => "/",
                    ArithBinOp::Rem => "%",
                    ArithBinOp::Pow => "**",
                    ArithBinOp::LShift => "<<",
                    ArithBinOp::RShift => ">>",
                    ArithBinOp::BitAnd => "&",
                    ArithBinOp::BitOr => "|",
                    ArithBinOp::BitXor => "^",
                    ArithBinOp::Range => "..",
                };
                format!(
                    "({} {op_str} {})",
                    lhs.pretty_print(db),
                    rhs.pretty_print(db)
                )
            }
            ConstExpr::UnOp { op, expr } => {
                let op_str = match op {
                    UnOp::Plus => "+",
                    UnOp::Minus => "-",
                    UnOp::Not => "!",
                    UnOp::BitNot => "~",
                };
                format!("({op_str}{})", expr.pretty_print(db))
            }
            ConstExpr::Cast { expr, to } => {
                format!("({} as {})", expr.pretty_print(db), to.pretty_print(db))
            }
            ConstExpr::TraitConst { inst, name } => {
                format!("{}::{}", inst.self_ty(db).pretty_print(db), name.data(db))
            }
            ConstExpr::LocalBinding(binding) => format!("{binding:?}"),
        }
    }
}
