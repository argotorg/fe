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
            }
            | ConstExpr::UserConstFnCall {
                func,
                generic_args,
                args,
            } => pretty_print_const_fn_call(db, *func, generic_args, args),
            ConstExpr::ArithBinOp { op, lhs, rhs } => {
                format!(
                    "({} {} {})",
                    lhs.pretty_print(db),
                    op.pretty_print(),
                    rhs.pretty_print(db)
                )
            }
            ConstExpr::UnOp { op, expr } => {
                pretty_print_un_op(*op, expr.pretty_print(db).to_string())
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

fn pretty_print_const_fn_call<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    generic_args: &[TyId<'db>],
    args: &[TyId<'db>],
) -> String {
    let name = func
        .name(db)
        .to_opt()
        .map(|n| n.data(db).as_str())
        .unwrap_or("<unknown>");
    let generic_args = generic_args
        .iter()
        .map(|arg| arg.pretty_print(db).to_string())
        .collect::<Vec<_>>();
    let args = args
        .iter()
        .map(|arg| arg.pretty_print(db).to_string())
        .collect::<Vec<_>>();

    format!(
        "{name}{}({})",
        pretty_print_generic_args(&generic_args),
        args.join(", ")
    )
}

pub(super) fn pretty_print_generic_args(args: &[String]) -> String {
    if args.is_empty() {
        String::new()
    } else {
        format!("<{}>", args.join(", "))
    }
}

pub(super) fn pretty_print_un_op(op: UnOp, expr: String) -> String {
    let op_str = op.pretty_print();
    if matches!(op, UnOp::Mut | UnOp::Ref) {
        format!("({op_str} {expr})")
    } else {
        format!("({op_str}{expr})")
    }
}
