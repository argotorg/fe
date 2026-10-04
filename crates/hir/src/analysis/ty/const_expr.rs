use crate::analysis::ty::assoc_const::{AssocConstUse, InherentConstUse};
use crate::analysis::ty::ty_def::TyId;
use crate::analysis::ty::{corelib::ctfe_extern_intrinsic_kind, ty_check::BodyOwner};
use crate::analysis::{HirAnalysisDb, semantic::SemanticInstanceKey};
use crate::hir_def::{ArithBinOp, CompBinOp, UnOp, attr::ArithmeticMode};
use salsa::Update;

#[salsa::interned]
#[derive(Debug)]
pub struct ConstExprId<'db> {
    #[return_ref]
    pub data: ConstExpr<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum ConstExpr<'db> {
    Invocation(ConstInvocation<'db>),
    ArithBinOp {
        op: ArithBinOp,
        mode: ArithmeticMode,
        lhs: TyId<'db>,
        rhs: TyId<'db>,
    },
    UnOp {
        op: UnOp,
        mode: ArithmeticMode,
        expr: TyId<'db>,
    },
    Cast {
        expr: TyId<'db>,
        to: TyId<'db>,
    },
    Compare {
        op: CompBinOp,
        lhs: TyId<'db>,
        rhs: TyId<'db>,
    },
    /// A conditional whose arms are forced only when the condition selects
    /// them, as execution evaluates only the arm it takes.
    Select {
        cond: TyId<'db>,
        then: TyId<'db>,
        otherwise: TyId<'db>,
    },
    ArrayRepeat {
        value: TyId<'db>,
        len: TyId<'db>,
    },
    ArrayIndex {
        array: TyId<'db>,
        index: TyId<'db>,
    },
    Field {
        value: TyId<'db>,
        index: usize,
    },
    TraitConst(AssocConstUse<'db>),
    InherentConst(InherentConstUse<'db>),
}

/// A call's final selected instance and original owned arguments. The result
/// type belongs to the enclosing `ConstTyData::Abstract`. The call is its
/// identity: every occurrence of the same call shares one term, wherever it
/// is written, because parameters in the key and arguments name their owners.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ConstInvocation<'db> {
    pub key: SemanticInstanceKey<'db>,
    pub args: Vec<TyId<'db>>,
}

impl<'db> ConstExprId<'db> {
    /// Bodyless user externs can participate in type identity, although the
    /// compiler has no implementation with which to force their values.
    pub(crate) fn is_opaque_extern(self, db: &'db dyn HirAnalysisDb) -> bool {
        matches!(self.data(db), ConstExpr::Invocation(invocation)
            if matches!(invocation.key.owner(db), BodyOwner::Func(func)
                if func.is_extern(db) && func.body(db).is_none()
                    && ctfe_extern_intrinsic_kind(db, func).is_none()))
    }

    pub fn pretty_print(self, db: &'db dyn HirAnalysisDb) -> String {
        match self.data(db) {
            ConstExpr::Invocation(invocation) => pretty_print_const_fn_call(db, invocation),
            ConstExpr::ArithBinOp { op, lhs, rhs, .. } => {
                format!(
                    "({} {} {})",
                    lhs.pretty_print(db),
                    op.pretty_print(),
                    rhs.pretty_print(db)
                )
            }
            ConstExpr::UnOp { op, expr, .. } => {
                pretty_print_un_op(*op, expr.pretty_print(db).to_string())
            }
            ConstExpr::Cast { expr, to } => {
                format!("({} as {})", expr.pretty_print(db), to.pretty_print(db))
            }
            ConstExpr::Compare { op, lhs, rhs } => format!(
                "({} {} {})",
                lhs.pretty_print(db),
                op.symbol(),
                rhs.pretty_print(db)
            ),
            ConstExpr::Select {
                cond,
                then,
                otherwise,
            } => format!(
                "(if {} {{ {} }} else {{ {} }})",
                cond.pretty_print(db),
                then.pretty_print(db),
                otherwise.pretty_print(db)
            ),
            ConstExpr::ArrayRepeat { value, len } => {
                format!("[{}; {}]", value.pretty_print(db), len.pretty_print(db))
            }
            ConstExpr::ArrayIndex { array, index } => {
                format!("{}[{}]", array.pretty_print(db), index.pretty_print(db))
            }
            ConstExpr::Field { value, index } => format!("{}.{index}", value.pretty_print(db)),
            ConstExpr::TraitConst(assoc) => {
                let inst = assoc.inst();
                let name = assoc.name();
                format!("{}::{}", inst.self_ty(db).pretty_print(db), name.data(db))
            }
            ConstExpr::InherentConst(use_) => {
                format!(
                    "{}::{}",
                    use_.receiver_ty().pretty_print(db),
                    use_.name().data(db)
                )
            }
        }
    }
}

fn pretty_print_const_fn_call<'db>(
    db: &'db dyn HirAnalysisDb,
    invocation: &ConstInvocation<'db>,
) -> String {
    let name = match invocation.key.owner(db) {
        BodyOwner::Func(func) => func
            .name(db)
            .to_opt()
            .map_or("<unknown>", |n| n.data(db).as_str()),
        _ => "<constant>",
    };
    let generic_args = invocation
        .key
        .subst(db)
        .generic_args(db)
        .iter()
        .map(|arg| arg.pretty_print(db).to_string())
        .collect::<Vec<_>>();
    let args = invocation
        .args
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
