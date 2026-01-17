use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::ty_def::TyId;
use crate::hir_def::Func;
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
        }
    }
}
