use fe_hir::origin::{HirExprOrigin, HirStmtOrigin};

fn takes_expr(_: HirExprOrigin<'_>) {}

fn main() {
    fn mismatch(stmt: HirStmtOrigin<'_>) {
        takes_expr(stmt);
    }
}
