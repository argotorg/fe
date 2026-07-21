use fe_hir::origin::{HirExprOrigin, HirStmtOrigin};

fn main() {
    fn mismatch(expr: HirExprOrigin<'_>, stmt: HirStmtOrigin<'_>) {
        let _ = expr.export_key("body:test");
        let _ = stmt.export_key("body:test");
    }
}
