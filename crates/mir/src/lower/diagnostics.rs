use hir::analysis::diagnostics::{SpannedHirAnalysisDb, format_diags};
use hir::analysis::ty::diagnostics::FuncBodyDiag;

pub(super) fn format_func_body_diags(
    db: &dyn SpannedHirAnalysisDb,
    diags: &[FuncBodyDiag<'_>],
) -> String {
    format_diags(db, diags)
}
