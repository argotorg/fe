use common::diagnostics::DiagnosticPass;
use fe_hir::test_db::{HirAnalysisTestDb, initialize_test_analysis_pass};

#[test]
fn test_db_analysis_pipeline_reports_arithmetic_attr_errors() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "arithmetic_attr_invalid.fe".into(),
        r#"#[arithmetic = checked]
fn bad() {}"#,
    );
    let (top_mod, _) = db.top_mod(file);

    let mut pass_manager = initialize_test_analysis_pass();
    let diags = pass_manager.run_on_module(&db, top_mod);

    assert!(
        diags.iter().any(|diag| {
            let diag = diag.to_complete(&db);
            diag.error_code.pass == DiagnosticPass::AttrMisuse && diag.error_code.local_code == 2
        }),
        "expected invalid attribute diagnostic from test-db analysis pipeline"
    );
}
