use fe_hir::analysis::initialize_analysis_pass;
use fe_hir::test_db::{HirAnalysisTestDb, format_diagnostics};

#[test]
fn oversized_string_constants_report_type_errors() {
    for source in [
        "const BAD: String<33> = \"abcdefghijklmnopqrstuvwxyz1234567\"",
        "const BAD: String<31> = \"abcdefghijklmnopqrstuvwxyz123456\"",
        "const BAD: String<33> = \"short\"",
        "struct Marker {} impl Marker { const BAD: String<33> = \"abcdefghijklmnopqrstuvwxyz1234567\" }",
        "const BAD: String<32> = \"éééééééééééééééé\"",
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("invalid_string.fe".into(), &format!("{source}\n"));
        let (module, _) = db.top_mod(file);
        let diags = initialize_analysis_pass().run_on_module(&db, module);
        let rendered = format_diagnostics(&db, &diags);
        assert!(!diags.is_empty(), "{source}");
        assert!(rendered.contains("String"), "{rendered}");
        assert!(!rendered.contains("internal"), "{rendered}");
    }
}

#[test]
fn maximum_inline_string_constant_is_valid() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "valid_string.fe".into(),
        "const VALUE: String<31> = \"abcdefghijklmnopqrstuvwxyz12345\"\n",
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
}
