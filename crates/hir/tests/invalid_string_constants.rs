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

#[test]
fn constant_annotation_paths_report_errors() {
    for (annotation, prefix, expected) in [
        ("Missing", "", "Missing"),
        (
            "hidden::Private",
            "mod hidden { struct Private {} }",
            "not visible",
        ),
        ("value", "fn value() -> u256 { 1 }", "type"),
    ] {
        for declaration in [
            format!("const BAD: {annotation} = 1"),
            format!("trait Bad {{ const VALUE: {annotation} }}"),
            format!("trait Bad {{ const VALUE: {annotation} = 1 }}"),
        ] {
            let mut db = HirAnalysisTestDb::default();
            let source = format!("{prefix}\n{declaration}\n");
            let file = db.new_stand_alone("invalid_annotation.fe".into(), &source);
            let (module, _) = db.top_mod(file);
            let diags = initialize_analysis_pass().run_on_module(&db, module);
            let rendered = format_diagnostics(&db, &diags);
            assert!(!diags.is_empty(), "{source}");
            assert!(rendered.contains(expected), "{source}\n{rendered}");
        }
    }
}

#[test]
fn trait_constant_headers_report_invalid_types() {
    for declaration in [
        "trait Bad { const VALUE: String<33> }",
        "trait Bad { const VALUE: String<33> = \"short\" }",
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("invalid_trait_const.fe".into(), declaration);
        let (module, _) = db.top_mod(file);
        let diags = initialize_analysis_pass().run_on_module(&db, module);
        let rendered = format_diagnostics(&db, &diags);
        assert_eq!(diags.len(), 1, "{declaration}\n{rendered}");
        assert!(rendered.contains("String"), "{rendered}");
    }
}

#[test]
fn valid_trait_constant_headers_accept_generics() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "valid_trait_const.fe".into(),
        "trait Valid<T> { const VALUE: T const TEXT: String<31> = \"short\" }\n",
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
}
