use common::InputDb;
use driver::DriverDataBase;
use fe_mir::{
    MirDiagnosticsMode, MirLowerError, ValueOrigin, collect_mir_diagnostics, lower_module,
};
use url::Url;

#[test]
fn lower_module_reports_analysis_diagnostics_as_error() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///analysis_diagnostics.fe").unwrap();
    let src = r#"
pub fn mismatched_ret() -> bool {
    1
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);

    let err = lower_module(&db, top_mod).expect_err("analysis diagnostics should fail lowering");

    let MirLowerError::AnalysisDiagnostics {
        func_name,
        diagnostics,
    } = err
    else {
        panic!("expected AnalysisDiagnostics, got {err:?}");
    };

    assert!(
        func_name.contains("mismatched_ret"),
        "func name is {func_name}"
    );
    assert!(diagnostics.contains("type mismatch"));
}

#[test]
fn collect_mir_diagnostics_keeps_analysis_diagnostics_without_panicking() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///analysis_diagnostics_in_monomorphization.fe").unwrap();
    let src = r#"
fn foo(x: u8) -> u256 {
    x
}

#[test]
fn bar_test() {
    foo(42)
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);

    let output = collect_mir_diagnostics(&db, top_mod, MirDiagnosticsMode::CompilerParity);
    assert!(
        output.internal_errors.iter().any(|err| {
            matches!(
                err,
                MirLowerError::AnalysisDiagnostics {
                    func_name,
                    diagnostics
                } if func_name.contains("foo") && diagnostics.contains("type mismatch")
            )
        }),
        "expected AnalysisDiagnostics for `foo`, got: {:?}",
        output.internal_errors
    );
}

#[test]
fn lower_module_reports_const_array_materialization_failures_as_unsupported() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///const_array_materialization_unsupported.fe").unwrap();
    let src = r#"
const BIG: [String<64>; 1] = [
    "This is a long string that exceeds thirty-two bytes in length!!",
]

pub fn bad() -> String<64> {
    BIG[0]
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);

    let err = lower_module(&db, top_mod).expect_err("const-array materialization should fail");

    let MirLowerError::Unsupported { func_name, message } = err else {
        panic!("expected Unsupported, got {err:?}");
    };

    assert!(func_name.contains("bad"), "func name is {func_name}");
    assert!(
        message.contains("failed to materialize const"),
        "message is {message}"
    );
    assert!(message.contains("String<64>"), "message is {message}");
}

#[test]
fn lower_module_materializes_large_string_literal_via_const_region() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///large_string_literal_unsupported.fe").unwrap();
    let src = r#"
pub fn bad() -> String<64> {
    "This is a long string that exceeds thirty-two bytes in length!!"
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let module = lower_module(&db, top_mod).expect("large string literal should lower");
    assert!(
        module
            .functions
            .iter()
            .flat_map(|func| func.body.values.iter())
            .any(|value| matches!(value.origin, ValueOrigin::ConstRegion(_)))
    );
}

#[test]
fn lower_module_materializes_large_const_string_via_const_region() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///large_const_string_unsupported.fe").unwrap();
    let src = r#"
const BIG: String<64> = "This is a long string that exceeds thirty-two bytes in length!!"

pub fn bad() -> String<64> {
    BIG
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let module = lower_module(&db, top_mod).expect("large const string should lower");
    assert!(
        module
            .functions
            .iter()
            .flat_map(|func| func.body.values.iter())
            .any(|value| matches!(value.origin, ValueOrigin::ConstRegion(_)))
    );
}
