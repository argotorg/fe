use common::InputDb;
use driver::DriverDataBase;
use fe_mir::{MirDiagnosticsMode, MirLowerError, collect_mir_diagnostics, lower_module};
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
