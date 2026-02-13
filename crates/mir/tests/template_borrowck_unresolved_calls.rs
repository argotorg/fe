use common::InputDb;
use driver::DriverDataBase;
use fe_mir::{MirDiagnosticsMode, collect_mir_diagnostics};
use url::Url;

#[test]
fn templates_report_borrow_conflicts_for_unresolved_borrow_return_calls() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///template_borrowck_unresolved_calls.fe").unwrap();
    let src = r#"
fn passthrough(x: mut u256) -> mut u256 {
    x
}

fn consume(_a: mut u256, _b: mut u256) {}

pub fn template_borrowck_unresolved_calls(x: mut u256) {
    let y: mut u256 = passthrough(x)
    let z: mut u256 = x
    consume(y, z)
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);

    let output = collect_mir_diagnostics(&db, top_mod, MirDiagnosticsMode::TemplatesOnly);
    assert!(
        output
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains("borrow conflict")),
        "expected template MIR borrow diagnostics, got: {:?}",
        output.diagnostics
    );
}
