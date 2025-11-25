mod test_db;

use common::stdlib::{HasBuiltinCore, HasBuiltinStd};
use driver::{DriverDataBase, db::DiagnosticsCollection};

#[cfg(target_arch = "wasm32")]
use test_utils::url_utils::UrlExt;

#[test]
fn analyze_corelib() {
    let db = DriverDataBase::default();
    let core = db.builtin_core();

    let core_diags = db.run_on_ingot(core);
    assert_builtin_clean(&db, core_diags, "core");
}

#[test]
fn analyze_stdlib() {
    let db = DriverDataBase::default();
    let std_ingot = db.builtin_std();

    let std_diags = db.run_on_ingot(std_ingot);
    assert_builtin_clean(&db, std_diags, "std");
}

fn assert_builtin_clean(db: &DriverDataBase, diags: DiagnosticsCollection<'_>, name: &str) {
    if diags.is_empty() {
        return;
    }

    diags.emit(db);
    panic!(
        "expected no diagnostics for builtin {name}, but got:\n{}",
        diags.format_diags(db)
    );
}
