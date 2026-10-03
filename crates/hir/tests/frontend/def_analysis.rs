use std::path::Path;

use dir_test::{Fixture, dir_test};
use fe_hir::test_db::HirAnalysisTestDb;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/def_analysis",
    glob: "*.fe"
)]
fn def_analysis_standalone(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn finite_wrapper_nesting_has_no_recursion_depth_cutoff() {
    let mut ty = "u256".to_string();
    for _ in 0..128 {
        ty = format!("Inline<{ty}>");
    }
    let source = format!("struct Inline<T> {{ value: T }}\nstruct Finite {{ value: {ty} }}");
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("finite_wrapper_nesting.fe".into(), &source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
}

#[test]
fn expanding_wrapper_recursion_diagnoses_in_bounded_time() {
    let (sender, receiver) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "expanding_wrapper_recursion.fe".into(),
            "struct Inline<T> { value: T }\nstruct Grow<T> { next: Inline<Grow<[T; 1]>> }",
        );
        let (module, _) = db.top_mod(file);
        let diagnostics = fe_hir::analysis::initialize_analysis_pass().run_on_module(&db, module);
        sender
            .send(
                diagnostics
                    .iter()
                    .map(|diag| diag.to_complete(&db).message)
                    .collect::<Vec<_>>(),
            )
            .unwrap();
    });
    let messages = receiver
        .recv_timeout(std::time::Duration::from_secs(60))
        .expect("expanding representation checking must terminate");
    assert!(
        messages
            .iter()
            .any(|message| message == "recursive type definition"),
        "{messages:?}"
    );
}
