use common::InputDb;
use fe_hir::{analysis::initialize_analysis_pass, test_db::HirAnalysisTestDb};
use url::Url;

#[test]
fn recv_file_module_reports_diagnostic() {
    let mut db = HirAnalysisTestDb::default();
    let root = Url::from_directory_path(std::env::current_dir().unwrap())
        .unwrap()
        .join("recv_file_module/")
        .unwrap();
    let workspace = db.workspace();
    workspace.touch(
        &mut db,
        root.join("fe.toml").unwrap(),
        Some(
            r#"[ingot]
name = "recv_file_module"
version = "0.0.0"
"#
            .into(),
        ),
    );
    workspace.touch(
        &mut db,
        root.join("src/test_mod.fe").unwrap(),
        Some("pub msg TestMsg {}".into()),
    );
    let file = db.new_stand_alone(
        root.join("src/lib.fe")
            .unwrap()
            .to_file_path()
            .unwrap()
            .try_into()
            .unwrap(),
        "pub contract Token { recv test_mod {} }",
    );
    let (top_mod, _) = db.top_mod(file);
    // The focused HIR test pipeline skips contract validation.
    let diags: Vec<_> = initialize_analysis_pass()
        .run_on_module(&db, top_mod)
        .into_iter()
        .map(|diag| diag.to_complete(&db))
        .collect();

    assert_eq!(diags.len(), 1, "{diags:#?}");
    let diag = &diags[0];
    assert_eq!(diag.error_code.to_string(), "8-0090");
    assert_eq!(diag.message, "recv block expects a msg module");
    assert_eq!(
        diag.sub_diagnostics[0].message,
        "expected `msg` module, but file module `test_mod` is given"
    );
    let span = diag.sub_diagnostics[0].span.as_ref().unwrap();
    assert_eq!(span.file, file);
    assert_eq!(&file.text(&db)[span.range.clone()], "test_mod");
}
