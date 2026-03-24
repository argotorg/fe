use common::InputDb;
use dir_test::{Fixture, dir_test};
use driver::DriverDataBase;
use fe_codegen::emit_test_module_yul;
use std::path::PathBuf;
use test_utils::snap_test;
use url::Url;

/// Snapshot test for emitted test Yul objects.
///
/// * `fixture` - Fixture containing the input file and contents.
///
/// Returns nothing; asserts on the emitted Yul output.
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/test_output",
    glob: "*.fe"
)]
fn yul_test_object_snap(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let file_url = Url::from_file_path(fixture.path()).expect("fixture path should be absolute");
    db.workspace().touch(
        &mut db,
        file_url.clone(),
        Some(fixture.content().to_string()),
    );
    let file = db
        .workspace()
        .get(&db, &file_url)
        .expect("file should be loaded");
    let top_mod = db.top_mod(file);

    let output = match emit_test_module_yul(&db, top_mod, None) {
        Ok(output) => output,
        Err(err) => panic!("MIR ERROR: {err}"),
    };

    assert_eq!(output.tests.len(), 1, "fixture should yield one test");
    snap_test!(output.tests[0].yul, fixture.path());
}

#[test]
fn yul_test_filter_limits_emitted_tests() {
    let mut db = DriverDataBase::default();
    let fixture_path =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/test_output/effect_test.fe");
    let source = std::fs::read_to_string(&fixture_path).expect("filter fixture should be readable");
    let file_url = Url::from_file_path(&fixture_path).expect("fixture path should be absolute");
    db.workspace()
        .touch(&mut db, file_url.clone(), Some(source.to_string()));
    let file = db
        .workspace()
        .get(&db, &file_url)
        .expect("file should be loaded");
    let top_mod = db.top_mod(file);

    let output = emit_test_module_yul(&db, top_mod, Some("does_not_match"))
        .expect("filtered Yul test emission should succeed");

    assert!(output.tests.is_empty());
}

#[test]
fn yul_test_filter_skips_unselected_invalid_tests() {
    let mut db = DriverDataBase::default();
    let file_url = Url::from_file_path(
        std::env::temp_dir().join("yul_test_filter_skips_unselected_invalid_tests.fe"),
    )
    .expect("fixture path should be absolute");
    db.workspace().touch(
        &mut db,
        file_url.clone(),
        Some(
            r#"
#[test]
fn keep() {
    assert(true)
}

#[test]
fn drop_me() -> u256 {
    1
}
"#
            .to_string(),
        ),
    );
    let file = db
        .workspace()
        .get(&db, &file_url)
        .expect("file should be loaded");
    let top_mod = db.top_mod(file);

    let output = emit_test_module_yul(&db, top_mod, Some("keep"))
        .expect("filtered Yul test emission should ignore unrelated invalid tests");

    assert_eq!(output.tests.len(), 1, "expected exactly one filtered test");
    assert_eq!(output.tests[0].hir_name, "keep");
}
