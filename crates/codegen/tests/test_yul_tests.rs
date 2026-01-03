use common::InputDb;
use dir_test::{Fixture, dir_test};
use driver::DriverDataBase;
use fe_codegen::emit_test_module_yul;
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

    let output = match emit_test_module_yul(&db, top_mod) {
        Ok(output) => output,
        Err(err) => panic!("MIR ERROR: {err}"),
    };

    assert_eq!(output.tests.len(), 1, "fixture should yield one test");
    snap_test!(output.tests[0].yul, fixture.path());
}
