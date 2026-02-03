use common::InputDb;
use dir_test::{Fixture, dir_test};
use driver::DriverDataBase;
use fe_mir::lower_module;
use test_utils::snap_test;
use url::Url;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/errors",
    glob: "*.fe"
)]
fn mir_lowering_errors(fixture: Fixture<&str>) {
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

    let err = lower_module(&db, top_mod).expect_err("expected MIR lowering to fail");
    snap_test!(err.to_string(), fixture.path());
}
