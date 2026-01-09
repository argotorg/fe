use common::InputDb;
use dir_test::{Fixture, dir_test};
use driver::DriverDataBase;
use fe_mir::{fmt::format_module, lower_module};
use test_utils::snap_test;
use url::Url;

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures", glob: "*.fe")]
fn mir_lowering_snap(fixture: Fixture<&str>) {
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

    let mir_module = lower_module(&db, top_mod).expect("MIR lowering failed");
    let mir_output = format_module(&db, &mir_module);
    let mir_path = fixture.path().replace(".fe", ".mir.fe");
    snap_test!(mir_output, &mir_path);
}
