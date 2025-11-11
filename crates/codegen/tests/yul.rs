use common::InputDb;
use dir_test::{Fixture, dir_test};
use driver::DriverDataBase;
use fe_codegen::emit_module_yul;
use test_utils::snap_test;
use url::Url;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/testdata/simple_yul",
    glob: "*.fe"
)]
fn yul_snap(fixture: Fixture<&str>) {
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

    let output = match emit_module_yul(&db, top_mod) {
        Ok(results) => results
            .into_iter()
            .map(|res| match res {
                Ok(yul) => yul,
                Err(err) => format!("ERROR: {err}"),
            })
            .collect::<Vec<_>>()
            .join("\n"),
        Err(err) => format!("MIR ERROR: {err}"),
    };

    snap_test!(output, fixture.path());
}
