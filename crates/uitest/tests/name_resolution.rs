use common::InputDb;
use driver::DriverDataBase;
use test_utils::{fixture_shards, fixtures::Fixture, snap_test};
use url::Url;

#[cfg(target_arch = "wasm32")]
use test_utils::url_utils::UrlExt;

fixture_shards!(
    name_resolution,
    concat!(env!("CARGO_MANIFEST_DIR"), "/fixtures/name_resolution"),
    "*.fe",
    DriverDataBase::default,
    run_name_resolution,
);

fn run_name_resolution(db: &mut DriverDataBase, fixture: &Fixture) {
    let file = db.workspace().touch(
        db,
        Url::from_file_path(fixture.path()).expect("path should be absolute"),
        Some(fixture.content().to_string()),
    );
    let top_mod = db.top_mod(file);
    let diags = db.run_on_top_mod(top_mod).format_diags(db);
    snap_test!(diags, fixture.path());
}

#[cfg(target_family = "wasm")]
mod wasm {
    use super::*;
    use dir_test::{Fixture, dir_test};
    use wasm_bindgen_test::wasm_bindgen_test;

    #[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/name_resolution",
    glob: "*.fe",
    postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn run_name_resolution(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::from_file_path(fixture.path()).unwrap_or_else(|_| Url::parse("file:///").unwrap()),
            Some(fixture.content().to_string()),
        );

        let top_mod = db.top_mod(file);
        db.run_on_top_mod(top_mod);
    }
}
