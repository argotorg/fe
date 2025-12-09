use common::InputDb;
use dir_test::{Fixture, dir_test};
use driver::DriverDataBase;
use test_utils::snap_test;
use url::Url;

#[cfg(target_arch = "wasm32")]
use test_utils::url_utils::UrlExt;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/name_resolution",
    glob: "*.fe"
)]
fn run_name_resolution(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::from_file_path(fixture.path()).expect("path should be absolute"),
        Some(fixture.content().to_string()),
    );

    let top_mod = db.top_mod(file);

    let diags = db.run_on_top_mod(top_mod);
    let diags = diags.format_diags(&db);
    snap_test!(diags, fixture.path());
}

#[cfg(target_family = "wasm")]
mod wasm {
    use super::*;
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
