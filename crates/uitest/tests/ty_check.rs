use common::InputDb;
use dir_test::{Fixture, dir_test};
use driver::DriverDataBase;
use test_utils::snap_test;

#[cfg(target_arch = "wasm32")]
use test_utils::url_utils::UrlExt;

use url::Url;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/ty_check",
    glob: "**/*.fe"
)]
fn run_ty_check(fixture: Fixture<&str>) {
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
    use test_utils::url_utils::UrlExt;
    use url::Url;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/fixtures/ty_check",
        glob: "*.fe",
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn run_ty_check(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            <Url as UrlExt>::from_file_path_lossy(fixture.path()),
            Some(fixture.content().to_string()),
        );

        let top_mod = db.top_mod(file);
        db.run_on_top_mod(top_mod);
    }
}
