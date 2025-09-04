use common::InputDb;
use dir_test::{Fixture, dir_test};
use driver::DriverDataBase;
use test_utils::snap_test;

#[cfg(target_arch = "wasm32")]
use test_utils::url_utils::UrlExt;

use url::Url;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/ty/def",
    glob: "*.fe"
)]
fn run_ty_def(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        url::Url::from_file_path(fixture.path()).expect("path should be absolute"),
        Some(fixture.content().to_string()),
    );

    let top_mod = db.top_mod(file);

    let diags = db.run_on_top_mod(top_mod);
    let diags = diags.format_diags(&db);
    snap_test!(diags, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/ty/const_ty",
    glob: "*.fe"
)]
fn run_const_ty(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        url::Url::from_file_path(fixture.path()).expect("path should be absolute"),
        Some(fixture.content().to_string()),
    );

    let top_mod = db.top_mod(file);

    let diags = db.run_on_top_mod(top_mod);
    let diags = diags.format_diags(&db);
    snap_test!(diags, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/ty/trait_bound",
    glob: "*.fe"
)]
fn run_trait_bound(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::from_file_path(fixture.path()).unwrap(),
        Some(fixture.content().to_string()),
    );

    let top_mod = db.top_mod(file);

    let diags = db.run_on_top_mod(top_mod);
    let diags = diags.format_diags(&db);
    snap_test!(diags, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/ty/trait_impl",
    glob: "*.fe"
)]
fn run_trait_impl(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::from_file_path(fixture.path()).unwrap(),
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
    use url::Url;
    use wasm_bindgen_test::wasm_bindgen_test;

    mod def {
        use super::*;
        use test_utils::url_utils::UrlExt;

        #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/fixtures/ty/def",
        glob: "*[!_cycle].fe",
        postfix: "wasm"
        )]
        #[dir_test_attr(
            #[wasm_bindgen_test]
        )]
        fn run_ty_def(fixture: Fixture<&str>) {
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

    mod const_ty {
        use super::*;
        use test_utils::url_utils::UrlExt;

        #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/fixtures/ty/const_ty",
        glob: "*.fe",
        postfix: "wasm"
        )]
        #[dir_test_attr(
            #[wasm_bindgen_test]
        )]
        fn run_const_ty(fixture: Fixture<&str>) {
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

    mod trait_bound {
        use super::*;
        use test_utils::url_utils::UrlExt;

        #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/fixtures/ty/trait_bound",
        glob: "*.fe",
        postfix: "wasm"
        )]
        #[dir_test_attr(
            #[wasm_bindgen_test]
        )]
        fn run_trait_bound(fixture: Fixture<&str>) {
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

    mod trait_impl {
        use test_utils::url_utils::UrlExt;
        use url::Url;

        use super::*;

        #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/fixtures/ty/trait_impl",
        glob: "*.fe",
        postfix: "wasm"
        )]
        #[dir_test_attr(
            #[wasm_bindgen_test]
        )]
        fn run_trait_impl(fixture: Fixture<&str>) {
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
}
