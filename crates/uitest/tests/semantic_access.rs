use common::InputDb;
use driver::DriverDataBase;
use test_utils::{fixture_shards, fixtures::Fixture, snap_test};
use url::Url;

#[cfg(target_arch = "wasm32")]
use test_utils::url_utils::UrlExt;

fixture_shards!(
    semantic_access,
    concat!(env!("CARGO_MANIFEST_DIR"), "/fixtures/semantic_access"),
    "**/*.fe",
    DriverDataBase::default,
    run_semantic_access,
);

fn run_semantic_access(db: &mut DriverDataBase, fixture: &Fixture) {
    let file = db.workspace().touch(
        db,
        Url::from_file_path(fixture.path()).expect("path should be absolute"),
        Some(fixture.content().to_string()),
    );
    let top_mod = db.top_mod(file);
    let hir = db.run_on_top_mod(top_mod);
    let diags = if hir.has_errors(db) {
        hir.format_diags(db)
    } else {
        let diags = db.mir_diagnostics_for_top_mod(top_mod);
        db.format_complete_diagnostics(&diags)
    };
    snap_test!(diags, fixture.path());
}

#[cfg(target_family = "wasm")]
mod wasm {
    use super::*;
    use dir_test::{Fixture, dir_test};
    use test_utils::url_utils::UrlExt;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/fixtures/semantic_access",
        glob: "*.fe",
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn run_semantic_access(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            <Url as UrlExt>::from_file_path_lossy(fixture.path()),
            Some(fixture.content().to_string()),
        );

        let top_mod = db.top_mod(file);
        db.mir_diagnostics_for_top_mod(top_mod);
    }
}
