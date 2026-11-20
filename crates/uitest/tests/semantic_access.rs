#[path = "common/prepared_source.rs"]
mod prepared_source;

use common::{InputDb, compilation::Target};
use driver::DriverDataBase;
use salsa::Setter;
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
    snap_test!(
        diagnostics(db, fixture.path(), fixture.content()),
        fixture.path()
    );
}

/// The diagnostics of a program checked as a standalone file: its HIR
/// errors, or else its semantic access errors.
fn diagnostics(db: &mut DriverDataBase, path: &str, content: &str) -> String {
    prepared_source::diagnostics(
        db,
        Url::from_file_path(path).expect("path should be absolute"),
        content,
        |db, file| {
            let top_mod = db.top_mod(file);
            let hir = db.run_on_top_mod(top_mod);
            if hir.has_errors(db) {
                hir.format_diags(db)
            } else {
                let diags = db.mir_diagnostics_for_top_mod(top_mod);
                db.format_complete_diagnostics(&diags)
            }
        },
    )
}

// A variadic call reads its extra arguments, which only native code makes.
#[cfg(not(target_arch = "wasm32"))]
#[test]
fn native_variadic_extras_are_read() {
    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/fixtures/native/variadic_access.fe"
    );
    let mut db = DriverDataBase::default();
    db.compilation_settings()
        .set_target(&mut db)
        .to(Target::Native);
    let diags = diagnostics(
        &mut db,
        path,
        include_str!("../fixtures/native/variadic_access.fe"),
    );
    snap_test!(diags, path);
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
