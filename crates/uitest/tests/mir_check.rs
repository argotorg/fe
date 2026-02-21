//! MIR UI tests.
//!
//! The main `ty_check` ui test suite only runs the HIR analysis pipeline. `mut/ref/move` borrow
//! checking lives in MIR, so we snapshot MIR lowering/analysis failures separately.

use common::InputDb;
use dir_test::{Fixture, dir_test};
use driver::DriverDataBase;
use mir::lower_module;
use test_utils::snap_test;
use url::Url;

#[cfg(not(target_family = "wasm"))]
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/mir_check",
    glob: "**/*.fe"
)]
fn run_mir_check(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::from_file_path(fixture.path()).expect("path should be absolute"),
        Some(fixture.content().to_string()),
    );

    let top_mod = db.top_mod(file);
    let res = match lower_module(&db, top_mod) {
        Ok(_) => String::new(),
        Err(err) => err.to_string(),
    };

    snap_test!(res, fixture.path());
}
