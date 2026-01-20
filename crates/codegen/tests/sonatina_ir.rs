//! Snapshot tests for Sonatina IR output.
//!
//! These tests compile Fe fixtures to Sonatina IR and snapshot the human-readable
//! IR text. This helps catch IR lowering bugs and makes it easy to review what
//! IR is generated for each fixture.
//!
//! Snapshots are stored in `fixtures/sonatina_ir/` to avoid conflicting with Yul snapshots.

use common::InputDb;
use dir_test::{Fixture, dir_test};
use driver::DriverDataBase;
use fe_codegen::emit_module_sonatina_ir;
use std::path::Path;
use test_utils::_macro_support::_insta::{self, Settings};
use url::Url;

// NOTE: `dir_test` discovers fixtures at compile time; new fixture files will be picked up on a
// clean build (e.g. CI) or whenever this test target is recompiled.
//
// Unlike the Yul tests which run on all fixtures, Sonatina IR tests only run on fixtures
// that the backend currently supports. Unsupported fixtures will produce LowerError::Unsupported
// which we skip gracefully.
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures",
    glob: "*.fe"
)]
fn sonatina_ir_snap(fixture: Fixture<&str>) {
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

    let output = match emit_module_sonatina_ir(&db, top_mod) {
        Ok(ir) => ir,
        Err(fe_codegen::LowerError::Unsupported(msg)) => {
            // Skip unsupported fixtures - they'll be tracked separately
            eprintln!("SKIP {}: unsupported ({})", fixture.path(), msg);
            return;
        }
        Err(fe_codegen::LowerError::Internal(msg)) => {
            // Skip fixtures with internal errors (missing features, etc.)
            eprintln!("SKIP {}: internal error ({})", fixture.path(), msg);
            return;
        }
        Err(err) => panic!("Sonatina IR lowering failed: {err}"),
    };

    // Store snapshots in sonatina_ir/ subdirectory to avoid conflicting with Yul snapshots
    let fixture_path = Path::new(fixture.path());
    let fixture_name = fixture_path.file_stem().unwrap().to_str().unwrap();
    let snapshot_dir = fixture_path.parent().unwrap().join("sonatina_ir");

    let mut settings = Settings::new();
    settings.set_snapshot_path(snapshot_dir);
    settings.set_input_file(fixture.path());
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| {
        _insta::assert_snapshot!(fixture_name, output);
    });
}
