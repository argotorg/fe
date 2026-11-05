use std::path::Path;

use fe_hir::lower::map_file_to_mod;
use fe_hir::test_db::HirAnalysisTestDb;
use test_utils::{fixture_shards, fixtures::Fixture, snap_test};

// Tests HIR desugaring by pretty-printing the desugared output and
// verifying it can be parsed and analyzed without errors.
//
// This test:
// 1. Parses and lowers a `.fe` file to HIR
// 2. Pretty-prints the desugared HIR
// 3. Generates a snapshot of the desugared output
// 4. Re-parses and analyzes the output to verify it's valid Fe code
fixture_shards!(
    hir_desugar,
    concat!(env!("CARGO_MANIFEST_DIR"), "/test_files/desugar"),
    "**/*.fe",
    HirAnalysisTestDb::default,
    hir_desugar,
);

fn hir_desugar(db: &mut HirAnalysisTestDb, fixture: &Fixture) {
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let top_mod = map_file_to_mod(db, file);

    let output = top_mod.pretty_print(db);
    snap_test!(output, fixture.path());

    // Parse and analyze the printed output to verify it's valid Fe code
    let roundtrip_file = db.new_stand_alone(format!("{file_name}.roundtrip.fe").into(), &output);
    let roundtrip_mod = map_file_to_mod(db, roundtrip_file);
    db.assert_no_diags(roundtrip_mod);
}
