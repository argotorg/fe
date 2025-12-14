use std::path::Path;

use dir_test::{Fixture, dir_test};
use fe_hir::lower::map_file_to_mod;
use fe_hir::test_db::HirAnalysisTestDb;
use test_utils::snap_test;

/// Tests HIR desugaring by pretty-printing the desugared output and
/// verifying it can be parsed and analyzed without errors.
///
/// This test:
/// 1. Parses and lowers a `.fe` file to HIR
/// 2. Pretty-prints the desugared HIR
/// 3. Generates a snapshot of the desugared output
/// 4. Re-parses and analyzes the output to verify it's valid Fe code
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/desugar",
    glob: "**/*.fe"
)]
fn hir_desugar(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let top_mod = map_file_to_mod(&db, file);

    let output = top_mod.pretty_print(&db);
    snap_test!(output, fixture.path());

    // Parse and analyze the printed output to verify it's valid Fe code
    let mut roundtrip_db = HirAnalysisTestDb::default();
    let roundtrip_file =
        roundtrip_db.new_stand_alone(format!("{}.roundtrip.fe", file_name).into(), &output);
    let roundtrip_mod = map_file_to_mod(&roundtrip_db, roundtrip_file);
    roundtrip_db.assert_no_diags(roundtrip_mod);
}
