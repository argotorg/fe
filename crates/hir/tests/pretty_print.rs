use dir_test::{Fixture, dir_test};
use fe_hir::lower::map_file_to_mod;
use fe_hir::test_db::HirAnalysisTestDb;
use test_utils::snap_test;

/// Tests HIR pretty-printing by emitting all items and snapshotting the output.
/// Roundtrips are skipped because contracts desugar into helper modules.
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/pretty_print",
    glob: "**/*.fe"
)]
fn hir_pretty_print(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(fixture.path().into(), fixture.content());
    let top_mod = map_file_to_mod(&db, file);

    let output = top_mod.pretty_print(&db);

    snap_test!(output, fixture.path());
}
