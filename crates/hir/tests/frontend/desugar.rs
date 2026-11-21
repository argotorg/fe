use std::path::Path;

use common::InputDb;
use fe_hir::lower::map_file_to_mod;
use fe_hir::test_db::HirAnalysisTestDb;
use test_utils::{fixture_shards, fixtures::Fixture, snap_test};

// Analyze the prepared program, then snapshot and reparse its displayed HIR.
// Generated quotations can resolve names in the producer's scope. The display
// cannot carry that scope into a new source file, so semantic round trips
// apply to HIR without generated members.
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
    crate::prepared_fixture::prepare_generation(db, file);
    let generated = !db.generation_inputs().requests(db).is_empty();
    let top_mod = map_file_to_mod(db, file);
    db.assert_no_diags(top_mod);

    let output = top_mod.pretty_print(db);
    let (_, errors) = parser::parse_source_file(&output, parser::RecoveryMode::NoRecover);
    assert!(
        errors.is_empty(),
        "displayed HIR does not parse: {errors:?}"
    );
    snap_test!(output, fixture.path());

    if !generated {
        let roundtrip_file =
            db.new_stand_alone(format!("{file_name}.roundtrip.fe").into(), &output);
        let roundtrip_mod = map_file_to_mod(db, roundtrip_file);
        db.assert_no_diags(roundtrip_mod);
    }
}
