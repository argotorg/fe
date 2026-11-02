use std::path::Path;

use fe_hir::test_db::HirAnalysisTestDb;
use test_utils::{fixture_shards, fixtures::Fixture};

fixture_shards!(
    accepts_trait_resolution_conformance_cases,
    concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/test_files/trait_resolution_conformance"
    ),
    "*.fe",
    HirAnalysisTestDb::default,
    accepts_trait_resolution_conformance_cases,
);

fn accepts_trait_resolution_conformance_cases(db: &mut HirAnalysisTestDb, fixture: &Fixture) {
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, _) = db.top_mod(file);

    db.assert_no_diags(top_mod);
}

#[test]
fn cyclic_impl_assoc_type_candidates_terminate() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "cyclic_impl_assoc_type_candidates.fe".into(),
        r#"
trait AssocCycle {
    type A
    type B
}

struct AssocCycleStruct {}

impl AssocCycle for AssocCycleStruct {
    type A = Self::B
    type B = Self::A
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);

    let diagnostics = db.run_on_top_mod(top_mod);
    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic.to_complete(&db).message == "cycle detected while resolving this type"
    }));
}
