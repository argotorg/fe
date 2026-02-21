use common::InputDb;
use dir_test::{Fixture, dir_test};
use driver::DriverDataBase;
use fe_doc_engine::{DocExtractor, DocIndex};
use test_utils::snap_test;

fn normalize_paths(output: &str) -> String {
    let fixtures_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
    output.replace(&fixtures_dir.to_string_lossy().to_string(), "<fixtures>")
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures",
    glob: "*.fe",
)]
fn test_extract(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let url = url::Url::from_file_path(fixture.path()).expect("path should be absolute");
    let file = db.workspace().touch(&mut db, url, Some(fixture.content().to_string()));
    let top_mod = db.top_mod(file);

    let extractor = DocExtractor::new(&db);
    let index = extractor.extract_module(top_mod);

    let json = serde_json::to_string_pretty(&index).expect("serialize DocIndex");
    let output = normalize_paths(&json);
    snap_test!(output, fixture.path());
}

/// Integration test: extract → JSON → deserialize round-trip
/// Validates the full pipeline from HIR through to consumer.
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures",
    glob: "*.fe",
)]
fn test_round_trip(fixture: Fixture<&str>) {
    let mut db = DriverDataBase::default();
    let url = url::Url::from_file_path(fixture.path()).expect("path should be absolute");
    let file = db.workspace().touch(&mut db, url, Some(fixture.content().to_string()));
    let top_mod = db.top_mod(file);

    let extractor = DocExtractor::new(&db);
    let original = extractor.extract_module(top_mod);

    // Serialize to JSON
    let json = serde_json::to_string(&original).expect("serialize DocIndex");

    // Deserialize back
    let restored: DocIndex = serde_json::from_str(&json).expect("deserialize DocIndex");

    // Verify item count matches
    assert_eq!(
        original.items.len(),
        restored.items.len(),
        "item count mismatch for {}",
        fixture.path()
    );

    // Verify key items survive the round-trip
    for (a, b) in original.items.iter().zip(restored.items.iter()) {
        assert_eq!(a.path, b.path, "path mismatch");
        assert_eq!(a.name, b.name, "name mismatch");
        assert_eq!(a.kind, b.kind, "kind mismatch");
        assert_eq!(a.children.len(), b.children.len(), "children count mismatch for {}", a.path);
    }

    // Verify module tree survives
    assert_eq!(original.modules.len(), restored.modules.len(), "module tree mismatch");
}
