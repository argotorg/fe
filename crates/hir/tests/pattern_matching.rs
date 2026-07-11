use std::path::Path;

use dir_test::{Fixture, dir_test};
use fe_hir::test_db::{HirAnalysisTestDb, format_diagnostics, initialize_test_analysis_pass};
use test_utils::snap_test;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/pattern_matching",
    glob: "exhaustive/*.fe"
)]
#[allow(clippy::print_stderr)]
fn exhaustive_matches(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, _) = db.top_mod(file);

    // Exhaustive tests should have no diagnostics
    let mut manager = initialize_test_analysis_pass();
    let diags = manager.run_on_module(&db, top_mod);

    if !diags.is_empty() {
        let mut diagnostic_output = format!(
            "Exhaustive test file {} has {} diagnostic(s):\n\n",
            file_name,
            diags.len()
        );
        diagnostic_output.push_str(&format_diagnostics(&db, &diags));
        eprintln!("{diagnostic_output}");

        panic!(
            "Exhaustive test file {} should have no diagnostics but found {} diagnostic(s)",
            file_name,
            diags.len()
        );
    }
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/pattern_matching",
    glob: "non_exhaustive/*.fe"
)]
fn non_exhaustive_matches(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, _) = db.top_mod(file);

    let mut manager = initialize_test_analysis_pass();

    let diags = manager.run_on_module(&db, top_mod);

    // Verify we have at least one diagnosis
    assert!(
        !diags.is_empty(),
        "Expected pattern matching errors but found none in file: {file_name}"
    );

    let mut diagnostic_output = format!(
        "Non-exhaustive test file {} has {} diagnostic(s):\n\n",
        file_name,
        diags.len()
    );
    diagnostic_output.push_str(&format_diagnostics(&db, &diags));
    snap_test!(diagnostic_output, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/pattern_matching",
    glob: "unreachable/*.fe"
)]
fn unreachable_patterns(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, _) = db.top_mod(file);

    let mut manager = initialize_test_analysis_pass();

    let diags = manager.run_on_module(&db, top_mod);

    // Verify we have at least one diagnosis
    assert!(
        !diags.is_empty(),
        "Expected unreachable pattern errors but found none in file: {file_name}"
    );

    let mut diagnostic_output = format!(
        "Unreachable test file {} has {} diagnostic(s):\n\n",
        file_name,
        diags.len()
    );
    diagnostic_output.push_str(&format_diagnostics(&db, &diags));
    snap_test!(diagnostic_output, fixture.path());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/pattern_matching",
    glob: "misc_tests/*.fe"
)]
fn misc_pattern_tests(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, _) = db.top_mod(file);

    // Misc tests may have diagnostics (for testing error cases) or no diagnostics (for testing correct behavior)
    let mut manager = initialize_test_analysis_pass();
    let diags = manager.run_on_module(&db, top_mod);

    if !diags.is_empty() {
        let mut diagnostic_output = format!(
            "Misc test file {} has {} diagnostic(s):\n\n",
            file_name,
            diags.len()
        );
        diagnostic_output.push_str(&format_diagnostics(&db, &diags));
        snap_test!(diagnostic_output, fixture.path());
    }
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/pattern_matching",
    glob: "edge_cases/*.fe"
)]
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/pattern_matching",
    glob: "stress_tests/*.fe"
)]
fn stress_pattern_tests(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, _) = db.top_mod(file);

    // Stress tests should have no diagnostics (they test performance and correctness)
    let mut manager = initialize_test_analysis_pass();
    let diags = manager.run_on_module(&db, top_mod);

    if !diags.is_empty() {
        let mut diagnostic_output = format!(
            "Stress test file {} has {} diagnostic(s):\n\n",
            file_name,
            diags.len()
        );
        diagnostic_output.push_str(&format_diagnostics(&db, &diags));
        snap_test!(diagnostic_output, fixture.path());
    }
}

#[test]
fn invalid_pattern_constructor_paths_emit_hir_diagnostics() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "invalid_pattern_constructor_paths_emit_hir_diagnostics.fe".into(),
        r#"
enum E {
    A,
    B(u8),
    C { x: u8 },
}

fn invalid_path_pattern(e: E) -> u8 {
    match e {
        E::Missing => 0
        _ => 1
    }
}

fn invalid_tuple_pattern(e: E) -> u8 {
    match e {
        E::MissingTuple(0) => 0
        _ => 1
    }
}

fn invalid_record_pattern(e: E) -> u8 {
    match e {
        E::MissingRecord { x } => 0
        _ => 1
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let mut manager = initialize_test_analysis_pass();
    let diags = manager.run_on_module(&db, top_mod);
    let rendered = format_diagnostics(&db, &diags);

    assert!(rendered.contains("`Missing` is not found"), "{rendered}");
    assert!(
        rendered.contains("`MissingTuple` is not found"),
        "{rendered}"
    );
    assert!(
        rendered.contains("`MissingRecord` is not found"),
        "{rendered}"
    );
}
