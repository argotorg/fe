use std::path::Path;

use codespan_reporting::term::{
    self,
    termcolor::{BufferWriter, ColorChoice},
};
use dir_test::{Fixture, dir_test};
use driver::diagnostics::{CsDbWrapper, ToCsDiag};
use fe_hir::test_db::{HirAnalysisTestDb, initialize_analysis_pass};
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
    let mut manager = initialize_analysis_pass();
    let diags = manager.run_on_module(&db, top_mod);

    if !diags.is_empty() {
        // Format diagnostics using codespan with arrow indicators
        let writer = BufferWriter::stderr(ColorChoice::Never);
        let mut buffer = writer.buffer();
        let config = term::Config::default();

        let mut complete_diags: Vec<_> = diags.iter().map(|d| d.to_complete(&db)).collect();
        complete_diags.sort_by(|lhs, rhs| match lhs.error_code.cmp(&rhs.error_code) {
            std::cmp::Ordering::Equal => lhs.primary_span().cmp(&rhs.primary_span()),
            ord => ord,
        });

        let mut diagnostic_output = format!(
            "Exhaustive test file {} has {} diagnostic(s):\n\n",
            file_name,
            diags.len()
        );

        for diag in complete_diags {
            let cs_diag = &diag.to_cs(&db);
            term::emit(&mut buffer, &config, &CsDbWrapper(&db), cs_diag).unwrap();
        }

        diagnostic_output.push_str(std::str::from_utf8(buffer.as_slice()).unwrap());
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

    let mut manager = initialize_analysis_pass();

    let diags = manager.run_on_module(&db, top_mod);

    // Verify we have at least one diagnosis
    assert!(
        !diags.is_empty(),
        "Expected pattern matching errors but found none in file: {file_name}"
    );

    // Format diagnostics using codespan with arrow indicators
    let writer = BufferWriter::stderr(ColorChoice::Never);
    let mut buffer = writer.buffer();
    let config = term::Config::default();

    let mut complete_diags: Vec<_> = diags.iter().map(|d| d.to_complete(&db)).collect();
    complete_diags.sort_by(|lhs, rhs| match lhs.error_code.cmp(&rhs.error_code) {
        std::cmp::Ordering::Equal => lhs.primary_span().cmp(&rhs.primary_span()),
        ord => ord,
    });

    let mut diagnostic_output = format!(
        "Non-exhaustive test file {} has {} diagnostic(s):\n\n",
        file_name,
        diags.len()
    );

    for diag in complete_diags {
        let cs_diag = &diag.to_cs(&db);
        term::emit(&mut buffer, &config, &CsDbWrapper(&db), cs_diag).unwrap();
    }

    diagnostic_output.push_str(std::str::from_utf8(buffer.as_slice()).unwrap());
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

    let mut manager = initialize_analysis_pass();

    let diags = manager.run_on_module(&db, top_mod);

    // Verify we have at least one diagnosis
    assert!(
        !diags.is_empty(),
        "Expected unreachable pattern errors but found none in file: {file_name}"
    );

    // Format diagnostics using codespan with arrow indicators
    let writer = BufferWriter::stderr(ColorChoice::Never);
    let mut buffer = writer.buffer();
    let config = term::Config::default();

    let mut complete_diags: Vec<_> = diags.iter().map(|d| d.to_complete(&db)).collect();
    complete_diags.sort_by(|lhs, rhs| match lhs.error_code.cmp(&rhs.error_code) {
        std::cmp::Ordering::Equal => lhs.primary_span().cmp(&rhs.primary_span()),
        ord => ord,
    });

    let mut diagnostic_output = format!(
        "Unreachable test file {} has {} diagnostic(s):\n\n",
        file_name,
        diags.len()
    );

    for diag in complete_diags {
        let cs_diag = &diag.to_cs(&db);
        term::emit(&mut buffer, &config, &CsDbWrapper(&db), cs_diag).unwrap();
    }

    diagnostic_output.push_str(std::str::from_utf8(buffer.as_slice()).unwrap());
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
    let mut manager = initialize_analysis_pass();
    let diags = manager.run_on_module(&db, top_mod);

    if !diags.is_empty() {
        // Format diagnostics using codespan with arrow indicators
        let writer = BufferWriter::stderr(ColorChoice::Never);
        let mut buffer = writer.buffer();
        let config = term::Config::default();

        let mut complete_diags: Vec<_> = diags.iter().map(|d| d.to_complete(&db)).collect();
        complete_diags.sort_by(|lhs, rhs| match lhs.error_code.cmp(&rhs.error_code) {
            std::cmp::Ordering::Equal => lhs.primary_span().cmp(&rhs.primary_span()),
            ord => ord,
        });

        let mut diagnostic_output = format!(
            "Misc test file {} has {} diagnostic(s):\n\n",
            file_name,
            diags.len()
        );

        for diag in complete_diags {
            let cs_diag = &diag.to_cs(&db);
            term::emit(&mut buffer, &config, &CsDbWrapper(&db), cs_diag).unwrap();
        }

        diagnostic_output.push_str(std::str::from_utf8(buffer.as_slice()).unwrap());
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
    let mut manager = initialize_analysis_pass();
    let diags = manager.run_on_module(&db, top_mod);

    if !diags.is_empty() {
        // Format diagnostics using codespan with arrow indicators
        let writer = BufferWriter::stderr(ColorChoice::Never);
        let mut buffer = writer.buffer();
        let config = term::Config::default();

        let mut complete_diags: Vec<_> = diags.iter().map(|d| d.to_complete(&db)).collect();
        complete_diags.sort_by(|lhs, rhs| match lhs.error_code.cmp(&rhs.error_code) {
            std::cmp::Ordering::Equal => lhs.primary_span().cmp(&rhs.primary_span()),
            ord => ord,
        });

        let mut diagnostic_output = format!(
            "Stress test file {} has {} diagnostic(s):\n\n",
            file_name,
            diags.len()
        );

        for diag in complete_diags {
            let cs_diag = &diag.to_cs(&db);
            term::emit(&mut buffer, &config, &CsDbWrapper(&db), cs_diag).unwrap();
        }

        diagnostic_output.push_str(std::str::from_utf8(buffer.as_slice()).unwrap());
        snap_test!(diagnostic_output, fixture.path());
    }
}
