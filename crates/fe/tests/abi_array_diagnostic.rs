use std::{fs, process::Command};

#[test]
fn fixed_array_fields_report_arrays_instead_of_tuples() {
    let dir = tempfile::tempdir().unwrap();
    for (attribute, ty, expected) in [
        ("event", "[u256; 2]", "fixed-size arrays are not supported"),
        ("error", "[u256; 2]", "fixed-size arrays are not supported"),
        ("event", "(u256, u256)", "tuples are not supported"),
        ("error", "(u256, u256)", "tuples are not supported"),
    ] {
        let path = dir.path().join("field.fe");
        fs::write(
            &path,
            format!("#[{attribute}]\nstruct Posted {{ words: {ty} }}\n"),
        )
        .unwrap();
        let output = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["check", "--standalone", "--color", "never"])
            .arg(&path)
            .output()
            .unwrap();
        let diagnostics = String::from_utf8_lossy(&output.stderr);
        assert!(!output.status.success());
        assert!(diagnostics.contains(expected), "{diagnostics}");
        if ty.starts_with('[') {
            assert!(diagnostics.contains("std::abi::DynArray"), "{diagnostics}");
            assert!(
                !diagnostics.contains("tuples are not supported"),
                "{diagnostics}"
            );
        }
    }
}
