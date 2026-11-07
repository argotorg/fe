use std::{fs, process::Command};

#[test]
fn black_box_arrays_are_rejected_during_check_at_the_call() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("black_box.fe");
    for (value, valid) in [
        ("[1 as i8, 2 as i8, 3 as i8, 4 as i8]", false),
        ("1 as u256", true),
        ("true", true),
    ] {
        fs::write(
            &path,
            format!("use core::hint::black_box\nfn f() {{ let value = black_box({value}) }}\n"),
        )
        .unwrap();
        let output = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["check", "--standalone", "--color", "never"])
            .arg(&path)
            .output()
            .unwrap();
        let diagnostics = String::from_utf8_lossy(&output.stderr);
        assert_eq!(output.status.success(), valid, "{diagnostics}");
        if !valid {
            assert!(
                diagnostics.contains("black_box` requires a scalar argument"),
                "{diagnostics}"
            );
            assert!(diagnostics.contains("[i8; 4]"), "{diagnostics}");
            assert!(diagnostics.contains("black_box.fe:2:"), "{diagnostics}");
        }
    }
}
