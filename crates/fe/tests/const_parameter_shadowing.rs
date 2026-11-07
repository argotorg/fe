use std::{fs, process::Command};

#[test]
fn parameter_shadowing_in_const_types_names_the_parameter() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("shadow.fe");
    for (param, valid) in [("half", false), ("values", true)] {
        fs::write(&path, format!("const fn half(_ n: usize) -> usize {{ n / 2 }}\npub fn first<const N: usize>(_ {param}: [u256; half(N)]) -> u256 {{ {param}[0] }}\n")).unwrap();
        let output = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["check", "--standalone", "--color", "never"])
            .arg(&path)
            .output()
            .unwrap();
        let diagnostics = String::from_utf8_lossy(&output.stderr);
        assert_eq!(output.status.success(), valid, "{diagnostics}");
        if !valid {
            assert!(
                diagnostics.contains("`half` resolves to a function parameter"),
                "{diagnostics}"
            );
            assert!(
                diagnostics.contains("rename the parameter"),
                "{diagnostics}"
            );
            assert!(diagnostics.contains("shadow.fe:2:"), "{diagnostics}");
        }
    }
}
