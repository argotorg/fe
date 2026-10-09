use std::{path::Path, process::Command};

#[test]
fn core_byte_writer_contract() {
    let core = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../ingots/core");
    for level in ["1", "2"] {
        let output = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args([
                "test",
                "-O",
                level,
                "--jobs",
                "1",
                "--grouped",
                "--filter",
                "packed_",
            ])
            .arg(&core)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "O{level}: {}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(String::from_utf8_lossy(&output.stdout).contains("7 passed"));
    }
}
