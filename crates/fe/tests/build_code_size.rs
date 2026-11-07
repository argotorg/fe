use std::{fs, process::Command};

#[test]
fn build_warns_about_oversized_code_and_still_writes_artifacts() {
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("size.fe");
    let out = dir.path().join("out");
    for length in [16, 1024, 2048] {
        let program = format!(
            r#"
const fn table() -> [u8; {length}] {{
    var t: [u8; {length}] = [0; {length}]
    var i: usize = 0
    var s: u256 = 12345
    while i < {length} {{
        s = (s * 1664525 + 1013904223) % 4294967296
        t[i] = ((s >> 16) % 256).downcast_truncate()
        i += 1
    }}
    t
}}
const TABLE: [u8; {length}] = table()
msg M {{
    #[selector = sol("get(uint256)")]
    Get {{ i: usize }} -> u256,
}}
pub contract Big {{
    recv M {{ Get {{ i }} -> u256 {{ TABLE[i] as u256 }} }}
}}
"#
        );
        fs::write(&source, program).unwrap();
        let output = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["build", "--out-dir"])
            .arg(&out)
            .arg(&source)
            .output()
            .unwrap();
        let diagnostics = String::from_utf8_lossy(&output.stderr);
        assert!(output.status.success(), "{diagnostics}");
        let runtime_size = fs::read_to_string(out.join("Big.runtime.bin"))
            .unwrap()
            .trim()
            .len()
            / 2;
        let init_size = fs::read_to_string(out.join("Big.bin"))
            .unwrap()
            .trim()
            .len()
            / 2;
        assert_eq!(
            diagnostics.contains("EIP-170"),
            runtime_size > 24_576,
            "{diagnostics}"
        );
        assert_eq!(
            diagnostics.contains("EIP-3860"),
            init_size > 49_152,
            "{diagnostics}"
        );
        if runtime_size > 24_576 {
            assert!(
                diagnostics.contains(&format!("`Big` has {runtime_size} bytes of runtime code")),
                "{diagnostics}"
            );
        }
    }
}
