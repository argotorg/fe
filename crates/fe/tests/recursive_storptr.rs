use std::{fs, path::Path, process::Command};

use contract_harness::{ExecutionOptions, RuntimeInstance, U256};
use tempfile::tempdir;

#[test]
fn minimal_recursive_storage_contract_checks_and_builds() {
    let source =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/recursive_storptr_minimal.fe");
    for command in ["check", "build"] {
        let out = tempdir().unwrap();
        let mut cmd = Command::new(env!("CARGO_BIN_EXE_fe"));
        cmd.args([command, "--standalone"]).arg(&source);
        if command == "build" {
            cmd.arg("--out-dir").arg(out.path());
        }
        let result = cmd.output().unwrap();
        assert!(
            result.status.success(),
            "fe {command}: {}",
            String::from_utf8_lossy(&result.stderr)
        );
        if command == "build" {
            assert!(
                !fs::read_to_string(out.path().join("Foo.bin"))
                    .unwrap()
                    .trim()
                    .is_empty()
            );
        }
    }
}

#[test]
fn recursive_storage_handles_persist_across_transactions() {
    let source =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fe_test/recursive_storptr.fe");
    for level in [None, Some("0"), Some("1")] {
        let out = tempdir().unwrap();
        let mut cmd = Command::new(env!("CARGO_BIN_EXE_fe"));
        cmd.args(["build", "--standalone", "--out-dir"])
            .arg(out.path())
            .arg(&source);
        if let Some(level) = level {
            cmd.args(["-O", level]);
        }
        let result = cmd.output().unwrap();
        assert!(
            result.status.success(),
            "build at {level:?}: {}",
            String::from_utf8_lossy(&result.stderr)
        );
        let init = fs::read_to_string(out.path().join("Tree.bin")).unwrap();
        let mut runtime = RuntimeInstance::deploy(init.trim()).expect("deploy recursive tree");
        let mut call = |selector: u32, arg: Option<u64>, expected: Option<u64>| {
            let mut calldata = selector.to_be_bytes().to_vec();
            if let Some(arg) = arg {
                calldata.extend_from_slice(&U256::from(arg).to_be_bytes::<32>());
            }
            let result = runtime
                .call_raw(&calldata, ExecutionOptions::default())
                .unwrap_or_else(|error| panic!("selector {selector} at {level:?}: {error:?}"));
            if let Some(expected) = expected {
                assert_eq!(
                    result.return_data,
                    U256::from(expected).to_be_bytes::<32>(),
                    "selector {selector} at {level:?}"
                );
            }
        };
        // Every call is a separate transaction on the same deployed instance.
        call(1, None, Some(50));
        call(2, Some(31), None);
        call(1, None, Some(51));
        call(3, None, None);
        call(4, None, Some(51));
        call(5, Some(35), None);
        call(1, None, Some(55));
        call(4, None, Some(55));
        call(6, None, Some(777));
    }
}
