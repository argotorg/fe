//! A contract embedded by `create2<Child>` must be deployed with exactly the
//! init code `fe build` writes to `Child.bin`, so that off-chain CREATE2
//! address derivation from the artifact matches the on-chain deployment.

use std::{fs, path::Path, process::Command};

use contract_harness::{Address, ExecutionOptions, RuntimeInstance, U256};
use tempfile::tempdir;
use tiny_keccak::{Hasher, Keccak};

fn keccak256(bytes: &[u8]) -> [u8; 32] {
    let mut hasher = Keccak::v256();
    hasher.update(bytes);
    let mut out = [0; 32];
    hasher.finalize(&mut out);
    out
}

fn read_hex(path: &Path) -> Vec<u8> {
    let text =
        fs::read_to_string(path).unwrap_or_else(|err| panic!("read {}: {err}", path.display()));
    hex::decode(text.trim()).unwrap_or_else(|err| panic!("decode {}: {err}", path.display()))
}

fn word(value: u64) -> [u8; 32] {
    U256::from(value).to_be_bytes()
}

fn calldata(selector: [u8; 4], args: &[[u8; 32]]) -> Vec<u8> {
    let mut data = selector.to_vec();
    for arg in args {
        data.extend_from_slice(arg);
    }
    data
}

fn contains(haystack: &[u8], needle: &[u8]) -> bool {
    haystack
        .windows(needle.len())
        .any(|window| window == needle)
}

#[test]
fn embedded_child_matches_standalone_artifact() {
    let fixture =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/embedded_contract_bytecode");
    for opt_level in ["0", "1"] {
        let out_dir = tempdir().expect("tempdir");
        let output = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["build", "-O", opt_level, "--out-dir"])
            .arg(out_dir.path())
            .arg(&fixture)
            .env("NO_COLOR", "1")
            .output()
            .expect("run fe build");
        assert!(
            output.status.success(),
            "fe build -O{opt_level} failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );

        let child_init = read_hex(&out_dir.path().join("Child.bin"));
        let factory_init = read_hex(&out_dir.path().join("Factory.bin"));
        let trace_path = out_dir.path().join("trace.jsonl");
        let trace = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["dev", "trace", "emit", "-O", opt_level, "--out"])
            .arg(&trace_path)
            .arg(&fixture)
            .output()
            .expect("emit trace");
        assert!(
            trace.status.success(),
            "{}",
            String::from_utf8_lossy(&trace.stderr)
        );
        let bundle = trace_facts::JsonlTraceReader::new(std::io::BufReader::new(
            fs::File::open(&trace_path).unwrap(),
        ))
        .read_bundle()
        .unwrap();
        trace_facts::TraceValidator::validate(&bundle.facts).unwrap();
        for (name, bytes) in [("Child", &child_init), ("Factory", &factory_init)] {
            let expected_hash = format!("blake3:{}", blake3::hash(bytes).to_hex());
            assert!(
                bundle.facts.iter().any(|fact| matches!(fact,
                    trace_facts::TraceFact::CodeObject(object)
                        if object.kind == trace_facts::CodeObjectKind::EvmCreationBytecode
                        && object.code_object.owner_key().contains(&format!("contract:{name}"))
                        && object.code_hash.as_deref() == Some(expected_hash.as_str())
                )),
                "{name} trace differs from its build artifact at -O{opt_level}"
            );
        }

        // Inject the independently built child's hash into a test-only copy.
        // The test package also calls a shared helper, so compiling it as one
        // combined module would change the child's embedded code.
        let test_package = tempdir().unwrap();
        fs::create_dir(test_package.path().join("src")).unwrap();
        fs::copy(fixture.join("fe.toml"), test_package.path().join("fe.toml")).unwrap();
        for entry in fs::read_dir(fixture.join("src")).unwrap() {
            let entry = entry.unwrap();
            fs::copy(
                entry.path(),
                test_package.path().join("src").join(entry.file_name()),
            )
            .unwrap();
        }
        let lib_path = test_package.path().join("src/lib.fe");
        let mut lib = fs::read_to_string(&lib_path).unwrap();
        lib.push_str(&format!(r#"
use std::evm::{{Contract, Evm, RawOps}}
#[test]
fn embedded_child_matches_build() uses (evm: mut Evm) {{
    let mut buffer = core::ptr::MemBuffer::alloc(child::Child::init_code_len())
    evm.codecopy(dest: mut buffer, dest_offset: 0, src_offset: child::Child::init_code_offset(), len: child::Child::init_code_len())
    assert(std::evm::keccak256(buffer.span()) == 0x{})
    assert(shared::mix(7, 0) == 7)
}}
"#, hex::encode(keccak256(&child_init))));
        fs::write(&lib_path, lib).unwrap();
        let tests = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["test", "-O", opt_level])
            .arg(test_package.path())
            .output()
            .unwrap();
        assert!(
            tests.status.success(),
            "{}\n{}",
            String::from_utf8_lossy(&tests.stdout),
            String::from_utf8_lossy(&tests.stderr)
        );

        assert!(
            contains(&factory_init, &child_init),
            "-O{opt_level}: Factory does not embed Child.bin"
        );

        let mut factory =
            RuntimeInstance::deploy(&hex::encode(&factory_init)).expect("deploy Factory");
        let salt = 0x5eed;
        let make = factory
            .call_raw(
                &calldata([0x22, 0x22, 0x22, 0x22], &[word(salt), word(0)]),
                ExecutionOptions::default(),
            )
            .expect("Factory.make");
        let child = Address::from_slice(&make.return_data[12..32]);

        // CREATE2 address: keccak256(0xff ++ deployer ++ salt ++ keccak256(init code)).
        let mut preimage = vec![0xff];
        preimage.extend_from_slice(factory.address().as_slice());
        preimage.extend_from_slice(&word(salt));
        preimage.extend_from_slice(&keccak256(&child_init));
        let expected = Address::from_slice(&keccak256(&preimage)[12..]);
        assert_eq!(
            child, expected,
            "-O{opt_level}: deployed Child address does not match CREATE2 of Child.bin"
        );

        let get = factory
            .call_raw_at(
                child,
                &calldata([0x11, 0x11, 0x11, 0x11], &[word(7), word(0)]),
                ExecutionOptions::default(),
            )
            .expect("Child.get");
        let owner = U256::from_be_slice(factory.address().as_slice());
        assert_eq!(
            U256::from_be_slice(&get.return_data),
            owner + U256::from(7),
            "-O{opt_level}: deployed Child misbehaves"
        );
    }
}
