//! `std::evm::init_code_hash<C>(args)` must equal `keccak256(C.bin ++
//! abi.encode(args))` computed off-chain from the `fe build` artifacts, and
//! `create2<C>` must deploy at the address derived from that hash.

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

fn calldata(selector: u8, args: &[[u8; 32]]) -> Vec<u8> {
    let mut data = vec![selector; 4];
    for arg in args {
        data.extend_from_slice(arg);
    }
    data
}

fn create2_address(deployer: Address, salt: [u8; 32], init_code: &[u8]) -> Address {
    let mut preimage = vec![0xff];
    preimage.extend_from_slice(deployer.as_slice());
    preimage.extend_from_slice(&salt);
    preimage.extend_from_slice(&keccak256(init_code));
    Address::from_slice(&keccak256(&preimage)[12..])
}

#[test]
fn init_code_hash_matches_build_artifacts() {
    // Embedded child code equals the standalone artifact at every level.
    for opt in ["0", "1"] {
        check_init_code_hash_at(opt);
    }
}

fn check_init_code_hash_at(opt: &str) {
    let fixture =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/init_code_hash_artifact");
    let out_dir = tempdir().expect("tempdir");
    let output = Command::new(env!("CARGO_BIN_EXE_fe"))
        .args(["build", "-O", opt, "--out-dir"])
        .arg(out_dir.path())
        .arg(&fixture)
        .env("NO_COLOR", "1")
        .output()
        .expect("run fe build");
    assert!(
        output.status.success(),
        "fe build -O {opt} failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let child_init = read_hex(&out_dir.path().join("Child.bin"));
    let arg_child_init = read_hex(&out_dir.path().join("ArgChild.bin"));
    let mut factory =
        RuntimeInstance::deploy(&hex::encode(read_hex(&out_dir.path().join("Factory.bin"))))
            .expect("deploy Factory");
    let call = |factory: &mut RuntimeInstance, data: Vec<u8>| {
        factory
            .call_raw(&data, ExecutionOptions::default())
            .expect("Factory call")
            .return_data
    };

    // No constructor arguments: the hash of the bare creation code.
    let hash = call(&mut factory, calldata(0x22, &[]));
    assert_eq!(hash, keccak256(&child_init));

    // Static constructor arguments are ABI-encoded after the creation code.
    let owner = word(0xabc);
    let mut arg_child_code = arg_child_init.clone();
    arg_child_code.extend_from_slice(&word(42));
    arg_child_code.extend_from_slice(&owner);
    let hash = call(&mut factory, calldata(0x33, &[word(42), owner]));
    assert_eq!(hash, keccak256(&arg_child_code));

    // `create2` deploys exactly that init code.
    let made = call(&mut factory, calldata(0x55, &[word(42), owner, word(9)]));
    assert_eq!(
        Address::from_slice(&made[12..32]),
        create2_address(factory.address(), word(9), &arg_child_code)
    );
    // `mix(salt, 0) == salt`, but the factory still calls the shared helper.
    let made = call(&mut factory, calldata(0x44, &[word(0x5eed), word(0)]));
    let child = Address::from_slice(&made[12..32]);
    assert_eq!(
        child,
        create2_address(factory.address(), word(0x5eed), &child_init)
    );
    let get = factory
        .call_raw_at(
            child,
            &calldata(0x11, &[word(7), word(4)]),
            ExecutionOptions::default(),
        )
        .expect("Child.get");
    assert_eq!(get.return_data.len(), 32, "deployed Child misbehaves");
}
