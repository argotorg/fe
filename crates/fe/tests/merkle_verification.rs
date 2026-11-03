//! Merkle verification regression from banteg/evm-compiler-bench, with
//! independent ABI/Keccak expectations. The snapshot records code size and EVM
//! instruction gas, excluding transaction intrinsic costs and the calldata
//! floor. No Solidity/Foundry needed.

use std::fmt::Write;

use contract_harness::{ExecutionOptions, HarnessError, RuntimeInstance};
use ethers_core::{
    abi::{AbiParser, Token, encode},
    types::U256,
    utils::keccak256,
};
use fe::bench_support::compile_fe_sonatina_bytecode;
use test_utils::snap_test;

fn calldata(signature: &str, args: &[Token]) -> Vec<u8> {
    AbiParser::default()
        .parse_function(signature)
        .unwrap()
        .encode_input(args)
        .unwrap()
}

fn hash_pair(a: [u8; 32], b: [u8; 32]) -> [u8; 32] {
    let (left, right) = if a < b { (a, b) } else { (b, a) };
    keccak256([left, right].concat())
}

#[test]
fn merkle_verification_gas_and_correctness() {
    let source = include_str!("evm_compiler_bench/merkle_verifier.fe");
    let bytecode = compile_fe_sonatina_bytecode(source, "MerkleVerifier", "MerkleVerifier")
        .expect("compile Merkle benchmark");
    let mut report = format!(
        "runtime bytes: {}\nverify gas by proof length:\n",
        bytecode.runtime.len()
    );
    let mut runtime = RuntimeInstance::deploy(&hex::encode(bytecode.deploy)).unwrap();
    let siblings: Vec<_> = (0u32..64).map(|i| keccak256(i.to_be_bytes())).collect();
    let leaf = keccak256("leaf");
    for length in [0, 1, 2, 8, 16, 32, 64] {
        let proof = &siblings[..length];
        let root = proof.iter().copied().fold(leaf, hash_pair);
        let proof_token = Token::Array(
            proof
                .iter()
                .map(|v| Token::FixedBytes(v.to_vec()))
                .collect(),
        );
        for valid in [true, false] {
            let mut expected_root = root;
            if !valid {
                expected_root[0] ^= 1;
            }
            let input = calldata(
                "verify(bytes32[],bytes32,bytes32)",
                &[
                    proof_token.clone(),
                    Token::FixedBytes(expected_root.to_vec()),
                    Token::FixedBytes(leaf.to_vec()),
                ],
            );
            if valid {
                // Includes decoding/copying the proof.
                let gas = runtime
                    .call_raw_gas_profile(&input, ExecutionOptions::default())
                    .total_step_gas;
                writeln!(report, "  {length}: {gas}").unwrap();
            }
            let result = runtime
                .call_raw(&input, ExecutionOptions::default())
                .unwrap();
            assert_eq!(result.return_data, encode(&[Token::Bool(valid)]));
        }
    }
    for (a, b) in [(leaf, siblings[0]), (siblings[0], leaf), (leaf, leaf)] {
        let input = calldata(
            "hashPair(bytes32,bytes32)",
            &[Token::FixedBytes(a.to_vec()), Token::FixedBytes(b.to_vec())],
        );
        let result = runtime
            .call_raw(&input, ExecutionOptions::default())
            .unwrap();
        assert_eq!(result.return_data, hash_pair(a, b));
    }

    let input = calldata(
        "verify(bytes32[],bytes32,bytes32)",
        &[
            Token::Array(vec![Token::FixedBytes(siblings[0].to_vec())]),
            Token::FixedBytes(hash_pair(leaf, siblings[0]).to_vec()),
            Token::FixedBytes(leaf.to_vec()),
        ],
    );
    // Every truncation of a one-element proof is incomplete. In particular,
    // the final partial element must be rejected before get can read it.
    for end in 0..input.len() {
        assert!(
            matches!(
                runtime.call_raw(&input[..end], ExecutionOptions::default()),
                Err(HarnessError::Revert(_))
            ),
            "accepted truncation at {end}"
        );
    }
    for (start, word) in [
        (4, U256::MAX),
        (4, U256::from(0x1000)),
        (100, U256::MAX),
        (100, U256::one() << 251),
        (100, (U256::one() << 251) - U256::one()),
    ] {
        let mut malformed = input.clone();
        word.to_big_endian(&mut malformed[start..start + 32]);
        assert!(
            matches!(
                runtime.call_raw(&malformed, ExecutionOptions::default()),
                Err(HarnessError::Revert(_))
            ),
            "accepted offset/count {word}"
        );
    }
    snap_test!(
        report,
        concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/evm_compiler_bench/merkle_verifier"
        )
    );
}
