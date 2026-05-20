//! Differential test: our Fe ETH 2.0 deposit contract vs the official
//! Solidity deposit contract (mainnet 0x00000000219ab540356cBB839Cbe05303d7705Fa).
//!
//! For each test vector:
//!   * Pre-compute the expected `deposit_data_root` in Rust so both contracts
//!     accept the deposit.
//!   * Call `deposit(...)` on both with matching value.
//!   * Call `get_deposit_root()` / `get_deposit_count()` on both and assert
//!     byte-for-byte equality.
//!   * Record gas per call.

use std::path::PathBuf;

use contract_harness::{ExecutionOptions, HarnessError, RuntimeInstance, U256};
use ethers_core::abi::{AbiParser, Token};
use ethers_core::utils::keccak256;
use fe::bench_support::{
    PRIMARY_SOL_IDX, SOL_VARIANTS, SolGasRow, compile_fe_sonatina, compile_solidity_pipeline,
    print_sol_gas_table, sol_variant_label,
};
use sha2::{Digest, Sha256};

/// Use a plain u128 for ETH amounts; 32 ether easily fits.
type Wei = u128;

// ---------------------------------------------------------------------------
// Paths & sources
// ---------------------------------------------------------------------------

fn fixture_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/differential_deposit")
}

fn find_executable_in_path(name: &str) -> Option<PathBuf> {
    let path = std::env::var_os("PATH")?;
    std::env::split_paths(&path)
        .map(|dir| dir.join(name))
        .find(|candidate| candidate.is_file())
}

fn resolve_solc_path() -> Option<String> {
    std::env::var_os("FE_SOLC_PATH")
        .map(PathBuf::from)
        .filter(|p| p.is_file())
        .or_else(|| find_executable_in_path("solc"))
        .map(|p| p.to_string_lossy().into_owned())
}

// ---------------------------------------------------------------------------
// Expected deposit-data root computed in Rust (no EVM involved)
// ---------------------------------------------------------------------------

/// Compute sha256(a || b) for two 32-byte inputs.
fn sha256_pair(a: &[u8; 32], b: &[u8; 32]) -> [u8; 32] {
    let mut h = Sha256::new();
    h.update(a);
    h.update(b);
    h.finalize().into()
}

/// SSZ-style deposit-data root, byte-for-byte matching the formula used by
/// both contracts internally.
fn deposit_data_root(
    pubkey: &[u8; 48],
    withdrawal_credentials: &[u8; 32],
    amount_gwei: u64,
    signature: &[u8; 96],
) -> [u8; 32] {
    // pubkey (48) padded to 64 → sha256
    let mut pad = [0u8; 64];
    pad[..48].copy_from_slice(pubkey);
    let pubkey_root: [u8; 32] = Sha256::digest(pad).into();

    // signature (96) → sha256( sha256(sig[0..64]) || sha256(sig[64..96] || 32 zero bytes) )
    let mut pad = [0u8; 64];
    pad[..32].copy_from_slice(&signature[64..96]);
    let sig_tail: [u8; 32] = Sha256::digest(pad).into();
    let sig_head: [u8; 32] = Sha256::digest(&signature[..64]).into();
    let sig_root = sha256_pair(&sig_head, &sig_tail);

    // amount (LE u64) || 24 zero bytes || signature_root
    let mut amount_pad = [0u8; 64];
    amount_pad[..8].copy_from_slice(&amount_gwei.to_le_bytes());
    amount_pad[32..64].copy_from_slice(&sig_root);
    let right: [u8; 32] = Sha256::digest(amount_pad).into();

    // pubkey_root || withdrawal_credentials → sha256
    let left = sha256_pair(&pubkey_root, withdrawal_credentials);

    sha256_pair(&left, &right)
}

// ---------------------------------------------------------------------------
// Calldata encoding
// ---------------------------------------------------------------------------

/// Standard Solidity ABI calldata for `deposit(bytes,bytes,bytes,bytes32)`.
/// Both contracts share the selector since Fe derives it from the same
/// signature string via `sol(...)`.
fn deposit_calldata(
    pubkey: &[u8; 48],
    withdrawal_credentials: &[u8; 32],
    signature: &[u8; 96],
    deposit_data_root: &[u8; 32],
) -> Vec<u8> {
    deposit_calldata_dyn(
        pubkey.to_vec(),
        withdrawal_credentials.to_vec(),
        signature.to_vec(),
        deposit_data_root,
    )
}

fn deposit_calldata_dyn(
    pubkey: Vec<u8>,
    withdrawal_credentials: Vec<u8>,
    signature: Vec<u8>,
    deposit_data_root: &[u8; 32],
) -> Vec<u8> {
    let function = AbiParser::default()
        .parse_function("deposit(bytes,bytes,bytes,bytes32)")
        .expect("parse deposit signature");
    let tokens = vec![
        Token::Bytes(pubkey),
        Token::Bytes(withdrawal_credentials),
        Token::Bytes(signature),
        Token::FixedBytes(deposit_data_root.to_vec()),
    ];
    function.encode_input(&tokens).expect("encode deposit")
}

fn supports_interface_calldata(interface_id: [u8; 4]) -> Vec<u8> {
    let function = AbiParser::default()
        .parse_function("supportsInterface(bytes4)")
        .expect("parse supportsInterface signature");
    function
        .encode_input(&[Token::FixedBytes(interface_id.to_vec())])
        .expect("encode supportsInterface")
}

fn expect_revert<T>(result: Result<T, HarnessError>, label: &str) -> Vec<u8> {
    match result {
        Err(HarnessError::Revert(data)) => data.0,
        Err(other) => panic!("{label}: expected revert, got {other:?}"),
        Ok(_) => panic!("{label}: expected revert, call succeeded"),
    }
}

fn assert_reverts_match(
    name: &str,
    fe: &mut RuntimeInstance,
    sol: &mut RuntimeInstance,
    calldata: &[u8],
    value: Wei,
) {
    let opts = ExecutionOptions {
        value: U256::from(value),
        gas_limit: 3_000_000,
        ..Default::default()
    };

    let fe_data = expect_revert(fe.call_raw(calldata, opts), &format!("fe {name}"));
    let sol_data = expect_revert(sol.call_raw(calldata, opts), &format!("sol {name}"));
    assert_eq!(fe_data, sol_data, "{name}: revert payload mismatch");
}

// ---------------------------------------------------------------------------
// Compile helpers
// ---------------------------------------------------------------------------

fn compile_fe_deposit_runtime() -> String {
    // Use the Sonatina backend. The Yul backend currently has a spill-slot bug
    // in `encode_root` that inflates dynamic-bytes returns by one 32-byte word
    // and leaks a raw memory pointer; Sonatina lowers the same Fe source
    // correctly. Once that Yul bug is fixed we can run the differential against
    // both backends (or switch back to Yul for parity with deployed contracts).
    //
    // We reuse the fe_test fixture as the single source of truth for the Fe
    // deposit contract. `compile_fe_sonatina` targets the named contract and
    // ignores the co-located `#[test]` functions, so the extra test scaffolding
    // in that file costs us nothing here.
    let fe_source_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test/deposit_contract.fe");
    let fe_source = std::fs::read_to_string(&fe_source_path).expect("read fe source");
    let bytes = compile_fe_sonatina(&fe_source, "DepositContract", "DepositContract")
        .expect("fe -> sonatina compile");
    hex::encode(&bytes)
}

fn compile_sol_deposit_runtimes(solc_path: Option<&str>) -> Vec<String> {
    let sol_source = std::fs::read_to_string(fixture_dir().join("OfficialDepositContract.sol"))
        .expect("read sol source");
    SOL_VARIANTS
        .iter()
        .map(|(pipeline, optimize)| {
            compile_solidity_pipeline(
                &sol_source,
                "DepositContract",
                *optimize,
                *pipeline,
                solc_path,
            )
            .unwrap_or_else(|e| {
                panic!(
                    "{} compile deposit: {e}",
                    sol_variant_label(*pipeline, *optimize)
                )
            })
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Test vectors
// ---------------------------------------------------------------------------

struct Vector {
    pubkey: [u8; 48],
    withdrawal_credentials: [u8; 32],
    signature: [u8; 96],
    amount_wei: Wei,
}

fn vectors() -> Vec<Vector> {
    fn mk_pk(seed: u8) -> [u8; 48] {
        let mut pk = [0u8; 48];
        for (i, b) in pk.iter_mut().enumerate() {
            *b = seed.wrapping_add(i as u8);
        }
        pk
    }
    fn mk_wc(seed: u8) -> [u8; 32] {
        let mut wc = [0u8; 32];
        wc[0] = 0x00; // ETH1 withdrawal prefix byte (BLS uses 0x00, ETH1 uses 0x01; arbitrary here)
        for (i, b) in wc.iter_mut().enumerate().skip(1) {
            *b = seed.wrapping_mul(i as u8 + 1);
        }
        wc
    }
    fn mk_sig(seed: u8) -> [u8; 96] {
        let mut sig = [0u8; 96];
        for (i, b) in sig.iter_mut().enumerate() {
            *b = seed.wrapping_add((i * 7) as u8);
        }
        sig
    }
    // 1 ether, 16 ether, 32 ether (min, middle, max of what the contract accepts).
    let one_eth: Wei = 1_000_000_000_000_000_000;
    vec![
        Vector {
            pubkey: mk_pk(0x01),
            withdrawal_credentials: mk_wc(0xaa),
            signature: mk_sig(0x11),
            amount_wei: one_eth,
        },
        Vector {
            pubkey: mk_pk(0x02),
            withdrawal_credentials: mk_wc(0xbb),
            signature: mk_sig(0x22),
            amount_wei: one_eth * 16,
        },
        Vector {
            pubkey: mk_pk(0x03),
            withdrawal_credentials: mk_wc(0xcc),
            signature: mk_sig(0x33),
            amount_wei: one_eth * 32,
        },
    ]
}

// ---------------------------------------------------------------------------
// Test
// ---------------------------------------------------------------------------

#[test]
#[allow(clippy::print_stdout, clippy::print_stderr)]
fn differential_deposit() {
    let solc_path = resolve_solc_path();
    if solc_path.is_none() {
        eprintln!("skipping: no solc found (set FE_SOLC_PATH or install solc on PATH)");
        return;
    }
    let solc_path_str = solc_path.as_deref();

    let fe_bytecode = compile_fe_deposit_runtime();
    let sol_bytecodes = compile_sol_deposit_runtimes(solc_path_str);

    let mut fe = RuntimeInstance::deploy(&fe_bytecode).expect("deploy fe");
    let mut sols: Vec<RuntimeInstance> = sol_bytecodes
        .iter()
        .enumerate()
        .map(|(i, bc)| {
            let (pipeline, optimize) = SOL_VARIANTS[i];
            RuntimeInstance::deploy(bc).unwrap_or_else(|e| {
                panic!("deploy {}: {e:?}", sol_variant_label(pipeline, optimize))
            })
        })
        .collect();

    // Fund the default caller (Address::ZERO) so it can send ETH along with
    // each deposit call, including the "deposit value too high" revert case.
    let caller = contract_harness::Address::ZERO;
    let funding = U256::from(u128::MAX / 2);
    fe.fund_account(caller, funding);
    for sol in &mut sols {
        sol.fund_account(caller, funding);
    }

    // Both contracts expose the same Solidity-derived ABI, so one selector
    // per method drives both sides.
    let get_root_selector: Vec<u8> = keccak256(b"get_deposit_root()")[..4].to_vec();
    let get_count_selector: Vec<u8> = keccak256(b"get_deposit_count()")[..4].to_vec();
    for (name, cd) in [
        (
            "erc165",
            supports_interface_calldata([0x01, 0xff, 0xc9, 0xa7]),
        ),
        (
            "deposit",
            supports_interface_calldata([0x85, 0x64, 0x09, 0x07]),
        ),
        (
            "unknown",
            supports_interface_calldata([0xff, 0xff, 0xff, 0xff]),
        ),
    ] {
        let fe_support = fe
            .call_raw(&cd, ExecutionOptions::default())
            .unwrap_or_else(|e| panic!("fe supportsInterface({name}): {e:?}"));
        let sol_support = sols[PRIMARY_SOL_IDX]
            .call_raw(&cd, ExecutionOptions::default())
            .unwrap_or_else(|e| panic!("sol supportsInterface({name}): {e:?}"));
        assert_eq!(
            &fe_support.return_data[..],
            &sol_support.return_data[..],
            "supportsInterface({name}) mismatch"
        );
    }

    let one_eth: Wei = 1_000_000_000_000_000_000;
    let sample_vectors = vectors();
    let sample = &sample_vectors[0];
    let valid_root = deposit_data_root(
        &sample.pubkey,
        &sample.withdrawal_credentials,
        (one_eth / 1_000_000_000u128) as u64,
        &sample.signature,
    );
    let valid_deposit_cd = deposit_calldata(
        &sample.pubkey,
        &sample.withdrawal_credentials,
        &sample.signature,
        &valid_root,
    );

    {
        let sol = &mut sols[PRIMARY_SOL_IDX];
        assert_reverts_match(
            "invalid pubkey length",
            &mut fe,
            sol,
            &deposit_calldata_dyn(
                sample.pubkey[..47].to_vec(),
                sample.withdrawal_credentials.to_vec(),
                sample.signature.to_vec(),
                &valid_root,
            ),
            one_eth,
        );
        assert_reverts_match(
            "invalid withdrawal_credentials length",
            &mut fe,
            sol,
            &deposit_calldata_dyn(
                sample.pubkey.to_vec(),
                sample.withdrawal_credentials[..31].to_vec(),
                sample.signature.to_vec(),
                &valid_root,
            ),
            one_eth,
        );
        assert_reverts_match(
            "invalid signature length",
            &mut fe,
            sol,
            &deposit_calldata_dyn(
                sample.pubkey.to_vec(),
                sample.withdrawal_credentials.to_vec(),
                sample.signature[..95].to_vec(),
                &valid_root,
            ),
            one_eth,
        );
        assert_reverts_match("deposit value too low", &mut fe, sol, &valid_deposit_cd, 0);
        assert_reverts_match(
            "deposit value not multiple of gwei",
            &mut fe,
            sol,
            &valid_deposit_cd,
            one_eth + 1,
        );
        assert_reverts_match(
            "deposit value too high",
            &mut fe,
            sol,
            &valid_deposit_cd,
            ((u64::MAX as u128) + 1) * 1_000_000_000,
        );
        assert_reverts_match(
            "mismatched deposit_data_root",
            &mut fe,
            sol,
            &deposit_calldata(
                &sample.pubkey,
                &sample.withdrawal_credentials,
                &sample.signature,
                &[0xff; 32],
            ),
            one_eth,
        );

        // Sanity: initial roots must match.
        let fe_root0 = fe
            .call_raw(&get_root_selector, ExecutionOptions::default())
            .expect("fe root 0");
        let sol_root0 = sol
            .call_raw(&get_root_selector, ExecutionOptions::default())
            .expect("sol root 0");
        assert_eq!(
            &fe_root0.return_data[..],
            &sol_root0.return_data[..],
            "initial deposit root mismatch (empty tree)"
        );

        let fe_count0 = fe
            .call_raw(&get_count_selector, ExecutionOptions::default())
            .expect("fe count");
        let sol_count0 = sol
            .call_raw(&get_count_selector, ExecutionOptions::default())
            .expect("sol count");
        assert_eq!(
            &fe_count0.return_data[..],
            &sol_count0.return_data[..],
            "empty: get_deposit_count return mismatch"
        );
    }

    // Table for reporting.
    let mut gas_rows: Vec<SolGasRow> = Vec::new();

    for (i, v) in vectors().into_iter().enumerate() {
        let amount_gwei: u64 = (v.amount_wei / 1_000_000_000u128) as u64;
        let expected_root = deposit_data_root(
            &v.pubkey,
            &v.withdrawal_credentials,
            amount_gwei,
            &v.signature,
        );

        let cd = deposit_calldata(
            &v.pubkey,
            &v.withdrawal_credentials,
            &v.signature,
            &expected_root,
        );

        let opts = ExecutionOptions {
            value: U256::from(v.amount_wei),
            gas_limit: 3_000_000,
            ..Default::default()
        };

        let fe_res = fe
            .call_raw_with_logs(&cd, opts)
            .unwrap_or_else(|e| panic!("fe deposit #{i}: {e:?}"));

        // Drive every Solidity variant in lock-step so each one accumulates the
        // same deposit history. Correctness is asserted against the primary
        // variant only; the others share its source so logs/state must agree.
        let mut sol_gas = [0u64; 4];
        for (idx, sol) in sols.iter_mut().enumerate() {
            let (pipeline, optimize) = SOL_VARIANTS[idx];
            let label = sol_variant_label(pipeline, optimize);
            let res = sol
                .call_raw_with_logs(&cd, opts)
                .unwrap_or_else(|e| panic!("{label} deposit #{i}: {e:?}"));
            sol_gas[idx] = res.result.gas_used;

            if idx == PRIMARY_SOL_IDX {
                assert_eq!(
                    fe_res.raw_logs.len(),
                    res.raw_logs.len(),
                    "deposit #{i}: emitted log count differs (fe vs {label})",
                );
                for (li, (fl, sl)) in fe_res.raw_logs.iter().zip(&res.raw_logs).enumerate() {
                    assert_eq!(
                        fl.data.topics(),
                        sl.data.topics(),
                        "deposit #{i}, log {li}: topics mismatch (fe vs {label})",
                    );
                    assert_eq!(
                        fl.data.data, sl.data.data,
                        "deposit #{i}, log {li}: data mismatch (fe vs {label})",
                    );
                }
            }
        }

        gas_rows.push(SolGasRow {
            label: format!("deposit#{i}"),
            fe: fe_res.result.gas_used,
            sol_variants: sol_gas,
        });

        // After each deposit, roots and counts must match between Fe and the
        // primary Solidity variant.
        let sol = &mut sols[PRIMARY_SOL_IDX];
        let fe_root = fe
            .call_raw(&get_root_selector, ExecutionOptions::default())
            .expect("fe root");
        let sol_root = sol
            .call_raw(&get_root_selector, ExecutionOptions::default())
            .expect("sol root");
        assert_eq!(
            &fe_root.return_data[..],
            &sol_root.return_data[..],
            "deposit #{i}: root mismatch after deposit"
        );
        let fe_count = fe
            .call_raw(&get_count_selector, ExecutionOptions::default())
            .expect("fe count");
        let sol_count = sol
            .call_raw(&get_count_selector, ExecutionOptions::default())
            .expect("sol count");
        assert_eq!(
            &fe_count.return_data[..],
            &sol_count.return_data[..],
            "after deposit #{i}: get_deposit_count return mismatch"
        );
    }

    println!();
    println!(
        "Differential deposit — correctness OK across {} deposits.",
        gas_rows.len()
    );
    print_sol_gas_table(&gas_rows, "call", 12);
}
