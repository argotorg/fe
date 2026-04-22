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
//!
//! The Fe contract and the Solidity contract have *different* ABIs: Fe splits
//! the dynamic `bytes` inputs into u256 chunks, Sol uses real `bytes`. The
//! test encodes both calldata formats from the same logical test input.

use std::path::PathBuf;

use contract_harness::{ExecutionOptions, RuntimeInstance, U256};
use ethers_core::abi::{AbiParser, Token};
use ethers_core::utils::keccak256;
use sha2::{Digest, Sha256};

/// Use a plain u128 for ETH amounts; 32 ether easily fits.
type Wei = u128;

// ---------------------------------------------------------------------------
// Paths & sources
// ---------------------------------------------------------------------------

fn fixture_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/differential_deposit")
}

fn resolve_solc_path() -> Option<String> {
    std::env::var("FE_SOLC_PATH").ok().or_else(|| {
        let fallback = std::path::PathBuf::from(std::env::var("HOME").ok()?)
            .join("Downloads/solc");
        fallback.is_file().then(|| fallback.to_string_lossy().into_owned())
    })
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
/// Both contracts share the selector now that Fe derives it from the same
/// signature string via `sol(...)`.
fn deposit_calldata(
    pubkey: &[u8; 48],
    withdrawal_credentials: &[u8; 32],
    signature: &[u8; 96],
    deposit_data_root: &[u8; 32],
) -> Vec<u8> {
    let function = AbiParser::default()
        .parse_function("deposit(bytes,bytes,bytes,bytes32)")
        .expect("parse deposit signature");
    let tokens = vec![
        Token::Bytes(pubkey.to_vec()),
        Token::Bytes(withdrawal_credentials.to_vec()),
        Token::Bytes(signature.to_vec()),
        Token::FixedBytes(deposit_data_root.to_vec()),
    ];
    function.encode_input(&tokens).expect("encode deposit")
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
    let bytes = fe::bench::compile_fe_sonatina(&fe_source, "DepositContract", "DepositContract")
        .expect("fe -> sonatina compile");
    hex::encode(&bytes)
}

fn compile_sol_deposit_runtime(solc_path: Option<&str>) -> String {
    let sol_source = std::fs::read_to_string(fixture_dir().join("OfficialDepositContract.sol"))
        .expect("read sol source");
    let bc = solc_runner::compile_solidity("DepositContract", &sol_source, true, solc_path)
        .expect("solc compile deposit");
    bc.bytecode
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
fn differential_deposit() {
    let solc_path = resolve_solc_path();
    if solc_path.is_none() {
        eprintln!("skipping: no solc found (set FE_SOLC_PATH or install ~/Downloads/solc)");
        return;
    }
    let solc_path_str = solc_path.as_deref();

    let fe_bytecode = compile_fe_deposit_runtime();
    let sol_bytecode = compile_sol_deposit_runtime(solc_path_str);

    let mut fe = RuntimeInstance::deploy(&fe_bytecode).expect("deploy fe");
    let mut sol = RuntimeInstance::deploy(&sol_bytecode).expect("deploy sol");

    // Fund the default caller (Address::ZERO) so it can send ETH along with
    // each deposit call. 100 ETH is enough for many deposits up to 32 ETH.
    let caller = contract_harness::Address::ZERO;
    let funding = U256::from(100u64) * U256::from(1_000_000_000_000_000_000u128);
    fe.fund_account(caller, funding);
    sol.fund_account(caller, funding);

    // Both contracts expose the same Solidity-derived ABI, so one selector
    // per method drives both sides.
    let get_root_selector: Vec<u8> = keccak256(b"get_deposit_root()")[..4].to_vec();
    let get_count_selector: Vec<u8> = keccak256(b"get_deposit_count()")[..4].to_vec();

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

    let assert_count_matches =
        |tag: &str, fe: &mut RuntimeInstance, sol: &mut RuntimeInstance| {
            let fe_r = fe
                .call_raw(&get_count_selector, ExecutionOptions::default())
                .expect("fe count");
            let sol_r = sol
                .call_raw(&get_count_selector, ExecutionOptions::default())
                .expect("sol count");
            assert_eq!(
                &fe_r.return_data[..],
                &sol_r.return_data[..],
                "{tag}: get_deposit_count return mismatch"
            );
        };
    assert_count_matches("empty", &mut fe, &mut sol);

    // Table for reporting.
    let mut gas_rows: Vec<(u64, u64)> = Vec::new();

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

        let mut opts = ExecutionOptions::default();
        opts.value = U256::from(v.amount_wei);
        opts.gas_limit = 3_000_000;

        let fe_res = fe
            .call_raw_with_logs(&cd, opts)
            .unwrap_or_else(|e| panic!("fe deposit #{i}: {e:?}"));
        let sol_res = sol
            .call_raw_with_logs(&cd, opts)
            .unwrap_or_else(|e| panic!("sol deposit #{i}: {e:?}"));

        // Compare emitted logs byte-for-byte (topics + data; the emitter
        // address differs between the two deployments and is not semantic).
        assert_eq!(
            fe_res.raw_logs.len(),
            sol_res.raw_logs.len(),
            "deposit #{i}: emitted log count differs",
        );
        for (idx, (fl, sl)) in fe_res.raw_logs.iter().zip(&sol_res.raw_logs).enumerate() {
            assert_eq!(
                fl.data.topics(),
                sl.data.topics(),
                "deposit #{i}, log {idx}: topics mismatch",
            );
            assert_eq!(
                fl.data.data, sl.data.data,
                "deposit #{i}, log {idx}: data mismatch",
            );
        }

        gas_rows.push((fe_res.result.gas_used, sol_res.result.gas_used));

        // After each deposit, roots must match.
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
        assert_count_matches(&format!("after deposit #{i}"), &mut fe, &mut sol);
    }

    // Report.
    println!();
    println!("Differential deposit — correctness OK across {} deposits.", gas_rows.len());
    println!("{:<8} {:>12} {:>12} {:>10}", "call", "fe gas", "sol gas", "delta");
    println!("{}", "-".repeat(46));
    for (i, (fe, sol)) in gas_rows.iter().enumerate() {
        let delta = *fe as i64 - *sol as i64;
        let pct = (delta as f64 / *sol as f64) * 100.0;
        println!("deposit#{i:<3} {fe:>12} {sol:>12} {delta:>+6} ({pct:+.1}%)");
    }
}
