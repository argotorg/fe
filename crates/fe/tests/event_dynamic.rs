use std::path::PathBuf;

use contract_harness::{ExecutionOptions, RuntimeInstance};
use fe::bench_support::compile_fe_sonatina_bytecode;

#[test]
fn add_liquidity_log_matches_solidity_vector() {
    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/fe_test/event_dyn_array_u256.fe");
    let source = std::fs::read_to_string(fixture).expect("read dynamic event fixture");
    let bytecode = compile_fe_sonatina_bytecode(&source, "event_dyn_array_u256", "Probe")
        .expect("compile dynamic event fixture");
    let mut instance =
        RuntimeInstance::deploy(&hex::encode(bytecode.deploy)).expect("deploy Probe");

    let outcome = instance
        .call_raw_with_logs(&[0x26, 0x12, 0x1f, 0xf0], ExecutionOptions::default())
        .expect("call f()");
    assert_eq!(outcome.raw_logs.len(), 1);

    let log = &outcome.raw_logs[0];
    let topics = log.data.topics();
    assert_eq!(topics.len(), 2);
    assert_eq!(
        topics[0].as_slice(),
        hex::decode("df21883620cd435343a655a765cce604ee5e962b399aed0ebbbd63a9c7e570c4").unwrap(),
    );
    assert_eq!(topics[1].as_slice(), &[0u8; 32]);

    let expected_data = hex::decode(concat!(
        "0000000000000000000000000000000000000000000000000000000000000040",
        "0000000000000000000000000000000000000000000000000000000000000003",
        "0000000000000000000000000000000000000000000000000000000000000001",
        "0000000000000000000000000000000000000000000000000000000000000007",
    ))
    .unwrap();
    assert_eq!(log.data.data.as_ref(), expected_data);
}
