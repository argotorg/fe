use ethers_core::{
    abi::{ParamType, Token, decode, encode},
    utils::keccak256,
};
use fe_contract_harness::{
    ExecutionOptions, FeContractHarness, Log, RuntimeInstance, encode_function_call,
};

const PROBE_SOURCE: &str = r#"
use std::abi::{sol, Bytes}
use std::evm::effects::Log

msg ProbeMsg {
    #[selector = sol("emit(bytes)")]
    Emit { data: Bytes },
    #[selector = sol("emitPair(bytes,bytes)")]
    EmitPair { first: Bytes, second: Bytes },
    #[selector = sol("emitStatic(uint256)")]
    EmitStatic { value: u256 },
    #[selector = sol("emitIndexed(uint256,bytes)")]
    EmitIndexed { id: u256, data: Bytes },
}

#[event]
struct SingleDynamic {
    data: Bytes,
}

#[event]
struct PairDynamic {
    first: Bytes,
    second: Bytes,
}

#[event]
struct SingleStatic {
    value: u256,
}

#[event]
struct IndexedDynamic {
    #[indexed]
    id: u256,
    data: Bytes,
}

pub contract EventProbe uses (log: mut Log) {
    recv ProbeMsg {
        Emit { data } uses (mut log) {
            log.emit(SingleDynamic { data })
        }

        EmitPair { first, second } uses (mut log) {
            log.emit(PairDynamic { first, second })
        }

        EmitStatic { value } uses (mut log) {
            log.emit(SingleStatic { value })
        }

        EmitIndexed { id, data } uses (mut log) {
            log.emit(IndexedDynamic { id, data })
        }
    }
}
"#;

fn deploy_probe() -> RuntimeInstance {
    FeContractHarness::compile("EventProbe", PROBE_SOURCE)
        .expect("event probe should compile")
        .deploy_with_init()
        .expect("event probe should deploy")
}

fn emit_event(instance: &mut RuntimeInstance, signature: &str, args: &[Token]) -> Log {
    let calldata = encode_function_call(signature, args).expect("calldata should encode");
    let outcome = instance
        .call_raw_with_logs(&calldata, ExecutionOptions::default())
        .expect("event should emit");
    let [log]: [Log; 1] = outcome
        .raw_logs
        .try_into()
        .expect("call should emit exactly one event");
    log
}

fn assert_event_data(log: &Log, args: &[Token], types: &[ParamType], expected_len: usize) {
    let data = log.data.data.as_ref();
    assert_eq!(data.len(), expected_len, "{args:?}");
    assert_eq!(data, encode(args), "{args:?}");
    assert_eq!(
        decode(types, data).expect("event data should decode as its argument sequence"),
        args,
        "{args:?}",
    );
}

#[test]
fn single_dynamic_event_data_includes_outer_head() {
    let mut instance = deploy_probe();

    // Even a single non-indexed field is an ABI argument sequence. Its dynamic
    // tail needs an outer offset word, including when the bytes are empty.
    for (length, expected_len) in [(0, 64), (1, 96), (31, 96), (32, 96), (33, 128), (65, 160)] {
        let args = [Token::Bytes(vec![0xaa; length])];
        let log = emit_event(&mut instance, "emit(bytes)", &args);
        assert_eq!(log.data.topics().len(), 1);
        assert_event_data(&log, &args, &[ParamType::Bytes], expected_len);
    }
}

#[test]
fn two_dynamic_event_fields_are_canonical() {
    let args = [Token::Bytes(vec![0x01]), Token::Bytes(vec![0x02, 0x03])];
    let log = emit_event(&mut deploy_probe(), "emitPair(bytes,bytes)", &args);
    assert_eq!(log.data.topics().len(), 1);
    assert_event_data(&log, &args, &[ParamType::Bytes, ParamType::Bytes], 192);
}

#[test]
fn single_static_event_data_is_canonical() {
    let args = [Token::Uint(42.into())];
    let log = emit_event(&mut deploy_probe(), "emitStatic(uint256)", &args);
    assert_eq!(log.data.topics().len(), 1);
    assert_event_data(&log, &args, &[ParamType::Uint(256)], 32);
}

#[test]
fn indexed_field_preserves_single_dynamic_data_head() {
    let id = Token::Uint(42.into());
    let data = Token::Bytes(vec![0xaa]);
    let log = emit_event(
        &mut deploy_probe(),
        "emitIndexed(uint256,bytes)",
        &[id.clone(), data.clone()],
    );

    let topics = log.data.topics();
    assert_eq!(topics.len(), 2);
    assert_eq!(
        topics[0].as_slice(),
        keccak256("IndexedDynamic(uint256,bytes)"),
    );
    assert_eq!(topics[1].as_slice(), encode(&[id]));
    assert_event_data(&log, &[data], &[ParamType::Bytes], 96);
}
