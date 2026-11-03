use std::fmt::Write;

use contract_harness::{ExecutionOptions, RuntimeInstance};
use ethers_core::abi::{AbiParser, Token, encode};
use fe::bench_support::compile_fe_sonatina_bytecode;
use test_utils::snap_test;

#[test]
fn packed_string_encoding_matches_abi_at_every_byte_boundary() {
    let source = r#"
msg Strings {
    #[selector = sol("empty(uint256)")]
    Empty { word: u256 } -> String<0>,
    #[selector = sol("short(uint256)")]
    Short { word: u256 } -> String<4>,
    #[selector = sol("full(uint256)")]
    Full { word: u256 } -> String<31>,
}

pub contract PackedStrings {
    recv Strings {
        Empty { word } -> String<0> { word as String<0> }
        Short { word } -> String<4> { word as String<4> }
        Full { word } -> String<31> { word as String<31> }
    }
}
"#;
    let bytecode = compile_fe_sonatina_bytecode(source, "PackedStrings", "PackedStrings")
        .expect("compile packed string contract");
    let mut runtime = RuntimeInstance::deploy(&hex::encode(bytecode.deploy)).expect("deploy");

    for (signature, capacity) in [
        ("empty(uint256)", 0),
        ("short(uint256)", 4),
        ("full(uint256)", 31),
    ] {
        let function = AbiParser::default().parse_function(signature).unwrap();
        // The high byte is outside every supported String capacity. It must
        // neither affect the encoded length nor turn an empty string nonempty.
        for dirty_high_byte in [0, 0xff] {
            for length in 0..=31 {
                for first_byte in [1, 0x80, 0xff] {
                    let mut word = [0u8; 32];
                    word[0] = dirty_high_byte;
                    if length != 0 {
                        word[32 - length] = first_byte;
                        // Include zero bytes inside and at the end of a string.
                        // Effective length strips leading zeros only.
                        for (index, byte) in word[33 - length..].iter_mut().enumerate() {
                            *byte = if index % 3 == 0 { 0 } else { 0x61 };
                        }
                    }
                    let visible = &word[32 - capacity..];
                    let start = visible
                        .iter()
                        .position(|byte| *byte != 0)
                        .unwrap_or(visible.len());
                    let expected = encode(&[Token::Bytes(visible[start..].to_vec())]);
                    let input = function.encode_input(&[Token::Uint(word.into())]).unwrap();
                    let result = runtime
                        .call_raw(&input, ExecutionOptions::default())
                        .unwrap();
                    assert_eq!(
                        result.return_data, expected,
                        "{signature}, word={word:02x?}"
                    );
                }
            }
        }
    }
}

#[test]
fn constant_string_getters() {
    let source = r#"
msg Metadata {
    #[selector = sol("name()")]
    Name -> String<31>,
    #[selector = sol("symbol()")]
    Symbol -> String<31>,
    #[selector = sol("empty()")]
    Empty -> String<31>,
}

pub contract ConstantStrings {
    recv Metadata {
        Name -> String<31> { "Bench Token" }
        Symbol -> String<31> { "BENCH" }
        Empty -> String<31> { "" }
    }
}
"#;
    let bytecode = compile_fe_sonatina_bytecode(source, "ConstantStrings", "ConstantStrings")
        .expect("compile constant string contract");
    let mut report = format!("runtime bytes: {}\n", bytecode.runtime.len());
    let mut runtime = RuntimeInstance::deploy(&hex::encode(bytecode.deploy)).expect("deploy");
    for (signature, value) in [
        ("name()", "Bench Token"),
        ("symbol()", "BENCH"),
        ("empty()", ""),
    ] {
        let function = AbiParser::default().parse_function(signature).unwrap();
        let input = function.encode_input(&[]).unwrap();
        let gas = runtime
            .call_raw_gas_profile(&input, ExecutionOptions::default())
            .total_step_gas;
        writeln!(report, "{signature} gas: {gas}").unwrap();
        let result = runtime
            .call_raw(&input, ExecutionOptions::default())
            .unwrap();
        assert_eq!(
            result.return_data,
            encode(&[Token::String(value.to_string())])
        );
    }
    snap_test!(
        report,
        concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/packed_string_abi/constant_string_getters"
        )
    );
}
