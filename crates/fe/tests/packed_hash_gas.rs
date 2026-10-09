use codegen::OptLevel;
use contract_harness::{CompileOptions, ExecutionOptions, FeContractHarness};
use ethers_core::types::U256;
use tiny_keccak::{Hasher, Keccak};

const SOURCE: &str = include_str!("fixtures/packed_hash_bench.fe");

#[test]
fn runtime_packed_hash_matches_bytes_and_packed_baselines() {
    let cases = [
        (
            "words",
            "(a, b)",
            "mem.mstore(addr: dest, value: a)\nmem.mstore(addr: ptr::offset_bytes(dest, 32), value: b)",
            64,
        ),
        (
            "string_word",
            "(a as String<5>, b)",
            "mem.mstore(addr: dest, value: ((a & 0xffffffffff) << 216) | (b >> 40))\nmem.mstore(addr: ptr::offset_bytes(dest, 32), value: b << 216)",
            37,
        ),
        (
            "word_bytes_word",
            "(a, bytes, b)",
            "mem.mstore(addr: dest, value: a)\nmem.mstore(addr: ptr::offset_bytes(dest, 32), value: ((c & 0xffffff) << 232) | (b >> 24))\nmem.mstore(addr: ptr::offset_bytes(dest, 64), value: b << 232)",
            67,
        ),
    ];
    // Full-width dirty bits in a also prove that String<5> ignores high bits.
    let inputs = [
        [
            U256::MAX - U256::from(0xbebdbcbbbau64),
            U256::one() << 255,
            U256::from(0xabcdefu64),
        ],
        [U256::from(0x3132333435u64), U256::MAX, U256::zero()],
    ];
    let mut failures = Vec::new();
    for level in [OptLevel::O1, OptLevel::O2] {
        for (name, tuple, stores, width) in cases {
            let mut measurements = Vec::new();
            for variant in ["core", "packed", "direct"] {
                let body = match variant {
                    "core" => format!("core::keccak({tuple})"),
                    "packed" => format!("keccak_packed({tuple})"),
                    "direct" => format!(
                        "let dest = ptr::alloc_bytes({capacity})\n{stores}\nkeccak256(MemSpan::from_raw_parts(ptr: dest, len: {width}))",
                        capacity = width / 32 * 32 + if width % 32 == 0 { 0 } else { 32 },
                    ),
                    _ => unreachable!(),
                };
                let bytes = "let bytes: [u8; 3] = [(c >> 16).downcast_truncate(), (c >> 8).downcast_truncate(), c.downcast_truncate()]\n";
                let source = SOURCE.replace("core::keccak((a, b))", &format!("{bytes}{body}"));
                let harness = FeContractHarness::compile_from_source(
                    "HashBench",
                    &source,
                    CompileOptions { opt_level: level },
                )
                .unwrap_or_else(|err| panic!("{name}/{variant}/{level:?}: {err}"));
                let runtime = harness.deploy_instance().expect("runtime instance");
                let mut gas = Vec::new();
                for [a, b, c] in inputs {
                    let mut words = [[0u8; 32]; 3];
                    for (word, value) in words.iter_mut().zip([a, b, c]) {
                        value.to_big_endian(word);
                    }
                    let [a, b, c] = words;
                    let preimage = match name {
                        "words" => [a.as_slice(), b.as_slice()].concat(),
                        "string_word" => [&a[27..], b.as_slice()].concat(),
                        "word_bytes_word" => [a.as_slice(), &c[29..], b.as_slice()].concat(),
                        _ => unreachable!(),
                    };
                    let mut expected = [0; 32];
                    let mut keccak = Keccak::v256();
                    keccak.update(&preimage);
                    keccak.finalize(&mut expected);
                    let calldata = [&[1, 2, 3, 4], words.as_flattened()].concat();
                    let result = harness
                        .call_raw(&calldata, ExecutionOptions::default())
                        .unwrap_or_else(|err| panic!("{name}/{variant}/{level:?}: {err}"));
                    assert_eq!(result.return_data, expected, "{name}/{variant}/{level:?}");
                    gas.push(
                        runtime
                            .call_raw_gas_profile(&calldata, ExecutionOptions::default())
                            .total_step_gas,
                    );
                }
                println!(
                    "{level:?} {name} {variant}: execution_gas={gas:?}, runtime_bytes={}",
                    harness.runtime_bytecode().len() / 2
                );
                measurements.push(gas);
            }
            for (variant, baseline) in ["packed", "direct"].into_iter().zip(&measurements[1..]) {
                // The array case uses the existing packed helper as its gate.
                // Direct stores reconstruct bytes from c without an array loop.
                let required = name != "word_bytes_word" || variant == "packed";
                for (actual, baseline) in measurements[0].iter().zip(baseline) {
                    assert!(
                        *actual > 0 && *baseline > 0,
                        "execution gas trace must be nonempty"
                    );
                    if required && actual * 2 > baseline * 3 {
                        failures.push(format!(
                            "{level:?} {name}/{variant}: core execution gas {actual} exceeds 1.5x {baseline}"
                        ));
                    }
                }
            }
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
