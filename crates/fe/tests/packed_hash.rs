use codegen::OptLevel;
use contract_harness::{CompileOptions, ExecutionOptions, FeContractHarness};
use ethers_core::types::U256;
use tiny_keccak::{Hasher, Keccak};

#[test]
fn calldata_packed_hash_covers_empty_dirty_and_word_boundaries() {
    for level in [OptLevel::O1, OptLevel::O2] {
        let harness = FeContractHarness::compile_from_source(
            "RuntimeHash",
            include_str!("fixtures/runtime_packed_hash.fe"),
            CompileOptions { opt_level: level },
        )
        .expect("runtime hash contract");
        for input in [
            U256::zero(),
            U256::one(),
            U256::one() << 255,
            U256::MAX,
            U256::from(17),
        ] {
            let mut word = [0; 32];
            input.to_big_endian(&mut word);
            for kind in 0..22u8 {
                let preimage = match kind {
                    0 => word.to_vec(),
                    1 => vec![],
                    2 => word[31..].to_vec(),
                    3 => word[1..].to_vec(),
                    4..=11 => {
                        let width = [0, 1, 31, 32, 33, 63, 64, 65][usize::from(kind - 4)];
                        (0..width).map(|i| word[31].wrapping_add(i)).collect()
                    }
                    12 => [&word[31..], word.as_slice(), word.as_slice()].concat(),
                    13 => vec![word[31]; 16],
                    _ => {
                        let width = [0, 1, 31, 32, 33, 63, 64, 65][usize::from(kind - 14)];
                        (0..width).map(|i| 17 + i).collect()
                    }
                };
                let mut expected = [0; 32];
                let mut keccak = Keccak::v256();
                keccak.update(&preimage);
                keccak.finalize(&mut expected);
                let mut calldata = vec![1, 2, 3, 4];
                calldata.extend([0; 31]);
                calldata.push(kind);
                calldata.extend(word);
                let result = harness
                    .call_raw(&calldata, ExecutionOptions::default())
                    .unwrap_or_else(|err| panic!("{level:?}/{kind}/{input}: {err}"));
                assert_eq!(result.return_data, expected, "{level:?}/{kind}/{input}");
            }
        }
    }
}
