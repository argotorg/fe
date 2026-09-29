//! `std::evm::eip712::Domain` rederives its separator after a chain id change.

use contract_harness::{ExecutionOptions, RuntimeInstance};
use ethers_core::abi::{AbiParser, Token, encode};
use ethers_core::types::{H160, U256};
use ethers_core::utils::keccak256;
use fe::bench_support::compile_fe_sonatina_bytecode;

const SOURCE: &str = r#"
use std::evm::Ctx
use std::evm::eip712::Domain

msg SignerMsg {
    #[selector = sol("separator()")]
    Separator -> u256,
    #[selector = sol("digest(uint256)")]
    Digest { struct_hash: u256 } -> u256,
}

pub contract Signer {
    domain: Domain,

    init() uses (mut domain, ctx: Ctx) {
        domain = Domain::new(name_hash: core::keccak("Test"), version_hash: core::keccak("1"))
    }

    recv SignerMsg {
        Separator -> u256 uses (domain, ctx: Ctx) {
            domain.separator()
        }

        Digest { struct_hash } -> u256 uses (domain, ctx: Ctx) {
            domain.digest(struct_hash)
        }
    }
}
"#;

/// OpenZeppelin `_buildDomainSeparator`, computed with ethers.
fn expected_separator(chain_id: u64, contract: H160) -> [u8; 32] {
    let typehash = keccak256(
        "EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)",
    );
    keccak256(encode(&[
        Token::FixedBytes(typehash.to_vec()),
        Token::FixedBytes(keccak256("Test").to_vec()),
        Token::FixedBytes(keccak256("1").to_vec()),
        Token::Uint(U256::from(chain_id)),
        Token::Address(contract),
    ]))
}

fn expected_digest(separator: [u8; 32], struct_hash: [u8; 32]) -> [u8; 32] {
    let mut preimage = vec![0x19, 0x01];
    preimage.extend_from_slice(&separator);
    preimage.extend_from_slice(&struct_hash);
    keccak256(preimage)
}

fn call_word(runtime: &mut RuntimeInstance, signature: &str, args: &[Token]) -> [u8; 32] {
    let input = AbiParser::default()
        .parse_function(signature)
        .unwrap()
        .encode_input(args)
        .unwrap();
    let result = runtime
        .call_raw(&input, ExecutionOptions::default())
        .unwrap();
    result.return_data.try_into().expect("one word")
}

#[test]
fn domain_separator_follows_chain_id() {
    let bytecode =
        compile_fe_sonatina_bytecode(SOURCE, "Signer", "Signer").expect("compile signer");
    let mut runtime = RuntimeInstance::deploy(&hex::encode(bytecode.deploy)).expect("deploy");
    let contract = H160::from_slice(runtime.address().as_slice());
    let struct_hash = keccak256("struct");

    for chain_id in [1, 10, u64::MAX, 1] {
        runtime.set_chain_id(chain_id);
        let separator = expected_separator(chain_id, contract);
        assert_eq!(
            call_word(&mut runtime, "separator()", &[]),
            separator,
            "chain {chain_id}"
        );
        assert_eq!(
            call_word(
                &mut runtime,
                "digest(uint256)",
                &[Token::Uint(U256::from(struct_hash))]
            ),
            expected_digest(separator, struct_hash),
            "chain {chain_id}"
        );
    }
}
