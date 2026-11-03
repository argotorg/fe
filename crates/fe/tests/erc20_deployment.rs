//! ERC20 deployment-size regression from banteg/evm-compiler-bench, with
//! independent ABI expectations. The snapshot records code sizes. No Solidity
//! compiler or Foundry is required.

use contract_harness::{ExecutionOptions, HarnessError, RuntimeInstance};
use ethers_core::{
    abi::{AbiParser, Token, encode},
    types::Address,
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

#[test]
fn erc20_deployment_size_and_behavior() {
    let source = include_str!("evm_compiler_bench/erc20_minimal.fe");
    let bytecode = compile_fe_sonatina_bytecode(source, "Erc20Minimal", "Erc20Minimal")
        .expect("compile ERC20 benchmark");
    let report = format!(
        "initcode bytes: {}\nruntime bytes: {}\n",
        bytecode.deploy.len(),
        bytecode.runtime.len()
    );
    let mut runtime = RuntimeInstance::deploy_with_constructor_args(
        &hex::encode(bytecode.deploy),
        &encode(&[Token::Uint(1_000_000.into())]),
    )
    .unwrap();
    let owner = Token::Address(Address::zero());
    let recipient = Token::Address(Address::from_low_u64_be(123));
    let calls = [
        ("name()", vec![], Token::String("Bench Token".into())),
        ("symbol()", vec![], Token::String("BENCH".into())),
        ("decimals()", vec![], Token::Uint(18.into())),
        ("totalSupply()", vec![], Token::Uint(1_000_000.into())),
        (
            "transfer(address,uint256)",
            vec![recipient.clone(), Token::Uint(10.into())],
            Token::Bool(true),
        ),
        (
            "approve(address,uint256)",
            vec![owner.clone(), Token::Uint(7.into())],
            Token::Bool(true),
        ),
        (
            "transferFrom(address,address,uint256)",
            vec![owner.clone(), recipient.clone(), Token::Uint(5.into())],
            Token::Bool(true),
        ),
        (
            "balanceOf(address)",
            vec![recipient],
            Token::Uint(15.into()),
        ),
        (
            "allowance(address,address)",
            vec![owner.clone(), owner.clone()],
            Token::Uint(2.into()),
        ),
        (
            "transfer(address,uint256)",
            vec![owner.clone(), Token::Uint(1.into())],
            Token::Bool(true),
        ),
        (
            "balanceOf(address)",
            vec![owner],
            Token::Uint(999_985.into()),
        ),
    ];
    for (signature, args, expected) in calls {
        let input = calldata(signature, &args);
        let result = runtime
            .call_raw(&input, ExecutionOptions::default())
            .unwrap();
        assert_eq!(result.return_data, encode(&[expected]), "{signature}");
    }
    for (signature, args, message) in [
        (
            "transfer(address,uint256)",
            vec![
                Token::Address(Address::from_low_u64_be(123)),
                Token::Uint(1_000_000.into()),
            ],
            "balance",
        ),
        (
            "transferFrom(address,address,uint256)",
            vec![
                Token::Address(Address::zero()),
                Token::Address(Address::from_low_u64_be(123)),
                Token::Uint(3.into()),
            ],
            "allowance",
        ),
    ] {
        let input = calldata(signature, &args);
        let Err(HarnessError::Revert(data)) = runtime.call_raw(&input, ExecutionOptions::default())
        else {
            panic!("{signature}: expected revert")
        };
        assert_eq!(
            data.0,
            calldata("Error(string)", &[Token::String(message.into())])
        );
    }
    snap_test!(
        report,
        concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/evm_compiler_bench/erc20_minimal"
        )
    );
}
