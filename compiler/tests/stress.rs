//! Stress tests that test broad behavior

#![cfg(feature = "solc-backend")]
use primitive_types::H160;
use std::iter;

mod utils;
use utils::*;

pub fn deploy_contract(
    executor: &mut Executor,
    fixture: &str,
    contract_name: &str,
    init_params: &[ethabi::Token],
) -> ContractHarness {
    utils::deploy_contract(
        executor,
        &format!("stress/{}", fixture),
        contract_name,
        init_params,
    )
}

pub fn load_contract(address: H160, fixture: &str, contract_name: &str) -> ContractHarness {
    utils::load_contract(address, &format!("stress/{}", fixture), contract_name)
}

#[test]
fn data_copying_stress() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "data_copying_stress.fe", "Foo", &[]);

        harness.test_function(
            &mut executor,
            "set_my_vals",
            &[
                string_token("my string"),
                string_token("my other string"),
                uint_token(26),
                uint_token(42),
            ],
            None,
        );

        harness.test_function(&mut executor, "emit_my_event", &[], None);

        harness.test_function(&mut executor, "set_to_my_other_vals", &[], None);

        harness.test_function(&mut executor, "emit_my_event", &[], None);

        let my_array = u256_array_token(&[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
        let my_mutated_array = u256_array_token(&[1, 2, 3, 5, 5, 6, 7, 8, 9, 10]);

        let my_addrs = address_array_token(&["0", "1", "2"]);
        let my_second_addr = address_token("1");

        harness.test_function(
            &mut executor,
            "mutate_and_return",
            &[my_array.clone()],
            Some(&my_mutated_array),
        );

        harness.test_function(
            &mut executor,
            "multiple_references_shared_memory",
            &[my_array.clone()],
            None,
        );

        harness.test_function(
            &mut executor,
            "clone_and_return",
            &[my_array.clone()],
            Some(&my_array),
        );

        harness.test_function(
            &mut executor,
            "clone_mutate_and_return",
            &[my_array.clone()],
            Some(&my_array),
        );

        harness.test_function(
            &mut executor,
            "assign_my_nums_and_return",
            &[],
            Some(&u256_array_token(&[42, 26, 0, 1, 255])),
        );

        harness.test_function(&mut executor, "set_my_addrs", &[my_addrs], None);
        harness.test_function(
            &mut executor,
            "get_my_second_addr",
            &[],
            Some(&my_second_addr),
        );

        harness.events_emitted(
            executor,
            &[
                ("MyEvent", &[string_token("my string"), uint_token(26)]),
                (
                    "MyEvent",
                    &[string_token("my other string"), uint_token(42)],
                ),
            ],
        );
    });
}

#[test]
fn abi_encoding_stress() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "abi_encoding_stress.fe", "Foo", &[]);

        let my_addrs = address_array_token(&["a", "b", "c", "d", "e"]);
        let my_u128 = uint_token(42);
        let my_string = string_token("my string");
        let my_u8s = u256_array_token(&(0..255).collect::<Vec<_>>());
        let my_bool = bool_token(true);
        let my_bytes = bytes_token(
            iter::repeat("ten bytes.")
                .take(10)
                .collect::<String>()
                .as_str(),
        );

        harness.test_function(&mut executor, "set_my_addrs", &[my_addrs.clone()], None);
        harness.test_function(&mut executor, "get_my_addrs", &[], Some(&my_addrs));

        harness.test_function(&mut executor, "set_my_u128", &[my_u128.clone()], None);
        harness.test_function(&mut executor, "get_my_u128", &[], Some(&my_u128));

        harness.test_function(&mut executor, "set_my_string", &[my_string.clone()], None);
        harness.test_function(&mut executor, "get_my_string", &[], Some(&my_string));

        harness.test_function(&mut executor, "set_my_u8s", &[my_u8s.clone()], None);
        harness.test_function(&mut executor, "get_my_u8s", &[], Some(&my_u8s));

        harness.test_function(&mut executor, "set_my_bool", &[my_bool.clone()], None);
        harness.test_function(&mut executor, "get_my_bool", &[], Some(&my_bool));

        harness.test_function(&mut executor, "set_my_bytes", &[my_bytes.clone()], None);
        harness.test_function(&mut executor, "get_my_bytes", &[], Some(&my_bytes));

        harness.test_function(&mut executor, "emit_my_event", &[], None);

        harness.events_emitted(
            executor,
            &[(
                "MyEvent",
                &[my_addrs, my_u128, my_string, my_u8s, my_bool, my_bytes],
            )],
        );
    });
}
