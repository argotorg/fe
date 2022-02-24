use fe_yulgen::constructor;
use fe_yulgen::names::abi as abi_names;
use fe_yulgen::operations::{abi as abi_operations, data as data_operations};
use fe_yulgen::runtime::abi_dispatcher;
use fe_yulgen::runtime::functions::{abi as abi_functions, revert as revert_functions};
use fe_yulgen::types::{AbiDecodeLocation, AbiType};
use insta::assert_display_snapshot;
use smol_str::SmolStr;
use wasm_bindgen_test::wasm_bindgen_test;
use yultsur::*;

macro_rules! test_yulgen {
    ($name:ident, $code:expr) => {
        #[test]
        #[wasm_bindgen_test]
        fn $name() {
            if cfg!(target_arch = "wasm32") {
                fe_common::assert_snapshot_wasm!(
                    concat!("snapshots/yulgen__", stringify!($name), ".snap"),
                    $code.to_string()
                );
            } else {
                assert_display_snapshot!($code);
            }
        }
    };
}

// constructor
test_yulgen! { constructor_no_init,  constructor::build() }

#[allow(clippy::type_complexity)]
fn functions() -> Vec<(SmolStr, SmolStr, Vec<AbiType>, Option<AbiType>, bool)> {
    vec![
        (
            "hello_world".into(),
            "$$somemod$hello_world".into(),
            vec![],
            Some(AbiType::String { max_size: 42 }),
            false,
        ),
        (
            "add".into(),
            "$$somemod$add".into(),
            vec![AbiType::Uint { size: 32 }, AbiType::Uint { size: 32 }],
            Some(AbiType::Uint { size: 32 }),
            true,
        ),
    ]
}

// ABI dispatcher
test_yulgen! { abi_dispatcher,  abi_dispatcher::dispatcher(&functions()) }

// ABI encoding functions
test_yulgen! {
    abi_encode_u256_address_function,
    abi_functions::encode(&[AbiType::Uint { size: 32 }, AbiType::Address])
}

// ABI decoding functions
test_yulgen! {
    abi_decode_data_address_bool_mem_function,
    yul::Block { statements: abi_functions::decode_functions(&[AbiType::Bool, AbiType::Address], AbiDecodeLocation::Memory) }
}
test_yulgen! {
    abi_decode_data_u256_bytes_string_bool_address_bytes_calldata_function,
    yul::Block { statements: abi_functions::decode_functions(&[
        AbiType::Uint { size: 32 },
        AbiType::Bytes { size: 100 },
        AbiType::String { max_size: 42 },
        AbiType::Bool,
        AbiType::Address,
        AbiType::Bytes { size: 100 },
    ], AbiDecodeLocation::Calldata) }
}
test_yulgen! {
    abi_decode_component_uint256_mem_function,
    abi_functions::decode_component_uint(32, AbiDecodeLocation::Memory)
}
test_yulgen! {
    abi_decode_component_int16_calldata_function,
    abi_functions::decode_component_int(2, AbiDecodeLocation::Calldata)
}
test_yulgen! {
    abi_decode_component_bool_calldata_function,
    abi_functions::decode_component_bool(AbiDecodeLocation::Calldata)
}
test_yulgen! {
    abi_decode_component_address_mem_function,
    abi_functions::decode_component_bool(AbiDecodeLocation::Memory)
}
test_yulgen! {
    abi_decode_component_static_array_address_calldata_function,
    abi_functions::decode_component_static_array(&AbiType::Address, 42, AbiDecodeLocation::Calldata)
}
test_yulgen! {
    abi_decode_component_tuple_u256_address_mem_function,
    abi_functions::decode_component_tuple(
        &[AbiType::Uint { size: 32 }, AbiType::Address],
        AbiDecodeLocation::Memory
    )
}
test_yulgen! {
    abi_decode_component_bytes_26_mem_function,
    abi_functions::decode_component_bytes(26, AbiDecodeLocation::Memory)
}
test_yulgen! {
    abi_decode_component_string_26_calldata_function,
    abi_functions::decode_component_bytes(26, AbiDecodeLocation::Calldata)
}

// data operations
test_yulgen! {
    emit_event_no_indexed_operation,
    data_operations::emit_event("MyEvent", &[(AbiType::Uint { size: 32 }, false), (AbiType::Address, false)], vec![expression! { 26 }, expression! { 0x42 }])
}
test_yulgen! {
    emit_event_one_indexed_operation,
    data_operations::emit_event("MyEvent", &[(AbiType::Uint { size: 32 }, true), (AbiType::Address, false)], vec![expression! { 26 }, expression! { 0x42 }])
}
test_yulgen! {
    sum_operation,
    data_operations::sum(expressions! { 42 26 99 })
}

// ABI operations
test_yulgen! {
    encode_u256_operation,
    abi_operations::encode(&[AbiType::Uint { size: 32 }], vec![expression! { 42 }])
}
test_yulgen! {
    encode_size_u256_operation,
    abi_operations::encoding_size(&[AbiType::Uint { size: 32 }], &[expression! { 42 }])
}
test_yulgen! {
    decode_string_calldata_operation,
    abi_operations::decode_data(
        &[AbiType::String { max_size: 26 }],
        expression! { 42 },
        expression! { 10 },
        AbiDecodeLocation::Calldata
    )
}

// ABI names
test_yulgen! {
    encode_name,
    abi_names::encode(&[
        AbiType::Uint { size: 32 },
        AbiType::Bytes { size: 100 }
    ])
}
test_yulgen! {
    decode_data_name_u256_calldata_name,
    abi_names::decode_data(&[AbiType::Uint { size: 32 }], AbiDecodeLocation::Calldata)
}
test_yulgen! {
    decode_data_name_string_mem_name,
    abi_names::decode_data(&[AbiType::String { max_size: 42 }], AbiDecodeLocation::Memory)
}

// revert functions
test_yulgen! {
    revert_string_error,
    revert_functions::error_revert(&AbiType::String { max_size: 3 })
}
