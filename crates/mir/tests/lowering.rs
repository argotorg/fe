use fe_analyzer::{
    namespace::items::{IngotId, ModuleId},
    AnalyzerDb,
};
use fe_common::{db::Upcast, db::UpcastMut, files::Utf8Path};
use fe_mir::{
    analysis::{ControlFlowGraph, DomTree, LoopTree, PostDomTree},
    db::{MirDb, NewDb},
};

macro_rules! test_lowering {
    ($name:ident, $path:expr) => {
        #[test]
        fn $name() {
            let mut db = NewDb::default();

            let file_name = Utf8Path::new($path).file_name().unwrap();
            let module = ModuleId::new_standalone(&mut db, file_name, test_files::fixture($path));

            let diags = module.diagnostics(&db);
            if !diags.is_empty() {
                panic!("lowering failed")
            }

            for func in db.mir_lower_module_all_functions(module).iter() {
                let body = func.body(&db);
                ControlFlowGraph::compute(&body);
            }
        }
    };
}

#[test]
fn mir_lower_std_lib() {
    let mut db = NewDb::default();

    // Should return the same id
    let std_ingot = IngotId::std_lib(&mut db);

    let adb_mut: &mut dyn AnalyzerDb = db.upcast_mut();
    adb_mut.set_root_ingot(std_ingot);

    let diags = std_ingot.diagnostics(&db);
    if !diags.is_empty() {
        panic!("std lib analysis failed")
    }

    for &module in std_ingot.all_modules(db.upcast()).iter() {
        for func in db.mir_lower_module_all_functions(module).iter() {
            let body = func.body(&db);
            let cfg = ControlFlowGraph::compute(&body);
            let domtree = DomTree::compute(&cfg);
            LoopTree::compute(&cfg, &domtree);
            PostDomTree::compute(&body);
        }
    }
}

test_lowering! { mir_erc20_token, "demos/erc20_token.fe"}
test_lowering! { mir_guest_book, "demos/guest_book.fe"}
test_lowering! { mir_uniswap, "demos/uniswap.fe"}
test_lowering! { mir_assert, "features/assert.fe"}
test_lowering! { mir_aug_assign, "features/aug_assign.fe"}
test_lowering! { mir_call_statement_with_args, "features/call_statement_with_args.fe"}
test_lowering! { mir_call_statement_with_args_2, "features/call_statement_with_args_2.fe"}
test_lowering! { mir_call_statement_without_args, "features/call_statement_without_args.fe"}
test_lowering! { mir_checked_arithmetic, "features/checked_arithmetic.fe"}
test_lowering! { mir_constructor, "features/constructor.fe"}
test_lowering! { mir_create2_contract, "features/create2_contract.fe"}
test_lowering! { mir_create_contract, "features/create_contract.fe"}
test_lowering! { mir_create_contract_from_init, "features/create_contract_from_init.fe"}
test_lowering! { mir_empty, "features/empty.fe"}
test_lowering! { mir_events, "features/events.fe"}
test_lowering! { mir_module_level_events, "features/module_level_events.fe"}
test_lowering! { mir_external_contract, "features/external_contract.fe"}
test_lowering! { mir_for_loop_with_break, "features/for_loop_with_break.fe"}
test_lowering! { mir_for_loop_with_continue, "features/for_loop_with_continue.fe"}
test_lowering! { mir_for_loop_with_static_array, "features/for_loop_with_static_array.fe"}
test_lowering! { mir_if_statement, "features/if_statement.fe"}
test_lowering! { mir_if_statement_2, "features/if_statement_2.fe"}
test_lowering! { mir_if_statement_with_block_declaration, "features/if_statement_with_block_declaration.fe"}
test_lowering! { mir_keccak, "features/keccak.fe"}
test_lowering! { mir_math, "features/math.fe"}
test_lowering! { mir_module_const, "features/module_const.fe"}
test_lowering! { mir_multi_param, "features/multi_param.fe"}
test_lowering! { mir_nested_map, "features/nested_map.fe"}
test_lowering! { mir_numeric_sizes, "features/numeric_sizes.fe"}
test_lowering! { mir_ownable, "features/ownable.fe"}
test_lowering! { mir_pure_fn_standalone, "features/pure_fn_standalone.fe"}
test_lowering! { mir_revert, "features/revert.fe"}
test_lowering! { mir_self_address, "features/self_address.fe"}
test_lowering! { mir_send_value, "features/send_value.fe"}
test_lowering! { mir_balances, "features/balances.fe"}
test_lowering! { mir_sized_vals_in_sto, "features/sized_vals_in_sto.fe"}
test_lowering! { mir_strings, "features/strings.fe"}
test_lowering! { mir_structs, "features/structs.fe"}
test_lowering! { mir_struct_fns, "features/struct_fns.fe"}
test_lowering! { mir_ternary_expression, "features/ternary_expression.fe"}
test_lowering! { mir_two_contracts, "features/two_contracts.fe"}
test_lowering! { mir_u8_u8_map, "features/u8_u8_map.fe"}
test_lowering! { mir_u16_u16_map, "features/u16_u16_map.fe"}
test_lowering! { mir_u32_u32_map, "features/u32_u32_map.fe"}
test_lowering! { mir_u64_u64_map, "features/u64_u64_map.fe"}
test_lowering! { mir_u128_u128_map, "features/u128_u128_map.fe"}
test_lowering! { mir_u256_u256_map, "features/u256_u256_map.fe"}
test_lowering! { mir_while_loop, "features/while_loop.fe"}
test_lowering! { mir_while_loop_with_break, "features/while_loop_with_break.fe"}
test_lowering! { mir_while_loop_with_break_2, "features/while_loop_with_break_2.fe"}
test_lowering! { mir_while_loop_with_continue, "features/while_loop_with_continue.fe"}
test_lowering! { mir_abi_encoding_stress, "stress/abi_encoding_stress.fe"}
test_lowering! { mir_data_copying_stress, "stress/data_copying_stress.fe"}
test_lowering! { mir_tuple_stress, "stress/tuple_stress.fe"}
test_lowering! { mir_type_aliases, "features/type_aliases.fe"}
test_lowering! { mir_const_generics, "features/const_generics.fe" }
test_lowering! { mir_const_local, "features/const_local.fe" }
