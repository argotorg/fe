use crate::layout_test_support::{parse_module, parse_ok};
use fe_hir::{
    analysis::ty::{
        abi_ty::{AbiTypeError, semantic_ty_to_abi_desc},
        ty_check::check_func_body,
        ty_def::InvalidCause,
    },
    test_db::find_func,
};

#[test]
fn specialized_adt_array_field_has_concrete_abi_extent() {
    parse_ok!(
        db,
        top_mod,
        "struct S<const N: usize> { x: [u8; { 10 / N }] }\nfn use_s(value: S<2>) {}\n",
    );
    let func = find_func(&db, top_mod, "use_s");
    let typed_body = check_func_body(&db, func).1.clone();
    let binding = typed_body.param_binding(0).expect("parameter binding");
    let ty = typed_body.binding_ty(&db, binding);
    let desc = semantic_ty_to_abi_desc(&db, ty).expect("concrete ABI description");
    assert_eq!(desc.canonical_type, "(uint8[5])");
}

#[test]
fn recursive_dynamic_arrays_have_no_finite_abi_schema() {
    parse_ok!(
        db,
        top_mod,
        "use std::abi::DynArray\nstruct Node { children: DynArray<Node> }\nfn use_node(value: Node) {}\n",
    );
    let func = find_func(&db, top_mod, "use_node");
    let typed_body = check_func_body(&db, func).1.clone();
    let binding = typed_body.param_binding(0).expect("parameter binding");
    let ty = typed_body.binding_ty(&db, binding);
    assert!(matches!(
        semantic_ty_to_abi_desc(&db, ty),
        Err(AbiTypeError::Unsupported(message)) if message.contains("recursive ABI type"),
    ));
}

#[test]
fn invalid_specialized_adt_array_field_retains_source_cause() {
    parse_module!(
        db,
        top_mod,
        "struct S<const N: usize> { x: [u8; { 10 / N }] }\nfn use_s(value: S<0>) {}\n",
    );
    let func = find_func(&db, top_mod, "use_s");
    let typed_body = check_func_body(&db, func).1.clone();
    let binding = typed_body.param_binding(0).expect("parameter binding");
    let ty = typed_body.binding_ty(&db, binding);
    let error = semantic_ty_to_abi_desc(&db, ty).expect_err("invalid ABI extent");
    assert!(matches!(
        error,
        AbiTypeError::InvalidConst {
            cause: InvalidCause::ConstEvalDivisionByZero { .. },
            ..
        }
    ));
}
