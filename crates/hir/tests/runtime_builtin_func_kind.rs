use fe_hir::analysis::ty::corelib::{
    IntrinsicMemoryAccess, IntrinsicMemoryProjection, IntrinsicPointerReturn, MemoryAccessKind,
    RuntimeBuiltinFuncKind, intrinsic_contract, is_std_evm_effect_method, resolve_lib_func_path,
    runtime_builtin_func_kind,
};
use fe_hir::test_db::HirAnalysisTestDb;

#[test]
fn classifies_core_and_std_runtime_builtins() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "runtime_builtin_func_kind_classifies_core_and_std_runtime_builtins.fe".into(),
        "fn f() {}",
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let func = top_mod.all_funcs(&db)[0];

    let alloc = resolve_lib_func_path(&db, func.scope(), "core::ptr::alloc_raw")
        .expect("failed to resolve core::ptr::alloc_raw");
    let ptr_eq = resolve_lib_func_path(&db, func.scope(), "core::ptr::addr_eq")
        .expect("failed to resolve core::ptr::addr_eq");
    let mload = resolve_lib_func_path(&db, func.scope(), "std::evm::ops::mload")
        .expect("failed to resolve std::evm::ops::mload");
    let revert_empty = resolve_lib_func_path(&db, func.scope(), "std::evm::ops::revert_empty")
        .expect("failed to resolve std::evm::ops::revert_empty");
    let raw_mstore = resolve_lib_func_path(&db, func.scope(), "std::evm::effects::RawMem::mstore")
        .expect("failed to resolve std::evm::effects::RawMem::mstore");
    let array_elem = resolve_lib_func_path(&db, func.scope(), "core::ptr::array_elem")
        .expect("failed to resolve core::ptr::array_elem");
    let panic = resolve_lib_func_path(&db, func.scope(), "core::panic")
        .expect("failed to resolve core::panic");
    let keccak = resolve_lib_func_path(&db, func.scope(), "core::intrinsic::__keccak256")
        .expect("failed to resolve core::intrinsic::__keccak256");

    assert_eq!(
        runtime_builtin_func_kind(&db, alloc),
        Some(RuntimeBuiltinFuncKind::Malloc)
    );
    assert_eq!(
        runtime_builtin_func_kind(&db, ptr_eq),
        Some(RuntimeBuiltinFuncKind::PtrEq)
    );
    assert_eq!(
        runtime_builtin_func_kind(&db, mload),
        Some(RuntimeBuiltinFuncKind::Mload)
    );
    assert_eq!(
        runtime_builtin_func_kind(&db, revert_empty),
        Some(RuntimeBuiltinFuncKind::RevertEmpty)
    );
    assert_eq!(
        runtime_builtin_func_kind(&db, panic),
        Some(RuntimeBuiltinFuncKind::Panic)
    );
    assert_eq!(
        runtime_builtin_func_kind(&db, keccak),
        Some(RuntimeBuiltinFuncKind::IntrinsicKeccak256)
    );
    let alloc_contract = intrinsic_contract(&db, alloc).expect("allocator intrinsic contract");
    assert_eq!(
        alloc_contract.pointer_return,
        Some(IntrinsicPointerReturn::FreshMemory)
    );
    assert_eq!(
        alloc_contract.memory.expect("allocator memory contract"),
        &[]
    );
    assert_eq!(
        intrinsic_contract(&db, mload)
            .expect("mload intrinsic contract")
            .memory
            .expect("mload memory contract"),
        &[IntrinsicMemoryAccess {
            input: 0,
            projection: IntrinsicMemoryProjection::Pointee,
            kind: MemoryAccessKind::Read,
        }]
    );
    assert_eq!(
        intrinsic_contract(&db, revert_empty)
            .expect("empty revert intrinsic contract")
            .memory
            .expect("empty revert memory contract"),
        &[]
    );
    assert_eq!(
        intrinsic_contract(&db, raw_mstore)
            .expect("RawMem::mstore intrinsic contract")
            .memory
            .expect("RawMem::mstore memory contract"),
        &[
            IntrinsicMemoryAccess {
                input: 0,
                projection: IntrinsicMemoryProjection::Value,
                kind: MemoryAccessKind::MutAccess,
            },
            IntrinsicMemoryAccess {
                input: 1,
                projection: IntrinsicMemoryProjection::Pointee,
                kind: MemoryAccessKind::Write,
            },
        ]
    );
    assert!(is_std_evm_effect_method(&db, raw_mstore));
    let array_elem_contract =
        intrinsic_contract(&db, array_elem).expect("array element intrinsic contract");
    assert_eq!(
        array_elem_contract.pointer_return,
        Some(IntrinsicPointerReturn::InputArrayElem)
    );
    assert_eq!(array_elem_contract.memory, None);
}
