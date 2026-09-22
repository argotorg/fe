use fe_hir::analysis::ty::corelib::{
    IntrinsicMemoryAccess, IntrinsicMemoryExtent, IntrinsicMemoryTarget, IntrinsicPointerReturn,
    MemoryAccessKind, RuntimeBuiltinFuncKind, intrinsic_contract, is_std_evm_effect_method,
    resolve_lib_func_path, runtime_builtin_func_kind,
};
use fe_hir::test_db::HirAnalysisTestDb;

#[test]
fn numeric_memory_contracts_require_compiler_defined_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "numeric_contracts.fe".into(),
        "extern { fn __add_u256(a: u256, b: u256) -> u256 }\nfn anchor() {}",
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let func = module
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "__add_u256")
        })
        .unwrap();
    assert!(intrinsic_contract(&db, func).is_none());
    for path in [
        "core::num::__add_u256",
        "core::num::__checked_add",
        "core::num::__bitcast",
        "core::num::__not_bool",
        "core::num_intrinsics::__div_u256",
        "core::intrinsic::size_of",
        "core::intrinsic::contract_field_slot",
    ] {
        let intrinsic = resolve_lib_func_path(&db, func.scope(), path).unwrap();
        assert_eq!(
            intrinsic_contract(&db, intrinsic).unwrap().memory,
            Some(&[][..]),
            "{path}"
        );
    }
}

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
    let copy_mem = resolve_lib_func_path(&db, func.scope(), "core::ptr::copy_mem")
        .expect("failed to resolve core::ptr::copy_mem");
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
        runtime_builtin_func_kind(&db, copy_mem),
        Some(RuntimeBuiltinFuncKind::Mcopy)
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
            target: IntrinsicMemoryTarget::Pointee(0),
            kind: MemoryAccessKind::Read,
            extent: IntrinsicMemoryExtent::Bytes(32),
        }]
    );
    assert_eq!(
        intrinsic_contract(&db, copy_mem)
            .expect("memory-copy intrinsic contract")
            .memory
            .expect("memory-copy memory contract"),
        &[
            IntrinsicMemoryAccess {
                target: IntrinsicMemoryTarget::Pointee(1),
                kind: MemoryAccessKind::Read,
                extent: IntrinsicMemoryExtent::Argument(2),
            },
            IntrinsicMemoryAccess {
                target: IntrinsicMemoryTarget::Pointee(0),
                kind: MemoryAccessKind::Write,
                extent: IntrinsicMemoryExtent::Argument(2),
            },
        ]
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
                target: IntrinsicMemoryTarget::Value(0),
                kind: MemoryAccessKind::MutAccess,
                extent: IntrinsicMemoryExtent::Typed,
            },
            IntrinsicMemoryAccess {
                target: IntrinsicMemoryTarget::Pointee(1),
                kind: MemoryAccessKind::Write,
                extent: IntrinsicMemoryExtent::Bytes(32),
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

#[test]
fn host_io_memory_contracts_require_standard_library_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "host_io_contracts.fe".into(),
        r#"
extern { fn getchar() -> i32 fn putchar(c: i32) -> i32 fn abs(value: i32) -> i32 }
mod std { pub mod io { extern { pub fn getchar() -> i32 pub fn putchar(c: i32) -> i32 } } }
fn anchor() {}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for func in module.all_funcs(&db) {
        assert!(intrinsic_contract(&db, *func).is_none());
    }

    // Resolve via a separate module so the adversarial local `std` cannot shadow
    // the real standard library when selecting the positive controls.
    let file = db.new_stand_alone("host_io_trusted.fe".into(), "fn trusted() {}");
    let (trusted, _) = db.top_mod(file);
    db.assert_no_diags(trusted);
    for path in ["std::io::getchar", "std::io::putchar"] {
        let func = resolve_lib_func_path(&db, trusted.all_funcs(&db)[0].scope(), path).unwrap();
        let contract = intrinsic_contract(&db, func).expect("trusted host I/O contract");
        assert_eq!(contract.memory, Some(&[][..]), "{path}");
        assert_eq!(contract.pointer_return, None, "{path}");
    }
}
