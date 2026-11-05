use fe_hir::analysis::ty::corelib::{
    MemoryAccessKind, RuntimeBuiltinFuncKind, external_call_state_access, resolve_lib_func_path,
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
    let try_alloc = resolve_lib_func_path(&db, func.scope(), "core::ptr::try_alloc_raw")
        .expect("failed to resolve core::ptr::try_alloc_raw");
    let dealloc = resolve_lib_func_path(&db, func.scope(), "core::ptr::dealloc_raw")
        .expect("failed to resolve core::ptr::dealloc_raw");
    let ptr_eq = resolve_lib_func_path(&db, func.scope(), "core::ptr::addr_eq")
        .expect("failed to resolve core::ptr::addr_eq");
    let copy_mem = resolve_lib_func_path(&db, func.scope(), "core::ptr::copy_mem")
        .expect("failed to resolve core::ptr::copy_mem");
    let mload = resolve_lib_func_path(&db, func.scope(), "std::evm::ops::mload")
        .expect("failed to resolve std::evm::ops::mload");
    let revert_empty = resolve_lib_func_path(&db, func.scope(), "std::evm::ops::revert_empty")
        .expect("failed to resolve std::evm::ops::revert_empty");
    let panic = resolve_lib_func_path(&db, func.scope(), "core::panic")
        .expect("failed to resolve core::panic");
    let keccak = resolve_lib_func_path(&db, func.scope(), "core::intrinsic::__keccak256")
        .expect("failed to resolve core::intrinsic::__keccak256");
    let keccak_words = resolve_lib_func_path(&db, func.scope(), "core::intrinsic::__keccak_words")
        .expect("failed to resolve core::intrinsic::__keccak_words");

    assert_eq!(
        runtime_builtin_func_kind(&db, alloc),
        Some(RuntimeBuiltinFuncKind::Malloc)
    );
    assert_eq!(
        runtime_builtin_func_kind(&db, try_alloc),
        Some(RuntimeBuiltinFuncKind::TryMalloc)
    );
    assert_eq!(
        runtime_builtin_func_kind(&db, dealloc),
        Some(RuntimeBuiltinFuncKind::Free)
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
    assert_eq!(
        runtime_builtin_func_kind(&db, keccak_words),
        Some(RuntimeBuiltinFuncKind::IntrinsicKeccakWords)
    );
}

#[test]
fn external_executions_access_persistent_state() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("external_call_state_access.fe".into(), "fn f() {}");
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let scope = top_mod.all_funcs(&db)[0].scope();
    for (path, access) in [
        ("std::evm::ops::call", Some(MemoryAccessKind::Write)),
        ("std::evm::ops::delegatecall", Some(MemoryAccessKind::Write)),
        ("std::evm::ops::create2", Some(MemoryAccessKind::Write)),
        ("std::evm::ops::staticcall", Some(MemoryAccessKind::Read)),
        ("std::evm::ops::staticcall_precompile", None),
        ("std::evm::ops::mload", None),
        (
            "std::evm::effects::Call::raw_call",
            Some(MemoryAccessKind::Write),
        ),
        (
            "std::evm::effects::Call::raw_staticcall",
            Some(MemoryAccessKind::Read),
        ),
        (
            "std::evm::effects::Create::create_raw",
            Some(MemoryAccessKind::Write),
        ),
        ("std::evm::effects::RawStorage::sstore", None),
    ] {
        let func = resolve_lib_func_path(&db, scope, path)
            .unwrap_or_else(|| panic!("failed to resolve {path}"));
        assert_eq!(
            external_call_state_access(&db, func, None),
            access,
            "{path}"
        );
    }
}
