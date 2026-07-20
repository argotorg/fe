use fe_hir::analysis::ty::corelib::{
    RuntimeBuiltinFuncKind, resolve_lib_func_path, runtime_builtin_func_kind,
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

    let alloc = resolve_lib_func_path(&db, func.scope(), "std::evm::mem::alloc")
        .expect("failed to resolve std::evm::mem::alloc");
    let mload = resolve_lib_func_path(&db, func.scope(), "std::evm::ops::mload")
        .expect("failed to resolve std::evm::ops::mload");
    let panic = resolve_lib_func_path(&db, func.scope(), "core::panic")
        .expect("failed to resolve core::panic");
    let keccak = resolve_lib_func_path(&db, func.scope(), "core::intrinsic::__keccak256")
        .expect("failed to resolve core::intrinsic::__keccak256");
    let signextend = resolve_lib_func_path(&db, func.scope(), "std::evm::ops::signextend")
        .expect("failed to resolve std::evm::ops::signextend");

    assert_eq!(
        runtime_builtin_func_kind(&db, alloc),
        Some(RuntimeBuiltinFuncKind::Malloc)
    );
    assert_eq!(
        runtime_builtin_func_kind(&db, mload),
        Some(RuntimeBuiltinFuncKind::Mload)
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
        runtime_builtin_func_kind(&db, signextend),
        Some(RuntimeBuiltinFuncKind::SignExtend)
    );
}
