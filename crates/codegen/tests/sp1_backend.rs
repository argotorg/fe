#![cfg(feature = "sp1")]

use common::{InputDb, compilation::Target};
use driver::DriverDataBase;
use fe_codegen::{NativeOutputSelection, NativeTarget, OptLevel, emit_module_native_artifacts};
use salsa::Setter;
use url::Url;

#[test]
fn sp1_guest_io_emits_rv64_object_with_external_adapters() {
    let mut db = DriverDataBase::default();
    db.compilation_settings()
        .set_target(&mut db)
        .to(Target::Native);
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///sp1_increment.fe").unwrap(),
        Some(include_str!("../../../examples/sp1_increment.fe").to_string()),
    );
    let top_mod = db.top_mod(file);
    for level in [OptLevel::O0, OptLevel::O2] {
        let artifacts = emit_module_native_artifacts(
            &db,
            top_mod,
            NativeTarget::Sp1,
            level,
            NativeOutputSelection {
                ir: true,
                object: true,
            },
        )
        .expect("SP1 guest should compile without a toolchain");
        let ir = artifacts.ir.unwrap();
        assert!(
            ir.contains("riscv64im-succinct-zkvm-elf"),
            "unexpected target:\n{ir}"
        );
        assert!(
            ir.contains("func public %main() -> i32"),
            "missing entry:\n{ir}"
        );
        for adapter in [
            "sys_sp1_read_u32",
            "sys_sp1_read_u64",
            "sys_sp1_commit_u32",
            "sys_sp1_commit_u64",
        ] {
            assert!(
                ir.contains(&format!("declare external %{adapter}(")),
                "missing {adapter}:\n{ir}"
            );
        }
        let object = artifacts.object.unwrap();
        assert_eq!(&object[..4], b"\x7fELF");
        assert_eq!(object[4], 2, "ELF64");
        assert_eq!(object[5], 1, "little-endian ELF");
        assert_eq!(
            u16::from_le_bytes([object[18], object[19]]),
            243,
            "EM_RISCV"
        );
        assert_eq!(
            u32::from_le_bytes(object[48..52].try_into().unwrap()),
            0,
            "RV64IM soft-float flags"
        );
    }
}

#[test]
fn sp1_memory_copy_reserves_the_runtime_symbol() {
    let mut db = DriverDataBase::default();
    db.compilation_settings()
        .set_target(&mut db)
        .to(Target::Native);
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///sp1_memory_copy.fe").unwrap(),
        Some(include_str!("../../fe/tests/fixtures/fe_test/raw_memory_copy.fe").to_string()),
    );
    for level in [OptLevel::O0, OptLevel::O2] {
        let artifacts = emit_module_native_artifacts(
            &db,
            db.top_mod(file),
            NativeTarget::Sp1,
            level,
            NativeOutputSelection {
                ir: true,
                object: false,
            },
        )
        .expect("SP1 memory copy must use the runtime import");
        let ir = artifacts.ir.unwrap();
        assert!(ir.contains("declare external %memmove("), "{ir}");
        assert!(!ir.contains("evm_mcopy"), "{ir}");
        if level == OptLevel::O0 {
            assert!(ir.contains("local__memmove("), "{ir}");
        }
        // Raw allocations are represented in IR, but the SP1 backend deliberately
        // rejects host heap allocation when translating to machine code.
        let error = emit_module_native_artifacts(
            &db,
            db.top_mod(file),
            NativeTarget::Sp1,
            level,
            NativeOutputSelection {
                ir: false,
                object: true,
            },
        )
        .expect_err("SP1 must reject unsupported host heap allocation");
        assert!(
            error
                .to_string()
                .contains("host heap allocation is unavailable on SP1"),
            "{error}"
        );
    }
}

#[test]
fn sp1_rejects_host_argv_entry() {
    let mut db = DriverDataBase::default();
    db.compilation_settings()
        .set_target(&mut db)
        .to(Target::Native);
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///sp1_argv.fe").unwrap(),
        Some("pub fn main(argc: i32, argv: **u8) -> i32 { argc }".to_string()),
    );
    let error = emit_module_native_artifacts(
        &db,
        db.top_mod(file),
        NativeTarget::Sp1,
        OptLevel::O0,
        NativeOutputSelection {
            ir: true,
            object: true,
        },
    )
    .expect_err("SP1 runtime requires a zero-argument entry");
    assert!(
        error
            .to_string()
            .contains("SP1 executable `main` must have signature `pub fn main() -> i32`"),
        "{error}"
    );
}

#[test]
fn sp1_keeps_entry_identity_when_helpers_are_named_main() {
    let mut db = DriverDataBase::default();
    db.compilation_settings()
        .set_target(&mut db)
        .to(Target::Native);
    let source = include_str!("../../fe/tests/fixtures/cli_output/native/entry_name_collision.fe")
        .to_string();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///sp1_entry.fe").unwrap(),
        Some(source),
    );
    let artifacts = emit_module_native_artifacts(
        &db,
        db.top_mod(file),
        NativeTarget::Sp1,
        OptLevel::O0,
        NativeOutputSelection {
            ir: true,
            object: true,
        },
    )
    .unwrap();
    let ir = artifacts.ir.unwrap();
    assert!(ir.contains("func public %main() -> i32"));
    assert_eq!(ir.matches(" = call %").count(), 3);
}
