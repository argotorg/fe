#![cfg(all(
    feature = "cranelift",
    any(target_arch = "x86_64", target_arch = "aarch64")
))]

use common::InputDb;
use driver::DriverDataBase;
use url::Url;

fn with_top_mod_for_source<T>(
    name: &str,
    source: &str,
    f: impl for<'db> FnOnce(&'db DriverDataBase, hir::hir_def::TopLevelMod<'db>) -> T,
) -> T {
    let mut db = DriverDataBase::default();
    let url = Url::parse(&format!("file:///{name}")).expect("test URL should parse");
    let file = db.workspace().touch(&mut db, url, Some(source.to_string()));
    let top_mod = db.top_mod(file);
    f(&db, top_mod)
}

#[test]
fn native_executable_roots_main_and_reachable_generic_helpers_only() {
    let ir = with_top_mod_for_source(
        "native_main_root.fe",
        r#"
pub fn unused<const N: usize>() -> usize {
    N
}

fn identity<T>(value: own T) -> T {
    value
}

pub fn main() -> i32 {
    identity(value: 42)
}
"#,
        |db, top_mod| fe_codegen::emit_module_native_ir(db, top_mod, fe_codegen::OptLevel::O0),
    )
    .expect("native executable should ignore unreachable generic roots");

    assert!(ir.contains("%main"), "missing main function:\n{ir}");
    assert!(ir.contains("identity"), "missing reachable helper:\n{ir}");
    assert!(
        !ir.contains("unused"),
        "found unreachable generic root:\n{ir}"
    );
}

#[test]
fn native_ir_uses_host_isa_and_target_neutral_signed_division() {
    let ir = with_top_mod_for_source(
        "native_div.fe",
        r#"
pub fn divide(left: i32, right: i32) -> i32 {
    left / right
}

pub fn main() -> i32 {
    divide(84, 2)
}
"#,
        |db, top_mod| fe_codegen::emit_module_native_ir(db, top_mod, fe_codegen::OptLevel::O0),
    )
    .expect("native IR emission should succeed");

    let expected_target = if cfg!(target_arch = "x86_64") {
        "x86_64-unknown-native"
    } else {
        "aarch64-unknown-native"
    };
    assert!(ir.contains(expected_target), "unexpected native IR:\n{ir}");
    assert!(ir.contains("sdiv"), "expected target-neutral sdiv:\n{ir}");
    assert!(
        !ir.contains("evm_sdiv"),
        "found EVM division in native IR:\n{ir}"
    );
}

#[test]
fn native_object_emission_requires_c_main_signature() {
    let error = with_top_mod_for_source(
        "invalid_main.fe",
        "pub fn main() -> u64 { 0 }\n",
        |db, top_mod| fe_codegen::emit_module_native_object(db, top_mod, fe_codegen::OptLevel::O0),
    )
    .expect_err("native object emission should reject a non-C main signature");

    assert!(
        error
            .to_string()
            .contains("native executable `main` must have signature `pub fn main() -> i32`"),
        "unexpected error: {error}"
    );
}

#[test]
fn native_object_emission_produces_host_object() {
    let object = with_top_mod_for_source(
        "native_object.fe",
        "pub fn main() -> i32 { 0 }\n",
        |db, top_mod| fe_codegen::emit_module_native_object(db, top_mod, fe_codegen::OptLevel::O1),
    )
    .expect("native object emission should succeed");

    assert!(!object.is_empty(), "native object must not be empty");
}

#[test]
fn native_object_emission_lowers_first_class_pointer_memzero() {
    let object = with_top_mod_for_source(
        "native_memzero.fe",
        r#"
pub fn zero_bytes(ptr: *u8, len: u256) {
    core::ptr::zero_bytes(ptr, len)
}

pub fn main() -> i32 {
    0
}
"#,
        |db, top_mod| fe_codegen::emit_module_native_object(db, top_mod, fe_codegen::OptLevel::O0),
    )
    .expect("native object emission should lower pointer memory zeroing");

    assert!(!object.is_empty(), "native object must not be empty");
}

#[test]
fn native_object_emission_uses_typed_pointer_memory_accesses() {
    let object = with_top_mod_for_source(
        "native_pointer_access.fe",
        r#"
pub fn replace_byte(ptr: *u8, value: u8) -> u8 {
    let previous = *ptr
    *ptr = value
    previous
}

pub fn main() -> i32 {
    0
}
"#,
        |db, top_mod| fe_codegen::emit_module_native_object(db, top_mod, fe_codegen::OptLevel::O0),
    )
    .expect("native object emission should lower typed pointer loads and stores");

    assert!(!object.is_empty(), "native object must not be empty");
}
