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
fn native_entry_symbol_is_reserved_when_reachable_helpers_are_named_main() {
    let ir = with_top_mod_for_source(
        "native_entry_collision.fe",
        include_str!("../../fe/tests/fixtures/cli_output/native/entry_name_collision.fe"),
        |db, top_mod| fe_codegen::emit_module_native_ir(db, top_mod, fe_codegen::OptLevel::O0),
    )
    .expect("native entry should retain its symbol when reachable helpers are named main");

    assert!(
        ir.contains("func public %main() -> i32"),
        "missing exported C main:\n{ir}"
    );
    assert_eq!(
        ir.lines()
            .filter(|line| line.starts_with("func ") && line.contains("__main("))
            .count(),
        3,
        "associated, trait, and module helpers should have qualified symbols:\n{ir}"
    );
    assert_eq!(ir.matches(" = call %").count(), 3, "missing calls:\n{ir}");
}

#[test]
fn native_executable_rejects_main_outside_the_root_scope() {
    let error = with_top_mod_for_source(
        "nested_main_only.fe",
        "mod nested { pub fn main() -> i32 { 0 } }\n",
        |db, top_mod| fe_codegen::emit_module_native_ir(db, top_mod, fe_codegen::OptLevel::O0),
    )
    .expect_err("a nested main is not the executable entry");

    assert!(
        error
            .to_string()
            .contains("requires `pub fn main() -> i32`"),
        "unexpected error: {error}"
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
fn native_import_symbols_are_reserved_from_local_helpers() {
    let ir = with_top_mod_for_source(
        "native_import_collision.fe",
        r#"
extern { fn abs(value: i32) -> i32 }
mod local { pub fn abs(value: i32) -> i32 { value + 1 } }
pub fn main() -> i32 { local::abs(value: abs(value: -6)) }
"#,
        |db, top_mod| fe_codegen::emit_module_native_ir(db, top_mod, fe_codegen::OptLevel::O0),
    )
    .expect("native imports should retain their host symbol");
    assert!(
        ir.contains("declare external %abs(i32) -> i32"),
        "missing host import:\n{ir}"
    );
    assert!(
        ir.contains("local__abs"),
        "missing qualified local helper:\n{ir}"
    );
}
