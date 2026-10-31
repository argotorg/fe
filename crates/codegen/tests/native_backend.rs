#![cfg(all(
    feature = "cranelift",
    any(target_arch = "x86_64", target_arch = "aarch64")
))]

use common::{InputDb, compilation::Target};
use driver::DriverDataBase;
use fe_codegen::{
    NativeOutputKind, NativeOutputSelection, NativeTarget, OptLevel, emit_module_native_artifacts,
};
use mir::{build_native_package, runtime::format_runtime_package};
use salsa::Setter;
use url::Url;

fn with_top_mod_for_source<T>(
    name: &str,
    source: &str,
    f: impl for<'db> FnOnce(&'db DriverDataBase, hir::hir_def::TopLevelMod<'db>) -> T,
) -> T {
    let mut db = DriverDataBase::default();
    db.compilation_settings()
        .set_target(&mut db)
        .to(Target::Native);
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
    assert_eq!(
        ir.lines()
            .filter(|line| line.contains(" = call %") && line.contains("__main"))
            .count(),
        3,
        "missing calls:\n{ir}"
    );
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
extern {
    fn putchar(_ c: i32) -> i32
}
mod local { pub fn putchar(value: i32) -> i32 { value + 1 } }
pub fn main() -> i32 {
    let _ = unsafe { putchar(65) }
    local::putchar(value: 41)
}
"#,
        |db, top_mod| fe_codegen::emit_module_native_ir(db, top_mod, fe_codegen::OptLevel::O0),
    )
    .expect("native imports should retain their host symbol");
    assert!(
        ir.contains("declare external %putchar(i32) -> i32"),
        "missing host import:\n{ir}"
    );
    assert!(
        ir.contains("local__putchar"),
        "missing qualified local helper:\n{ir}"
    );
}

#[test]
fn native_effect_place_is_one_object_across_calls() {
    let ir = with_top_mod_for_source(
        "native_effect_place.fe",
        r#"
struct Handle { value: i32, calls: i32 }
fn step() uses (handle: mut Handle) { handle.calls += 1 }
pub fn main() -> i32 {
    var handle = Handle { value: 20, calls: 0 }
    with (handle) {
        step()
        step()
    }
    handle.calls
}
"#,
        |db, top_mod| fe_codegen::emit_module_native_ir(db, top_mod, fe_codegen::OptLevel::O0),
    )
    .expect("native effect place IR");
    let roots = ir
        .lines()
        .filter(|line| line.contains(" = obj.alloc "))
        .map(|line| line.trim().split_once('.').expect("object local").0)
        .collect::<Vec<_>>();
    let arguments = ir
        .lines()
        .filter(|line| line.contains("call %step "))
        .map(|line| {
            line.rsplit_once(' ')
                .expect("effect argument")
                .1
                .trim_end_matches(';')
        })
        .collect::<Vec<_>>();
    assert_eq!(arguments.len(), 2, "two provider calls:\n{ir}");
    assert_eq!(arguments[0], arguments[1], "one provider identity:\n{ir}");
    assert!(
        roots.contains(&arguments[0]),
        "provider uses object storage:\n{ir}"
    );
}

#[test]
fn native_trait_effect_calls_share_the_captured_object_before_optimization() {
    let ir = with_top_mod_for_source(
        "native_provider_identity.fe",
        r#"
trait Tick { fn next(mut self) -> i32 }
struct Counter { value: i32 }
impl Tick for Counter {
    fn next(mut self) -> i32 {
        self.value += 1
        self.value
    }
}
fn counter() -> Counter { Counter { value: 0 } }
fn step_effect() -> i32 uses (tick: mut Tick) { tick.next() }
pub fn main() -> i32 {
    with (Tick = counter()) { step_effect() + step_effect() }
}
"#,
        |db, top_mod| fe_codegen::emit_module_native_ir(db, top_mod, fe_codegen::OptLevel::O0),
    )
    .expect("native provider IR");
    let main = ir
        .split_once("func public %main()")
        .expect("main function")
        .1
        .split_once("\n}")
        .expect("main body")
        .0;
    let roots = main
        .lines()
        .filter(|line| line.contains(" = obj.alloc "))
        .map(|line| line.trim().split_once('.').expect("object local").0)
        .collect::<Vec<_>>();
    assert_eq!(roots.len(), 1, "one captured provider object:\n{main}");
    let arguments = main
        .lines()
        .filter(|line| line.contains(" = call %") && line.contains("step_effect"))
        .map(|line| {
            line.rsplit_once(' ')
                .expect("effect argument")
                .1
                .trim_end_matches(';')
        })
        .collect::<Vec<_>>();
    assert_eq!(arguments, vec![roots[0], roots[0]], "{main}");
}

#[test]
fn native_argv_entry_has_a_host_pointer_bridge() {
    for mode in ["", "own "] {
        let ir = with_top_mod_for_source(
            "native_argv.fe",
            &format!("pub fn main(argc: {mode}i32, argv: **u8) -> i32 {{ argc }}"),
            |db, top_mod| fe_codegen::emit_module_native_ir(db, top_mod, fe_codegen::OptLevel::O0),
        )
        .unwrap();
        assert!(ir.contains("func public %main("), "{ir}");
        // The host vector is a `**u8` as Fe lays it out, so it passes as is.
        assert!(ir.contains("**i8"), "{ir}");
        assert!(ir.contains("bitcast"), "{ir}");
        assert!(!ir.contains("ptr_to_int"), "{ir}");
        assert!(!ir.contains("mem.alloc_dynamic"), "{ir}");
    }
}

#[test]
fn native_entry_checks_fe_types_before_accepting_the_lowered_abi() {
    for signature in [
        "(argc: i32, argv: u256) -> i32",
        "(argc: u32, argv: **u8) -> i32",
        "(argc: i32, argv: *u8) -> i32",
        "(argc: i32, argv: **u16) -> i32",
        "() -> u32",
    ] {
        let error = with_top_mod_for_source(
            "invalid_native_argv.fe",
            &format!("pub fn main{signature} {{ 0 }}"),
            |db, top_mod| {
                fe_codegen::emit_module_native_object(db, top_mod, fe_codegen::OptLevel::O0)
            },
        )
        .expect_err("Fe type distinctions must survive entry validation");
        assert!(
            error
                .to_string()
                .contains("native executable `main` must have signature"),
            "{signature}: {error}"
        );
    }
}

#[test]
fn native_external_functions_remain_declarations_with_exact_symbols() {
    let source = r#"
extern {
    fn getchar() -> i32
    fn putchar(_ c: i32) -> i32
}

mod helpers {
    pub fn getchar() -> i32 { 7 }
}

pub fn main() -> i32 {
    let _ = unsafe { putchar(getchar() + helpers::getchar()) }
    0
}
"#;
    with_top_mod_for_source("native_extern.fe", source, |db, top_mod| {
        let package = build_native_package(db, top_mod, NativeOutputKind::Executable)
            .expect("external declarations should not require runtime bodies");
        let text = format_runtime_package(db, &package);
        assert!(
            text.contains("external") && text.contains("declare () -> int32"),
            "missing external linkage:\n{text}"
        );
        let artifacts = emit_module_native_artifacts(
            db,
            top_mod,
            NativeTarget::Host,
            NativeOutputKind::Executable,
            OptLevel::O0,
            NativeOutputSelection {
                ir: true,
                object: true,
            },
        )
        .expect("external calls should compile to object relocations");
        let ir = artifacts.ir.unwrap();
        assert!(
            ir.contains("declare external %getchar() -> i32"),
            "missing input declaration:\n{ir}"
        );
        assert!(
            ir.contains("declare external %putchar(i32) -> i32"),
            "missing output declaration:\n{ir}"
        );
        assert!(
            ir.contains("helpers__getchar("),
            "helper must not rename the external symbol:\n{ir}"
        );
        assert!(!artifacts.object.unwrap().is_empty());
    });
}

#[test]
fn native_memory_addresses_are_pointers() {
    let ir = with_top_mod_for_source(
        "memory_pointers.fe",
        r#"
use core::ptr::MemVec
pub fn main() -> i32 {
    var values: MemVec<u64> = MemVec::new()
    values.push(1)
    values.push(2)
    if values.len() == 2 { 0 } else { 1 }
}
"#,
        |db, top_mod| fe_codegen::emit_module_native_ir(db, top_mod, fe_codegen::OptLevel::O0),
    )
    .expect("native memory pointer IR");
    // A vector holds a pointer, and an element's address steps from it.
    assert!(ir.contains(" = {*i8, i64, i64};"), "{ir}");
    assert!(ir.contains(" = gep "), "{ir}");
    // Only constants, such as null, become pointers from integers.
    for line in ir.lines().filter(|line| line.contains("int_to_ptr")) {
        assert!(!line.contains("int_to_ptr v"), "{line}\n{ir}");
    }
}

#[test]
fn native_access_parameters_are_noalias() {
    let ir = with_top_mod_for_source(
        "noalias_parameters.fe",
        r#"
use core::option::Option
struct Big { a: i32, b: i32, c: i32 }
struct Ctx { n: i32 }
impl Big {
    fn total(self) -> i32 { self.a + self.b + self.c }
    fn bump(mut self) { self.a += 1 }
}
fn tick() uses (ctx: mut Ctx) { ctx.n += 1 }
fn consume(var _ current: own Option<i32>) -> i32 {
    var sum = 0
    while let Option::Some(value) = current {
        sum += value
        current = if value == 0 { Option::None } else { Option::Some(value - 1) }
    }
    sum
}
pub fn main() -> i32 {
    var big = Big { a: 1, b: 2, c: 3 }
    big.bump()
    var ctx = Ctx { n: 0 }
    with (ctx) { tick() }
    big.total() + consume(Option::Some(3)) + ctx.n
}
"#,
        |db, top_mod| fe_codegen::emit_module_native_ir(db, top_mod, fe_codegen::OptLevel::O0),
    )
    .expect("native noalias parameter IR");
    let signature = |name: &str| {
        ir.lines()
            .find(|line| line.starts_with("func ") && line.contains(name))
            .unwrap_or_else(|| panic!("missing {name}:\n{ir}"))
    };
    // A view, `mut` or effect parameter is an access of its caller's place.
    for name in ["total", "bump", "tick"] {
        assert!(
            signature(name).contains("(noalias v0.objref<"),
            "{name}:\n{ir}"
        );
    }
    // An owned parameter is not, even when passed by reference.
    let consume = signature("consume");
    assert!(
        consume.contains("(v0.objref<") && !consume.contains("noalias"),
        "{ir}"
    );
}

#[test]
fn native_exports_are_public_under_their_own_names() {
    let source = r#"
mod math {
    pub fn add(_ a: i64, _ b: i64) -> i64 { a - b }
}

#[export]
pub fn add(_ a: i64, _ b: i64) -> i64 { a + b }

#[export]
pub fn store(_ p: *i64, _ value: i64) {
    unsafe { *p = value }
}

pub fn main() -> i32 {
    if add(2, 1) == math::add(4, 1) { 0 } else { 1 }
}
"#;
    for level in [OptLevel::O0, OptLevel::O2] {
        let ir = with_top_mod_for_source("native_exports.fe", source, |db, top_mod| {
            fe_codegen::emit_module_native_ir(db, top_mod, level)
        })
        .expect("native exports should compile");
        // An optimized module keeps them, unused and with their signatures.
        assert!(
            ir.contains("func public %add(v0.i64, v1.i64) -> i64"),
            "{level:?}:\n{ir}"
        );
        assert!(
            ir.contains("func public %store(v0.*i8, v1.i64)"),
            "{level:?}:\n{ir}"
        );
        assert!(
            ir.contains("func public %main() -> i32"),
            "{level:?}:\n{ir}"
        );
        if level == OptLevel::O0 {
            assert!(ir.contains("math__add(v0.i64, v1.i64)"), "{ir}");
        }
    }
}

#[test]
fn native_exports_reject_symbols_and_values_c_cannot_share() {
    for (source, error) in [
        (
            "#[export]\npub fn narrow(_ a: u8, _ b: bool) -> u8 { a }",
            "exported function `narrow` must take and return i32/i64 or raw-pointer ABI values, found lowered `narrow(i8, i1) -> i8`",
        ),
        (
            "#[export]\npub fn wide(_ a: u256) -> u256 { a }",
            "found lowered `wide(i256) -> i256`",
        ),
        (
            "#[export]\npub fn bump(_ a: mut i64) { a += 1 }",
            "found lowered `bump(objref<i64>)`",
        ),
        (
            "#[export]\npub fn write(_ a: i64) -> i64 { a }",
            "exported symbol `write` is reserved: native code calls the C library's `write`",
        ),
        (
            "#[export]\npub fn __fe_answer() -> i64 { 42 }",
            "exported symbol `__fe_answer` is reserved: symbols starting with `__fe_` are the compiler's",
        ),
        (
            "mod a {\n    #[export]\n    pub fn same() -> i64 { 1 }\n}\n#[export]\npub fn same() -> i64 { 2 }",
            "`native_exports_reject::a::same` and `native_exports_reject::same` both define the native symbol `same`",
        ),
        (
            "mod cli {\n    #[export]\n    pub fn main() -> i32 { 1 }\n}",
            "`native_exports_reject::main` and `native_exports_reject::cli::main` both define the native symbol `main`",
        ),
    ] {
        let source = format!("{source}\npub fn main() -> i32 {{ 0 }}\n");
        let message =
            with_top_mod_for_source("native_exports_reject.fe", &source, |db, top_mod| {
                fe_codegen::emit_module_native_ir(db, top_mod, OptLevel::O0)
            })
            .expect_err("an export C cannot share should be rejected")
            .to_string();
        assert!(message.contains(error), "{source}\n{message}");
    }
}
