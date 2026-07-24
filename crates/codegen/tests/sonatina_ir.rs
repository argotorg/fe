//! Snapshot tests for Sonatina IR output.
//!
//! These tests compile Fe fixtures to Sonatina IR and snapshot the human-readable
//! IR text. This helps catch IR lowering bugs and makes it easy to review what
//! IR is generated for each fixture.
//!
//! Snapshots are stored in `fixtures/sonatina_ir/`.

use common::InputDb;
use dir_test::{Fixture, dir_test};
use driver::DriverDataBase;
use fe_codegen::emit_module_sonatina_ir;
use std::{collections::HashSet, path::Path};
use test_utils::_macro_support::_insta::{self, Settings};
use tracing::{info, warn};
use url::Url;

fn with_top_mod_for_source<T>(
    name: &str,
    source: &str,
    f: impl for<'db> FnOnce(&'db DriverDataBase, hir::hir_def::TopLevelMod<'db>) -> T,
) -> T {
    let mut db = DriverDataBase::default();
    let file_url = Url::parse(&format!("file:///{name}")).expect("test URL should parse");
    db.workspace()
        .touch(&mut db, file_url.clone(), Some(source.to_string()));
    let file = db
        .workspace()
        .get(&db, &file_url)
        .expect("file should be loaded");
    let top_mod = db.top_mod(file);
    f(&db, top_mod)
}

fn sonatina_function_names(ir: &str) -> Vec<String> {
    ir.lines()
        .filter_map(|line| {
            let rest = line.trim_start().strip_prefix("func ")?;
            let (_, rest) = rest.split_once('%')?;
            let end = rest.find('(')?;
            Some(rest[..end].to_string())
        })
        .collect()
}

fn sonatina_function_body<'a>(ir: &'a str, symbol_segment: &str) -> Option<&'a str> {
    let header = ir
        .lines()
        .find(|line| line.starts_with("func ") && line.contains(symbol_segment))?;
    let start = ir.find(header)?;
    let body = &ir[start..];
    let end = body.find("\n}\n").map_or(body.len(), |end| end + 2);
    Some(&body[..end])
}

#[test]
fn zero_sized_const_aggregates_do_not_emit_const_regions() {
    let ir = with_top_mod_for_source(
        "zero_sized_const_aggregates_do_not_emit_const_regions.fe",
        r#"
struct Empty {
}

pub fn new_empty() -> Empty {
    Empty {}
}
"#,
        |db, top_mod| emit_module_sonatina_ir(db, top_mod).expect("Sonatina IR should emit"),
    );

    assert!(
        !ir.contains("global private const"),
        "zero-sized const aggregate should not emit a const global:\n{ir}"
    );
    assert!(
        !ir.contains("const.ref"),
        "zero-sized const aggregate should not emit a const ref:\n{ir}"
    );
    assert!(
        !ir.contains("data $const_region"),
        "zero-sized const aggregate should not emit section data:\n{ir}"
    );
}

#[test]
fn assert_macro_message_lowers_to_direct_revert_payload() {
    let ir = with_top_mod_for_source(
        "assert_macro_message_lowers_to_direct_revert_payload.fe",
        r#"
pub fn main() {
    assert!(false, "boom")
}
"#,
        |db, top_mod| emit_module_sonatina_ir(db, top_mod).expect("Sonatina IR should emit"),
    );

    assert!(
        ir.contains("evm_revert") && ir.contains("100.i256"),
        "`assert!` with a message should lower to direct Solidity Error(string) revert data:\n{ir}"
    );
}

#[test]
fn sonatina_function_names_disambiguate_module_conflicts() {
    let ir = with_top_mod_for_source(
        "sonatina_function_names_disambiguate_module_conflicts.fe",
        r#"
pub mod left {
    pub fn same() -> u8 {
        1
    }
}

pub mod right {
    pub fn same() -> u8 {
        2
    }
}

pub fn main() -> u8 {
    left::same() + right::same()
}
"#,
        |db, top_mod| emit_module_sonatina_ir(db, top_mod).expect("Sonatina IR should emit"),
    );

    let names = sonatina_function_names(&ir);
    let unique_names = names.iter().collect::<HashSet<_>>();
    assert_eq!(
        names.len(),
        unique_names.len(),
        "Sonatina function names must be unique across source modules:\n{ir}"
    );
    assert!(
        names
            .iter()
            .filter(|name| name.ends_with("__same") || name.contains("__same_"))
            .all(|name| name.contains("__left__same") || name.contains("__right__same")),
        "colliding module functions should include their module paths:\n{ir}"
    );
}

#[test]
fn sonatina_function_names_disambiguate_generic_specializations() {
    let ir = with_top_mod_for_source(
        "sonatina_function_names_disambiguate_generic_specializations.fe",
        r#"
fn identity<T>(_ value: own T) -> T {
    value
}

fn bool_score(value: bool) -> u32 {
    if value {
        1
    } else {
        0
    }
}

pub fn main() -> u32 {
    identity(7) + bool_score(identity(true))
}
"#,
        |db, top_mod| emit_module_sonatina_ir(db, top_mod).expect("Sonatina IR should emit"),
    );

    let names = sonatina_function_names(&ir);
    let unique_names = names.iter().collect::<HashSet<_>>();
    assert_eq!(
        names.len(),
        unique_names.len(),
        "Sonatina function names must be unique across generic specializations:\n{ir}"
    );
    assert!(
        names
            .iter()
            .filter(|name| name.starts_with("identity"))
            .all(|name| name.starts_with("identity__g")),
        "colliding generic specializations should include generic identity components:\n{ir}"
    );
}

#[test]
fn first_class_pointer_fixture_lowers_to_sonatina_ir() {
    let ir = with_top_mod_for_source(
        "pointer_first_class.fe",
        include_str!("fixtures/pointer_first_class.fe"),
        |db, top_mod| emit_module_sonatina_ir(db, top_mod).expect("Sonatina IR should emit"),
    );

    assert!(
        ir.contains("evm_malloc") && ir.contains("mstore") && ir.contains("mload"),
        "first-class pointer fixture should lower through memory operations:\n{ir}"
    );
}

#[test]
fn wildcard_storage_map_root_reports_runtime_root_error() {
    let err = with_top_mod_for_source(
        "wildcard_storage_map_root_reports_runtime_root_error.fe",
        r#"
use std::evm::StorageMap

pub fn main() -> u256
    uses (balances: mut StorageMap<u256, u256>)
{
    balances.get(key: 1)
}
"#,
        |db, top_mod| {
            emit_module_sonatina_ir(db, top_mod)
                .expect_err("wildcard StorageMap roots should be rejected")
        },
    );
    let message = err.to_string();
    assert!(
        message.contains("standalone runtime root")
            && message.contains("inferred layout const")
            && message.contains("no caller to supply a concrete provider")
            && message.contains("with (...)"),
        "unexpected error message:\n{message}"
    );
}

#[test]
fn explicit_storage_map_root_compiles_without_a_runtime_provider() {
    let output = with_top_mod_for_source(
        "explicit_storage_map_root_compiles_without_a_runtime_provider.fe",
        r#"
use std::evm::StorageMap

pub fn main() -> u256
    uses (balances: mut StorageMap<u256, u256, 0>)
{
    balances.get(key: 1)
}
"#,
        |db, top_mod| emit_module_sonatina_ir(db, top_mod).expect("explicit root should compile"),
    );
    assert!(
        output.contains("call %storagemap_get_word_with_salt v0 0.i256"),
        "explicit root was not lowered as the concrete StorageMap salt:\n{output}"
    );
}

#[test]
fn persistent_layout_maps_lower_with_checked_projection_control_flow() {
    let output = with_top_mod_for_source(
        "persistent_layout_maps_lower_with_checked_projection_control_flow.fe",
        r#"
struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Copy for Rooted<ROOT> {}

impl<const ROOT: u256> Rooted<ROOT> {
    fn root(self) -> u256 {
        ROOT
    }
}

fn repeat<const ROOT: u256>(value: Rooted<ROOT>) -> [Rooted<ROOT>; 3] {
    [value; 3]
}

fn reorder<const ROOT: u256>(values: [Rooted<ROOT>; 3]) -> [Rooted<ROOT>; 3] {
    [values[2], values[0], values[1]]
}

fn fresh<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

fn replace<const ROOT: u256>(
    values: own [Rooted<ROOT>; 3],
    lane: usize,
) -> [Rooted<ROOT>; 3] {
    let mut result = values
    result[lane] = fresh()
    result
}

fn patch<const ROOT: u256>(
    values: own [Rooted<ROOT>; 3],
    lane: usize,
    replacement: Rooted<ROOT>,
) -> [Rooted<ROOT>; 3] {
    let mut result = values
    result[lane] = replacement
    result
}

fn select<const ROOT: u256>(values: [Rooted<ROOT>; 3], lane: usize) -> Rooted<ROOT> {
    values[lane]
}

msg Msg {
    #[selector = 1]
    Get { lane: usize } -> u256,
}

pub contract C {
    mut values: [Rooted; 3],

    recv Msg {
        Get { lane } -> u256 uses (values) {
            let repeated = repeat(value: values[0])
            let reordered = reorder(values: repeated)
            let replaced = replace(
                values: reordered,
                lane: lane,
            )
            let patched = patch(
                values: replaced,
                lane: lane,
                replacement: values[0],
            )
            select(values: patched, lane: lane).root()
        }
    }
}
"#,
        |db, top_mod| emit_module_sonatina_ir(db, top_mod).expect("layout maps should lower"),
    );

    assert!(
        output.contains("br_table") && output.contains("evm_revert") && output.contains("lt "),
        "layout-map projection must dispatch safely after a bounds check:\n{output}"
    );
    assert!(
        output.matches("evm_malloc").count() >= 4,
        "persistent layout-map constructors must allocate their nodes:\n{output}"
    );
    let patch = sonatina_function_body(&output, "patch").unwrap_or_else(|| {
        panic!(
            "expected the layout-map patch regression function; functions={:?}",
            sonatina_function_names(&output)
        )
    });
    assert!(
        patch.contains("lt ") && patch.contains("evm_revert"),
        "layout-map patches must enforce their own index bounds:\n{patch}"
    );
    assert!(
        !patch.contains("br_table"),
        "a terminal layout-map patch should not project and dispatch through its source:\n{patch}"
    );
}

#[test]
fn inferred_storage_map_roots_skip_explicit_contract_salts() {
    let output = with_top_mod_for_source(
        "inferred_storage_map_roots_skip_explicit_contract_salts.fe",
        r#"
use std::evm::StorageMap

msg M {
    #[selector = 0x01]
    X { key: u256 } -> u256,
    #[selector = 0x02]
    Y { key: u256 } -> u256,
    #[selector = 0x03]
    Z { key: u256 } -> u256,
}

pub contract C {
    mut x: StorageMap<u256, u256, 1>,
    mut y: StorageMap<u256, u256>,
    mut z: StorageMap<u256, u256>,

    recv M {
        X { key } -> u256 uses x { x.get(key) }
        Y { key } -> u256 uses y { y.get(key) }
        Z { key } -> u256 uses z { z.get(key) }
    }
}
"#,
        |db, top_mod| {
            emit_module_sonatina_ir(db, top_mod)
                .expect("mixed explicit and inferred roots should compile")
        },
    );
    for salt in ["0.i256", "1.i256", "2.i256"] {
        assert!(
            output.lines().any(|line| {
                line.contains("call %storagemap_get_word_with_salt") && line.contains(salt)
            }),
            "missing StorageMap salt {salt}:\n{output}"
        );
    }
}

#[test]
fn wildcard_storage_map_free_function_compiles_with_concrete_provider() {
    let output = with_top_mod_for_source(
        "wildcard_storage_map_free_function_compiles_with_concrete_provider.fe",
        r#"
use std::evm::StorageMap

fn get_balance(addr: u256) -> u256
    uses (balances: StorageMap<u256, u256>)
{
    balances.get(key: addr)
}

msg M {
    #[selector = 0]
    Get -> u256,
}

contract C {
    balances: StorageMap<u256, u256>

    recv M {
        Get -> u256 uses (balances) {
            with (balances) {
                get_balance(addr: 1)
            }
        }
    }
}
"#,
        |db, top_mod| {
            emit_module_sonatina_ir(db, top_mod)
                .expect("wildcard StorageMap helpers should compile from a concrete provider")
        },
    );
    assert!(
        output.contains("func private %get_balance") && output.contains("object @C"),
        "concrete-provider StorageMap helper should emit real Sonatina IR:\n{output}"
    );
}

#[test]
fn generic_noesc_storage_specialization_is_rejected_during_runtime_lowering() {
    let err = with_top_mod_for_source(
        "generic_noesc_storage_specialization_is_rejected_during_runtime_lowering.fe",
        r#"
struct Box<T> {
    value: T,
}

fn store_generic<T>(value: own T) uses (slot: mut Box<T>) {
    slot = Box<T> { value }
}

pub contract GenericNoEsc {
    mut slot: Box<mut u256>

    init() uses (mut slot) {
        let mut x: u256 = 0
        store_generic<mut u256>(mut x)
    }
}
"#,
        |db, top_mod| {
            emit_module_sonatina_ir(db, top_mod)
                .expect_err("runtime lowering should reject specialized noesc storage escape")
        },
    );
    let message = err.to_string();
    assert!(
        message.contains("semantic noesc checking failed")
            && message.contains("noesc violation in `fn store_generic`"),
        "unexpected error message:\n{message}"
    );
}

#[test]
fn sonatina_ir_rejects_target_only_output() {
    let err = with_top_mod_for_source(
        "sonatina_ir_rejects_target_only_output.fe",
        r#"
fn helper(value: u256) -> u256 {
    value
}
"#,
        |db, top_mod| {
            emit_module_sonatina_ir(db, top_mod).expect_err("empty packages should not emit IR")
        },
    );
    let message = err.to_string();
    assert!(
        message.contains("no root objects") && message.contains("target-only Sonatina IR"),
        "unexpected error message:\n{message}"
    );
}

#[test]
fn raw_log_emit_sonatina_ir_lowers_native_pointer_provider() {
    let output = with_top_mod_for_source(
        "raw_log_emit_sonatina_ir_lowers_mem_ptr.fe",
        include_str!("fixtures/raw_log_emit.fe"),
        |db, top_mod| {
            emit_module_sonatina_ir(db, top_mod)
                .expect("native pointer providers should lower for Sonatina")
        },
    );

    assert!(
        output.contains("func private %raw_emit") && output.contains("object @main"),
        "raw_log_emit should emit real Sonatina IR, not target-only output:\n{output}"
    );
}

#[test]
fn constant_oob_index_terminates_without_continuation_projection() {
    let output = with_top_mod_for_source(
        "constant_oob_index_terminates_without_continuation_projection.fe",
        r#"
fn main() -> u256 {
    let arr: [u256; 2] = [10, 20]
    return arr[2]
}
"#,
        |db, top_mod| {
            emit_module_sonatina_ir(db, top_mod)
                .expect("constant out-of-bounds array access should lower to a revert")
        },
    );

    assert!(
        output.contains("evm_revert 0.i256 0.i256"),
        "constant out-of-bounds array access should lower to a revert:\n{output}"
    );
    assert!(
        !output.contains("br 1.i1"),
        "constant out-of-bounds array access should not emit a conditional true branch plus continuation:\n{output}"
    );
    assert!(
        !output.contains("obj_index") && !output.contains("const_index"),
        "constant out-of-bounds array access should not continue into index projection IR:\n{output}"
    );
}

#[test]
fn semantic_never_returning_recv_returns_emit_sonatina_ir() {
    let output = with_top_mod_for_source(
        "semantic_never_returning_recv_returns_emit_sonatina_ir.fe",
        r#"
use core::abi::Bytes

msg Msg {
    #[selector = 0x01]
    GetBytes -> Bytes,
    #[selector = 0x02]
    GetScalar -> u256,
}

fn abort() -> ! {
    core::panic()
}

pub contract C {
    recv Msg {
        GetBytes -> Bytes {
            abort()
        }

        GetScalar -> u256 {
            abort()
        }
    }
}
"#,
        |db, top_mod| {
            emit_module_sonatina_ir(db, top_mod)
                .expect("semantic never-returning recv arms should emit Sonatina IR")
        },
    );

    assert!(
        output.contains("object @C") && output.contains("evm_invalid"),
        "never-returning recv arms should lower to real terminating IR:\n{output}"
    );
}

// NOTE: `dir_test` discovers fixtures at compile time; new fixture files will be picked up on a
// clean build (e.g. CI) or whenever this test target is recompiled.
//
// Sonatina IR tests only run on fixtures that the backend currently supports. Unsupported
// fixtures will produce LowerError::Unsupported, which we skip gracefully.
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures",
    glob: "*.fe"
)]
fn sonatina_ir_snap(fixture: Fixture<&str>) {
    let _logging = test_utils::setup_test_tracing();
    let mut db = DriverDataBase::default();
    let file_url = Url::from_file_path(fixture.path()).expect("fixture path should be absolute");
    db.workspace().touch(
        &mut db,
        file_url.clone(),
        Some(fixture.content().to_string()),
    );
    let file = db
        .workspace()
        .get(&db, &file_url)
        .expect("file should be loaded");
    let top_mod = db.top_mod(file);

    let output = match emit_module_sonatina_ir(&db, top_mod) {
        Ok(ir) => ir,
        Err(fe_codegen::LowerError::Unsupported(msg)) => {
            info!("SKIP {}: unsupported ({msg})", fixture.path());
            return;
        }
        Err(fe_codegen::LowerError::Internal(msg)) => {
            warn!("SKIP {}: internal error ({msg})", fixture.path());
            return;
        }
        Err(err) => panic!("Sonatina IR lowering failed: {err}"),
    };

    // Store snapshots in the sonatina_ir/ subdirectory.
    let fixture_path = Path::new(fixture.path());
    let fixture_name = fixture_path.file_stem().unwrap().to_str().unwrap();
    let snapshot_dir = fixture_path.parent().unwrap().join("sonatina_ir");

    let mut settings = Settings::new();
    settings.set_snapshot_path(snapshot_dir);
    settings.set_input_file(fixture.path());
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| {
        _insta::assert_snapshot!(fixture_name, output);
    });
}

/// End-to-end guard for the generic-storage-field aliasing fix: a hole-bearing
/// storage type passed as one generic argument and reused by two struct fields
/// (`struct Pair<T> { left: T, right: T }` as `Pair<StorageMap<..>>`) must lower
/// to *distinct* storage roots. Before the fix both fields shared one root,
/// silently merging their storage in deployed bytecode.
#[test]
fn repeated_generic_storage_fields_lower_to_distinct_slots() {
    let ir = with_top_mod_for_source(
        "repeated_generic_storage_fields_lower_to_distinct_slots.fe",
        r#"
use std::evm::{Address, StorageMap, StorPtr}

struct Pair<T> {
    left: T,
    right: T,
}

msg Msg {
    #[selector = 1]
    Swap { user: Address, amount: u256 } -> u256,
}

pub contract C {
    mut pair: StorPtr<Pair<StorageMap<Address, u256>>>,

    recv Msg {
        Swap { user, amount } -> u256 uses (mut pair) {
            pair.left.set(key: user, value: amount)
            pair.right.get(key: user)
        }
    }
}
"#,
        |db, top_mod| emit_module_sonatina_ir(db, top_mod).expect("Sonatina IR should emit"),
    );

    // `pair.left.set` and `pair.right.get` each lower to a storage-map access
    // salted by the field's root slot. The two salts must differ. The salt is
    // the literal `N.i256` operand on the `call %storagemap_*_with_salt` line.
    let salt = |op: &str| -> String {
        let needle = format!("call %storagemap_{op}_word_with_salt");
        let line = ir
            .lines()
            .find(|l| l.contains(&needle))
            .unwrap_or_else(|| panic!("missing {needle}:\n{ir}"));
        line.split_whitespace()
            .find_map(|tok| {
                tok.trim_end_matches(';')
                    .strip_suffix(".i256")
                    .filter(|n| n.parse::<u64>().is_ok())
            })
            .unwrap_or_else(|| panic!("no salt literal on line `{line}`"))
            .to_string()
    };
    let set_salt = salt("set");
    let get_salt = salt("get");
    assert_ne!(
        set_salt, get_salt,
        "left/right storage roots aliased (both salt {set_salt})"
    );
}
