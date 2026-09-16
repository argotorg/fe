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
use fe_codegen::{OptLevel, emit_module_sonatina_ir, emit_module_sonatina_ir_optimized};
use std::{collections::HashSet, path::Path};
use test_utils::_macro_support::_insta::{self, Settings};
use tracing::{info, warn};
use url::Url;

fn with_top_mod_for_source<T>(
    fixture: &Fixture<&str>,
    f: impl for<'db> FnOnce(&'db DriverDataBase, hir::hir_def::TopLevelMod<'db>) -> T,
) -> T {
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

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "observability_preserves_creation_and_runtime_bytecode.fe")]
fn observability_preserves_creation_and_runtime_bytecode(fixture: Fixture<&str>) {
    with_top_mod_for_source(&fixture, |db, top_mod| {
        for level in [OptLevel::O0, OptLevel::O2] {
            let plain = fe_codegen::emit_module_sonatina_bytecode(db, top_mod, level, None)
                .expect("ordinary bytecode compilation");
            let observed = fe_codegen::emit_module_sonatina_bytecode_with_observability(
                db, top_mod, level, None,
            )
            .expect("observable bytecode compilation");
            assert!(!plain.is_empty());
            assert_eq!(
                plain.keys().collect::<Vec<_>>(),
                observed.keys().collect::<Vec<_>>()
            );
            for (name, ordinary) in &plain {
                let instrumented = &observed[name];
                assert!(!ordinary.deploy.is_empty());
                assert!(!ordinary.runtime.is_empty());
                assert_eq!(
                    ordinary.deploy, instrumented.deploy,
                    "creation: {name}, {level:?}"
                );
                assert_eq!(
                    ordinary.runtime, instrumented.runtime,
                    "runtime: {name}, {level:?}"
                );
                assert!(instrumented.deploy_observability.is_some());
                assert!(instrumented.runtime_observability.is_some());
                assert!(ordinary.deploy_observability.is_none());
                assert!(ordinary.runtime_observability.is_none());
            }
        }
    });
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

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "zero_sized_const_aggregates_do_not_emit_const_regions.fe")]
fn zero_sized_const_aggregates_do_not_emit_const_regions(fixture: Fixture<&str>) {
    let ir = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod).expect("Sonatina IR should emit")
    });

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

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "assert_macro_message_lowers_to_direct_revert_payload.fe")]
fn assert_macro_message_lowers_to_direct_revert_payload(fixture: Fixture<&str>) {
    let ir = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod).expect("Sonatina IR should emit")
    });

    assert!(
        ir.contains("evm_revert") && ir.contains("100.i256"),
        "`assert!` with a message should lower to direct Solidity Error(string) revert data:\n{ir}"
    );
}

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "sonatina_function_names_disambiguate_module_conflicts.fe")]
fn sonatina_function_names_disambiguate_module_conflicts(fixture: Fixture<&str>) {
    let ir = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod).expect("Sonatina IR should emit")
    });

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

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "sonatina_function_names_disambiguate_generic_specializations.fe")]
fn sonatina_function_names_disambiguate_generic_specializations(fixture: Fixture<&str>) {
    let ir = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod).expect("Sonatina IR should emit")
    });

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

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures", glob: "pointer_first_class.fe")]
fn first_class_pointer_fixture_lowers_to_sonatina_ir(fixture: Fixture<&str>) {
    let ir = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod).expect("Sonatina IR should emit")
    });

    assert!(
        ir.contains("evm_malloc") && ir.contains("mstore") && ir.contains("mload"),
        "first-class pointer fixture should lower through memory operations:\n{ir}"
    );
}

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "fixed_mem_buffer_exposes_constant_nonescaping_malloc_to_backend.fe")]
fn fixed_mem_buffer_exposes_constant_nonescaping_malloc_to_backend(fixture: Fixture<&str>) {
    let ir = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir_optimized(db, top_mod, OptLevel::O1, None)
            .expect("optimized Sonatina IR should emit")
    });

    assert!(
        ir.split("\n}\n")
            .any(|body| body.contains("evm_malloc 64.i256") && body.contains("evm_keccak256")),
        "fixed buffers must expose a constant malloc in the consuming function so backend \
         non-escape planning can place it in static scratch memory:\n{ir}"
    );
    assert!(
        ir.split("\n}\n").any(|body| {
            body.contains("evm_malloc 128.i256")
                && body.contains("evm_malloc 64.i256")
                && body.contains("evm_static_call")
        }),
        "fixed precompile input/output buffers must remain constant allocations in the consuming \
         function so backend non-escape planning can place them in static scratch memory:\n{ir}"
    );
}

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "wildcard_storage_map_root_reports_runtime_root_error.fe")]
fn wildcard_storage_map_root_reports_runtime_root_error(fixture: Fixture<&str>) {
    let err = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod)
            .expect_err("wildcard StorageMap roots should be rejected")
    });
    let message = err.to_string();
    assert!(
        message.contains("standalone runtime root")
            && message.contains("inferred layout const")
            && message.contains("no caller to supply a concrete provider")
            && message.contains("with (...)"),
        "unexpected error message:\n{message}"
    );
}

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "explicit_storage_map_root_compiles_without_a_runtime_provider.fe")]
fn explicit_storage_map_root_compiles_without_a_runtime_provider(fixture: Fixture<&str>) {
    let output = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod).expect("explicit root should compile")
    });
    assert!(
        output.contains("call %storagemap_get_word_with_salt v0 0.i256"),
        "explicit root was not lowered as the concrete StorageMap salt:\n{output}"
    );
}

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "persistent_layout_maps_lower_with_checked_projection_control_flow.fe")]
fn persistent_layout_maps_lower_with_checked_projection_control_flow(fixture: Fixture<&str>) {
    let output = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod).expect("layout maps should lower")
    });

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

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "inferred_storage_map_roots_skip_explicit_contract_salts.fe")]
fn inferred_storage_map_roots_skip_explicit_contract_salts(fixture: Fixture<&str>) {
    let output = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod)
            .expect("mixed explicit and inferred roots should compile")
    });
    for salt in ["0.i256", "1.i256", "2.i256"] {
        assert!(
            output.lines().any(|line| {
                line.contains("call %storagemap_get_word_with_salt") && line.contains(salt)
            }),
            "missing StorageMap salt {salt}:\n{output}"
        );
    }
}

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "wildcard_storage_map_free_function_compiles_with_concrete_provider.fe")]
fn wildcard_storage_map_free_function_compiles_with_concrete_provider(fixture: Fixture<&str>) {
    let output = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod)
            .expect("wildcard StorageMap helpers should compile from a concrete provider")
    });
    assert!(
        output.contains("func private %get_balance") && output.contains("object @C"),
        "concrete-provider StorageMap helper should emit real Sonatina IR:\n{output}"
    );
}

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "generic_noesc_storage_specialization_is_rejected_during_runtime_lowering.fe")]
fn generic_noesc_storage_specialization_is_rejected_during_runtime_lowering(
    fixture: Fixture<&str>,
) {
    let err = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod)
            .expect_err("runtime lowering should reject specialized noesc storage escape")
    });
    let message = err.to_string();
    assert!(
        message.contains("semantic noesc checking failed")
            && message.contains("noesc violation in `fn store_generic`"),
        "unexpected error message:\n{message}"
    );
}

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "sonatina_ir_rejects_target_only_output.fe")]
fn sonatina_ir_rejects_target_only_output(fixture: Fixture<&str>) {
    let err = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod).expect_err("empty packages should not emit IR")
    });
    let message = err.to_string();
    assert!(
        message.contains("no root objects") && message.contains("target-only Sonatina IR"),
        "unexpected error message:\n{message}"
    );
}

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures", glob: "raw_log_emit.fe")]
fn raw_log_emit_sonatina_ir_lowers_native_pointer_provider(fixture: Fixture<&str>) {
    let output = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod)
            .expect("native pointer providers should lower for Sonatina")
    });

    assert!(
        output.contains("func private %raw_emit") && output.contains("object @main"),
        "raw_log_emit should emit real Sonatina IR, not target-only output:\n{output}"
    );
}

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "constant_oob_index_terminates_without_continuation_projection.fe")]
fn constant_oob_index_terminates_without_continuation_projection(fixture: Fixture<&str>) {
    let output = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod)
            .expect("constant out-of-bounds array access should lower to a revert")
    });

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

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "semantic_never_returning_recv_returns_emit_sonatina_ir.fe")]
fn semantic_never_returning_recv_returns_emit_sonatina_ir(fixture: Fixture<&str>) {
    let output = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod)
            .expect("semantic never-returning recv arms should emit Sonatina IR")
    });

    assert!(
        output.contains("object @C") && output.contains("evm_invalid"),
        "never-returning recv arms should lower to real terminating IR:\n{output}"
    );
}

#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "runtime_abi_head_guard_matches_modern_solidity_signed_size_check.fe")]
fn runtime_abi_head_guard_matches_modern_solidity_signed_size_check(fixture: Fixture<&str>) {
    let output = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod).expect("Sonatina IR should emit")
    });

    assert!(
        output.lines().any(|line| {
            line.contains("call %validate_runtime_head") && line.contains("4.i256 32.i256")
        }),
        "runtime decoder should validate its one-word head after the selector:\n{output}"
    );
    assert!(
        output.lines().any(|line| {
            line.contains("call %validate_runtime_head") && line.contains("4.i256 0.i256")
        }),
        "zero-argument runtime decoder should retain its empty-head path:\n{output}"
    );

    let guard = sonatina_function_body(&output, "validate_runtime_head")
        .expect("Sol ABI should provide runtime head validation");
    let comparisons = guard
        .lines()
        .filter(|line| line.contains(" = lt ") || line.contains(" = slt "))
        .collect::<Vec<_>>();
    assert_eq!(
        comparisons.len(),
        1,
        "Sol runtime head validation should use one size comparison:\n{guard}"
    );
    assert!(
        comparisons[0].contains(" = slt "),
        "Sol runtime head validation should use Solidity's signed size comparison:\n{guard}"
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
#[dir_test(dir: "$CARGO_MANIFEST_DIR/tests/fixtures/sonatina_ir_semantic", glob: "repeated_generic_storage_fields_lower_to_distinct_slots.fe")]
fn repeated_generic_storage_fields_lower_to_distinct_slots(fixture: Fixture<&str>) {
    let ir = with_top_mod_for_source(&fixture, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod).expect("Sonatina IR should emit")
    });

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
