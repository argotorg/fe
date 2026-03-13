use common::InputDb;
use driver::DriverDataBase;
use fe_mir::{
    MirDiagnosticsMode, MirLowerError,
    analysis::{ContractRegion, ContractRegionKind, build_contract_graph},
    collect_mir_diagnostics, lower_module,
};
use url::Url;

#[test]
fn lower_module_reports_analysis_diagnostics_as_error() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///analysis_diagnostics.fe").unwrap();
    let src = r#"
pub fn mismatched_ret() -> bool {
    1
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);

    let err = lower_module(&db, top_mod).expect_err("analysis diagnostics should fail lowering");

    let MirLowerError::AnalysisDiagnostics {
        func_name,
        diagnostics,
    } = err
    else {
        panic!("expected AnalysisDiagnostics, got {err:?}");
    };

    assert!(
        func_name.contains("mismatched_ret"),
        "func name is {func_name}"
    );
    assert!(diagnostics.contains("type mismatch"));
}

#[test]
fn collect_mir_diagnostics_keeps_analysis_diagnostics_without_panicking() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///analysis_diagnostics_in_monomorphization.fe").unwrap();
    let src = r#"
fn foo(x: u8) -> u256 {
    x
}

#[test]
fn bar_test() {
    foo(42)
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);

    let output = collect_mir_diagnostics(&db, top_mod, MirDiagnosticsMode::CompilerParity);
    assert!(
        output.internal_errors.iter().any(|err| {
            matches!(
                err,
                MirLowerError::AnalysisDiagnostics {
                    func_name,
                    diagnostics
                } if func_name.contains("foo") && diagnostics.contains("type mismatch")
            )
        }),
        "expected AnalysisDiagnostics for `foo`, got: {:?}",
        output.internal_errors
    );
}

#[test]
fn templates_report_read_before_assign_for_immutable_init() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///immutable_read_before_assign.fe").unwrap();
    let src = r#"
pub contract Counter {
    value: u256

    init() uses (value) {
        let current = value
        value = current
    }
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);

    let output = collect_mir_diagnostics(&db, top_mod, MirDiagnosticsMode::TemplatesOnly);
    assert!(
        output
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains("may be read before it is assigned")),
        "expected immutable read-before-assign diagnostic, got: {:?}",
        output.diagnostics
    );
}

#[test]
fn templates_report_missing_assignment_on_some_init_paths() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///immutable_maybe_uninitialized.fe").unwrap();
    let src = r#"
pub contract Counter {
    value: u256

    init(flag: bool) uses (value) {
        if flag {
            value = 1
        }
    }
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);

    let output = collect_mir_diagnostics(&db, top_mod, MirDiagnosticsMode::TemplatesOnly);
    assert!(
        output.diagnostics.iter().any(|diag| diag
            .message
            .contains("may be uninitialized when `init` returns")),
        "expected immutable definite-assignment diagnostic, got: {:?}",
        output.diagnostics
    );
}

#[test]
fn templates_report_missing_assignment_for_unbound_immutable_field() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///immutable_unbound_missing_assignment.fe").unwrap();
    let src = r#"
pub contract Counter {
    value: u256

    init() {}
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);

    let output = collect_mir_diagnostics(&db, top_mod, MirDiagnosticsMode::TemplatesOnly);
    assert!(
        output.diagnostics.iter().any(|diag| {
            diag.message
                .contains("immutable field `value` may be uninitialized")
        }),
        "expected missing-assignment diagnostic for unbound immutable field, got: {:?}",
        output.diagnostics
    );
}

#[test]
fn templates_report_dynamic_immutable_access_in_init() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///immutable_dynamic_access.fe").unwrap();
    let src = r#"
pub contract Counter {
    values: [u256; 2]

    init(i: usize) uses (values) {
        values = [1, 2]
        let current = values[i]
    }
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);

    let output = collect_mir_diagnostics(&db, top_mod, MirDiagnosticsMode::TemplatesOnly);
    assert!(
        output.diagnostics.iter().any(|diag| diag
            .message
            .contains("cannot be accessed through a dynamic projection")),
        "expected unsupported immutable dynamic-access diagnostic, got: {:?}",
        output.diagnostics
    );
}

#[test]
fn templates_remap_nested_helper_immutable_reads_through_effect_args() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///immutable_nested_helper_read.fe").unwrap();
    let src = r#"
struct X {
    val: u256,
}

struct Y {
    val: u256,
}

fn inner() -> u256 uses (y: Y) {
    y.val
}

fn outer() -> u256 uses (x: X, y: Y) {
    inner()
}

pub contract Counter {
    a: X
    b: Y

    init() uses (a, b) {
        a = X { val: 1 }
        let current = with (a, b) {
            outer()
        }
    }
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);

    let output = collect_mir_diagnostics(&db, top_mod, MirDiagnosticsMode::TemplatesOnly);
    assert!(
        output.diagnostics.iter().any(|diag| diag
            .message
            .contains("immutable field `b` may be read before it is assigned")),
        "expected nested helper read to map back to immutable field `b`, got: {:?}",
        output.diagnostics
    );
}

#[test]
fn templates_allow_forwarding_immutable_effect_without_read() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///immutable_forward_only.fe").unwrap();
    let src = r#"
struct Y {
    val: u256,
}

fn inner() uses (y: Y) {}

fn outer() uses (y: Y) {
    inner()
}

pub contract Counter {
    b: Y

    init() uses (b) {
        outer()
        b = Y { val: 1 }
    }
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);

    let output = collect_mir_diagnostics(&db, top_mod, MirDiagnosticsMode::TemplatesOnly);
    assert!(
        output.diagnostics.is_empty() && output.internal_errors.is_empty(),
        "expected forwarding an immutable effect without dereference to be accepted, got diagnostics: {:?}, internal_errors: {:?}",
        output.diagnostics,
        output.internal_errors
    );
}

#[test]
fn templates_report_helper_dynamic_immutable_access_in_init() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///immutable_helper_dynamic_access.fe").unwrap();
    let src = r#"
struct Values {
    inner: [u256; 2]
}

fn get(i: usize) -> u256 uses (values: Values) {
    values.inner[i]
}

pub contract Counter {
    values: Values

    init(i: usize) uses (values) {
        values = Values { inner: [1, 2] }
        with (values) {
            let current = get(i)
        }
    }
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);

    let output = collect_mir_diagnostics(&db, top_mod, MirDiagnosticsMode::TemplatesOnly);
    assert!(
        output.diagnostics.iter().any(|diag| diag
            .message
            .contains("cannot be accessed through a dynamic projection")),
        "expected helper dynamic-access diagnostic, got: {:?}",
        output.diagnostics
    );
}

#[test]
fn contract_graph_keeps_parent_child_dependencies_directional() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///contract_graph_parent_child.fe").unwrap();
    let src = r#"
use std::evm::Create

pub contract Child {
    init(x: u256, y: u256) {}
}

pub contract Parent uses (create: mut Create) {
    init(seed: u256) uses (mut create) {
        create.create<Child>(value: 0, args: (seed, seed))
    }
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let module = lower_module(&db, top_mod).expect("lowered MIR");
    let graph = build_contract_graph(&db, &module.functions);

    assert!(graph.contracts.contains_key("Parent"));
    assert!(graph.contracts.contains_key("Child"));

    let parent_init = ContractRegion {
        contract_name: "Parent".to_string(),
        kind: ContractRegionKind::Init,
    };
    let parent_runtime = ContractRegion {
        contract_name: "Parent".to_string(),
        kind: ContractRegionKind::Deployed,
    };
    let child_init = ContractRegion {
        contract_name: "Child".to_string(),
        kind: ContractRegionKind::Init,
    };

    assert!(
        graph
            .region_deps
            .get(&parent_init)
            .is_some_and(|deps| deps.iter().any(|dep| dep == &child_init)),
        "expected Parent init to depend on Child init, got: {:?}",
        graph.region_deps
    );
    assert!(
        !graph
            .region_deps
            .iter()
            .any(|(region, deps)| region.contract_name != "Parent"
                && deps.iter().any(|dep| dep.contract_name == "Parent")),
        "expected Parent to remain a root contract, got: {:?}",
        graph.region_deps
    );
    assert!(
        graph
            .region_deps
            .get(&parent_runtime)
            .is_none_or(|deps| deps.is_empty()),
        "expected Parent runtime to have no create-time deps, got: {:?}",
        graph.region_deps.get(&parent_runtime)
    );
}

#[test]
fn contract_graph_tracks_runtime_create_dependencies() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///contract_graph_runtime_create.fe").unwrap();
    let src = r#"
use std::evm::{Address, Create}

msg FactoryMsg {
    #[selector = 1]
    Deploy { x: u256, y: u256 } -> Address,
}

pub contract Child {
    init(x: u256, y: u256) {}
}

pub contract Factory uses (create: mut Create) {
    recv FactoryMsg {
        Deploy { x, y } -> Address uses (mut create) {
            create.create<Child>(value: 0, args: (x, y))
        }
    }
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let module = lower_module(&db, top_mod).expect("lowered MIR");
    let graph = build_contract_graph(&db, &module.functions);

    let factory_runtime = ContractRegion {
        contract_name: "Factory".to_string(),
        kind: ContractRegionKind::Deployed,
    };
    let child_init = ContractRegion {
        contract_name: "Child".to_string(),
        kind: ContractRegionKind::Init,
    };

    assert!(graph.contracts.contains_key("Factory"));
    assert!(graph.contracts.contains_key("Child"));
    assert!(
        graph
            .region_deps
            .get(&factory_runtime)
            .is_some_and(|deps| deps.iter().any(|dep| dep == &child_init)),
        "expected Factory runtime to depend on Child init, got: {:?}",
        graph.region_deps
    );
    assert!(
        !graph
            .region_deps
            .iter()
            .any(|(region, deps)| region.contract_name != "Factory"
                && deps.iter().any(|dep| dep.contract_name == "Factory")),
        "expected Factory to remain a root contract, got: {:?}",
        graph.region_deps
    );
}
