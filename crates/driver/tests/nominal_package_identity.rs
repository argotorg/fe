use common::{
    InputDb,
    diagnostics::Severity,
    file::File,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
use fe_driver::DriverDataBase;
use hir::{
    analysis::semantic::eval_body_owner_const_with_args, analysis::ty::ty_check::BodyOwner,
    hir_def::Func,
};
use url::Url;

const VALUE_PACKAGE: &str = r#"
pub struct Value { pub raw: u256 }
pub const fn make() -> Value { Value { raw: 42 } }
pub const fn take(value: own Value) -> u256 { value.raw }
"#;

const CONSUMER: &str = "const fn run() -> u256 { right::take(value: left::make()) }\n";

fn database() -> DriverDataBase {
    let mut db = DriverDataBase::default();
    db.initialize_builtin_core();
    db.initialize_builtin_std();
    db
}

fn touch(db: &mut DriverDataBase, path: &str, source: String) -> File {
    db.workspace().touch(
        db,
        Url::parse(&format!("file:///nominal-identity/{path}")).unwrap(),
        Some(source),
    )
}

fn package(
    db: &mut DriverDataBase,
    dir: &str,
    name: &str,
    dependencies: &[(&str, &str)],
    source: &str,
) -> File {
    let mut config = format!("[ingot]\nname = \"{name}\"\nversion = \"0.0.0\"\n");
    if !dependencies.is_empty() {
        config.push_str("\n[dependencies]\n");
        for (alias, path) in dependencies {
            config.push_str(&format!("{alias} = {{ path = \"{path}\" }}\n"));
        }
    }
    touch(db, &format!("{dir}/fe.toml"), config);
    touch(db, &format!("{dir}/src/lib.fe"), source.to_owned())
}

fn formatted_errors(db: &DriverDataBase, file: File) -> String {
    let ingot = db.top_mod(file).ingot(db);
    let hir = db.run_on_ingot(ingot);
    let mut errors = if hir.has_errors(db) {
        hir.format_diags(db)
    } else {
        String::new()
    };
    let semantic = db.mir_diagnostics_for_ingot(ingot);
    if semantic
        .iter()
        .any(|diagnostic| diagnostic.severity == Severity::Error)
    {
        errors.push_str(&db.format_complete_diagnostics(&semantic));
    }
    errors
}

fn assert_graph_valid(db: &DriverDataBase, files: &[File]) {
    for &file in files {
        let errors = formatted_errors(db, file);
        assert!(
            errors.is_empty(),
            "unexpected package diagnostics:\n{errors}"
        );
    }
}

fn assert_consumer_rejected(db: &DriverDataBase, dependencies: &[File], consumer: File) {
    assert_graph_valid(db, dependencies);
    let errors = formatted_errors(db, consumer);
    assert!(!errors.is_empty(), "distinct nominal types were accepted");
    assert!(
        errors.contains("type mismatch") || errors.contains("mismatched types"),
        "expected nominal argument mismatch:\n{errors}"
    );
}

fn named<'db>(db: &'db DriverDataBase, file: File, name: &str) -> Func<'db> {
    db.top_mod(file)
        .all_funcs(db)
        .iter()
        .copied()
        .find(|func| {
            func.name(db)
                .to_opt()
                .is_some_and(|ident| ident.data(db) == name)
        })
        .unwrap_or_else(|| panic!("missing function `{name}`"))
}

fn evaluate(db: &DriverDataBase, file: File) -> String {
    eval_body_owner_const_with_args(
        db,
        BodyOwner::Func(named(db, file, "run")),
        Vec::new(),
        Vec::new(),
    )
    .expect("checked consumer evaluates")
    .pretty_print(db)
}

#[test]
fn two_aliases_of_one_package_share_the_nominal_type() {
    let mut db = database();
    let shared = package(&mut db, "shared", "shared", &[], VALUE_PACKAGE);
    let consumer = package(
        &mut db,
        "consumer",
        "consumer",
        &[("left", "../shared"), ("right", "../shared")],
        CONSUMER,
    );
    assert_graph_valid(&db, &[shared, consumer]);
    assert_eq!(evaluate(&db, consumer), "42");
}

#[test]
fn byte_identical_packages_keep_distinct_nominal_types() {
    let mut db = database();
    let left = package(&mut db, "left", "left", &[], VALUE_PACKAGE);
    let right = package(&mut db, "right", "right", &[], VALUE_PACKAGE);
    let consumer = package(
        &mut db,
        "consumer",
        "consumer",
        &[("left", "../left"), ("right", "../right")],
        CONSUMER,
    );
    assert_consumer_rejected(&db, &[left, right], consumer);
}

fn diamond(db: &mut DriverDataBase, split_shared: bool) -> (Vec<File>, File) {
    let shared_left_dir = if split_shared {
        "shared-left"
    } else {
        "shared"
    };
    let shared_right_dir = if split_shared {
        "shared-right"
    } else {
        "shared"
    };
    let shared_left = package(db, shared_left_dir, "shared", &[], VALUE_PACKAGE);
    let mut files = vec![shared_left];
    if split_shared {
        files.push(package(db, shared_right_dir, "shared", &[], VALUE_PACKAGE));
    }
    let left = package(
        db,
        "left",
        "left",
        &[("common", &format!("../{shared_left_dir}"))],
        "pub const fn make() -> common::Value { common::make() }\n",
    );
    let right = package(
        db,
        "right",
        "right",
        &[("common", &format!("../{shared_right_dir}"))],
        "pub const fn take(value: own common::Value) -> u256 { common::take(value: value) }\n",
    );
    files.extend([left, right]);
    let consumer = package(
        db,
        "consumer",
        "consumer",
        &[("left", "../left"), ("right", "../right")],
        CONSUMER,
    );
    (files, consumer)
}

#[test]
fn diamond_dependencies_share_one_nominal_definition() {
    let mut db = database();
    let (mut dependencies, consumer) = diamond(&mut db, false);
    dependencies.push(consumer);
    assert_graph_valid(&db, &dependencies);
    assert_eq!(evaluate(&db, consumer), "42");
}

#[test]
fn diamond_dependencies_with_byte_identical_copies_remain_distinct() {
    let mut db = database();
    let (dependencies, consumer) = diamond(&mut db, true);
    assert_consumer_rejected(&db, &dependencies, consumer);
}
