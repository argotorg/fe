use common::{
    InputDb,
    file::File,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
use fe_driver::{
    DriverDataBase,
    generation::{
        FunctionTemplate, GenerationBudget, GenerationRequest, GenerationSession,
        imports::{
            FrozenArtifact,
            packages::{
                FrozenPackage, PackageError, PackageErrorKind, PackageLimits, compose_packages,
            },
        },
    },
};
use hir::{
    analysis::{semantic::eval_body_owner_const_with_args, ty::ty_check::BodyOwner},
    hir_def::Func,
};

const RECORD: &str = r#"
const fn limit() -> u256 {}
pub struct Record { pub value: u256 }
pub const fn make(value: u256) -> Record { Record { value } }
pub const fn valid(record: Record) -> bool { record.value <= ingot::limit() }
"#;
const CONSUMER: &str = "pub const fn run() -> bool { right::valid(record: left::make(value: 15)) }";
const LEFT: &str =
    "pub const fn make(value: u256) -> shared::Record { shared::make(value: value) }";
const RIGHT: &str =
    "pub const fn valid(record: shared::Record) -> bool { shared::valid(record: record) }";

fn named<'db>(db: &'db DriverDataBase, file: File, name: &str) -> Func<'db> {
    db.top_mod(file)
        .all_funcs(db)
        .iter()
        .copied()
        .find(|f| f.name(db).to_opt().is_some_and(|n| n.data(db) == name))
        .unwrap()
}

fn artifact(limit: u64) -> FrozenArtifact {
    let mut db = DriverDataBase::default();
    db.initialize_builtin_core();
    db.initialize_builtin_std();
    let file = db.workspace().touch(&mut db, "file:///record-provider.fe".parse().unwrap(), Some(format!("use core::meta::FunctionBody\nconst fn provide() -> FunctionBody<u256> {{ FunctionBody {{ value: {limit} }} }}")));
    let result = GenerationSession::new("records".into(), GenerationBudget::new(1, 65536))
        .generate_value_function(
            &db,
            named(&db, file, "provide"),
            GenerationRequest {
                key: "limit".into(),
                template: FunctionTemplate {
                    url: "file:///records.fe".parse().unwrap(),
                    source: RECORD.into(),
                    function_name: "limit".into(),
                },
            },
        )
        .unwrap();
    FrozenArtifact::new(result)
}
fn compose(
    source: &str,
    imports: &[(&str, &FrozenPackage)],
) -> Result<FrozenPackage, PackageError> {
    compose_packages(source.into(), imports, PackageLimits::new(16, 65536))
}
fn evaluate(package: &FrozenPackage) -> String {
    let db = package.database();
    eval_body_owner_const_with_args(
        db,
        BodyOwner::Func(named(db, package.file(), "run")),
        Vec::new(),
        Vec::new(),
    )
    .unwrap()
    .pretty_print(db)
}
fn rejected(result: Result<FrozenPackage, PackageError>, kind: PackageErrorKind) -> PackageError {
    let err = result.err().expect("unexpected successful composition");
    assert_eq!(err.kind, kind, "{}", err.message);
    err
}

#[test]
fn cloned_generated_artifact_is_one_nominal_package_under_two_aliases() {
    let original = artifact(20);
    let left = FrozenPackage::from(original.clone());
    let right = FrozenPackage::from(original);
    let package = compose(CONSUMER, &[("left", &left), ("right", &right)]).unwrap();
    assert_eq!(evaluate(&package), "true");
    let receipts = package.materializations().unwrap();
    assert_eq!(receipts.len(), 2);
    let root = receipts.iter().find(|r| r.source == CONSUMER).unwrap();
    assert_eq!(root.dependencies[0].1, root.dependencies[1].1);
    let rejected_value = compose(
        &CONSUMER.replace("15", "21"),
        &[("left", &left), ("right", &right)],
    )
    .unwrap();
    assert_eq!(evaluate(&rejected_value), "false");
}

#[test]
fn independently_generated_identical_packages_keep_distinct_nominal_types() {
    let left = FrozenPackage::from(artifact(20));
    let right = FrozenPackage::from(artifact(20));
    assert_eq!(left.source(), right.source());
    let error = rejected(
        compose(CONSUMER, &[("left", &left), ("right", &right)]),
        PackageErrorKind::Output,
    );
    assert!(error.message.contains("type mismatch"), "{}", error.message);
    let receipts = error.materializations.unwrap();
    assert_eq!(receipts.len(), 3);
    let origins = receipts
        .iter()
        .filter_map(|r| r.generation.as_ref())
        .collect::<Vec<_>>();
    assert_eq!(origins.len(), 2);
    assert_eq!(origins[0].request_identity, origins[1].request_identity);
}

#[test]
fn diamond_retains_shared_record_identity_after_intermediate_wrappers_drop() {
    let (left, right) = {
        let shared = FrozenPackage::from(artifact(20));
        (
            compose(LEFT, &[("shared", &shared)]).unwrap(),
            compose(RIGHT, &[("shared", &shared)]).unwrap(),
        )
    };
    let root = compose(CONSUMER, &[("left", &left), ("right", &right)]).unwrap();
    assert_eq!(evaluate(&root), "true");
    let receipts = root.materializations().unwrap();
    assert_eq!(receipts.len(), 4);
    let l = receipts.iter().find(|r| r.source == LEFT).unwrap();
    let r = receipts.iter().find(|r| r.source == RIGHT).unwrap();
    assert_eq!(l.dependencies[0].1, r.dependencies[0].1);
    let exact = CONSUMER.len()
        + LEFT.len()
        + RIGHT.len()
        + receipts
            .iter()
            .find(|r| r.generation.is_some())
            .unwrap()
            .source
            .len()
        + "left".len()
        + "right".len()
        + 2 * "shared".len();
    compose_packages(
        CONSUMER.into(),
        &[("left", &left), ("right", &right)],
        PackageLimits::new(4, exact),
    )
    .unwrap();
    rejected(
        compose_packages(
            CONSUMER.into(),
            &[("left", &left), ("right", &right)],
            PackageLimits::new(4, exact - 1),
        ),
        PackageErrorKind::Limit,
    );

    rejected(
        compose_packages(
            CONSUMER.into(),
            &[("left", &left), ("right", &right)],
            PackageLimits::new(3, 65536),
        ),
        PackageErrorKind::Limit,
    );
    drop(left);
    drop(right);
    assert_eq!(evaluate(&root), "true");
    let retained = compose(
        "const fn run() -> bool { previous::run() }",
        &[("previous", &root)],
    )
    .unwrap();
    assert_eq!(evaluate(&retained), "true");
    assert_eq!(retained.materializations().unwrap().len(), 5);
}

#[test]
fn independent_diamond_leaves_do_not_merge_by_source_or_request_key() {
    let one = FrozenPackage::from(artifact(20));
    let two = FrozenPackage::from(artifact(20));
    let left = compose(LEFT, &[("shared", &one)]).unwrap();
    let right = compose(RIGHT, &[("shared", &two)]).unwrap();
    let error = rejected(
        compose(CONSUMER, &[("left", &left), ("right", &right)]),
        PackageErrorKind::Output,
    );
    assert!(error.message.contains("type mismatch"), "{}", error.message);
    assert_eq!(error.materializations.unwrap().len(), 5);
}

#[test]
fn aliases_and_unique_graph_budgets_are_checked_before_compilation() {
    let leaf = FrozenPackage::from(artifact(20));
    let imports = [("left", &leaf), ("right", &leaf)];
    let exact = CONSUMER.len() + leaf.source().len() + "left".len() + "right".len();
    let bound = compose_packages(CONSUMER.into(), &imports, PackageLimits::new(2, exact)).unwrap();
    assert_eq!(evaluate(&bound), "true");
    for limits in [
        PackageLimits::new(1, exact),
        PackageLimits::new(2, exact - 1),
    ] {
        let error = rejected(
            compose_packages(CONSUMER.into(), &imports, limits),
            PackageErrorKind::Limit,
        );
        assert!(error.materializations.is_none());
    }
    for alias in [
        "core",
        "std",
        "ingot",
        "self",
        "super",
        "a\"\n",
        "bad-alias",
    ] {
        rejected(
            compose("const fn run() -> bool { true }", &[(alias, &leaf)]),
            PackageErrorKind::Admission,
        );
    }
    rejected(
        compose(CONSUMER, &[("left", &leaf), ("left", &leaf)]),
        PackageErrorKind::Admission,
    );
    rejected(
        compose_packages(
            "const fn run() -> bool { true }".into(),
            &[],
            PackageLimits::new(0, 65536),
        ),
        PackageErrorKind::Limit,
    );
}

#[test]
fn recipes_survive_a_composition_chain_and_import_aliases_remain_ordinary() {
    let mut package = compose("pub const fn run() -> bool { true }", &[]).unwrap();
    for _ in 0..6 {
        package = compose(
            "pub const fn run() -> bool { previous::run() }",
            &[("previous", &package)],
        )
        .unwrap();
    }
    assert_eq!(package.materializations().unwrap().len(), 7);
    assert_eq!(evaluate(&package), "true");
    let leaf = FrozenPackage::from(artifact(20));
    let shadow = compose(
        "mod dep { pub const fn run() -> bool { false } }\nconst fn run() -> bool { dep::run() }",
        &[("dep", &leaf)],
    )
    .unwrap();
    assert_eq!(evaluate(&shadow), "false");
}
