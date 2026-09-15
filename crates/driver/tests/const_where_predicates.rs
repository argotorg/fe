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

fn database() -> DriverDataBase {
    let mut db = DriverDataBase::default();
    db.initialize_builtin_core();
    db.initialize_builtin_std();
    db
}

fn input(db: &mut DriverDataBase, name: &str, source: &str) -> File {
    db.workspace().touch(
        db,
        Url::parse(&format!("file:///const-where/{name}.fe")).unwrap(),
        Some(source.to_owned()),
    )
}

fn diagnostics(db: &DriverDataBase, file: File) -> String {
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

fn checked(db: &mut DriverDataBase, name: &str, source: &str) -> File {
    let file = input(db, name, source);
    let errors = diagnostics(db, file);
    assert!(errors.is_empty(), "unexpected diagnostics:\n{errors}");
    file
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

fn evaluate(db: &DriverDataBase, file: File, name: &str) -> String {
    eval_body_owner_const_with_args(
        db,
        BodyOwner::Func(named(db, file, name)),
        Vec::new(),
        Vec::new(),
    )
    .unwrap_or_else(|error| panic!("failed to evaluate `{name}`: {error:?}"))
    .pretty_print(db)
}

#[test]
fn ground_predicates_are_checked_and_preserved_by_printing() {
    let mut db = database();
    let source = r"
const LIMIT: u256 = 8
const fn allowed(_ n: u256) -> bool { n < LIMIT }
const fn answer() -> u256 where allowed(3), !false, (1 < 2) { 42 }
struct Record where LIMIT == 8 { value: u256 }
enum Choice where true { Yes, No }
impl Record where true { fn get(self) -> u256 { self.value } }
const fn consume() -> u256 { answer() }
";
    let file = checked(&mut db, "positive", source);
    assert_eq!(evaluate(&db, file, "consume"), "42");
    let printed = db.top_mod(file).pretty_print(&db);
    assert!(printed.contains("where"));
    let copy = checked(&mut db, "printed", &printed);
    assert_eq!(evaluate(&db, copy, "consume"), "42");
}

#[test]
fn unused_false_predicates_fail_at_their_declarations() {
    let mut db = database();
    for (name, source) in [
        ("function", "fn unused() where false {}"),
        ("record", "struct Unused where false { value: u256 }"),
        ("enum", "enum Unused where false { Value }"),
        ("impl", "struct S {}\nimpl S where false {}"),
        (
            "trait_impl",
            "trait Marker {}\nstruct S {}\nimpl Marker for S where false {}",
        ),
        ("second", "fn unused() where true, false {}"),
        ("first", "fn unused() where false, true {}"),
    ] {
        let file = input(&mut db, name, source);
        let errors = diagnostics(&db, file);
        assert!(
            errors.contains("const where predicate failed"),
            "{name}: {errors}"
        );
        assert_eq!(errors.matches("error[8-0089]").count(), 1, "{errors}");
    }
}

#[test]
fn predicates_require_bool_and_check_untaken_code() {
    let mut db = database();
    for (name, source) in [
        ("integer", "fn unused() where 42 {}"),
        (
            "dead_type_error",
            "fn unused() where (if true { true } else { 42 }) {}",
        ),
        (
            "nonconst",
            "fn flag() -> bool { true }\nfn unused() where flag() {}",
        ),
        ("runtime_parameter", "fn unused(flag: bool) where flag {}"),
    ] {
        let file = input(&mut db, name, source);
        let errors = diagnostics(&db, file);
        assert!(!errors.is_empty(), "{name} was accepted");
        assert!(
            !errors.contains("const where predicate failed"),
            "{name}: {errors}"
        );
    }
}

#[test]
fn every_evaluation_failure_rejects_the_predicate() {
    let mut db = database();
    for (name, source, reason) in [
        (
            "division",
            "const fn divide(_ n: u256) -> bool { 1 / n == 0 }\nfn unused() where divide(0) {}",
            "division by zero",
        ),
        (
            "recursive_constant",
            "const A: bool = A\nfn unused() where A {}",
            "recursive",
        ),
        (
            "recursion",
            "const fn again() -> bool { again() }\nfn unused() where again() {}",
            "recursion",
        ),
        (
            "steps",
            "const fn forever() -> bool { while true {}\n true }\nfn unused() where forever() {}",
            "step limit",
        ),
    ] {
        let file = input(&mut db, name, source);
        let errors = diagnostics(&db, file);
        assert!(errors.to_lowercase().contains(reason), "{name}: {errors}");
    }
}

#[test]
fn generic_scopes_are_rejected_instead_of_dropping_obligations() {
    let mut db = database();
    for (name, source) in [
        ("trait_self", "trait Marker where true {}"),
        (
            "inherited",
            "trait Has { fn unused() }\nstruct S<T> { value: T }\nimpl<T> Has for S<T> { fn unused() where true {} }",
        ),
    ] {
        let file = input(&mut db, name, source);
        let errors = diagnostics(&db, file);
        assert!(
            errors.contains("const where predicates in generic scopes are not supported yet"),
            "{name}: {errors}"
        );
    }
}

#[test]
fn predicate_edits_match_fresh_compilation() {
    use salsa::Setter;
    let mut db = database();
    let file = input(&mut db, "edit", "");
    for condition in ["true", "false", "true"] {
        let source = format!("const FLAG: bool = {condition}\nfn unused() where FLAG {{}}");
        file.set_text(&mut db).to(source.clone());
        let warm = diagnostics(&db, file);
        let mut fresh = database();
        let fresh_file = input(&mut fresh, "edit", &source);
        assert_eq!(warm, diagnostics(&fresh, fresh_file));
        assert_eq!(warm.is_empty(), condition == "true");
    }
}

#[test]
fn generated_targets_retain_ground_predicate_obligations() {
    use fe_driver::generation::{FunctionTemplate, GenerationBudget, generate_scalar_function};
    let mut db = database();
    let file = checked(
        &mut db,
        "provider",
        "use core::meta::FunctionBody\nconst fn provide() -> FunctionBody<u256> { FunctionBody { value: 42 } }",
    );
    for condition in ["true", "false"] {
        let result = generate_scalar_function(
            &db,
            named(&db, file, "provide"),
            FunctionTemplate {
                url: Url::parse("file:///const-where/generated.fe").unwrap(),
                source: format!("const fn generated() -> u256 where {condition} {{}}"),
                function_name: "generated".into(),
            },
            &mut GenerationBudget::new(1, 4096),
        );
        if condition == "true" {
            let artifact = result.unwrap();
            assert!(artifact.source().contains("where true"));
            assert_eq!(
                evaluate(artifact.database(), artifact.file(), "generated"),
                "42"
            );
        } else {
            let error = result
                .err()
                .expect("false generated requirement was accepted");
            assert!(
                error.message.contains("const where predicate failed"),
                "{error}"
            );
            assert!(error.provenance.is_some());
        }
    }
}

#[test]
fn provider_protocol_does_not_silently_expand_to_const_requirements() {
    use fe_driver::generation::{
        FunctionTemplate, GenerationBudget, GenerationErrorKind, generate_scalar_function,
    };
    let mut db = database();
    let file = checked(
        &mut db,
        "provider_bound",
        "use core::meta::FunctionBody\nconst fn provide() -> FunctionBody<u256> where true { FunctionBody { value: 42 } }",
    );
    let result = generate_scalar_function(
        &db,
        named(&db, file, "provide"),
        FunctionTemplate {
            url: Url::parse("file:///const-where/generated.fe").unwrap(),
            source: "const fn generated() -> u256 {}".into(),
            function_name: "generated".into(),
        },
        &mut GenerationBudget::new(1, 4096),
    );
    assert_eq!(
        result
            .err()
            .expect("provider requirements were admitted")
            .kind,
        GenerationErrorKind::Protocol
    );
}

#[test]
fn frozen_exports_keep_their_requirement_exclusion() {
    use fe_driver::generation::{
        FunctionTemplate, GenerationBudget, generate_scalar_function,
        imports::{FrozenArtifact, ImportErrorKind},
    };
    let mut db = database();
    let file = checked(
        &mut db,
        "export_provider",
        "use core::meta::FunctionBody\nconst fn provide() -> FunctionBody<u256> { FunctionBody { value: 42 } }",
    );
    let artifact = generate_scalar_function(
        &db,
        named(&db, file, "provide"),
        FunctionTemplate {
            url: Url::parse("file:///const-where/export.fe").unwrap(),
            source: "pub const fn generated() -> u256 where true {}".into(),
            function_name: "generated".into(),
        },
        &mut GenerationBudget::new(1, 4096),
    )
    .unwrap();
    let frozen = FrozenArtifact::new(artifact);
    let error = frozen
        .export("generated")
        .err()
        .expect("export requirements were admitted");
    assert_eq!(error.kind, ImportErrorKind::Export);
}

#[test]
fn predicate_blocks_receive_semantic_borrow_checking() {
    let mut db = database();
    checked(
        &mut db,
        "valid_borrow",
        r"
fn unused() where ({
    let mut n: u256 = 1
    let a = mut n
    a += 1
    n == 2
}) {}
",
    );
    let file = input(
        &mut db,
        "conflicting_borrow",
        r"
fn unused() where ({
    let mut n: u256 = 1
    let a = mut n
    let b = mut n
    a += 1
    b += 1
    n == 3
}) {}
",
    );
    let hir = db.run_on_ingot(db.top_mod(file).ingot(&db));
    assert!(!hir.has_errors(&db), "{}", hir.format_diags(&db));
    let errors = diagnostics(&db, file);
    assert!(
        !errors.is_empty(),
        "conflicting borrows in predicate were accepted"
    );
    assert!(errors.contains("borrow conflict"), "{errors}");
}

#[test]
fn ground_predicate_calls_discharge_unused_helper_requirements() {
    for has_impl in [false, true] {
        let mut db = database();
        let implementation = if has_impl {
            "impl Required for u256 {}"
        } else {
            ""
        };
        let source = format!(
            "trait Required {{}}\n{implementation}\nconst fn allowed<T>() -> bool where T: Required {{ true }}\nfn unused() where allowed<u256>() {{}}"
        );
        let file = input(&mut db, "helper_requirement", &source);
        let errors = diagnostics(&db, file);
        if has_impl {
            assert!(errors.is_empty(), "{errors}");
        } else {
            assert!(errors.contains("error[6-0003]"), "{errors}");
            assert!(errors.contains("Required"), "{errors}");
        }
    }
}
