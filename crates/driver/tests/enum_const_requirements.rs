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
    use salsa::Setter;
    let file = db.workspace().touch(
        db,
        Url::parse(&format!("file:///const-where/{name}.fe")).unwrap(),
        Some(source.to_owned()),
    );
    // touch reuses an existing input without replacing its source.
    file.set_text(db).to(source.to_owned());
    file
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

const DECLARATION: &str =
    "enum Choice<const N: usize> where N > 0 { Empty, Tuple(u256), Record { value: u256 } }";
const READ: &str = "const fn read<const M: usize>(_ item: Choice<M>) -> u256 where M > 0 { match item { Choice::Empty => 7, Choice::Tuple(value) => value, Choice::Record { value } => value + 1 } }";

#[test]
fn enum_requirements_check_every_variant_and_match() {
    for (variant, expected) in [
        ("Empty", "7"),
        ("Tuple(42)", "42"),
        ("Record { value: 41 }", "42"),
    ] {
        let mut db = database();
        for n in [1, 0] {
            let source = format!(
                "{DECLARATION}\n{READ}\nconst fn answer() -> u256 {{ read(Choice<{n}>::{variant}) }}"
            );
            let file = input(&mut db, "variant", &source);
            let errors = diagnostics(&db, file);
            if n == 1 {
                assert!(errors.is_empty(), "{variant}: {errors}");
                assert_eq!(evaluate(&db, file, "answer"), expected);
            } else {
                assert!(
                    errors.contains("const requirement") && errors.contains("false"),
                    "{variant}: {errors}"
                );
            }
        }
    }
}

#[test]
fn enum_requirements_cover_unused_type_positions() {
    for (name, usage) in [
        ("parameter", "fn unused(_ x: Choice<0>) {}"),
        ("return", "fn unused() -> Choice<0> { Choice::Empty }"),
        ("alias", "type Unused = Choice<0>"),
        ("default", "fn unused<T = Choice<0>>() {}"),
        ("record_field", "struct Outer { item: Choice<0> }"),
        ("tuple_variant", "enum Outer { Value(Choice<0>), Empty }"),
        (
            "record_variant",
            "enum Outer { Value { item: Choice<0> }, Empty }",
        ),
        ("array", "fn unused(_ x: [Choice<0>; 0]) {}"),
        ("ascription", "fn unused() { let x: Choice<0> }"),
        (
            "argument",
            "fn ignore<T>() {}\nfn unused() { ignore<Choice<0>>() }",
        ),
        ("associated_default", "trait Has { type Item = Choice<0> }"),
    ] {
        let mut db = database();
        checked(
            &mut db,
            name,
            &format!("{DECLARATION}\n{}", usage.replace("Choice<0>", "Choice<1>")),
        );
        let file = input(&mut db, name, &format!("{DECLARATION}\n{usage}"));
        let errors = diagnostics(&db, file);
        assert!(
            errors.contains("const requirement") && errors.contains("false"),
            "{name}: {errors}"
        );
    }
}

#[test]
fn enum_and_record_payloads_forward_only_explicit_premises() {
    for (name, declaration) in [
        (
            "enum_tuple",
            "enum Outer<const M: usize> where M > 0 { Value(Choice<M>), Empty }",
        ),
        (
            "enum_record",
            "enum Outer<const M: usize> where M > 0 { Value { item: Choice<M> }, Empty }",
        ),
        (
            "record",
            "struct Outer<const M: usize> where M > 0 { item: Choice<M> }",
        ),
        (
            "function",
            "const fn make<const M: usize>() -> Choice<M> where M > 0 { Choice::Empty }",
        ),
        (
            "record_in_enum",
            "struct Bounded<const N: usize> where N > 0 { value: u256 }\nenum Outer<const M: usize> where M > 0 { Value(Bounded<M>) }",
        ),
        (
            "anonymous_length",
            "const fn length<const K: usize>() -> usize where K > 0 { 0 }\nenum Outer<const M: usize> where M > 0 { Value([u8; { length<M>() }]) }",
        ),
    ] {
        let mut db = database();
        checked(&mut db, name, &format!("{DECLARATION}\n{declaration}"));
        let file = input(
            &mut db,
            name,
            &format!("{DECLARATION}\n{}", declaration.replace("where M > 0", "")),
        );
        let errors = diagnostics(&db, file);
        assert!(errors.contains("const requirement"), "{name}: {errors}");
    }
}

#[test]
fn enum_requirements_do_not_supply_formation_evidence() {
    let prefix = "const fn length<const M: usize>() -> usize where M > 0 { 0 }";
    for n in ["1", "N"] {
        let mut db = database();
        let file = input(
            &mut db,
            "formation",
            &format!(
                "{prefix}\nenum Choice<const N: usize> where N > 0, ({{ let xs: [u8; {{ length<{n}>() }}] = []\n true }}) {{ Empty }}"
            ),
        );
        let errors = diagnostics(&db, file);
        assert_eq!(errors.is_empty(), n == "1", "{errors}");
        if n == "N" {
            assert!(errors.contains("const requirement"), "{errors}");
        }
    }
    for (condition, message) in [
        ("false", "const where predicate"),
        ("N + 1", "type mismatch"),
    ] {
        let mut db = database();
        let file = input(
            &mut db,
            "malformed",
            &format!("enum Choice<const N: usize> where {condition} {{ Empty }}"),
        );
        assert!(diagnostics(&db, file).contains(message));
    }
}

#[test]
fn enum_requirement_edits_match_fresh_compilation() {
    use hir::analysis::ty::ty_check::check_func_body;
    use salsa::Setter;
    for first in ["make", "answer"] {
        let mut db = database();
        let file = input(&mut db, "enum-edits", "");
        for (predicate, n, valid) in [
            ("M > 0", 1, true),
            ("M > 1", 1, false),
            ("M > 0", 0, false),
            ("M > 0", 1, true),
        ] {
            let source = format!(
                "{DECLARATION}\n{READ}\nconst fn make<const M: usize>() -> Choice<M> where {predicate} {{ Choice::Tuple(42) }}\nconst fn answer() -> u256 {{ read(make<{n}>()) }}"
            );
            file.set_text(&mut db).to(source.clone());
            let _ = check_func_body(&db, named(&db, file, first));
            let warm = diagnostics(&db, file);
            assert_eq!(warm.is_empty(), valid, "{first}: {warm}");
            let mut fresh = database();
            let fresh_file = input(&mut fresh, "enum-edits", &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
            if valid {
                assert_eq!(evaluate(&db, file, "answer"), "42");
            }
        }
    }
}

#[test]
fn enum_formation_cycles_reject_across_query_orders_and_edits() {
    use hir::analysis::ty::{
        ty_check::{check_anon_const_body, check_func_body},
        ty_def::TyId,
    };
    use hir::hir_def::{ItemKind, WhereClauseOwner};
    use salsa::Setter;
    for predicate_first in [false, true] {
        let mut db = database();
        let file = input(&mut db, "enum-cycle", "");
        for cyclic in [true, false, true] {
            let argument = if cyclic { "Choice<N>" } else { "u256" };
            let source = format!(
                "const fn yes<T>() -> bool {{ true }}\nenum Choice<const N: usize> where yes<{argument}>() {{ Empty }}\nfn use_it(_ item: Choice<1>) {{}}"
            );
            file.set_text(&mut db).to(source.clone());
            if predicate_first {
                let enum_ = db
                    .top_mod(file)
                    .all_items(&db)
                    .iter()
                    .find_map(|item| match item {
                        ItemKind::Enum(enum_) => Some(*enum_),
                        _ => None,
                    })
                    .unwrap();
                let predicate = WhereClauseOwner::Enum(enum_)
                    .clause(&db)
                    .id
                    .const_predicates(&db)[0];
                let _ = check_anon_const_body(&db, predicate, TyId::bool(&db));
            } else {
                let _ = check_func_body(&db, named(&db, file, "use_it"));
            }
            let warm = diagnostics(&db, file);
            assert_eq!(warm.is_empty(), !cyclic, "{warm}");
            if cyclic {
                assert!(warm.contains("const requirement"), "{warm}");
            }
            let mut fresh = database();
            let fresh_file = input(&mut fresh, "enum-cycle", &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
        }
    }
}

#[test]
fn enum_type_predicates_and_constructor_boundary() {
    let prefix = "trait Flag { const ALLOWED: bool }\nstruct Yes {}\nstruct No {}\nimpl Flag for Yes { const ALLOWED: bool = true }\nimpl Flag for No { const ALLOWED: bool = false }\nenum Choice<T: Flag> where T::ALLOWED { Empty }";
    let mut db = database();
    let file = checked(
        &mut db,
        "type-predicate",
        &format!(
            "{prefix}\nconst fn make<U: Flag>() -> Choice<U> where U::ALLOWED {{ Choice::Empty }}\nconst fn answer() -> u256 {{ let item = make<Yes>()\n match item {{ Choice::Empty => 42 }} }}"
        ),
    );
    assert_eq!(evaluate(&db, file, "answer"), "42");
    for usage in [
        "type Invalid = Choice<No>",
        "type Constructor = Choice",
        "const fn hidden<F: * -> *>() -> u256 { let unused: F<No>\n 42 }\nconst fn answer() -> u256 { hidden<Choice>() }",
    ] {
        let file = input(&mut db, "constructor", &format!("{prefix}\n{usage}"));
        let errors = diagnostics(&db, file);
        assert!(errors.contains("const requirement"), "{errors}");
        checked(
            &mut db,
            "plain-constructor",
            &format!("{}\n{usage}", prefix.replace("where T::ALLOWED", "")),
        );
    }
}

#[test]
fn generated_enum_consumers_use_ordinary_requirements() {
    use fe_driver::generation::{FunctionTemplate, GenerationBudget, generate_scalar_function};
    let mut db = database();
    let file = checked(
        &mut db,
        "provider",
        "use core::meta::FunctionBody\nconst fn provide() -> FunctionBody<u256> { FunctionBody { value: 42 } }",
    );
    for n in [1, 0] {
        let result = generate_scalar_function(
            &db,
            named(&db, file, "provide"),
            FunctionTemplate {
                url: Url::parse("file:///const-where/generated-enum.fe").unwrap(),
                source: format!(
                    "{DECLARATION}\nconst fn generated<const M: usize>(_ item: Choice<M>) -> u256 where M > 0 {{}}\nconst fn answer() -> u256 {{ generated<{n}>(Choice::Empty) }}"
                ),
                function_name: "generated".into(),
            },
            &mut GenerationBudget::new(1, 4096),
        );
        if n == 1 {
            let artifact = result.unwrap();
            assert_eq!(
                evaluate(artifact.database(), artifact.file(), "answer"),
                "42"
            );
        } else {
            assert!(format!("{:?}", result.err().unwrap()).contains("const requirement"));
        }
    }
}

#[test]
fn tuple_constructor_values_receive_a_diagnostic() {
    for prefix in [
        "enum Choice { Value(u256) }",
        "enum Choice<const N: usize> where N > 0 { Value(u256) }",
    ] {
        let constructor = if prefix.contains("const N") {
            "Choice<1>::Value"
        } else {
            "Choice::Value"
        };
        for usage in [
            format!("fn unused() {{ let constructor = {constructor} }}"),
            format!("fn unused() {{ let pair = ({constructor}, 0) }}"),
            format!("fn ignore<T>(_ item: T) {{}}\nfn unused() {{ ignore({constructor}) }}"),
        ] {
            let mut db = database();
            let file = input(&mut db, "constructor-value", &format!("{prefix}\n{usage}"));
            let errors = diagnostics(&db, file);
            assert!(
                errors.contains("tuple-variant constructors cannot be used as values"),
                "{errors}"
            );
        }
    }
}
