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
fn record_requirements_check_construction_and_type_uses() {
    let prefix = "struct Bounded<const N: usize> where N > 0 { value: u256 }";
    let mut db = database();
    let file = checked(
        &mut db,
        "record-positive",
        &format!(
            "{prefix}\nconst fn answer() -> u256 {{ let item = Bounded<1> {{ value: 42 }}\n item.value }}"
        ),
    );
    assert_eq!(evaluate(&db, file, "answer"), "42");
    for (name, usage) in [
        (
            "constructor",
            "fn invalid() { let item = Bounded<0> { value: 42 } }",
        ),
        ("parameter", "fn invalid(_ item: Bounded<0>) {}"),
        (
            "return",
            "fn invalid() -> Bounded<0> { Bounded { value: 42 } }",
        ),
        ("field", "struct Outer { value: Bounded<0> }"),
        ("alias", "type Invalid = Bounded<0>"),
        ("nested", "fn invalid(_ items: [Bounded<0>; 0]) {}"),
    ] {
        let file = input(&mut db, name, &format!("{prefix}\n{usage}"));
        let errors = diagnostics(&db, file);
        assert!(
            errors.contains("const requirement") && errors.contains("false"),
            "{name}: {errors}"
        );
    }
}

#[test]
fn generic_record_uses_require_explicit_premises() {
    let prefix = "struct Bounded<const N: usize> where N > 0 { value: u256 }";
    for (name, decl) in [
        (
            "function",
            "fn consume<const M: usize>(_ item: Bounded<M>) where M > 0 {}",
        ),
        (
            "record",
            "struct Outer<const M: usize> where M > 0 { item: Bounded<M> }",
        ),
    ] {
        let mut db = database();
        checked(&mut db, name, &format!("{prefix}\n{decl}"));
        let file = input(
            &mut db,
            "missing",
            &format!("{prefix}\n{}", decl.replace("where M > 0", "")),
        );
        assert!(diagnostics(&db, file).contains("const requirement"));
    }
}

#[test]
fn record_forwarding_and_edits_match_fresh_compilation() {
    use hir::analysis::ty::ty_check::check_func_body;
    use salsa::Setter;
    for first in ["make", "answer"] {
        let mut db = database();
        let file = input(&mut db, "record-edits", "");
        for (predicate, n, valid) in [
            ("M > 0", 1, true),
            ("M > 1", 1, false),
            ("M > 0", 0, false),
            ("M > 0", 1, true),
        ] {
            let source = format!(
                "struct Bounded<const N: usize> where N > 0 {{ value: u256 }}\nconst fn make<const M: usize>() -> Bounded<M> where {predicate} {{ Bounded {{ value: 42 }} }}\nconst fn answer() -> u256 {{ let item = make<{n}>()\n item.value }}"
            );
            file.set_text(&mut db).to(source.clone());
            let _ = check_func_body(&db, named(&db, file, first));
            let warm = diagnostics(&db, file);
            assert_eq!(warm.is_empty(), valid, "{first}, {predicate}, {n}: {warm}");
            let mut fresh = database();
            let fresh_file = input(&mut fresh, "record-edits", &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
            if valid {
                assert_eq!(evaluate(&db, file, "answer"), "42");
            }
        }
    }
}

#[test]
fn record_predicate_formation_cannot_assume_its_clause() {
    let prefix = "const fn length<const M: usize>() -> usize where M > 0 { 0 }";
    for n in ["N", "1"] {
        let mut db = database();
        let file = input(
            &mut db,
            "record-formation",
            &format!(
                "{prefix}\nstruct Bounded<const N: usize> where N > 0, ({{ let xs: [u8; {{ length<{n}>() }}] = []\n true }}) {{ value: u256 }}"
            ),
        );
        let errors = diagnostics(&db, file);
        assert_eq!(errors.is_empty(), n == "1", "{errors}");
        if n == "N" {
            assert!(errors.contains("const requirement"), "{errors}");
        }
    }
}

#[test]
fn generated_record_consumers_use_ordinary_requirements() {
    use fe_driver::generation::{FunctionTemplate, GenerationBudget, generate_scalar_function};
    let mut db = database();
    let file = checked(
        &mut db,
        "record-provider",
        "use core::meta::FunctionBody\nconst fn provide() -> FunctionBody<u256> { FunctionBody { value: 42 } }",
    );
    for n in [1, 0] {
        let result = generate_scalar_function(
            &db,
            named(&db, file, "provide"),
            FunctionTemplate {
                url: Url::parse("file:///const-where/generated-record.fe").unwrap(),
                source: format!(
                    "struct Bounded<const N: usize> where N > 0 {{ value: u256 }}\nconst fn generated<const M: usize>(_ item: Bounded<M>) -> u256 where M > 0 {{}}\nconst fn answer() -> u256 {{ generated<{n}>(Bounded {{ value: 0 }}) }}"
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
fn record_requirements_cover_declaration_and_inferred_positions() {
    let prefix = "struct Bounded<const N: usize> where N > 0 { value: u256 }";
    for (name, usage) in [
        ("generic_default", "fn use_it<T = Bounded<0>>() {}"),
        ("trait_constant", "trait Has { const BAD: Bounded<0> }"),
        ("trait_default", "trait Has { type Item = Bounded<0> }"),
        (
            "associated_bound",
            "trait Marker<T> {}\ntrait Has { type Item: Marker<Bounded<0>> }",
        ),
        (
            "associated_definition",
            "trait Has { type Item }\nstruct S {}\nimpl Has for S { type Item = Bounded<0> }",
        ),
        (
            "trait_argument",
            "trait Marker<T> {}\nfn use_it<T: Marker<Bounded<0>>>() {}",
        ),
        ("impl_target", "impl Bounded<0> {}"),
        (
            "trait_impl_target",
            "trait Marker {}\nimpl Marker for Bounded<0> {}",
        ),
        (
            "explicit_unused_argument",
            "fn ignore<T>() -> u256 { 42 }\nfn use_it() -> u256 { ignore<Bounded<0>>() }",
        ),
        (
            "ascription",
            "fn use_it() { let item: Bounded<0> = Bounded { value: 42 } }",
        ),
    ] {
        let mut db = database();
        let good = usage.replace("Bounded<0>", "Bounded<1>");
        checked(
            &mut db,
            &format!("{name}-good"),
            &format!("{prefix}\n{good}"),
        );
        let file = input(&mut db, name, &format!("{prefix}\n{usage}"));
        let errors = diagnostics(&db, file);
        assert!(
            errors.contains("const requirement") && errors.contains("false"),
            "{name}: {errors}"
        );
    }
}

#[test]
fn record_requirements_reject_bad_formation_and_preserve_ground_checks() {
    for (name, condition, message) in [
        ("ground_false", "false", "const where predicate"),
        ("not_bool", "N + 1", "type mismatch"),
        (
            "bad_nested_type",
            "({ let unused: Bounded<0>\n true })",
            "const requirement",
        ),
    ] {
        let mut db = database();
        let prefix = "struct Bounded<const N: usize> where N > 0 { value: u256 }";
        let file = input(
            &mut db,
            name,
            &format!(
                "{prefix}\nstruct Invalid<const N: usize> where {condition} {{ value: u256 }}"
            ),
        );
        let errors = diagnostics(&db, file);
        assert!(errors.contains(message), "{name}: {errors}");
    }
}

#[test]
fn record_formation_cycles_reject_across_query_orders_and_edits() {
    use hir::analysis::ty::ty_check::{check_anon_const_body, check_func_body};
    use hir::analysis::ty::ty_def::TyId;
    use hir::hir_def::{ItemKind, WhereClauseOwner};
    use salsa::Setter;
    for predicate_first in [false, true] {
        let mut db = database();
        let file = input(&mut db, "record-cycle", "");
        for cyclic in [true, false, true] {
            let argument = if cyclic { "Bounded<N>" } else { "u256" };
            let source = format!(
                "const fn yes<T>() -> bool {{ true }}\nstruct Bounded<const N: usize> where yes<{argument}>() {{ value: u256 }}\nfn use_it(_ value: Bounded<1>) {{}}"
            );
            file.set_text(&mut db).to(source.clone());
            if predicate_first {
                let record = db
                    .top_mod(file)
                    .all_items(&db)
                    .iter()
                    .find_map(|item| match item {
                        ItemKind::Struct(record) => Some(*record),
                        _ => None,
                    })
                    .unwrap();
                let predicate = WhereClauseOwner::Struct(record)
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
            let fresh_file = input(&mut fresh, "record-cycle", &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
        }
    }
}

#[test]
fn unapplied_record_constructors_cannot_drop_requirements() {
    let mut db = database();
    let prefix = "struct Bounded<const N: usize> where N > 0 { value: u256 }";
    checked(
        &mut db,
        "applied-alias",
        &format!("{prefix}\ntype Valid = Bounded<1>"),
    );
    checked(
        &mut db,
        "plain-unapplied-alias",
        &format!(
            "{}\ntype Constructor = Bounded",
            prefix.replace("where N > 0", "")
        ),
    );
    let file = input(
        &mut db,
        "unapplied-alias",
        &format!("{prefix}\ntype Constructor = Bounded"),
    );
    let errors = diagnostics(&db, file);
    assert!(errors.contains("const requirement"), "{errors}");
}

#[test]
fn higher_kind_helpers_cannot_hide_record_requirements() {
    let mut db = database();
    let prefix = "trait Flag { const ALLOWED: bool }\nstruct No {}\nimpl Flag for No { const ALLOWED: bool = false }";
    let helper = "const fn hidden<F: * -> *>() -> u256 { let unused: F<No>\n 42 }\nconst fn answer() -> u256 { hidden<Bounded>() }";
    let plain = format!("{prefix}\nstruct Bounded<T: Flag> {{ value: u256 }}\n{helper}");
    let file = checked(&mut db, "plain-constructor", &plain);
    assert_eq!(evaluate(&db, file, "answer"), "42");
    let constrained = plain.replace(
        "struct Bounded<T: Flag>",
        "struct Bounded<T: Flag> where T::ALLOWED",
    );
    let file = input(&mut db, "constrained-constructor", &constrained);
    let errors = diagnostics(&db, file);
    assert!(
        errors.contains("const requirement"),
        "constrained constructor diagnostics: {errors}"
    );
}

#[test]
fn record_type_parameter_predicates_use_declared_trait_bounds() {
    let mut db = database();
    let prefix = "trait Flag { const ALLOWED: bool }\nstruct Yes {}\nstruct No {}\nimpl Flag for Yes { const ALLOWED: bool = true }\nimpl Flag for No { const ALLOWED: bool = false }\nstruct Bounded<T: Flag> where T::ALLOWED { value: u256 }\nconst fn make<U: Flag>() -> Bounded<U> where U::ALLOWED { Bounded { value: 42 } }";
    let file = checked(
        &mut db,
        "record-type-parameter",
        &format!("{prefix}\nconst fn answer() -> u256 {{ let item = make<Yes>()\n item.value }}"),
    );
    assert_eq!(evaluate(&db, file, "answer"), "42");
    let file = input(
        &mut db,
        "record-type-false",
        &format!("{prefix}\ntype Invalid = Bounded<No>"),
    );
    let errors = diagnostics(&db, file);
    assert!(
        errors.contains("const requirement") && errors.contains("false"),
        "{errors}"
    );
}
