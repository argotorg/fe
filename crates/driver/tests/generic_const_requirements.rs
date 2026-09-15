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
fn concrete_calls_discharge_unused_requirements() {
    let mut db = database();
    let declaration = "const fn bounded<const N: usize>() -> u256 where N > 0 { 42 }";
    let file = checked(
        &mut db,
        "positive",
        &format!("{declaration}\nconst fn answer() -> u256 {{ bounded<1>() }}"),
    );
    assert_eq!(evaluate(&db, file, "answer"), "42");
    for (name, use_site) in [
        ("call", "fn use_it() -> u256 { bounded<0>() }"),
        ("generic_body", "fn use_it<T>() -> u256 { bounded<0>() }"),
        ("value", "fn use_it() { let f = bounded<0> }"),
        ("constant", "const VALUE: u256 = bounded<0>()"),
        (
            "dead_constant",
            "const VALUE: u256 = if false { bounded<0>() } else { 42 }",
        ),
        (
            "array_length",
            "fn use_it() { let values: [u256; { bounded<0>() as usize }] = [] }",
        ),
    ] {
        let file = input(&mut db, name, &format!("{declaration}\n{use_site}"));
        let errors = diagnostics(&db, file);
        assert!(errors.contains("const requirement"), "{name}: {errors}");
    }
}

#[test]
fn forwarding_uses_substituted_semantic_identity() {
    let mut db = database();
    let source = "const fn bounded<const N: usize>() -> u256 where N > 0 { 42 }\nconst fn forward<const RENAMED: usize>() -> u256 where RENAMED > 0 { bounded<RENAMED>() }\nconst fn answer() -> u256 { forward<1>() }";
    let file = checked(&mut db, "forward", source);
    assert_eq!(evaluate(&db, file, "answer"), "42");
    for (name, source) in [
        ("missing", source.replace("where RENAMED > 0", "")),
        (
            "wrong_argument",
            source.replace("bounded<RENAMED>()", "bounded<0>()"),
        ),
        ("false_root", source.replace("forward<1>()", "forward<0>()")),
    ] {
        let file = input(&mut db, name, &source);
        let errors = diagnostics(&db, file);
        assert!(errors.contains("const requirement"), "{name}: {errors}");
    }
}

#[test]
fn forwarding_distinguishes_declarations_and_parameter_positions() {
    let mut db = database();
    let prefix = "const fn allowed(_ n: usize) -> bool { n > 0 }\nconst fn different(_ n: usize) -> bool { n > 0 }\nconst fn bounded<const N: usize>() -> u256 where allowed(N) { 42 }";
    let positive = format!(
        "{prefix}\nconst fn forward<const A: usize, const B: usize>() -> u256 where allowed(B) {{ bounded<B>() }}\nconst fn answer() -> u256 {{ forward<0, 1>() }}"
    );
    let file = checked(&mut db, "positions", &positive);
    assert_eq!(evaluate(&db, file, "answer"), "42");
    for (name, source) in [
        (
            "wrong_position",
            positive.replace("bounded<B>()", "bounded<A>()"),
        ),
        (
            "different_declaration",
            positive.replace("where allowed(B)", "where different(B)"),
        ),
    ] {
        let file = input(&mut db, name, &source);
        assert!(
            diagnostics(&db, file).contains("const requirement"),
            "{name}"
        );
    }
}

#[test]
fn generated_targets_have_the_same_generic_requirements() {
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
                url: Url::parse("file:///const-where/generated-generic.fe").unwrap(),
                source: format!(
                    "const fn generated<const N: usize>() -> u256 where N > 0 {{}}\nconst fn answer() -> u256 {{ generated<{n}>() }}"
                ),
                function_name: "generated".into(),
            },
            &mut GenerationBudget::new(1, 4096),
        );
        if n == 1 {
            let artifact = result.unwrap();
            assert!(artifact.source().contains("where"));
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
fn generic_requirement_edits_match_fresh_compilation_in_both_query_orders() {
    use hir::analysis::ty::ty_check::check_func_body;
    use salsa::Setter;
    for caller_first in [false, true] {
        let mut db = database();
        let file = input(&mut db, "edit-generic", "");
        for (condition, n) in [("N > 0", 1), ("N > 1", 1), ("N > 0", 1), ("N > 0", 0)] {
            let source = format!(
                "const fn bounded<const N: usize>() -> u256 where {condition} {{ 42 }}\nconst fn answer() -> u256 {{ bounded<{n}>() }}"
            );
            file.set_text(&mut db).to(source.clone());
            let func = named(&db, file, if caller_first { "answer" } else { "bounded" });
            let _ = check_func_body(&db, func);
            let warm = diagnostics(&db, file);
            let mut fresh = database();
            let fresh_file = input(&mut fresh, "edit-generic", &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
            assert_eq!(warm.is_empty(), condition == "N > 0" && n == 1);
        }
    }
}

#[test]
fn requirement_failures_do_not_become_evidence() {
    let mut db = database();
    for (name, declaration, reason) in [
        (
            "malformed",
            "const fn bounded<const N: usize>() -> u256 where N { 42 }",
            "type mismatch",
        ),
        (
            "division",
            "const fn bounded<const N: usize>() -> u256 where 1 / N > 0 { 42 }",
            "division by zero",
        ),
        (
            "cycle",
            "const fn bounded<const N: usize>() -> bool where bounded<N>() { true }",
            "recursive",
        ),
        (
            "nonconst",
            "fn flag(_ n: usize) -> bool { n > 0 }\nconst fn bounded<const N: usize>() -> u256 where flag(N) { 42 }",
            "const",
        ),
    ] {
        let result_ty = if name == "cycle" { "bool" } else { "u256" };
        let source = format!("{declaration}\nfn answer() -> {result_ty} {{ bounded<0>() }}");
        let file = input(&mut db, name, &source);
        let errors = diagnostics(&db, file);
        assert!(errors.to_lowercase().contains(reason), "{name}: {errors}");
    }
}

#[test]
fn concrete_conditions_do_not_require_unrelated_type_parameters() {
    let mut db = database();
    let source = "const fn bounded<T, const N: usize>() -> u256 where N > 0 { 42 }\nconst fn forward<T>() -> u256 { bounded<T, 1>() }\nconst fn answer() -> u256 { forward<u256>() }";
    let file = checked(&mut db, "unrelated", source);
    assert_eq!(evaluate(&db, file, "answer"), "42");
}

#[test]
fn generic_predicates_check_const_language_before_becoming_premises() {
    let mut db = database();
    for (name, predicate) in [
        ("direct", "flag(N)"),
        ("untaken", "(if false { flag(N) } else { true })"),
    ] {
        let source = format!(
            "fn flag(_ n: usize) -> bool {{ n > 0 }}\nfn unused<const N: usize>() where {predicate} {{}}"
        );
        let file = input(&mut db, name, &source);
        let errors = diagnostics(&db, file);
        assert!(errors.contains("non-const"), "{name}: {errors}");
    }
}

#[test]
fn symbolic_blocks_are_explicitly_bounded_but_concrete_blocks_evaluate() {
    let mut db = database();
    let declaration =
        "const fn bounded<const N: usize>() -> u256 where ({ let value = N\n value > 0 }) { 42 }";
    let file = checked(
        &mut db,
        "concrete-block",
        &format!("{declaration}\nconst fn answer() -> u256 {{ bounded<1>() }}"),
    );
    assert_eq!(evaluate(&db, file, "answer"), "42");
    let file = input(
        &mut db,
        "symbolic-block",
        &format!(
            "{declaration}\nfn forward<const M: usize>() -> u256 where ({{ let value = M\n value > 0 }}) {{ bounded<M>() }}"
        ),
    );
    let errors = diagnostics(&db, file);
    assert!(errors.contains("symbolic forwarding"), "{errors}");
}

#[test]
fn generic_requirements_preserve_ctfe_execution_limits() {
    let mut db = database();
    for (name, helper, reason) in [
        (
            "steps",
            "const fn condition(_ n: usize) -> bool { while true {}\n n > 0 }",
            "step limit",
        ),
        (
            "recursion",
            "const fn condition(_ n: usize) -> bool { condition(n) }",
            "recursion",
        ),
    ] {
        let source = format!(
            "{helper}\nconst fn bounded<const N: usize>() -> u256 where condition(N) {{ 42 }}\nfn answer() -> u256 {{ bounded<1>() }}"
        );
        let file = input(&mut db, name, &source);
        let errors = diagnostics(&db, file);
        assert!(
            errors.contains("const requirement") && errors.contains(reason),
            "{name}: {errors}"
        );
    }
}

#[test]
fn ground_and_boolean_requirements_work_in_generic_functions() {
    let mut db = database();
    let file = checked(
        &mut db,
        "boolean",
        "const fn bounded<T, const FLAG: bool>() -> u256 where FLAG, true { 42 }\nconst fn forward<U, const ENABLED: bool>() -> u256 where ENABLED { bounded<U, ENABLED>() }\nconst fn answer() -> u256 { forward<u256, true>() }",
    );
    assert_eq!(evaluate(&db, file, "answer"), "42");
    let file = input(
        &mut db,
        "false-ground-generic",
        "fn unused<T>() where false {}",
    );
    assert!(diagnostics(&db, file).contains("const where predicate failed"));
}

#[test]
fn identical_spelling_does_not_identify_a_predicate() {
    let mut db = database();
    let prefix = "mod first {\n pub const fn allowed(_ n: usize) -> bool { n > 0 }\n pub const fn bounded<const N: usize>() -> u256 where allowed(N) { 42 }\n}";
    let suffix = "const fn forward<const M: usize>() -> u256 where allowed(M) { first::bounded<M>() }\nconst fn answer() -> u256 { forward<1>() }";
    let file = checked(
        &mut db,
        "same-declaration",
        &format!("{prefix}\nuse first::allowed\n{suffix}"),
    );
    assert_eq!(evaluate(&db, file, "answer"), "42");
    let file = input(
        &mut db,
        "different-declaration",
        &format!("{prefix}\nconst fn allowed(_ n: usize) -> bool {{ n > 0 }}\n{suffix}"),
    );
    let errors = diagnostics(&db, file);
    assert!(errors.contains("no matching const requirement"), "{errors}");
}

#[test]
fn forwarding_preserves_arithmetic_mode() {
    let mut db = database();
    let file = input(
        &mut db,
        "arithmetic-mode",
        "#[arithmetic(unchecked)]\nmod first { pub fn bounded<const N: u8>() where N + 1 > N {} }\nfn forward<const M: u8>() where M + 1 > M { first::bounded<M>() }",
    );
    let errors = diagnostics(&db, file);
    assert!(errors.contains("no matching const requirement"), "{errors}");
}

#[test]
fn nested_const_contexts_check_requirements_with_positive_controls() {
    let mut db = database();
    let declaration = "const fn length<const N: usize>() -> usize where N > 0 { 0 }";
    for (name, consumer) in [
        (
            "array-type",
            "fn answer() { let values: [u8; { length<ARG>() }] = [] }",
        ),
        (
            "array-repeat",
            "fn answer() { let values: [u8; 0] = [0; length<ARG>()] }",
        ),
        (
            "return-type",
            "fn answer() -> [u8; { length<ARG>() }] { [] }",
        ),
        (
            "inherent-const",
            "struct S {}\nimpl S { const VALUE: usize = length<ARG>() }",
        ),
        (
            "trait-const",
            "trait HasValue { const VALUE: usize }\nstruct S {}\nimpl HasValue for S { const VALUE: usize = length<ARG>() }",
        ),
        (
            "dead-const",
            "const VALUE: usize = if false { length<ARG>() } else { 0 }",
        ),
    ] {
        for argument in [1, 0] {
            let source = format!(
                "{declaration}\n{}",
                consumer.replace("ARG", &argument.to_string())
            );
            let file = input(&mut db, &format!("{name}-{argument}"), &source);
            let errors = diagnostics(&db, file);
            if argument == 1 {
                assert!(errors.is_empty(), "{name}: {errors}");
            } else {
                assert!(errors.contains("const requirement"), "{name}: {errors}");
                assert!(errors.contains("false"), "{name}: {errors}");
            }
        }
    }
}

#[test]
fn cyclic_requirements_reject_independently_of_query_order() {
    use hir::analysis::ty::ty_check::{check_anon_const_body, check_func_body};
    use hir::analysis::ty::ty_def::TyId;
    use hir::hir_def::WhereClauseOwner;
    for predicate_first in [true, false] {
        let mut db = database();
        let source = "const fn cyclic<const N: usize>() -> bool where cyclic<N>() { true }\nconst fn answer() -> bool { cyclic<1>() }";
        let file = input(&mut db, "cyclic-order", source);
        let func = named(&db, file, "cyclic");
        if predicate_first {
            let body = WhereClauseOwner::Func(func)
                .clause(&db)
                .id
                .const_predicates(&db)[0];
            let _ = check_anon_const_body(&db, body, TyId::bool(&db));
        } else {
            let _ = check_func_body(&db, named(&db, file, "answer"));
        }
        let warm = diagnostics(&db, file);
        assert!(warm.contains("recursive const requirement"), "{warm}");
        let mut fresh = database();
        let fresh_file = input(&mut fresh, "cyclic-order", source);
        assert_eq!(warm, diagnostics(&fresh, fresh_file));
    }
}

#[test]
fn cached_evaluation_is_not_requirement_evidence() {
    let mut db = database();
    let source = "const fn bounded<const N: usize>() -> u256 where N > 0 { 42 }\nconst fn answer() -> u256 { bounded<0>() }";
    let file = input(&mut db, "evaluation-first", source);
    let _ = eval_body_owner_const_with_args(
        &db,
        BodyOwner::Func(named(&db, file, "answer")),
        Vec::new(),
        Vec::new(),
    );
    let warm = diagnostics(&db, file);
    assert!(
        warm.contains("const requirement") && warm.contains("false"),
        "{warm}"
    );
    let mut fresh = database();
    let fresh_file = input(&mut fresh, "evaluation-first", source);
    assert_eq!(warm, diagnostics(&fresh, fresh_file));
}

#[test]
fn ordinary_recursion_can_forward_a_requirement() {
    let mut db = database();
    let file = checked(
        &mut db,
        "ordinary-recursion",
        "const fn repeat<const N: usize>(_ count: usize) -> u256 where N > 0 { if count == 0 { 42 } else { repeat<N>(count - 1) } }\nconst fn answer() -> u256 { repeat<1>(3) }",
    );
    assert_eq!(evaluate(&db, file, "answer"), "42");
}

#[test]
fn mutually_recursive_requirements_have_stable_diagnostics() {
    use hir::analysis::ty::ty_check::check_func_body;
    let source = "const fn first<const N: usize>() -> bool where second<N>() { true }\nconst fn second<const M: usize>() -> bool where first<M>() { true }\nconst fn answer() -> bool { first<1>() }";
    for first in ["first", "second", "answer"] {
        let mut db = database();
        let file = input(&mut db, "mutual", source);
        let _ = check_func_body(&db, named(&db, file, first));
        let warm = diagnostics(&db, file);
        assert!(warm.contains("recursive const requirement"), "{warm}");
        let mut fresh = database();
        let fresh_file = input(&mut fresh, "mutual", source);
        assert_eq!(warm, diagnostics(&fresh, fresh_file));
    }
}

#[test]
fn requirement_evaluation_cannot_reenter_an_unfinished_type_expression() {
    use hir::analysis::ty::ty_check::check_func_body;
    use salsa::Setter;
    let source = "const fn flag<const N: usize>() -> bool where count(N) == 0 { true }\nconst fn count(_ n: usize) -> usize { let values: [u8; { if flag<1>() { 0 } else { 0 } }] = []\n 0 }\nconst fn answer() -> bool { flag<1>() }";
    for first in ["count", "answer"] {
        let mut db = database();
        let file = input(&mut db, "type-cycle", "");
        for cyclic in [true, false, true] {
            let source = if cyclic {
                source.to_string()
            } else {
                source.replace("if flag<1>()", "if true")
            };
            file.set_text(&mut db).to(source.clone());
            let _ = check_func_body(&db, named(&db, file, first));
            let warm = diagnostics(&db, file);
            assert_eq!(warm.is_empty(), !cyclic, "{first}: {warm}");
            let mut fresh = database();
            let fresh_file = input(&mut fresh, "type-cycle", &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
        }
    }
}

#[test]
fn type_dependent_associated_constants_can_be_forwarded() {
    let mut db = database();
    let prefix = "trait Flag { const ALLOWED: bool }\nstruct Yes {}\nstruct No {}\nimpl Flag for Yes { const ALLOWED: bool = true }\nimpl Flag for No { const ALLOWED: bool = false }\nconst fn bounded<T: Flag>() -> u256 where T::ALLOWED { 42 }\nconst fn forward<U: Flag>() -> u256 where U::ALLOWED { bounded<U>() }";
    let file = checked(
        &mut db,
        "type-dependent",
        &format!("{prefix}\nconst fn answer() -> u256 {{ forward<Yes>() }}"),
    );
    assert_eq!(evaluate(&db, file, "answer"), "42");
    let file = input(
        &mut db,
        "type-dependent-false",
        &format!("{prefix}\nfn answer() -> u256 {{ forward<No>() }}"),
    );
    let errors = diagnostics(&db, file);
    assert!(
        errors.contains("const requirement") && errors.contains("false"),
        "{errors}"
    );
}
