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

const DECLARATION: &str = "struct Window<const N: usize> { value: u256 }\nimpl<const N: usize> Window<N> { const fn make() -> Self where N > 0 { Self { value: 42 } }\nconst fn read(self) -> u256 where N > 0 { self.value }\nconst fn take<const M: usize>(self) -> u256 where N > 0, M > N { self.value } }";

#[test]
fn inherent_methods_check_receiver_and_method_arguments() {
    for (name, call) in [
        ("associated", "Window<1>::make().value"),
        ("receiver", "Window<1> { value: 42 }.read()"),
        ("qualified", "Window<1>::read(Window { value: 42 })"),
        ("method_generic", "Window<1> { value: 42 }.take<2>()"),
    ] {
        let mut db = database();
        let source = format!("{DECLARATION}\nconst fn answer() -> u256 {{ {call} }}");
        let file = checked(&mut db, name, &source);
        assert_eq!(evaluate(&db, file, "answer"), "42");
        let file = input(&mut db, name, &source.replace("Window<1>", "Window<0>"));
        let errors = diagnostics(&db, file);
        assert!(
            errors.contains("const requirement") && errors.contains("false"),
            "{name}: {errors}"
        );
    }
    let mut db = database();
    let file = input(
        &mut db,
        "method-false",
        &format!("{DECLARATION}\nfn answer() -> u256 {{ Window<1> {{ value: 42 }}.take<1>() }}"),
    );
    let errors = diagnostics(&db, file);
    assert!(
        errors.contains("const requirement") && errors.contains("false"),
        "{errors}"
    );
}

#[test]
fn methods_forward_explicit_premises_across_binders() {
    let forward = "const fn forward<const A: usize, const B: usize>() -> u256 where A > 0, B > A { Window<A> { value: 42 }.take<B>() }\nconst fn answer() -> u256 { forward<1, 2>() }";
    let mut db = database();
    let file = checked(&mut db, "forward", &format!("{DECLARATION}\n{forward}"));
    assert_eq!(evaluate(&db, file, "answer"), "42");
    let file = input(
        &mut db,
        "forward",
        &format!(
            "{DECLARATION}\n{}",
            forward.replace("where A > 0, B > A", "where A > 0")
        ),
    );
    assert!(diagnostics(&db, file).contains("const requirement"));
    let prefix = "const fn helper<const K: usize>() -> u256 where K > 0 { 42 }\nstruct Window<const N: usize> {}";
    let method = "impl<const N: usize> Window<N> { const fn read(self) -> u256 where N > 0 { helper<N>() } }";
    let file = checked(
        &mut db,
        "method-forward",
        &format!("{prefix}\n{method}\nconst fn answer() -> u256 {{ Window<1> {{}}.read() }}"),
    );
    assert_eq!(evaluate(&db, file, "answer"), "42");
    let file = input(
        &mut db,
        "method-forward",
        &format!("{prefix}\n{}", method.replace("where N > 0", "")),
    );
    assert!(diagnostics(&db, file).contains("const requirement"));
}

#[test]
fn method_signatures_can_forward_but_formation_cannot() {
    let prefix = "const fn length<const K: usize>() -> usize where K > 0 { 0 }\nstruct Window<const N: usize> {}";
    let good = "impl<const N: usize> Window<N> { const fn read(self, _ xs: [u8; { length<N>() }]) -> u256 where N > 0 { 42 } }\nconst fn answer() -> u256 { Window<1> {}.read([]) }";
    let mut db = database();
    let file = checked(&mut db, "signature", &format!("{prefix}\n{good}"));
    assert_eq!(evaluate(&db, file, "answer"), "42");
    let file = input(
        &mut db,
        "signature",
        &format!("{prefix}\n{}", good.replace("Window<1>", "Window<0>")),
    );
    assert!(diagnostics(&db, file).contains("const requirement"));
    for argument in ["1", "N"] {
        let file = input(
            &mut db,
            "formation",
            &format!(
                "{prefix}\nimpl<const N: usize> Window<N> {{ fn read(self) where N > 0, ({{ let xs: [u8; {{ length<{argument}>() }}] = []\n true }}) {{}} }}"
            ),
        );
        let errors = diagnostics(&db, file);
        assert_eq!(errors.is_empty(), argument == "1", "{errors}");
        if argument == "N" {
            assert!(errors.contains("const requirement"), "{errors}");
        }
    }
}

#[test]
fn method_signature_parameter_collection_is_query_order_independent() {
    use hir::analysis::ty::ty_check::check_func_body;
    for first in ["read", "answer"] {
        let mut db = database();
        for (n, m, valid) in [(1, 2, true), (1, 0, false), (0, 2, false), (1, 2, true)] {
            let source = format!(
                "const fn length<const K: usize>() -> usize where K > 0 {{ 0 }}\nstruct Window<const N: usize> {{}}\nimpl<const N: usize> Window<N> {{ const fn read<const M: usize>(self, _ a: [u8; {{ length<N>() }}], _ b: [u8; {{ length<M>() }}]) -> u256 where N > 0, M > 0 {{ 42 }} }}\nconst fn answer() -> u256 {{ Window<{n}> {{}}.read<{m}>([], []) }}"
            );
            let file = input(&mut db, "signature-order", &source);
            let _ = check_func_body(&db, named(&db, file, first));
            let warm = diagnostics(&db, file);
            assert_eq!(warm.is_empty(), valid, "{first}, {n}, {m}: {warm}");
            let mut fresh = database();
            let fresh_file = input(&mut fresh, "signature-order", &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
            if valid {
                assert_eq!(evaluate(&db, file, "answer"), "42");
            }
        }
    }
}

#[test]
fn method_requirement_edits_match_fresh_databases() {
    use hir::analysis::ty::ty_check::check_func_body;
    for first in ["take", "answer"] {
        let mut db = database();
        for (n, m, valid) in [(1, 2, true), (1, 1, false), (0, 2, false), (1, 2, true)] {
            let source = format!(
                "{DECLARATION}\nconst fn answer() -> u256 {{ Window<{n}> {{ value: 42 }}.take<{m}>() }}"
            );
            let file = input(&mut db, "edits", &source);
            let _ = check_func_body(&db, named(&db, file, first));
            let warm = diagnostics(&db, file);
            assert_eq!(warm.is_empty(), valid, "{first}, {n}, {m}: {warm}");
            let mut fresh = database();
            let fresh_file = input(&mut fresh, "edits", &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
            if valid {
                assert_eq!(evaluate(&db, file, "answer"), "42");
            }
        }
    }
}

#[test]
fn trait_and_impl_conditions_remain_explicitly_unsupported() {
    for source in [
        "struct S<const N: usize> {}\nimpl<const N: usize> S<N> where N > 0 { fn read(self) {} }",
        "trait Read<const N: usize> { fn read(self) where N > 0 {} }",
        "trait Read<const N: usize> { fn read(self) }\nstruct S {}\nimpl<const N: usize> Read<N> for S { fn read(self) where N > 0 {} }",
    ] {
        let mut db = database();
        let file = input(&mut db, "unsupported", source);
        assert!(
            diagnostics(&db, file)
                .contains("const where predicates in generic scopes are not supported"),
            "{}",
            diagnostics(&db, file)
        );
    }
}

#[test]
fn generated_consumers_call_checked_methods() {
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
                url: Url::parse("file:///const-where/generated-method.fe").unwrap(),
                source: format!(
                    "const fn generated() -> u256 {{}}\nstruct Window<const N: usize> {{}}\nimpl<const N: usize> Window<N> {{ const fn read(self) -> u256 where N > 0 {{ generated() }} }}\nconst fn answer() -> u256 {{ Window<{n}> {{}}.read() }}"
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
fn method_type_parameters_and_self_constants_keep_their_identity() {
    let mut db = database();
    let prefix = "trait Flag { const ALLOWED: bool }\nstruct Yes {}\nstruct No {}\nimpl Flag for Yes { const ALLOWED: bool = true }\nimpl Flag for No { const ALLOWED: bool = false }\nstruct Holder<T: Flag> {}\nimpl<T: Flag> Holder<T> { const fn read(self) -> u256 where T::ALLOWED { 42 } }";
    for (ty, valid) in [("Yes", true), ("No", false)] {
        let file = input(
            &mut db,
            "type-param",
            &format!("{prefix}\nconst fn answer() -> u256 {{ Holder<{ty}> {{}}.read() }}"),
        );
        let errors = diagnostics(&db, file);
        assert_eq!(errors.is_empty(), valid, "{errors}");
        if valid {
            assert_eq!(evaluate(&db, file, "answer"), "42");
        } else {
            assert!(
                errors.contains("const requirement") && errors.contains("false"),
                "{errors}"
            );
        }
    }
    for n in [1, 0] {
        let file = input(
            &mut db,
            "self-const",
            &format!(
                "struct Window<const N: usize> {{}}\nimpl<const N: usize> Window<N> {{ const ALLOWED: bool = N > 0\nconst fn read(self) -> u256 where Self::ALLOWED {{ 42 }} }}\nconst fn answer() -> u256 {{ Window<{n}> {{}}.read() }}"
            ),
        );
        let errors = diagnostics(&db, file);
        assert_eq!(errors.is_empty(), n == 1, "{errors}");
        if n == 1 {
            assert_eq!(evaluate(&db, file, "answer"), "42");
        } else {
            assert!(
                errors.contains("const requirement") && errors.contains("false"),
                "{errors}"
            );
        }
    }
}

#[test]
fn method_formation_cycles_do_not_become_evidence() {
    use hir::analysis::ty::ty_check::check_func_body;
    for first in ["allowed", "answer"] {
        let mut db = database();
        for cyclic in [true, false, true] {
            let condition = if cyclic {
                "Window<N>::allowed()"
            } else {
                "true"
            };
            let source = format!(
                "struct Window<const N: usize> {{}}\nimpl<const N: usize> Window<N> {{ const fn allowed() -> bool where {condition} {{ true }} }}\nconst fn answer() -> bool {{ Window<1>::allowed() }}"
            );
            let file = input(&mut db, "cycle", &source);
            let _ = check_func_body(&db, named(&db, file, first));
            let warm = diagnostics(&db, file);
            assert_eq!(warm.is_empty(), !cyclic, "{warm}");
            if cyclic {
                assert!(warm.contains("const requirement"), "{warm}");
            } else {
                assert_eq!(evaluate(&db, file, "answer"), "true");
            }
            let mut fresh = database();
            let fresh_file = input(&mut fresh, "cycle", &source);
            assert_eq!(warm, diagnostics(&fresh, fresh_file));
        }
    }
}

#[test]
fn same_impl_forwarding_preserves_receiver_and_local_slots() {
    let prefix = "struct Window<const N: usize> {}\nimpl<const N: usize> Window<N> { const fn take<const M: usize>(self) -> usize where N > 0, M > N { M }\nconst fn forward<const K: usize>(self) -> usize where N > 0, K > N { CALL } }\nconst fn answer() -> usize { Window<1> {}.forward<3>() }";
    for (call, valid) in [
        ("self.take<K>()", true),
        ("Window<K> {}.take<N>()", false),
        ("self.take<N>()", false),
    ] {
        let mut db = database();
        let file = input(&mut db, "same-impl", &prefix.replace("CALL", call));
        let errors = diagnostics(&db, file);
        assert_eq!(errors.is_empty(), valid, "{call}: {errors}");
        if valid {
            assert_eq!(evaluate(&db, file, "answer"), "3");
        } else {
            assert!(errors.contains("const requirement"), "{errors}");
        }
    }
}
