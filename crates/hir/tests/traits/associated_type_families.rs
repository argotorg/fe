//! Associated types with type parameters of their own.

use std::path::Path;

use camino::Utf8PathBuf;
use dir_test::{Fixture, dir_test};
use fe_hir::test_db::{HirAnalysisTestDb, format_diagnostics};
use salsa::Setter;

fn diagnostics(db: &HirAnalysisTestDb, file: common::file::File) -> String {
    let (top, _) = db.top_mod(file);
    format_diagnostics(db, &db.run_on_top_mod(top))
}

/// Checks each source in one database edited from case to case, and again in
/// a fresh database; both must report the same. `None` expects no
/// diagnostics, `Some(text)` a diagnostic containing `text`.
fn check_cases<S: AsRef<str>>(cases: impl IntoIterator<Item = (S, Option<&'static str>)>) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(Utf8PathBuf::from("cases.fe"), "");
    for (source, expected) in cases {
        let source = source.as_ref();
        file.set_text(&mut db).to(source.to_owned());
        let result = diagnostics(&db, file);
        match expected {
            None => assert!(result.is_empty(), "{source}\n{result}"),
            Some(text) => assert!(
                result.contains(text),
                "expected `{text}`\n{source}\n{result}"
            ),
        }
        let mut fresh = HirAnalysisTestDb::default();
        let fresh_file = fresh.new_stand_alone(Utf8PathBuf::from("cases.fe"), source);
        assert_eq!(result, diagnostics(&fresh, fresh_file), "{source}");
    }
}

/// `template` with `HOLE` replaced by each value, as cases for
/// [`check_cases`]. Each list starts and ends with an accepted value, so the
/// edited database must also recover.
fn variants(
    template: &str,
    values: &[(&str, Option<&'static str>)],
) -> Vec<(String, Option<&'static str>)> {
    values
        .iter()
        .map(|&(value, expected)| (template.replace("HOLE", value), expected))
        .collect()
}

const MISMATCH: Option<&str> = Some("type mismatch");
const UNSATISFIED: Option<&str> = Some("trait bound is not satisfied");
const LIMIT: Option<&str> = Some("type normalization limit exceeded");
const CYCLE: Option<&str> = Some("cycle detected while resolving this type");

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/associated_type_families",
    glob: "*.fe"
)]
fn associated_type_families_standalone(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn nested_bodies_keep_the_outer_arguments() {
    let explicit = r#"
trait Inner<V> { type Pair<U> }
struct S {}
impl<V> Inner<V> for S { type Pair<U> = (V, U) }
trait Outer { type Pair<T> }
struct O {}
impl Outer for O { type Pair<T> = <S as Inner<T>>::Pair<bool> }
fn direct(x: <S as Inner<u256>>::Pair<bool>) -> (u256, bool) { x }
fn nested(x: O::Pair<u256>) -> HOLE { x }
"#;
    let default = explicit
        .replace(
            "trait Inner<V> { type Pair<U> }",
            "trait Inner<V> { type Pair<U> = (V, U) }",
        )
        .replace(
            "impl<V> Inner<V> for S { type Pair<U> = (V, U) }",
            "impl<V> Inner<V> for S {}",
        );
    for template in [explicit, default.as_str()] {
        check_cases(variants(
            template,
            &[
                ("(u256, bool)", None),
                ("(bool, bool)", MISMATCH),
                ("(u256, bool)", None),
            ],
        ));
    }
}

#[test]
fn declared_bounds_are_proved_for_every_argument() {
    check_cases(variants(
        r#"
trait Wrap<T> {}
struct Box<T> {}
HOLE
trait Factory { type Out<T>: Wrap<T> }
struct Provider {}
impl Factory for Provider { type Out<U> = Box<U> }
"#,
        &[
            ("impl<T> Wrap<T> for Box<T> {}", None),
            ("impl Wrap<u256> for Box<u256> {}", UNSATISFIED),
            ("impl<T> Wrap<bool> for Box<T> {}", UNSATISFIED),
            ("impl<T> Wrap<T> for Box<T> {}", None),
        ],
    ));
}

#[test]
fn defaults_and_overrides_keep_the_declared_bound() {
    check_cases(variants(
        r#"
trait Wrap<T> {}
struct Box<V, T> {}
struct Bad<T> {}
impl<V, T> Wrap<T> for Box<V, T> {}
trait Factory<V> { type Out<T>: Wrap<T> = Box<V, T> }
struct Provider {}
impl Factory<bool> for Provider { HOLE }
"#,
        &[
            ("", None),
            ("type Out<U> = Bad<U>", UNSATISFIED),
            ("", None),
        ],
    ));
}

#[test]
fn an_unused_default_needs_a_completed_proof_of_its_bound() {
    check_cases(variants(
        r#"
trait Wrap<T> {}
struct Box<T> {}
impl<T> Wrap<T> for Box<T> HOLE {}
trait Factory { type Out<T>: Wrap<T> = Box<T> }
"#,
        &[
            ("", None),
            ("where Box<Box<T>>: Wrap<Box<T>>", UNSATISFIED),
            ("", None),
        ],
    ));
}

#[test]
fn an_unmet_bound_on_a_default_is_reported_at_the_default() {
    let source = "trait Show {}\ntrait Tr {\n    type Next\n    type Out<T>: Show = (T, bool)\n}";
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(Utf8PathBuf::from("default_span.fe"), source);
    let result = diagnostics(&db, file);
    assert!(result.contains("trait bound is not satisfied"), "{result}");
    assert!(result.contains("default_span.fe:4:"), "{result}");
    assert!(!result.contains("default_span.fe:2:"), "{result}");
}

#[test]
fn parameter_bounds_check_their_trait_arguments() {
    check_cases(variants(
        r#"
trait Allowed {}
impl Allowed for bool {}
trait Needs<V: Allowed> {}
trait Factory { type Out<T: Needs<HOLE>> }
"#,
        &[
            ("bool", None),
            ("u256", Some("`u256` doesn't implement `Allowed`")),
            ("Missing", Some("`Missing` is not found")),
            ("bool", None),
        ],
    ));
}

#[test]
fn declared_bounds_check_their_names_and_arguments() {
    check_cases(variants(
        r#"
trait Allowed {}
impl Allowed for bool {}
trait Needs<V: Allowed> {}
trait Parent {}
trait Child: Parent {}
trait F { type Out<T: Allowed>: HOLE }
"#,
        &[
            ("Allowed", None),
            ("Missing", Some("`Missing` is not found")),
            ("Needs<bool>", None),
            ("Needs<u256>", UNSATISFIED),
            ("Needs<Missing>", Some("`Missing` is not found")),
            ("Needs<T>", None),
            ("Child", None),
            ("Allowed", None),
        ],
    ));
}

#[test]
fn missing_traits_in_bounds_are_named() {
    check_cases([
        ("trait T1 { type Bar<T>: T2 }", Some("`T2` is not found")),
        ("trait T1 { type Bar<T: T2> }", Some("`T2` is not found")),
        ("trait T1 { type Bar<T> }", None),
    ]);
}

#[test]
fn parameter_bounds_can_rely_on_sibling_parameters() {
    for params in ["T: Needs<U>, HOLE", "HOLE, T: Needs<U>"] {
        let template = format!(
            "trait Allowed {{}}\ntrait Needs<V: Allowed> {{}}\ntrait Factory {{ type Out<{params}> }}"
        );
        check_cases(variants(
            &template,
            &[
                ("U: Allowed", None),
                ("U", Some("`U` doesn't implement `Allowed`")),
                ("U: Allowed", None),
            ],
        ));
    }
}

#[test]
fn recursive_definitions_do_not_accept_arbitrary_values() {
    check_cases(variants(
        r#"
trait F { type Out<T> }
struct S {}
impl F for S { type Out<T> = HOLE }
pub fn value() -> S::Out<u256> { true }
"#,
        &[
            ("bool", None),
            ("<S as F>::Out<T>", Some("cycle")),
            ("<S as F>::Out<(T,)>", LIMIT),
            ("(<S as F>::Out<T>, bool)", Some("cycle")),
            ("bool", None),
        ],
    ));
}

#[test]
fn inherited_and_mutual_recursion_is_rejected() {
    for declaration in [
        "trait F { type Out<T> = HOLE }\nimpl F for S {}",
        "trait F { type Out<T>\n type Other<T> }\nimpl F for S { type Out<T> = Self::Other<T>\n type Other<T> = HOLE }",
    ] {
        let template =
            format!("struct S {{}}\n{declaration}\npub fn value() -> S::Out<u256> {{ true }}");
        check_cases(variants(
            &template,
            &[
                ("bool", None),
                ("Self::Out<T>", Some("cycle")),
                ("bool", None),
            ],
        ));
    }
    check_cases([
        // A default is one definition for every type that uses it: one that
        // names itself is a cycle at the default, even if no impl uses it.
        (
            "trait F { type Out<T> = Self::Out<T> }\nstruct S {}\nimpl F for S { type Out<T> = bool }\nfn value() -> S::Out<u256> { true }",
            CYCLE,
        ),
        (
            "trait F { type Out<T> = (T, bool) }\nstruct S {}\nimpl F for S {}\nfn identity(_ x: S::Out<S::Out<u256>>) -> ((u256, bool), bool) { x }",
            None,
        ),
    ]);
}

#[test]
fn a_definition_that_names_itself_is_a_cycle() {
    // The definition names its own associated type with the same `Self`, so
    // resolving it needs itself, whatever the arguments: a cycle, reported at
    // the definition, whether or not the type is used.
    for definition in [
        "trait F { type Out<T> = Self::Out<(T, T)> }\nimpl F for S {}",
        "trait F { type Out<T> }\nimpl F for S { type Out<T> = <S as F>::Out<(T, T)> }",
    ] {
        let with_use = format!("struct S {{}}\n{definition}\nfn f(x: S::Out<u8>) -> u8 {{ x }}");
        let without_use = format!("struct S {{}}\n{definition}");
        check_cases([(with_use, CYCLE), (without_use, CYCLE)]);
    }
}

#[test]
fn results_too_large_to_write_out_stop_at_the_normalization_limit() {
    // Each layer doubles the argument. The result is small as a shared graph
    // but has 2^layers leaves written out. The work limit of 65,536 nodes
    // counts the arguments and the result of every reduction: 11 layers fit,
    // 12 do not.
    fn nested(layers: usize) -> String {
        "Wrap<".repeat(layers) + "Base" + &">".repeat(layers)
    }
    let template = "trait Tr { type Out<T> }\nstruct Base {}\nstruct Wrap<U> {}\n\
         impl Tr for Base { type Out<T> = T }\n\
         impl<U: Tr> Tr for Wrap<U> { type Out<T> = <U as Tr>::Out<(T, T)> }\n";
    let ty = |layers| format!("<{} as Tr>::Out<u8>", nested(layers));
    check_cases([
        (format!("{template}fn f() -> {} {{ 0 }}", ty(12)), LIMIT),
        (
            format!("{template}fn f(_ x: {}) -> u8 {{ x }}", ty(12)),
            LIMIT,
        ),
        (format!("{template}fn f(_ x: {}) {{}}", ty(12)), LIMIT),
        (format!("{template}fn f(_ x: {}) {{}}", ty(24)), LIMIT),
        (format!("{template}fn f() -> {} {{ 0 }}", ty(11)), MISMATCH),
        (
            format!("{template}fn f(_ x: {}) -> u8 {{ x }}", ty(11)),
            MISMATCH,
        ),
        (format!("{template}fn f(_ x: {}) {{}}", ty(11)), None),
    ]);
}

#[test]
fn generic_code_uses_only_the_declared_bounds() {
    check_cases(variants(
        r#"
trait Show<T> { fn show(self) -> u256 { 1 } }
trait Draw<T> { fn draw(self) -> u256 { 2 } }
trait Factory { type Out<T>: Show<T> }
struct Box<T> {}
struct Provider {}
impl<T> Show<T> for Box<T> {}
impl<T> Draw<T> for Box<T> {}
impl Factory for Provider { type Out<T> = Box<T> }
fn generic<B: Factory, T>(x: B::Out<T>) -> u256 { x.HOLE() }
"#,
        &[
            ("show", None),
            ("draw", Some("no method named `draw` found")),
            ("show", None),
        ],
    ));
}

#[test]
fn declared_bindings_resolve_like_those_of_plain_associated_types() {
    // `type O: Tr2<X = u8>` lets generic code resolve `<T::O as Tr2>::X` to
    // `u8`; the same bound on a type with parameters does the same for
    // `<T::F<V> as Tr2>::X`, once the parameter bounds hold.
    check_cases(variants(
        r#"
trait Allowed {}
trait Tr2 { type X }
trait Tr {
    type O: Tr2<X = u8>
    type F<U: Allowed>: Tr2<X = u8>
}
fn plain<T: Tr>(x: <<T as Tr>::O as Tr2>::X) -> u8 { x }
fn family<T: Tr, V HOLE>(x: <<T as Tr>::F<V> as Tr2>::X) -> u8 { x }
"#,
        &[(": Allowed", None), ("", UNSATISFIED), (": Allowed", None)],
    ));
}

#[test]
fn using_a_declared_bound_needs_the_parameter_bounds() {
    check_cases(variants(
        r#"
trait Allowed {}
trait Show<T> { fn show(self) -> u256 { 1 } }
trait Factory { type Out<T: Allowed>: Show<T> }
fn generic<B: Factory, T HOLE>(x: B::Out<T>) -> u256 { x.show() }
"#,
        &[(": Allowed", None), ("", UNSATISFIED), (": Allowed", None)],
    ));
}

#[test]
fn declared_bounds_keep_outer_arguments_and_supertraits() {
    check_cases(variants(
        r#"
trait Parent<A, B> { fn accept(self, _ a: A, _ b: B) {} }
trait Child<A, B>: Parent<A, B> {}
trait Factory<V> { type Out<T>: Child<V, T> }
fn generic<V, B: Factory<V>, T>(x: B::Out<T>, _ v: V, _ t: T) {
    x.accept(HOLE, t)
}
"#,
        &[("v", None), ("true", MISMATCH), ("v", None)],
    ));
}

#[test]
fn bounds_hold_on_another_instance_of_the_trait() {
    check_cases([
        (
            r#"
trait Show { fn show(self) -> u8 }
trait Tr {
    type Next: Tr
    type Out<T>: Show
    fn get<T>(_ x: <Self::Next as Tr>::Out<T>) -> u8 { x.show() }
}
"#,
            None,
        ),
        (
            r#"
trait Show { fn show(self) -> u8 }
trait Tr {
    type Next: Tr
    type Out<T: Show>: Show = <Self::Next as Tr>::Out<T>
}
"#,
            None,
        ),
        (
            r#"
trait Show { fn show(self) -> u8 }
trait Tr {
    type Next: Tr
    type Out<T: Show>: Show = <Self::Next as Tr>::Out<bool>
}
"#,
            UNSATISFIED,
        ),
    ]);
}

#[test]
fn parameter_bounds_are_checked_in_generic_signatures() {
    check_cases(variants(
        r#"
trait Allowed {}
trait Factory { type Out<T: Allowed> }
fn generic<B: Factory, T HOLE>(_ x: B::Out<T>) {}
"#,
        &[(": Allowed", None), ("", UNSATISFIED), (": Allowed", None)],
    ));
}

// Every place a type can be written is checked by the UI fixture
// `ty/trait_bound/assoc_type_family_positions.fe` (each rejected once, at the
// application) and by `test_files/associated_type_families/positions.fe`
// (each accepted).

#[test]
fn unused_defaults_check_their_body() {
    let template = r#"
trait Allowed {}
trait Extra {}
struct Box<T: REQUIREMENT> {}
trait Factory { type Out<T: Allowed> = BODY }
"#;
    check_cases(
        [
            ("Allowed", "Box<T>", None),
            ("Extra", "Box<T>", UNSATISFIED),
            ("Allowed", "Missing<T>", Some("`Missing` is not found")),
            ("Allowed", "Box<T>", None),
        ]
        .map(|(requirement, body, expected)| {
            (
                template
                    .replace("REQUIREMENT", requirement)
                    .replace("BODY", body),
                expected,
            )
        }),
    );
}

#[test]
fn parameter_bounds_keep_unused_enclosing_arguments() {
    let template = r#"
trait Allowed<V> {}
impl Allowed<bool> for bool {}
trait Factory<V> { type Out<T: Allowed<V>> DEFAULT }
struct Provider {}
impl<V> Factory<V> for Provider { BODY }
fn concrete(_ x: <Provider as Factory<HOLE>>::Out<bool>) {}
"#;
    for (default, body) in [("", "type Out<U> = u256"), ("= u256", "")] {
        let template = template.replace("DEFAULT", default).replace("BODY", body);
        check_cases(variants(
            &template,
            &[("bool", None), ("u256", UNSATISFIED), ("bool", None)],
        ));
    }
}

#[test]
fn bodies_rely_only_on_their_own_parameter_bounds() {
    let template = r#"
trait Allowed {}
trait Extra {}
trait Show<T> {}
struct Box<T: HOLE> {}
impl<U: HOLE> Show<U> for Box<U> {}
trait Factory { type Out<T: Allowed>: Show<T> DEFAULT }
struct Provider {}
impl Factory for Provider { BODY }
"#;
    for (default, body) in [("", "type Out<U> = Box<U>"), ("= Box<T>", "")] {
        let template = template.replace("DEFAULT", default).replace("BODY", body);
        check_cases(variants(
            &template,
            &[("Allowed", None), ("Extra", UNSATISFIED), ("Allowed", None)],
        ));
    }
}

#[test]
fn parameter_bounds_do_not_leak_to_sibling_types() {
    check_cases(variants(
        r#"
trait Allowed {}
trait Show<T> {}
struct Box<T> {}
impl<U: Allowed> Show<U> for Box<U> {}
trait Factory {
    type First<T: Allowed>: Show<T>
    type Second<T HOLE>: Show<T>
}
struct Provider {}
impl Factory for Provider {
    type First<U> = Box<U>
    type Second<U> = Box<U>
}
"#,
        &[(": Allowed", None), ("", UNSATISFIED), (": Allowed", None)],
    ));
}

#[test]
fn impl_definitions_match_the_declaration() {
    check_cases(
        [
            ("type Out<T>", "type Out<U> = u256", None),
            ("type Out<F: * -> *>", "type Out<G: * -> *> = u256", None),
            (
                "type Out<T>",
                "type Out = u256",
                Some("expected 1 type parameter"),
            ),
            (
                "type Out<T>",
                "type Out<U, V> = u256",
                Some("expected 1 type parameter"),
            ),
            // An impl that writes no kind has the declared one.
            ("type Out<F: * -> *>", "type Out<U> = U<u8>", None),
            (
                "type Out<F: * -> *>",
                "type Out<U: * -> * -> *> = u256",
                Some("associated type parameter does not match the trait"),
            ),
            (
                "type Out<T>",
                "type Out<U: * -> *> = u256",
                Some("associated type parameter does not match the trait"),
            ),
            (
                "type Out<T>",
                "type Out<U: Allowed> = u256",
                Some("bounds on associated type parameters must be written in the trait"),
            ),
            (
                "type Out<T: Missing>",
                "type Out<U> = u256",
                Some("`Missing` is not found"),
            ),
            (
                "type Out<T>",
                "type Out<U> = u256\n type Extra<V> = u256",
                Some("not defined in trait"),
            ),
            (
                "type Out<T>",
                "type Out<const N: usize> = u256",
                Some("associated types cannot have const parameters"),
            ),
            (
                "type Out<T, U = u8>",
                "type Out<A, B> = u256",
                Some("type parameters of associated types cannot have defaults"),
            ),
            ("type Out<T>", "type Out<U> = u256", None),
        ]
        .map(|(decl, definition, expected)| {
            (
                format!(
                    "trait Allowed {{}}\ntrait Family {{ {decl} }}\nstruct P {{}}\nimpl Family for P {{ {definition} }}"
                ),
                expected,
            )
        }),
    );
}

#[test]
fn parameters_use_the_generic_parameter_checks() {
    check_cases([
        (
            "trait Tr { type Out<T, T> }",
            Some("duplicate generic parameter name in associated type `Out`"),
        ),
        ("trait Tr<T> { type Out<U> = U }", None),
        (
            "trait Tr<T> { type Out<T> = T }",
            Some("generic parameter is already defined in the parent item"),
        ),
        (
            "trait Tr { type Out<T> }\nstruct S<T> {}\nimpl<T> Tr for S<T> { type Out<T> = T }",
            Some("generic parameter is already defined in the parent item"),
        ),
    ]);
}

#[test]
fn a_const_parameter_is_reported_once() {
    for source in [
        "trait Factory { type Arr<const N: usize> = [u8; N] }\nstruct F {}\nimpl Factory for F {}",
        "trait Factory { type Arr<const N: usize> = u8 }",
        "trait T { type A<const N: usize> }\nfn f<X: T>(_ x: X::A<3>) {}\nstruct S {}\nimpl T for S { type A<const N: usize> = [u8; N] }\nfn g(_ x: S::A<3>) {}",
        "trait T { type A<U> }\nstruct S {}\nimpl T for S { type A<const N: usize> = u8 }",
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(Utf8PathBuf::from("const_param.fe"), source);
        let result = diagnostics(&db, file);
        assert!(
            result.contains("associated types cannot have const parameters"),
            "{result}"
        );
        // Once for each parameter list that has one.
        let lists = source.matches("const N").count();
        assert_eq!(
            result.matches("error[").count(),
            lists,
            "{source}\n{result}"
        );
    }
}

#[test]
fn defaults_on_parameters_are_rejected_once() {
    let source = "trait Tr { type Out<T, U = u8> }\nstruct P {}\nimpl Tr for P { type Out<T, U> = (T, U) }\nfn one(_ x: <P as Tr>::Out<bool>) {}\nfn two(_ x: <P as Tr>::Out<bool, u16>) {}";
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(Utf8PathBuf::from("default.fe"), source);
    let result = diagnostics(&db, file);
    assert!(
        result.contains("type parameters of associated types cannot have defaults"),
        "{result}"
    );
    assert_eq!(result.matches("error[").count(), 1, "{result}");
}

#[test]
fn trait_bindings_cannot_set_a_type_with_parameters() {
    for consumer in [
        "fn unused<B: Family<Out = u256>>() {}",
        "fn unused<B>() where B: Family<Out = u256> {}",
    ] {
        let template = format!("trait Family {{ type OutHOLE }}\n{consumer}");
        check_cases(variants(
            &template,
            &[
                ("", None),
                (
                    "<T>",
                    Some("takes type parameters, so it cannot be set with `Out = ...`"),
                ),
                ("", None),
            ],
        ));
    }
}

#[test]
fn types_with_parameter_bounds_need_all_their_arguments() {
    for (declaration, definition, argument) in [
        ("type Out<T HOLE>", "type Out<U> = u256", ""),
        ("type Out<T HOLE> = u256", "", ""),
        ("type Out<T, U HOLE>", "type Out<A, B> = u256", "<bool>"),
    ] {
        for use_site in [
            "fn generic<B: Factory>(_ x: HCell<B::Out>) {}",
            "fn concrete(_ x: HCell<Provider::Out>) {}",
            "struct Holder<B: Factory> { value: HCell<B::Out> }",
            "type Alias = HCell<Provider::Out>",
        ] {
            let use_site = use_site.replace("::Out", &format!("::Out{argument}"));
            let template = format!(
                "trait Allowed {{}}\ntrait Factory {{ {declaration} }}\nstruct HCell<F: * -> *> {{}}\nstruct Provider {{}}\nimpl Factory for Provider {{ {definition} }}\n{use_site}"
            );
            check_cases(variants(
                &template,
                &[
                    ("", None),
                    (
                        ": Allowed",
                        Some(
                            "has bounds on its type parameters, so it needs all of its type arguments",
                        ),
                    ),
                    ("", None),
                ],
            ));
        }
    }
}

#[test]
fn extra_arguments_apply_to_a_family_that_returns_a_constructor() {
    // Arguments past the family's own parameters go to its result.
    let template = "trait Allowed {}\nimpl Allowed for bool {}\nstruct NotAllowed {}\n\
         struct Box<T> {}\n\
         trait Factory { type Out<T: Allowed>: * -> * }\n\
         struct Provider {}\n\
         impl Factory for Provider { type Out<U> = Box }\n\
         fn use_it(_ x: <Provider as Factory>::Out<HOLE, u256>) {}\n\
         fn generic<F: Factory>(_ x: <F as Factory>::Out<HOLE, u256>) {}";
    check_cases(variants(
        template,
        &[("bool", None), ("NotAllowed", UNSATISFIED), ("bool", None)],
    ));
}

#[test]
fn caller_arguments_are_not_rewritten_by_the_enclosing_trait() {
    // `Self::Next` replaces `Self` in the declaration, but the `Self` passed
    // as the family argument belongs to the caller.
    let template = "trait Allowed {}\n\
         trait Tr {\n\
             type Next: Tr\n\
             type Out<T: Allowed>\n\
             fn test(_ x: <Self::Next as Tr>::Out<Self>) HOLE\n\
         }";
    check_cases(variants(
        template,
        &[
            ("where Self: Allowed", None),
            ("", UNSATISFIED),
            ("where Self: Allowed", None),
        ],
    ));
}

#[test]
fn associated_const_captures_follow_impl_arguments() {
    // A plain associated type: projections are now cached by type rather than
    // by projection, which must not mix up two instances of the same impl.
    let template = "struct Slot<const N: usize> {}\n\
         struct S<const N: usize> {}\n\
         trait Step { type Out }\n\
         impl<const N: usize> Step for S<N> { type Out = Slot<{ N + 1 }> }\n\
         fn probe(x: <S<HOLE> as Step>::Out) -> Slot<OUT> { x }";
    check_cases(
        [(2, 3, None), (5, 6, None), (2, 6, MISMATCH), (2, 3, None)].map(
            |(argument, output, expected)| {
                (
                    template
                        .replace("HOLE", &argument.to_string())
                        .replace("OUT", &output.to_string()),
                    expected,
                )
            },
        ),
    );
}

/// Every permutation of independent tuple components reaches a limit or none
/// does, also when a family ignores an argument that reaches it, or when the
/// components share applications.
#[test]
fn reaching_a_limit_does_not_depend_on_the_order_of_components() {
    let prelude = "trait Tr {\n    type Out<T>\n    type Keep<T>\n}\nstruct Base {}\nstruct Wrap<U> {}\n\
         impl Tr for Base {\n    type Out<T> = T\n    type Keep<T> = u8\n}\n\
         impl<U: Tr> Tr for Wrap<U> {\n    type Out<T> = <U as Tr>::Out<(T, T)>\n    type Keep<T> = u8\n}\n\
         trait Exp {\n    type Two<X>\n}\nstruct Z {}\nstruct S<N> {}\n\
         impl Exp for Z {\n    type Two<X> = S<X>\n}\n\
         impl<N: Exp> Exp for S<N> {\n    type Two<X> = <N as Exp>::Two<<N as Exp>::Two<X>>\n}\n";
    let out = |layers: usize| {
        format!(
            "<{}Base{} as Tr>::Out<bool>",
            "Wrap<".repeat(layers),
            ">".repeat(layers)
        )
    };
    let keep = |inner: String| format!("<Base as Tr>::Keep<{inner}>");
    let two = |n: usize| format!("<{}Z{} as Exp>::Two<Z>", "S<".repeat(n), ">".repeat(n));
    // Components, and whether a limit is reached; `None` checks only that
    // every order agrees.
    let cases: Vec<(Vec<String>, Option<bool>)> = vec![
        (vec![out(8), out(8)], Some(false)),
        (vec![keep(out(12)), out(8)], Some(true)),
        (vec![out(11), keep(out(11)), out(5)], None),
        (vec![out(10), out(10), out(10)], None),
        (vec![two(13), out(5)], Some(true)),
        (vec![two(9), keep(two(9)), out(9)], None),
    ];
    fn permutations(items: &[String]) -> Vec<Vec<String>> {
        if items.len() <= 1 {
            return vec![items.to_vec()];
        }
        (0..items.len())
            .flat_map(|i| {
                let mut rest = items.to_vec();
                let first = rest.remove(i);
                permutations(&rest).into_iter().map(move |mut perm| {
                    perm.insert(0, first.clone());
                    perm
                })
            })
            .collect()
    }

    let mut src = prelude.to_string();
    let mut functions = Vec::new();
    for (case, (components, _)) in cases.iter().enumerate() {
        for (idx, perm) in permutations(components).into_iter().enumerate() {
            functions.push((case, format!("case{case}_{idx}")));
            src.push_str(&format!(
                "fn case{case}_{idx}(_ x: ({})) {{}}\n",
                perm.join(", ")
            ));
        }
    }
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(Utf8PathBuf::from("orders.fe"), &src);
    let result = diagnostics(&db, file);
    // The line of each function, and whether a limit is reported on it.
    let reported = |name: &str| {
        let line = src
            .lines()
            .position(|line| line.starts_with(&format!("fn {name}(")))
            .unwrap()
            + 1;
        result.contains(&format!("orders.fe:{line}:"))
    };
    for (case, (components, expected)) in cases.iter().enumerate() {
        let outcomes: Vec<bool> = functions
            .iter()
            .filter(|(c, _)| *c == case)
            .map(|(_, name)| reported(name))
            .collect();
        assert!(
            outcomes.iter().all(|&o| o == outcomes[0]),
            "case {case} {components:?}: the outcome depends on the order: {outcomes:?}\n{result}"
        );
        if let Some(expected) = expected {
            assert_eq!(
                outcomes[0], *expected,
                "case {case} {components:?}\n{result}"
            );
        }
    }
}

#[test]
fn an_omitted_kind_is_the_declared_one() {
    // The impl writes no kind for `F`; it has the trait's, also when the
    // family is applied.
    check_cases([(
        "trait Factory { type Out<F: * -> *> }\nstruct Box<T> {}\nstruct S {}\n\
         impl Factory for S { type Out<F> = F<u8> }\n\
         fn concrete(x: S::Out<Box>) -> Box<u8> { x }\n\
         fn generic<B: Factory>(_ x: B::Out<Box>) {}",
        None,
    )]);
}

#[test]
fn a_method_self_type_is_compared_with_the_resolved_impl_type() {
    // The impl's self type names an associated type; the method's `self`
    // has the same type once both are resolved.
    let plain = "trait Factory { type Plain }\nstruct R {}\nimpl Factory for R { type Plain = u8 }\n\
         struct Holder<G> { x: G }\n\
         impl Holder<<R as Factory>::Plain> { fn show(self) -> u8 { 1 } }\n\
         fn use_it(h: Holder<<R as Factory>::Plain>) -> u8 { h.show() }";
    let family = "trait Factory { type Pair<T> }\nstruct R {}\nimpl Factory for R { type Pair<T> = (bool, T) }\n\
         struct Holder<G: * -> *> { x: G<u8> }\n\
         impl Holder<<R as Factory>::Pair> { fn show(self) -> u8 { 1 } }\n\
         fn use_it(h: Holder<<R as Factory>::Pair>) -> u8 { h.show() }";
    check_cases([(plain, None), (family, None)]);
}

#[test]
fn unapplied_families_are_the_same_type_only_for_the_same_impl() {
    // Two impls may define a family alike. Passed without its arguments, a
    // family is told apart by the impl that defines it and that impl's
    // arguments, as a named type is, not by what its definition computes.
    let template = "trait Factory { type Pair<T> }\nstruct R<A> {}\nstruct Q {}\n\
         impl<A> Factory for R<A> { type Pair<T> = (bool, T) }\n\
         impl Factory for Q { type Pair<T> = (bool, T) }\n\
         struct Holder<G: * -> *> { x: G<u8> }\n\
         fn pass(h: Holder<<R<u8> as Factory>::Pair>) -> Holder<HOLE> { h }";
    check_cases(variants(
        template,
        &[
            ("<R<u8> as Factory>::Pair", None),
            ("<Q as Factory>::Pair", MISMATCH),
            ("<R<u16> as Factory>::Pair", MISMATCH),
            ("<R<u8> as Factory>::Pair", None),
        ],
    ));
}

#[test]
fn targeted_fallback_preserves_a_family_declared_binding() {
    check_cases([(
        "trait Base<X> { type Out }\ntrait Factory { type F<T>: Base<T, Out = bool> }\nfn family<T: Factory>(x: <T::F<u8> as Base<u8>>::Out) -> bool { x }\n",
        None,
    )]);
}

#[test]
fn a_family_application_is_looked_up_like_the_type_it_stands_for() {
    // `Provider::Out<u8>::make` and the qualified spelling both find the
    // inherent function of the type the definition gives, also when the
    // family returns a type constructor and is given one more argument.
    let template = "struct Holder<T> { v: T }\n\
         impl<T> Holder<T> { fn make(_ v: T) -> Holder<T> { Holder { v } } }\n\
         trait Factory { type Out<T>\n type Ctor<T>: * -> * }\n\
         struct S {}\n\
         impl Factory for S { type Out<T> = Holder<T>\n type Ctor<T> = Holder }\n\
         fn call() -> u8 {\n let _x = HOLE::make(1)\n 0\n}";
    check_cases(variants(
        template,
        &[
            ("S::Out<u8>", None),
            ("<S as Factory>::Out<u8>", None),
            ("S::Ctor<bool, u8>", None),
            ("<S as Factory>::Ctor<bool, u8>", None),
            ("S::Out<u8>", None),
        ],
    ));
}
