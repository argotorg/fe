//! Generation in packages: one prepared view per package revision, with
//! providers imported across files and from dependencies.
use common::{InputDb, dependencies::DependencyArguments};
use fe_driver::{DriverDataBase, generation::prepare_ingots};
use hir::{
    analysis::{
        semantic::{
            EvalOutcome, GenericSubst, SemConstScalar, SemConstValue, eval_body_owner_const,
        },
        ty::ty_check::BodyOwner,
    },
    hir_def::HirIngot,
};
use url::Url;

const ROOT: &str = "file:///tmp/fe-package-generation/";

fn url(path: &str) -> Url {
    Url::parse(&format!("{ROOT}{path}")).unwrap()
}

fn project(files: &[(&str, &str)]) -> DriverDataBase {
    let mut db = DriverDataBase::default();
    for (path, text) in files {
        db.workspace()
            .update(&mut db, url(path), (*text).to_string());
    }
    db
}

fn prepare(db: &DriverDataBase, root: &str) -> DriverDataBase {
    prepare_ingots(db, &[url(root)])
        .unwrap_or_else(|error| panic!("package preparation failed: {error}"))
        .expect("the package requests generation")
}

/// Evaluate a const function of one module to its integer value.
fn eval(db: &DriverDataBase, path: &str, name: &str) -> String {
    let file = db
        .workspace()
        .get(db, &url(path))
        .unwrap_or_else(|| panic!("missing file {path}"));
    let function = db
        .top_mod(file)
        .all_funcs(db)
        .iter()
        .copied()
        .find(|function| {
            function
                .name(db)
                .to_opt()
                .is_some_and(|ident| ident.data(db) == name)
        })
        .unwrap_or_else(|| panic!("missing function `{name}` in {path}"));
    let owner = BodyOwner::Func(function, None);
    let outcome = eval_body_owner_const(db, owner, GenericSubst::for_body_owner(db, owner, vec![]));
    let EvalOutcome::Ready(value) = outcome else {
        panic!("`{name}` did not evaluate: {outcome:?}")
    };
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = value.value(db)
    else {
        panic!("`{name}` did not return an integer: {:?}", value.value(db))
    };
    value.to_string()
}

/// All diagnostics of an ingot, formatted; empty when it checks cleanly.
fn ingot_errors(db: &DriverDataBase, root: &str) -> String {
    let ingot = db
        .workspace()
        .containing_ingot(db, url(root))
        .expect("indexed ingot");
    let diagnostics = db.run_on_ingot(ingot);
    if diagnostics.has_errors(db) {
        diagnostics.format_diags(db)
    } else {
        String::new()
    }
}

fn assert_authored_unchanged(db: &DriverDataBase, files: &[(&str, &str)]) {
    assert!(db.generation_inputs().functions(db).is_empty());
    assert!(db.generation_inputs().requests(db).is_empty());
    for (path, text) in files {
        let file = db.workspace().get(db, &url(path)).unwrap();
        assert_eq!(file.text(db).as_str(), *text, "{path} was rewritten");
    }
}

const APP_TOML: &str = "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n";
const APP_WITH_LIB_TOML: &str =
    "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n\n[dependencies]\nlib = { path = \"../lib\" }\n";
const LIB_TOML: &str = "[ingot]\nname = \"lib\"\nversion = \"0.1.0\"\n";

#[test]
fn two_consumers_share_one_deferred_codec_specialization() {
    let library =
        include_str!("../../fe/tests/fixtures/fe_test_runner/late_storage_codec/src/codec.fe");
    let caller =
        include_str!("../../fe/tests/fixtures/fe_test_runner/late_storage_codec/src/lib.fe")
            .replace("use ingot::codec::", "use lib::");
    let second = APP_WITH_LIB_TOML.replace("name = \"app\"", "name = \"other\"");
    let files = [
        ("lib/fe.toml", LIB_TOML),
        ("lib/src/lib.fe", library),
        ("app/fe.toml", APP_WITH_LIB_TOML),
        ("app/src/lib.fe", caller.as_str()),
        ("other/fe.toml", second.as_str()),
        ("other/src/lib.fe", caller.as_str()),
    ];
    let db = project(&files);
    assert!(prepare_ingots(&db, &[url("lib/")]).unwrap().is_none());

    let forward = prepare_ingots(&db, &[url("app/"), url("other/")])
        .unwrap()
        .expect("the concrete calls request a provider");
    let reverse = prepare_ingots(&db, &[url("other/"), url("app/")])
        .unwrap()
        .expect("the concrete calls request a provider");
    let shared_requests = |db: &DriverDataBase| {
        db.generation_inputs()
            .requests(db)
            .iter()
            .filter(|request| request.scoped_impl.is_some())
            .cloned()
            .collect::<Vec<_>>()
    };
    let requests = shared_requests(&forward);
    assert_eq!(requests, shared_requests(&reverse));
    assert_eq!(requests.len(), 1);
    let scoped = requests[0].scoped_impl.as_ref().unwrap();
    assert_eq!(scoped.demand.opening.file, url("lib/src/lib.fe"));
    for prepared in [&forward, &reverse] {
        for root in ["app/", "other/"] {
            let diagnostics = ingot_errors(prepared, root);
            assert!(diagnostics.is_empty(), "{root}: {diagnostics}");
        }
    }
    assert_authored_unchanged(&db, &files);
}

#[test]
fn deferred_dependency_codec_rebinds_arguments_in_the_provider_view() {
    let library =
        include_str!("../../fe/tests/fixtures/fe_test_runner/late_storage_codec/src/codec.fe")
            .replace(
                "pub struct Arg { value: u8 }",
                "pub struct Arg<T> { value: T }",
            )
            .replace("impl Gate for Arg {", "impl<T> Gate for Arg<T> {")
            .replace(
                "pub fn write<K>(array: mut SolArray<Elem<K, Arg>>)",
                "pub fn write<K, T>(array: mut SolArray<Elem<K, Arg<T>>>)",
            );
    let template =
        include_str!("../../fe/tests/fixtures/fe_test_runner/late_storage_codec/src/lib.fe")
            .replace("use ingot::codec::", "use lib::")
            .replace("msg M {", "struct PrivateArgument { value: u8 }\n\nmsg M {");
    let standin_dependency = "standin = { path = \"../standin\" }\n";
    let app_manifest = format!("{APP_WITH_LIB_TOML}{standin_dependency}");
    let library_with_standin = format!("{LIB_TOML}\n[dependencies]\n{standin_dependency}");
    let standin_manifest = r#"
[ingot]
name = "std"
version = "0.1.0"
[dependencies]
foundation = { path = "../foundation" }
"#;
    let standin = "pub struct Outside { value: u8 }\npub use foundation::FoundationType\n";
    let foundation_manifest = &LIB_TOML.replace("\"lib\"", "\"foundation\"");
    for (arg, imports, library_manifest, accepted) in [
        ("u8", "", LIB_TOML, true),
        ("(u8, [PrivateArgument; 1])", "", LIB_TOML, false),
        ("Outside", "use standin::Outside\n", LIB_TOML, false),
        (
            "FoundationType",
            "use standin::FoundationType\n",
            library_with_standin.as_str(),
            true,
        ),
    ] {
        let caller = format!("{imports}{template}")
            .replace("Elem<u8, Arg>", &format!("Elem<u8, Arg<{arg}>>"))
            .replace("write<u8>", &format!("write<u8, {arg}>"));
        let files = [
            ("lib/fe.toml", library_manifest),
            ("lib/src/lib.fe", library.as_str()),
            ("app/fe.toml", app_manifest.as_str()),
            ("app/src/lib.fe", caller.as_str()),
            ("standin/fe.toml", standin_manifest),
            ("standin/src/lib.fe", standin),
            ("foundation/fe.toml", foundation_manifest.as_str()),
            (
                "foundation/src/lib.fe",
                "pub struct FoundationType { value: u8 }",
            ),
        ];
        let db = project(&files);
        let result = prepare_ingots(&db, &[url("app/")]);
        if accepted {
            let prepared = result
                .unwrap_or_else(|error| panic!("{arg}: {error}"))
                .expect("the dependency codec is prepared");
            assert_eq!(ingot_errors(&prepared, "app/"), "");
        } else {
            let error = result.err().expect("the argument is outside the view");
            assert_eq!(error.file, url("lib/src/lib.fe"));
            assert_eq!(
                error.cause,
                "a scoped target argument names a declaration outside the provider's input files"
            );
        }
        assert_authored_unchanged(&db, &files);
    }
}

#[test]
fn quoted_import_paths_keep_the_producer_scope_after_dependency_edits() {
    let library = r#"
mod tuning {
    use core::meta::{Code, Expr}

    pub(ingot) const fn delta() -> u8 { 2 }
    const fn inner_delta() -> u8 { 2 }

    pub const fn fragment() -> Code<Expr> {
        quote {
            {
                use ingot::tuning as origin
                origin::inner_delta()
            }
        }
    }
}

pub mod producer {
    use core::derive::{Derive, DeriveInherent}
    use core::meta::{Code, Item, Record}

    use super::tuning
    const fn delta() -> u8 { 9 }

    pub struct Provider {}
    impl DeriveInherent for Provider {
        const fn derive(_ record: Record) -> Code<Item> {
            let inner = tuning::fragment()
            quote {
                pub const fn code(self) -> u8 {
                    use ingot::{tuning::{delta as from_ingot}}
                    use super::tuning::delta as from_parent
                    use tuning::delta as from_lexical_scope
                    use ingot::tuning as origin
                    use origin::delta
                    use origin as again
                    use again::delta as from_alias
                    let from_qualified = origin::delta() + again::delta()
                    from_ingot() + from_parent() + from_lexical_scope() + delta() + from_alias() + from_qualified + ${inner}
                }
            }
        }
    }
}
"#;
    let app = r#"
use lib::producer::Provider

pub mod tuning {
    pub const fn delta() -> u8 { 5 }
}

struct Target {}
impl Target with (Provider)
pub const fn result() -> u8 { Target {}.code() }
"#;
    let mut warm = project(&[
        ("app/fe.toml", APP_WITH_LIB_TOML),
        ("app/src/lib.fe", app),
        ("lib/fe.toml", LIB_TOML),
        ("lib/src/lib.fe", library),
    ]);
    let edited = library.replace("u8 { 2 }", "u8 { 3 }");

    for (source, expected) in [(library, "16"), (edited.as_str(), "24"), (library, "16")] {
        warm.workspace()
            .update(&mut warm, url("lib/src/lib.fe"), source.to_owned());
        let fresh = project(&[
            ("app/fe.toml", APP_WITH_LIB_TOML),
            ("app/src/lib.fe", app),
            ("lib/fe.toml", LIB_TOML),
            ("lib/src/lib.fe", source),
        ]);
        for db in [&warm, &fresh] {
            let prepared = prepare(db, "app/");
            assert_eq!(ingot_errors(&prepared, "app/"), "");
            assert_eq!(eval(&prepared, "app/src/lib.fe", "result"), expected);
        }
    }

    // The outer quotation cannot access a member that is private to the
    // producer of the spliced expression, even through its own alias.
    let private = library.replace("+ again::delta()", "+ again::inner_delta()");
    warm.workspace()
        .update(&mut warm, url("lib/src/lib.fe"), private);
    let prepared = prepare(&warm, "app/");
    let diagnostics = ingot_errors(&prepared, "app/");
    assert!(
        diagnostics.contains("`inner_delta` is not visible")
            && diagnostics.contains("again::inner_delta()")
            && !diagnostics.contains("origin::inner_delta()"),
        "{diagnostics}"
    );

    // A local value named `origin` does not provide the module binding.
    // Resolving the module through the outer quote must not grant access
    // from the producer of this fragment.
    let cross_splice = library.replacen(
        "use ingot::tuning as origin",
        "const fn origin() -> u8 { 0 }",
        1,
    );
    warm.workspace()
        .update(&mut warm, url("lib/src/lib.fe"), cross_splice);
    let prepared = prepare(&warm, "app/");
    let diagnostics = ingot_errors(&prepared, "app/");
    assert!(
        diagnostics.contains("`inner_delta` is not visible")
            && diagnostics.contains("origin::inner_delta()"),
        "{diagnostics}"
    );
}

#[test]
fn quoted_ingot_path_uses_the_dependency_producers_scope() {
    let library = r#"
trait Gate { const fn code(_ value: u8) -> u8 }
impl Gate for u8 { const fn code(_ value: u8) -> u8 { value } }

pub mod producer {
    use ingot::Gate
    use core::derive::{Derive, DeriveInherent}
    use core::meta::{Code, Item, Record}

    impl Gate for u8 as Encoded {
        const fn code(_ value: u8) -> u8 { value ^ 1 }
    }

    pub struct Provider {}
    impl DeriveInherent for Provider {
        const fn derive(_ record: Record) -> Code<Item> {
            quote {
                pub const fn code(self, _ input: u8) -> u8 {
                    with (ingot::producer::Encoded) { <u8 as Gate>::code(input) }
                }
            }
        }
    }
}
"#;
    let app = r#"
use lib::producer::Provider
struct Target {}
impl Target with (Provider)
pub const fn result() -> u8 { Target {}.code(7) }
"#;
    let files = [
        ("app/fe.toml", APP_WITH_LIB_TOML),
        ("app/src/lib.fe", app),
        ("lib/fe.toml", LIB_TOML),
        ("lib/src/lib.fe", library),
    ];
    let db = project(&files);
    let prepared = prepare(&db, "app/");
    assert_eq!(ingot_errors(&prepared, "app/"), "");
    assert_eq!(eval(&prepared, "app/src/lib.fe", "result"), "6");
    assert_authored_unchanged(&db, &files);
}

const SCORE: &str = "pub trait Score {\n    const fn score() -> usize\n}\n";
const MODEL: &str = "pub struct Pair {\n    pub a: u256,\n    pub b: u256,\n}\n";
const PROVIDERS: &str = r#"use core::derive::Derive
use core::meta::{Code, Item, Record, field_count}
use ingot::score::Score

pub struct FieldCount {}

impl Derive<trait Score> for FieldCount {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        quote { const fn score() -> usize { ${n} } }
    }
}
"#;
const REQUEST: &str = r#"use ingot::model::Pair
use ingot::providers::FieldCount
use ingot::score::Score

impl Score for Pair with (FieldCount)

pub const fn total() -> usize {
    Pair::score()
}
"#;

fn same_ingot_files() -> Vec<(&'static str, String)> {
    vec![
        ("app/fe.toml", APP_TOML.to_string()),
        ("app/src/score.fe", SCORE.to_string()),
        ("app/src/model.fe", MODEL.to_string()),
        ("app/src/providers.fe", PROVIDERS.to_string()),
        ("app/src/lib.fe", REQUEST.to_string()),
    ]
}

fn borrow<'a>(files: &'a [(&'static str, String)]) -> Vec<(&'static str, &'a str)> {
    files
        .iter()
        .map(|(path, text)| (*path, text.as_str()))
        .collect()
}

#[test]
fn explicit_derive_uses_a_provider_imported_from_another_file() {
    let files = same_ingot_files();
    let files = borrow(&files);
    let db = project(&files);
    let prepared = prepare(&db, "app/");
    // `Pair` has two fields; the request, provider, trait and target are in
    // four different files of one ingot.
    assert_eq!(eval(&prepared, "app/src/lib.fe", "total"), "2");
    assert_eq!(ingot_errors(&prepared, "app/"), "");
    assert_authored_unchanged(&db, &files);
    // Negative control: without preparation the header is not an
    // implementation, so the output above came from the prepared view.
    let authored = ingot_errors(&db, "app/");
    assert!(
        authored.contains("derive header was not prepared"),
        "{authored}"
    );
}

const LIB_GENERATE: &str = r#"use core::derive::Derive
use core::meta::{Code, Item, Record, field_count}

pub trait Score {
    const fn score() -> usize
}

pub struct FieldScore {}

impl Derive<trait Score> for FieldScore {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        quote {
            const fn score() -> usize { ${n} }
        }
    }
}
"#;
const APP_GENERATE_MODEL: &str = r#"pub struct Pair {
    pub a: u256,
    pub b: u256,
}
impl lib::Score for Pair with (lib::FieldScore)
"#;
const APP_GENERATE_LIB: &str = r#"use ingot::model::Pair
use lib::Score

pub const fn total() -> usize {
    Pair::score()
}
"#;

#[test]
fn dependency_provider_generates_an_impl_of_its_trait_for_an_app_record() {
    let files = [
        ("lib/fe.toml", LIB_TOML),
        ("lib/src/lib.fe", LIB_GENERATE),
        ("app/fe.toml", APP_WITH_LIB_TOML),
        ("app/src/model.fe", APP_GENERATE_MODEL),
        ("app/src/lib.fe", APP_GENERATE_LIB),
    ];
    let db = project(&files);
    let prepared = prepare(&db, "app/");
    assert_eq!(eval(&prepared, "app/src/lib.fe", "total"), "2");
    // The header in `model.fe` names the dependency's trait and provider by
    // path; `model.fe` imports nothing.
    assert_eq!(ingot_errors(&prepared, "app/"), "");
    assert_authored_unchanged(&db, &files);
    // Without preparation the header has no members, so the authored view
    // still reports an error.
    assert!(!ingot_errors(&db, "app/").is_empty());
}

const LIB_OWN_DERIVE: &str = r#"use core::derive::Derive
use core::meta::{Code, Item, Record, field_count}

pub trait Score {
    const fn score() -> usize
}

pub struct Counted {}

impl Derive<trait Score> for Counted {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        quote { const fn score() -> usize { ${n} } }
    }
}

pub struct Unit {
    pub x: u8,
    pub y: u8,
    pub z: u8,
}

impl Score for Unit with (Counted)

pub const fn unit_score() -> usize {
    Unit::score()
}
"#;
const APP_USES_LIB: &str = r#"use lib::{Score, Unit}

pub const fn from_dependency() -> usize {
    lib::unit_score()
}

pub const fn direct() -> usize {
    Unit::score()
}
"#;

/// An error inside generated code is reported where that code was written,
/// the dependency's quotation, not at an unrelated place in the target file.
#[test]
fn errors_in_code_generated_from_a_dependency_point_into_its_quote() {
    let mistyped = LIB_GENERATE.replace(
        "const fn score() -> usize { ${n} }",
        "const fn score() -> usize { true }",
    );
    assert_ne!(mistyped, LIB_GENERATE);
    let files = [
        ("lib/fe.toml", LIB_TOML),
        ("lib/src/lib.fe", mistyped.as_str()),
        ("app/fe.toml", APP_WITH_LIB_TOML),
        ("app/src/model.fe", APP_GENERATE_MODEL),
        ("app/src/lib.fe", APP_GENERATE_LIB),
    ];
    let db = project(&files);
    let prepared = prepare(&db, "app/");
    let errors = ingot_errors(&prepared, "app/");
    // Paths render relative to their ingot; the quoted line in the snippet is
    // read from the file the span resolved into, which only `lib` contains.
    let quote_line = mistyped
        .lines()
        .position(|line| line.contains("{ true }"))
        .unwrap()
        + 1;
    assert!(errors.contains("type mismatch"), "{errors}");
    assert!(
        errors.contains(&format!("src/lib.fe:{quote_line}:")),
        "expected line {quote_line}: {errors}"
    );
    assert!(
        errors.contains("const fn score() -> usize { true }"),
        "{errors}"
    );
    assert!(!errors.contains("src/model.fe"), "{errors}");
}

#[test]
fn a_dependency_s_own_generation_is_prepared_with_the_app() {
    let files = [
        ("lib/fe.toml", LIB_TOML),
        ("lib/src/lib.fe", LIB_OWN_DERIVE),
        ("app/fe.toml", APP_WITH_LIB_TOML),
        ("app/src/lib.fe", APP_USES_LIB),
    ];
    let db = project(&files);
    let prepared = prepare(&db, "app/");
    assert_eq!(eval(&prepared, "app/src/lib.fe", "from_dependency"), "3");
    assert_eq!(eval(&prepared, "app/src/lib.fe", "direct"), "3");
    assert_eq!(ingot_errors(&prepared, "app/"), "");
    assert_eq!(ingot_errors(&prepared, "lib/"), "");
}

/// Several packages that a command selects are prepared in one call: the
/// output of a dependency that they share is made once, and every selected
/// package reads it.
#[test]
fn several_selected_packages_share_one_preparation_of_a_common_dependency() {
    let app_toml = |name: &str| {
        format!(
            "[ingot]\nname = \"{name}\"\nversion = \"0.1.0\"\n\n[dependencies]\nlib = {{ path = \"../lib\" }}\n"
        )
    };
    let (first_toml, second_toml) = (app_toml("first"), app_toml("second"));
    let files = [
        ("lib/fe.toml", LIB_TOML),
        ("lib/src/lib.fe", LIB_OWN_DERIVE),
        ("first/fe.toml", first_toml.as_str()),
        ("first/src/lib.fe", APP_USES_LIB),
        ("second/fe.toml", second_toml.as_str()),
        ("second/src/lib.fe", APP_USES_LIB),
    ];
    let db = project(&files);
    let prepared = prepare_ingots(&db, &[url("first/"), url("second/")])
        .unwrap_or_else(|error| panic!("package preparation failed: {error}"))
        .expect("the dependency requests generation");
    assert_eq!(eval(&prepared, "first/src/lib.fe", "from_dependency"), "3");
    assert_eq!(eval(&prepared, "second/src/lib.fe", "from_dependency"), "3");
    // The one header of `lib` is run once, not once for each package.
    assert_eq!(prepared.generation_inputs().requests(&prepared).len(), 1);
    assert_eq!(ingot_errors(&prepared, "first/"), "");
    assert_eq!(ingot_errors(&prepared, "second/"), "");
    assert_authored_unchanged(&db, &files);
}

#[test]
fn a_faulting_dependency_provider_refuses_the_package_without_publishing() {
    let faulting = LIB_GENERATE.replace(
        "    let n = field_count(record)\n",
        "    let zero: usize = 0\n    let n = field_count(record) / zero\n",
    );
    let files = [
        ("lib/fe.toml", LIB_TOML),
        ("lib/src/lib.fe", faulting.as_str()),
        ("app/fe.toml", APP_WITH_LIB_TOML),
        ("app/src/model.fe", APP_GENERATE_MODEL),
        ("app/src/lib.fe", APP_GENERATE_LIB),
    ];
    let db = project(&files);
    let error = prepare_ingots(&db, &[url("app/")])
        .err()
        .expect("a provider fault refuses preparation");
    assert!(error.cause.contains("division by zero"), "{error}");
    assert_eq!(error.file, url("lib/src/lib.fe"), "{error}");
    assert_authored_unchanged(&db, &files);
}

#[test]
fn a_prepared_package_view_refuses_a_second_preparation() {
    let files = same_ingot_files();
    let db = project(&borrow(&files));
    let prepared = prepare(&db, "app/");
    let error = prepare_ingots(&prepared, &[url("app/")])
        .err()
        .expect("output is checked once");
    assert!(error.cause.contains("authored input view"), "{error}");
}

#[test]
fn a_package_without_requests_keeps_its_authored_view() {
    // An ordinary named choice is lexical selection, not a generation demand.
    let files = [
        ("app/fe.toml", APP_TOML),
        (
            "app/src/lib.fe",
            "trait Score { const fn score() -> u256 }\nstruct Pair {}\nimpl Score for Pair as Named { const fn score() -> u256 { 7 } }\nconst fn run() -> u256 { with (Named) { Pair::score() } }\n",
        ),
    ];
    let db = project(&files);
    assert!(prepare_ingots(&db, &[url("app/")]).unwrap().is_none());
}

/// Warm edits of one authored database against fresh databases built from
/// the same text: field, provider, and import alias edits, then restores.
#[test]
fn edits_and_restores_agree_with_fresh_preparation() {
    let original = same_ingot_files();
    let mut warm = project(&borrow(&original));
    let edit = |files: &[(&'static str, String)], path: &str, from: &str, to: &str| {
        files
            .iter()
            .map(|(name, text)| {
                let text = if *name == path {
                    assert!(text.contains(from), "{path} lacks {from:?}");
                    text.replace(from, to)
                } else {
                    text.clone()
                };
                (*name, text)
            })
            .collect::<Vec<_>>()
    };
    let steps = [
        (original.clone(), "2"),
        (
            edit(
                &original,
                "app/src/model.fe",
                "    pub b: u256,\n",
                "    pub b: u256,\n    pub c: bool,\n",
            ),
            "3",
        ),
        (original.clone(), "2"),
        (
            edit(
                &original,
                "app/src/providers.fe",
                "{ ${n} }",
                "{ ${n} + 10 }",
            ),
            "12",
        ),
        (original.clone(), "2"),
        (
            edit(
                &edit(
                    &original,
                    "app/src/lib.fe",
                    "use ingot::providers::FieldCount\n",
                    "use ingot::providers::FieldCount as Counter\n",
                ),
                "app/src/lib.fe",
                "with (FieldCount)",
                "with (Counter)",
            ),
            "2",
        ),
        (original.clone(), "2"),
    ];
    for (index, (files, expected)) in steps.iter().enumerate() {
        for (path, text) in files {
            warm.workspace().update(&mut warm, url(path), text.clone());
        }
        let warm_view = prepare(&warm, "app/");
        let fresh = project(&borrow(files));
        let fresh_view = prepare(&fresh, "app/");
        let warm_value = eval(&warm_view, "app/src/lib.fe", "total");
        let fresh_value = eval(&fresh_view, "app/src/lib.fe", "total");
        assert_eq!(warm_value, *expected, "warm value at step {index}");
        assert_eq!(fresh_value, *expected, "fresh value at step {index}");
        assert_eq!(
            ingot_errors(&warm_view, "app/"),
            ingot_errors(&fresh_view, "app/"),
            "diagnostics at step {index}"
        );
        assert_eq!(
            warm_view.generation_inputs().requests(&warm_view),
            fresh_view.generation_inputs().requests(&fresh_view),
            "frozen requests at step {index}"
        );
    }
}

#[test]
fn forked_inputs_keep_trust_roots_removals_and_settings() {
    let files = [
        ("lib/fe.toml", LIB_TOML),
        ("lib/src/lib.fe", "pub fn f() {}\n"),
        ("app/fe.toml", APP_WITH_LIB_TOML),
        ("app/src/lib.fe", "pub fn g() {}\n"),
        ("app/src/gone.fe", "pub fn h() {}\n"),
    ];
    let mut db = project(&files);
    let graph = db.dependency_graph();
    graph.register_root_ingot(&mut db, &url("app/"));
    graph.add_dependency(
        &mut db,
        &url("app/"),
        &url("lib/"),
        "lib".into(),
        DependencyArguments {
            allow_unsafe: true,
            ..DependencyArguments::default()
        },
    );
    db.workspace().remove(&mut db, &url("app/src/gone.fe"));
    fe_driver::project_target::set_target(&mut db, common::compilation::Target::Native);
    let fork = db.fork_inputs();
    let trusted = |db: &DriverDataBase| {
        let mut trusted = db
            .dependency_graph()
            .trusted_ingots(db)
            .into_iter()
            .collect::<Vec<_>>();
        trusted.sort();
        trusted
    };
    assert_eq!(trusted(&fork), trusted(&db));
    assert!(trusted(&fork).contains(&url("lib/")));
    for root in ["app/", "lib/"] {
        assert_eq!(
            fork.dependency_graph().is_trust_root(&fork, &url(root)),
            db.dependency_graph().is_trust_root(&db, &url(root)),
            "{root}"
        );
    }
    assert!(
        fork.workspace()
            .get(&fork, &url("app/src/gone.fe"))
            .is_none()
    );
    let urls = |db: &DriverDataBase| {
        db.workspace()
            .all_files(db)
            .iter()
            .map(|(url, file)| (url.clone(), file.text(db).clone()))
            .collect::<Vec<_>>()
    };
    assert_eq!(urls(&fork), urls(&db));
    assert_eq!(
        fork.compilation_settings().target(&fork),
        db.compilation_settings().target(&db)
    );
    assert_eq!(
        fork.compilation_settings().platform(&fork),
        db.compilation_settings().platform(&db)
    );
    let app = fork
        .workspace()
        .containing_ingot(&fork, url("app/"))
        .expect("app ingot");
    assert_eq!(
        app.resolved_external_ingots(&fork)
            .iter()
            .map(|(_, ingot)| ingot.base(&fork).clone())
            .filter(|base| base.scheme() == "file")
            .collect::<Vec<_>>(),
        vec![url("lib/")]
    );
}

/// A header whose `cfg` attribute does not hold is no request in a package
/// either: a package whose only header is dropped is prepared without it, and
/// the header of another file of the package is still prepared.
#[test]
fn a_header_whose_cfg_does_not_hold_is_not_a_request_in_a_package() {
    const PROVIDER: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}

struct P {}
impl DeriveInherent for P {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const fn marker() -> u256 { 7 } }
    }
}

struct S { a: u256 }

#[cfg(target_os = \"linux\")]
impl S with (P)
";
    const OTHER: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}

struct Q {}
impl DeriveInherent for Q {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const fn other() -> u256 { 9 } }
    }
}

struct T { a: u256 }
impl T with (Q)

const fn use_it() -> u256 { T::other() }
";
    let only = project(&[("app/fe.toml", APP_TOML), ("app/src/lib.fe", PROVIDER)]);
    let prepared = prepare_ingots(&only, &[url("app/")]);
    assert!(
        prepared.is_ok(),
        "{:?}",
        prepared.err().map(|error| error.cause)
    );
    let mixed = project(&[
        ("app/fe.toml", APP_TOML),
        ("app/src/lib.fe", PROVIDER),
        ("app/src/z.fe", OTHER),
    ]);
    let prepared = prepare(&mixed, "app/");
    assert_eq!(eval(&prepared, "app/src/z.fe", "use_it"), "9");
}

/// Module `a` declares `ProviderA` and `Left`, which derives with `ProviderB`;
/// module `b` declares `ProviderB` and `Right`, which derives with
/// `ProviderA`. Each module's output is written in the other module and names
/// the other module's declarations. `{b_score}` is `ProviderB`'s method body.
const MUTUAL_A_USES: &str = "use core::derive::Derive
use core::meta::{Code, Item, Record, field_count}
use ingot::score::Score
use ingot::{b}::ProviderB
";
const MUTUAL_A_ITEMS: [&str; 4] = [
    "pub struct ProviderA {}\n",
    r#"impl Derive<trait Score> for ProviderA {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        quote { const fn score() -> usize { ${n} } }
    }
}
"#,
    "pub struct Left {\n    pub only: u8,\n}\n\nimpl Score for Left with (ProviderB)\n",
    "pub const fn left_total() -> usize {\n    Left::score()\n}\n",
];
const MUTUAL_B_USES: &str = "use core::derive::Derive
use core::meta::{Code, Item, Record, field_count}
use ingot::score::Score
use ingot::{a}::ProviderA
";
const MUTUAL_B_ITEMS: [&str; 4] = [
    "pub struct ProviderB {}\n",
    r#"impl Derive<trait Score> for ProviderB {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        quote { const fn score() -> usize { {b_score} } }
    }
}
"#,
    "pub struct Right {\n    pub only: u8,\n}\n\nimpl Score for Right with (ProviderA)\n",
    "pub const fn right_total() -> usize {\n    Right::score()\n}\n",
];

fn module_path(name: &str) -> &'static str {
    match name {
        "alpha" => "app/src/alpha.fe",
        "beta" => "app/src/beta.fe",
        _ => unreachable!("fixture module {name}"),
    }
}

/// The mutual pair with module `a` named `a` and module `b` named `b`, items in
/// written or reversed order, and `ProviderB` emitting `b_score`.
fn mutual_package(
    a: &'static str,
    b: &'static str,
    reversed: bool,
    b_score: &str,
) -> Vec<(&'static str, String)> {
    let module = |uses: &str, items: &[&str]| {
        let mut items = items.to_vec();
        if reversed {
            items.reverse();
        }
        format!("{uses}\n{}", items.join("\n"))
            .replace("{a}", a)
            .replace("{b}", b)
            .replace("{b_score}", b_score)
    };
    vec![
        ("app/fe.toml", APP_TOML.to_string()),
        ("app/src/lib.fe", String::new()),
        ("app/src/score.fe", SCORE.to_string()),
        (module_path(a), module(MUTUAL_A_USES, &MUTUAL_A_ITEMS)),
        (module_path(b), module(MUTUAL_B_USES, &MUTUAL_B_ITEMS)),
    ]
}

/// The A4 destination. Executed at `4e49f5f54` this crashed (salsa cycle in
/// `scope_graph_impl`: lowering each module read the other's lowered HIR);
/// `071f26908` refused mutual generation before publication, and this test
/// was ignored. Generated code now names authored declarations of other
/// modules by authored references bound at resolution, so both modules
/// prepare. Both file orders and both declaration orders, with independent
/// expected values: each record has one field.
#[test]
fn mutually_deriving_files_prepare_without_a_cycle() {
    for (a, b) in [("alpha", "beta"), ("beta", "alpha")] {
        for reversed in [false, true] {
            let files = mutual_package(a, b, reversed, "${n}");
            let files = borrow(&files);
            let db = project(&files);
            let prepared = prepare(&db, "app/");
            let case = format!("a = {a}, reversed = {reversed}");
            assert_eq!(eval(&prepared, module_path(a), "left_total"), "1", "{case}");
            assert_eq!(
                eval(&prepared, module_path(b), "right_total"),
                "1",
                "{case}"
            );
            assert_eq!(ingot_errors(&prepared, "app/"), "", "{case}");
            assert_authored_unchanged(&db, &files);
        }
    }
}

/// Generated output may name other generated output across modules: `Left`'s
/// method, written in `b`, calls `Right`'s, written in `a`. Neither provider
/// executes generated code; the output is checked once.
#[test]
fn mutually_generated_outputs_name_each_other_across_modules() {
    let files = mutual_package("alpha", "beta", false, "${n} + Right::score()");
    let db = project(&borrow(&files));
    let prepared = prepare(&db, "app/");
    assert_eq!(eval(&prepared, "app/src/beta.fe", "right_total"), "1");
    assert_eq!(eval(&prepared, "app/src/alpha.fe", "left_total"), "2");
    assert_eq!(ingot_errors(&prepared, "app/"), "");
}

/// Warm edits in either module of the mutual pair against fresh preparations
/// of the same text: a field added to `Right` and a literal in `ProviderA`,
/// each followed by a restore.
#[test]
fn mutual_generation_edits_and_restores_agree_with_fresh_preparation() {
    let original = mutual_package("alpha", "beta", false, "${n} + Right::score()");
    let edit = |path: &str, from: &str, to: &str| {
        original
            .iter()
            .map(|(name, text)| {
                let text = if *name == path {
                    assert!(text.contains(from), "{path} lacks {from:?}");
                    text.replace(from, to)
                } else {
                    text.clone()
                };
                (*name, text)
            })
            .collect::<Vec<_>>()
    };
    let steps = [
        (original.clone(), ("2", "1")),
        (
            edit(
                "app/src/beta.fe",
                "    pub only: u8,\n",
                "    pub only: u8,\n    pub more: bool,\n",
            ),
            ("3", "2"),
        ),
        (original.clone(), ("2", "1")),
        (
            edit("app/src/alpha.fe", "{ ${n} }", "{ ${n} + 10 }"),
            ("12", "11"),
        ),
        (original.clone(), ("2", "1")),
    ];
    let mut warm = project(&borrow(&original));
    for (index, (files, (left, right))) in steps.iter().enumerate() {
        for (path, text) in files {
            warm.workspace().update(&mut warm, url(path), text.clone());
        }
        let warm_view = prepare(&warm, "app/");
        let fresh = project(&borrow(files));
        let fresh_view = prepare(&fresh, "app/");
        for view in [&warm_view, &fresh_view] {
            assert_eq!(
                eval(view, "app/src/alpha.fe", "left_total"),
                *left,
                "left at step {index}"
            );
            assert_eq!(
                eval(view, "app/src/beta.fe", "right_total"),
                *right,
                "right at step {index}"
            );
        }
        assert_eq!(
            ingot_errors(&warm_view, "app/"),
            ingot_errors(&fresh_view, "app/"),
            "diagnostics at step {index}"
        );
        assert_eq!(
            warm_view.generation_inputs().requests(&warm_view),
            fresh_view.generation_inputs().requests(&fresh_view),
            "frozen requests at step {index}"
        );
    }
}

const HOLDS: &str = "pub trait Holds {\n    type Part\n}\n";
const CYCLE_ALPHA: &str = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
use ingot::holds::Holds
use ingot::beta::PartOfRight

pub struct PartOfLeft {}

impl Derive<trait Holds> for PartOfLeft {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { type Part = <Left as Holds>::Part }
    }
}

pub struct Left {}

impl Holds for Left with (PartOfRight)
"#;
const CYCLE_BETA: &str = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
use ingot::holds::Holds
use ingot::alpha::PartOfLeft

pub struct PartOfRight {}

impl Derive<trait Holds> for PartOfRight {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { type Part = <Right as Holds>::Part }
    }
}

pub struct Right {}

impl Holds for Right with (PartOfLeft)
"#;
const CYCLE_ALPHA_HANDWRITTEN: &str = r#"use ingot::holds::Holds
use ingot::beta::Right

pub struct Left {}

impl Holds for Left {
    type Part = <Right as Holds>::Part
}
"#;
const CYCLE_BETA_HANDWRITTEN: &str = r#"use ingot::holds::Holds
use ingot::alpha::Left

pub struct Right {}

impl Holds for Right {
    type Part = <Left as Holds>::Part
}
"#;

fn error_headers(errors: &str) -> Vec<&str> {
    let mut headers = errors
        .lines()
        .filter(|line| line.starts_with("error"))
        .collect::<Vec<_>>();
    headers.sort();
    headers
}

/// A loop in the source program, here between two generated associated types
/// whose quotes name each other's records through qualified paths, is the
/// program's own business: preparation succeeds and the diagnostics are those
/// of the handwritten twin, whatever they are. The compiler currently accepts
/// the handwritten loop (`fe check` reports nothing for it, and for a single
/// `type Part = <Self as Holds>::Part`), so the contract tested here is
/// agreement with the twin and no generation cycle, not a cycle diagnostic.
#[test]
fn a_loop_between_mutually_generated_associated_types_agrees_with_its_handwritten_twin() {
    let generated = [
        ("app/fe.toml", APP_TOML),
        ("app/src/lib.fe", ""),
        ("app/src/holds.fe", HOLDS),
        ("app/src/alpha.fe", CYCLE_ALPHA),
        ("app/src/beta.fe", CYCLE_BETA),
    ];
    let handwritten = [
        ("app/fe.toml", APP_TOML),
        ("app/src/lib.fe", ""),
        ("app/src/holds.fe", HOLDS),
        ("app/src/alpha.fe", CYCLE_ALPHA_HANDWRITTEN),
        ("app/src/beta.fe", CYCLE_BETA_HANDWRITTEN),
    ];
    let db = project(&generated);
    let prepared = prepare(&db, "app/");
    let generated_errors = ingot_errors(&prepared, "app/");
    let handwritten_errors = ingot_errors(&project(&handwritten), "app/");
    assert_eq!(
        error_headers(&generated_errors),
        error_headers(&handwritten_errors),
        "generated:\n{generated_errors}\nhandwritten:\n{handwritten_errors}"
    );
    assert_authored_unchanged(&db, &generated);
}

const REPORT: &str = "pub trait Report {\n    const fn weight() -> u256\n}\n";

/// Prepare a three-module package (`score`, `alpha`, `beta` under `app/src`),
/// check that it is free of errors, and leave the authored text unchanged.
fn prepared_modules(score: &str, alpha: &str, beta: &str) -> DriverDataBase {
    let files = [
        ("app/fe.toml", APP_TOML),
        ("app/src/lib.fe", ""),
        ("app/src/score.fe", score),
        ("app/src/alpha.fe", alpha),
        ("app/src/beta.fe", beta),
    ];
    let db = project(&files);
    let prepared = prepare(&db, "app/");
    assert_eq!(ingot_errors(&prepared, "app/"), "");
    assert_authored_unchanged(&db, &files);
    prepared
}

/// A generic provider declared in `beta` and applied in `alpha` to a concrete
/// type argument, to a constant argument through an alias declared in `alpha`,
/// and twice in one function with different arguments. Each application is a
/// separate implementation, so the values do not mix.
#[test]
fn scoped_generic_provider_applications_cross_modules() {
    let mut prepared = prepared_modules(
        REPORT,
        r#"use ingot::beta::{Scaled, Weighted}
use ingot::score::Report

pub struct Pair {}
pub type Ten = Scaled<10>
pub const fn typed() -> u256 { with (Weighted<u8>) { Pair::weight() } }
pub const fn through_alias() -> u256 { with (Ten) { Pair::weight() } }
pub const fn both() -> u256 {
    with (Scaled<10>) { Pair::weight() } * 100 + with (Scaled<20>) { Pair::weight() }
}
pub const fn typed_pair() -> u256 {
    with (Weighted<u8>) { Pair::weight() } * 100 + with (Weighted<u256>) { Pair::weight() }
}
"#,
        r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
use ingot::score::Report

pub struct Weighted<T> {}
impl<T> Derive<trait Report> for Weighted<T> {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn weight() -> u256 { 7 } }
    }
}
pub struct Scaled<const K: u256> {}
impl<const K: u256> Derive<trait Report> for Scaled<K> {
    const fn derive(_ record: Record) -> Code<Item> {
        let k = K
        quote { const fn weight() -> u256 { ${k} } }
    }
}
"#,
    );
    let alpha = "app/src/alpha.fe";
    assert_eq!(eval(&prepared, alpha, "typed"), "7");
    assert_eq!(eval(&prepared, alpha, "through_alias"), "10");
    assert_eq!(eval(&prepared, alpha, "both"), "1020");
    assert_eq!(eval(&prepared, alpha, "typed_pair"), "707");

    use salsa::Setter;

    let inputs = prepared.generation_inputs();
    let original = inputs.requests(&prepared).clone();
    let mut edited = original.clone();
    let index = edited
        .iter()
        .position(|request| request.scoped_impl.is_some())
        .expect("the package prepared scoped implementations");
    let removed = edited.remove(index).scoped_impl.unwrap().demand;
    assert!(hir::lower::prepared_scoped_item(&prepared, &removed).is_some());
    inputs.set_requests(&mut prepared).to(edited);
    assert!(hir::lower::prepared_scoped_item(&prepared, &removed).is_none());
    inputs.set_requests(&mut prepared).to(original);
    assert!(hir::lower::prepared_scoped_item(&prepared, &removed).is_some());
    assert_eq!(eval(&prepared, alpha, "both"), "1020");
}

/// Two applications of one provider whose implementations differ per argument
/// (`Provider<u8>` and `Provider<u256>`), named through aliases of the
/// other module.
#[test]
fn scoped_generic_provider_aliases_pick_their_own_implementation_across_modules() {
    let prepared = prepared_modules(
        "pub trait Value {\n    const fn value() -> u256\n}\npub struct Pair {}\n",
        r#"use ingot::beta::Provider
use ingot::score::Pair

type P8 = Provider<u8>
type P256 = Provider<u256>
pub const fn observe() -> u256 {
    with (P8) { Pair::value() } * 100 + with (P256) { Pair::value() }
}
"#,
        r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
use ingot::score::Value

pub struct Provider<T> {}
impl Derive<trait Value> for Provider<u8> {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn value() -> u256 { 1 } }
    }
}
impl Derive<trait Value> for Provider<u256> {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn value() -> u256 { 2 } }
    }
}
"#,
    );
    assert_eq!(eval(&prepared, "app/src/alpha.fe", "observe"), "102");
}

const SEEDS: &str = r#"pub trait Score {
    const fn score() -> u256
}
pub trait Seed {
    const fn seed() -> u256
}
pub struct Pair {}
"#;

const RULE: &str = "what generation produces in its own package is not visible to it";

/// `A`, written in `alpha`, opens the scoped output of `B`, written in
/// `beta`; `B` opens that of `A` when `cyclic`, and is a leaf otherwise.
fn two_module_providers(cyclic: bool) -> (String, String) {
    let alpha = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
use ingot::score::{Pair, Score, Seed}
use ingot::beta::B

pub struct A {}
impl Derive<trait Score> for A {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = with (B) { Pair::seed() }
        quote { const fn score() -> u256 { ${n} + 1 } }
    }
}
pub const fn run() -> u256 { with (A) { Pair::score() } }
"#;
    let seed = if cyclic {
        "with (A) { Pair::score() }"
    } else {
        "41"
    };
    let beta = format!(
        r#"use core::derive::Derive
use core::meta::{{Code, Item, Record}}
use ingot::score::{{Pair, Score, Seed}}
use ingot::alpha::A

pub struct B {{}}
impl Derive<trait Seed> for B {{
    const fn derive(_ record: Record) -> Code<Item> {{
        let n: u256 = {seed}
        quote {{ const fn seed() -> u256 {{ ${{n}} + 1 }} }}
    }}
}}
"#
    );
    (alpha.to_string(), beta)
}

/// A provider that opens another module's provider reads what generation
/// produces, which the one-view rule refuses. The refusal comes before any
/// provider runs, at a `with` binding that names one of the two providers,
/// whether or not `B` opens `A` in turn.
#[test]
fn a_provider_that_opens_another_modules_provider_is_refused_where_it_waits() {
    for cyclic in [true, false] {
        let (alpha, beta) = two_module_providers(cyclic);
        let files = [
            ("app/fe.toml", APP_TOML),
            ("app/src/lib.fe", ""),
            ("app/src/score.fe", SEEDS),
            ("app/src/alpha.fe", alpha.as_str()),
            ("app/src/beta.fe", beta.as_str()),
        ];
        let db = project(&files);
        let error = prepare_ingots(&db, &[url("app/")])
            .err()
            .expect("a provider that opens a provider is refused");
        assert!(error.cause.contains(RULE), "{error}");
        assert_eq!(error.file, url("app/src/alpha.fe"), "{error}");
        let start = usize::from(error.range.start());
        let end = usize::from(error.range.end());
        assert!(
            matches!(&alpha[start..end], "A" | "B"),
            "{error}: {}",
            &alpha[start..end]
        );
        assert_authored_unchanged(&db, &files);
    }
}

/// The accepted way to share a value between providers of two modules: an
/// ordinary function that both read.
#[test]
fn providers_in_two_modules_share_an_authored_helper() {
    let alpha = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
use ingot::score::{Pair, Score, Seed}
use ingot::beta::{B, shared}

pub struct A {}
impl Derive<trait Score> for A {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = shared()
        quote { const fn score() -> u256 { ${n} + 1 } }
    }
}
pub const fn run() -> u256 { with (A) { Pair::score() } + with (B) { Pair::seed() } }
"#;
    let beta = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
use ingot::score::{Pair, Score, Seed}

pub const fn shared() -> u256 { 41 }
pub struct B {}
impl Derive<trait Seed> for B {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = shared()
        quote { const fn seed() -> u256 { ${n} + 1 } }
    }
}
"#;
    let prepared = prepared_modules(SEEDS, alpha, beta);
    assert_eq!(eval(&prepared, "app/src/alpha.fe", "run"), "84");
}

/// More independent scoped requests than the rounds a transaction may build:
/// one provider written in `alpha` serves 130 records written in `beta`. All
/// share a round, so the program prepares in two views.
#[test]
fn many_independent_scoped_requests_across_modules_share_a_round() {
    const RECORDS: usize = 130;
    let mut alpha = String::from(
        r#"use core::derive::Derive
use core::meta::{Code, Item, Record, field_count}
use ingot::score::Score
use ingot::beta::*

pub struct ProviderA {}
impl Derive<trait Score> for ProviderA {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record) as u256
        quote { const fn score() -> u256 { ${n} } }
    }
}
pub const fn total() -> u256 {
    with (ProviderA) {
        0
"#,
    );
    let mut beta = String::new();
    for record in 0..RECORDS {
        alpha += &format!("            + R{record}::score()\n");
        beta += &format!("pub struct R{record} {{ pub a: u8 }}\n");
    }
    alpha += "    }\n}\n";
    let prepared = prepared_modules(
        "pub trait Score {\n    const fn score() -> u256\n}\n",
        &alpha,
        &beta,
    );
    assert_eq!(
        eval(&prepared, "app/src/alpha.fe", "total"),
        RECORDS.to_string()
    );
}

const TYPES: &str = r#"pub trait Bits {
    const BITS: u256
}
impl Bits for u8 {
    const BITS: u256 = 8
}
pub struct Wrap<T> {
    pub item: T,
}
impl<T: Bits> Bits for Wrap<T> {
    const BITS: u256 = 2 * <T as Bits>::BITS
}
pub trait Report {
    const fn bits() -> u256
}
"#;

/// Two modules derive for each other's records, and the generated code names
/// a generic type applied to the other module's type (`Wrap<Right>`,
/// `Wrap<Left>`), taken from field types. Each value is derived from the
/// `Bits` implementations alone: `Left` holds `Wrap<Right>` (2 * 16) and
/// `Other` holds `Wrap<Left>` (2 * 100).
#[test]
fn generated_code_names_generic_types_of_the_other_module() {
    let alpha = r#"use core::derive::Derive
use core::meta::{Code, Item, Record, field, field_type}
use ingot::score::{Bits, Report, Wrap}
use ingot::beta::{ProviderB, Right}

pub struct ProviderA {}
impl Derive<trait Report> for ProviderA {
    const fn derive(_ record: Record) -> Code<Item> {
        let ty = field_type(field(record, 0))
        quote { const fn bits() -> u256 { <${ty} as Bits>::BITS } }
    }
}
pub struct Left { pub x: Wrap<Right> }
impl Bits for Left { const BITS: u256 = 100 }
impl Report for Left with (ProviderB)
pub const fn left_bits() -> u256 { Left::bits() }
"#;
    let beta = r#"use core::derive::Derive
use core::meta::{Code, Item, Record, field, field_type}
use ingot::score::{Bits, Report, Wrap}
use ingot::alpha::{Left, ProviderA}

pub struct ProviderB {}
impl Derive<trait Report> for ProviderB {
    const fn derive(_ record: Record) -> Code<Item> {
        let ty = field_type(field(record, 0))
        quote { const fn bits() -> u256 { <${ty} as Bits>::BITS } }
    }
}
pub struct Right { pub y: Wrap<u8> }
impl Bits for Right { const BITS: u256 = 16 }
impl Report for Right with (ProviderA)
pub struct Other { pub w: Wrap<Left> }
impl Report for Other with (ProviderA)
pub const fn right_bits() -> u256 { Right::bits() }
pub const fn other_bits() -> u256 { Other::bits() }
"#;
    let prepared = prepared_modules(TYPES, alpha, beta);
    assert_eq!(eval(&prepared, "app/src/alpha.fe", "left_bits"), "32");
    assert_eq!(eval(&prepared, "app/src/beta.fe", "right_bits"), "16");
    assert_eq!(eval(&prepared, "app/src/beta.fe", "other_bits"), "200");
}

/// A header on one record in each of two modules, each provider written in the
/// other module, and one output naming the other's.
#[test]
fn mutual_header_requests_across_modules() {
    let score = "pub trait Score {\n    const fn score() -> u256\n}\n";
    let alpha = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}
use ingot::score::Score
use ingot::beta::{MakeLeft, Right}

pub struct Left {}
impl Score for Left with (MakeLeft)

pub struct MakeRight {}
impl Derive<trait Score> for MakeRight {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn score() -> u256 { 2 } }
    }
}
pub const fn left_total() -> u256 { Left::score() }
"#;
    let beta = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}
use ingot::score::Score
use ingot::alpha::MakeRight

pub struct MakeLeft {}
impl Derive<trait Score> for MakeLeft {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn score() -> u256 { 1 + Right::score() } }
    }
}
pub struct Right {}
impl Score for Right with (MakeRight)
"#;
    let prepared = prepared_modules(score, alpha, beta);
    assert_eq!(eval(&prepared, "app/src/alpha.fe", "left_total"), "3");
}

/// A chain of scoped providers that alternates between modules: `A` (alpha)
/// opens `B` (beta), which opens `C` (alpha). Each link reads the output of
/// the next, so the chain is refused at the first binding that waits.
#[test]
fn a_scoped_chain_that_alternates_between_modules_is_refused() {
    let score = "pub trait Score {\n    const fn score() -> u256\n}\npub trait Seed {\n    const fn seed() -> u256\n}\npub trait Base {\n    const fn base() -> u256\n}\npub struct Pair {}\n";
    let alpha = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}
use ingot::score::{Score, Seed, Base, Pair}
use ingot::beta::B

pub struct A {}
impl Derive<trait Score> for A {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = with (B) { Pair::seed() }
        quote { const fn score() -> u256 { ${n} + 1 } }
    }
}
pub struct C {}
impl Derive<trait Base> for C {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn base() -> u256 { 40 } }
    }
}
pub const fn run() -> u256 { with (A) { Pair::score() } }
"#;
    let beta = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}
use ingot::score::{Seed, Base, Pair}
use ingot::alpha::C

pub struct B {}
impl Derive<trait Seed> for B {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = with (C) { Pair::base() }
        quote { const fn seed() -> u256 { ${n} + 1 } }
    }
}
"#;
    let files = [
        ("app/fe.toml", APP_TOML),
        ("app/src/lib.fe", ""),
        ("app/src/score.fe", score),
        ("app/src/alpha.fe", alpha),
        ("app/src/beta.fe", beta),
    ];
    let db = project(&files);
    let error = prepare_ingots(&db, &[url("app/")])
        .err()
        .expect("a provider chain is refused");
    assert!(error.cause.contains(RULE), "{error}");
    assert_eq!(error.file, url("app/src/alpha.fe"), "{error}");
    assert_authored_unchanged(&db, &files);
}

/// A generic header (`impl<T> Report for Box<T>`) whose provider is a generic
/// type applied to a concrete argument, with the providers written in the
/// other module, in both directions. Only a provider written over the
/// header's own parameter is refused.
#[test]
fn generic_headers_accept_ground_generic_providers_from_the_other_module() {
    let alpha = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
use ingot::score::Report
use ingot::beta::Weighted

pub struct Heavy<T> {}
impl<T> Derive<trait Report> for Heavy<T> {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn weight() -> u256 { 100 } }
    }
}
pub struct Box<T> { pub value: T }
impl<T> Report for Box<T> with (Weighted<u8>)
pub const fn box8() -> u256 { <Box<u8> as Report>::weight() }
"#;
    let beta = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
use ingot::score::Report
use ingot::alpha::Heavy

pub struct Weighted<T> {}
impl<T> Derive<trait Report> for Weighted<T> {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn weight() -> u256 { 7 } }
    }
}
pub struct Crate<T> { pub value: T }
impl<T> Report for Crate<T> with (Heavy<u8>)
pub const fn crate8() -> u256 { <Crate<u8> as Report>::weight() }
"#;
    let prepared = prepared_modules(REPORT, alpha, beta);
    assert_eq!(eval(&prepared, "app/src/alpha.fe", "box8"), "7");
    assert_eq!(eval(&prepared, "app/src/beta.fe", "crate8"), "100");
}

/// Two headers of one file generate the same member of one type. The program
/// is refused with a conflict between the two generated methods, whichever
/// other files the package holds.
#[test]
fn two_headers_that_generate_one_member_conflict_whichever_other_files_the_package_holds() {
    const LIB: &str = "
struct A1 {}
impl A1 { fn f1() -> u256 { 1 } }
struct A2 {}
impl A2 { fn f2() -> u256 { 2 } }
struct A3 {}
impl A3 { fn f3() -> u256 { 3 } }
struct A4 {}
impl A4 { fn f4() -> u256 { 4 } }
";
    const Z: &str = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}

struct P1 {}
impl DeriveInherent for P1 {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub fn size(self) -> u256 { 1 } }
    }
}
struct P2 {}
impl DeriveInherent for P2 {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub fn size(self) -> u256 { 2 } }
    }
}

struct X { a: u256 }
impl X with (P1)
impl X with (P2)
";
    let files = [
        ("app/fe.toml", APP_TOML),
        ("app/src/lib.fe", LIB),
        ("app/src/z.fe", Z),
    ];
    let db = project(&files);
    let prepared = prepare(&db, "app/");
    let errors = ingot_errors(&prepared, "app/");
    assert!(
        errors.contains("conflicting method implementations"),
        "{errors}"
    );
}
#[test]
fn quoted_imports_use_the_producers_dependency_edges() {
    let library = r#"
pub use rules::delta as reexported

pub mod producer {
    use core::derive::{Derive, DeriveInherent}
    use core::meta::{Code, Item, Record}

    pub struct Provider {}
    impl DeriveInherent for Provider {
        const fn derive(_ record: Record) -> Code<Item> {
            quote {
                pub const fn code(self) -> u8 {
                    use ingot::reexported
                    use rules::delta
                    reexported() + delta()
                }
            }
        }
    }
}
"#;
    let app = r#"
use lib::producer::Provider
struct Target {}
impl Target with (Provider)
pub const fn result() -> u8 { Target {}.code() }
"#;
    let dependency = "rules = { path = \"../rules\" }\n";
    let lib_with_rules = format!("{LIB_TOML}\n[dependencies]\n{dependency}");
    let app_with_rules = format!("{APP_WITH_LIB_TOML}{dependency}");
    let rules_toml = "[ingot]\nname = \"rules\"\nversion = \"0.1.0\"\n";
    let rules = "pub const fn delta() -> u8 { 2 }";
    let files = [
        ("app/fe.toml", APP_WITH_LIB_TOML),
        ("app/src/lib.fe", app),
        ("lib/fe.toml", lib_with_rules.as_str()),
        ("lib/src/lib.fe", library),
        ("rules/fe.toml", rules_toml),
        ("rules/src/lib.fe", rules),
    ];
    let db = project(&files);
    let prepared = prepare(&db, "app/");
    assert_eq!(ingot_errors(&prepared, "app/"), "");
    assert_eq!(eval(&prepared, "app/src/lib.fe", "result"), "4");

    // The provider can no longer see rules, although the app can. Remove the
    // authored re-export so only the quoted import tries to resolve rules.
    let no_lib_rule = library
        .replace("pub use rules::delta as reexported", "")
        .replace("use ingot::reexported", "")
        .replace("reexported() + delta()", "delta()");
    let files = [
        ("app/fe.toml", app_with_rules.as_str()),
        ("app/src/lib.fe", app),
        ("lib/fe.toml", LIB_TOML),
        ("lib/src/lib.fe", no_lib_rule.as_str()),
        ("rules/fe.toml", rules_toml),
        ("rules/src/lib.fe", rules),
    ];
    let db = project(&files);
    let prepared = prepare(&db, "app/");
    let diagnostics = ingot_errors(&prepared, "app/");
    assert!(
        diagnostics.contains("`rules` is not found"),
        "{diagnostics}"
    );
}

#[test]
fn scoped_quotation_uses_the_producers_dependency_edges() {
    let library = r#"
use core::meta::{Code, Item}

pub const fn helper() -> u8 { 9 }
pub mod local_rules { pub const fn score() -> u8 { 4 } }

pub const fn fragment() -> Code<Item> {
    quote {
        const fn score() -> u8 {
            use rules::score as imported
            imported() + rules::score()
        }
    }
}
"#;
    let app = r#"
use core::derive::Derive
use core::meta::{Code, Item, Record}

trait Score { const fn score() -> u8 }
struct Target {}
struct Provider {}

impl Derive<trait Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> { lib::fragment() }
}

pub const fn result() -> u8 { with (Provider) { Target::score() } }
"#;
    let lib_with_rules = format!("{LIB_TOML}\n[dependencies]\nrules = {{ path = \"../rules\" }}\n");
    let app_with_shadow = format!("{APP_WITH_LIB_TOML}rules = {{ path = \"../shadow\" }}\n");
    let shadow_toml = LIB_TOML.replace("\"lib\"", "\"shadow\"");
    let rules_toml = LIB_TOML.replace("\"lib\"", "\"rules\"");
    let check = |lib_manifest: &str, app_manifest: &str, library: &str| {
        let db = project(&[
            ("app/fe.toml", app_manifest),
            ("app/src/lib.fe", app),
            ("lib/fe.toml", lib_manifest),
            ("lib/src/lib.fe", library),
            ("rules/fe.toml", &rules_toml),
            ("rules/src/lib.fe", "pub const fn score() -> u8 { 3 }"),
            ("shadow/fe.toml", &shadow_toml),
            ("shadow/src/lib.fe", "pub const fn score() -> u8 { 5 }"),
        ]);
        prepare_ingots(&db, &[url("app/")])
    };
    let import = "use rules::score as imported";
    for value_binding in [
        "",
        "const rules: u8 = 0\n",
        "const fn rules() -> u8 { 0 }\n",
        "use ingot::helper as rules\n",
    ] {
        let library = library.replace(import, &format!("{value_binding}{import}"));
        for manifest in [APP_WITH_LIB_TOML, app_with_shadow.as_str()] {
            let prepared = check(&lib_with_rules, manifest, &library)
                .expect("the producer can use its dependency")
                .expect("the scoped request generated a method");
            assert_eq!(ingot_errors(&prepared, "app/"), "", "{value_binding}");
            assert_eq!(
                eval(&prepared, "app/src/lib.fe", "result"),
                "6",
                "{value_binding}"
            );
        }

        let prepared = check(LIB_TOML, &app_with_shadow, &library)
            .expect("body diagnostics are reported on the prepared package")
            .expect("the scoped request generated a method");
        let diagnostics = ingot_errors(&prepared, "app/");
        assert!(
            diagnostics.contains("`rules` is not found"),
            "{value_binding}: {diagnostics}"
        );
    }

    for (before, after, read, expected) in [
        ("const rules: u8 = 0", "", "selected", "3"),
        ("const fn rules() -> u8 { 0 }", "", "selected()", "3"),
        ("", "use ingot::helper as rules", "selected()", "12"),
    ] {
        let library = library
            .replace(import, &format!("{before}\nuse rules as selected\n{after}"))
            .replace(
                "imported() + rules::score()",
                &format!("{read} + selected::score()"),
            );
        for manifest in [APP_WITH_LIB_TOML, app_with_shadow.as_str()] {
            let prepared = check(&lib_with_rules, manifest, &library)
                .expect("the producer can use its dependency")
                .expect("the scoped request generated a method");
            assert_eq!(ingot_errors(&prepared, "app/"), "", "{library}");
            assert_eq!(eval(&prepared, "app/src/lib.fe", "result"), expected);
        }
        let prepared = check(LIB_TOML, &app_with_shadow, &library)
            .expect("body diagnostics are reported on the prepared package")
            .expect("the scoped request generated a method");
        let diagnostics = ingot_errors(&prepared, "app/");
        assert!(
            diagnostics.contains("`selected` is not found"),
            "{diagnostics}"
        );
    }

    let local = library.replace(
        import,
        &format!("use ingot::local_rules as rules\n{import}"),
    );
    let prepared = check(LIB_TOML, &app_with_shadow, &local)
        .expect("the local module alias is valid")
        .expect("the scoped request generated a method");
    assert_eq!(ingot_errors(&prepared, "app/"), "");
    assert_eq!(eval(&prepared, "app/src/lib.fe", "result"), "8");
    let local_single = local
        .replace(import, "use rules as selected")
        .replace("imported() + rules::score()", "selected::score()");
    let prepared = check(LIB_TOML, &app_with_shadow, &local_single)
        .expect("the quoted module alias supplies the type domain")
        .expect("the scoped request generated a method");
    assert_eq!(ingot_errors(&prepared, "app/"), "");
    assert_eq!(eval(&prepared, "app/src/lib.fe", "result"), "4");

    let glob = library
        .replace("use rules::score as imported", "use rules::*")
        .replace("imported()", "score()");
    let refusal = check(&lib_with_rules, APP_WITH_LIB_TOML, &glob)
        .err()
        .expect("glob imports in a scoped quotation are refused");
    assert!(
        refusal
            .cause
            .contains("a glob import is not supported inside a quotation"),
        "{refusal}"
    );
}

#[test]
fn quoted_qualifiers_check_visibility_from_the_producer() {
    let library = r#"
use core::meta::{Code, Item}

pub(ingot) mod helpers {
    pub(ingot) trait Marker { const VALUE: u8 }
    impl Marker for u8 { const VALUE: u8 = 7 }
    pub(ingot) const fn value() -> u8 { 7 }
}

pub const fn fragment() -> Code<Item> {
    quote {
        const fn score() -> u8 {
            /* generated body */
        }
    }
}
"#;
    let app = r#"
use core::derive::Derive
use core::meta::{Code, Item, Record}

trait Score { const fn score() -> u8 }
struct Target {}
struct Provider {}
impl Derive<trait Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> { lib::fragment() }
}

impl Score for Target with (Provider)
pub const fn result() -> u8 { Target::score() }
"#;
    let prepare_library = |library: &str| {
        let db = project(&[
            ("app/fe.toml", APP_WITH_LIB_TOML),
            ("app/src/lib.fe", app),
            ("lib/fe.toml", LIB_TOML),
            ("lib/src/lib.fe", library),
        ]);
        prepare(&db, "app/")
    };
    let cases = [
        (
            "use ingot::helpers as local\nfn check<T: local::Marker>() {}\n7",
            "Marker",
        ),
        (
            "const helpers: u8 = 0\nfn check<T: helpers::Marker>() {}\n7",
            "Marker",
        ),
        (
            "use ingot::helpers as local\n<u8 as local::Marker>::VALUE",
            "Marker",
        ),
        ("<u8 as helpers::Marker>::VALUE", "Marker"),
        (
            "const helpers: u8 = 0\nuse helpers::value as local\nlocal()",
            "value",
        ),
        ("const helpers: u8 = 0\nhelpers::value()", "value"),
    ];
    for (body, member) in cases {
        let library = library.replace("/* generated body */", body);
        let accepted = prepare_library(&library);
        assert_eq!(ingot_errors(&accepted, "app/"), "", "{body}");
        assert_eq!(eval(&accepted, "app/src/lib.fe", "result"), "7", "{body}");
        let private_module = library.replace("pub(ingot) mod helpers", "mod helpers");
        let accepted = prepare_library(&private_module);
        assert_eq!(ingot_errors(&accepted, "app/"), "", "{body}");
        assert_eq!(eval(&accepted, "app/src/lib.fe", "result"), "7", "{body}");

        let hidden = library.replace("pub(ingot) ", "");
        let refused = prepare_library(&hidden);
        let diagnostics = ingot_errors(&refused, "app/");
        assert!(
            diagnostics.contains("2-0003")
                && diagnostics.contains(&format!("`{member}` is not visible")),
            "{body}\n{diagnostics}"
        );
    }

    let splice = r#"
use core::meta::{Code, Item}

mod inner {
    use core::meta::{Code, Expr}
    pub mod helpers { pub const fn value() -> u8 { 3 } }

    pub const fn expression() -> Code<Expr> {
        quote { {
            const helpers: u8 = 0
            /* inner expression */
        } }
    }
}

mod outer {
    use core::meta::{Code, Item}
    pub mod helpers { pub const fn value() -> u8 { 5 } }

    pub const fn fragment() -> Code<Item> {
        let inner = super::inner::expression()
        quote {
            const fn score() -> u8 { ${inner} }
        }
    }
}

pub const fn fragment() -> Code<Item> { outer::fragment() }
"#;
    for body in [
        "helpers::value()",
        "use helpers::value as selected\nselected()",
    ] {
        let library = splice.replace("/* inner expression */", body);
        let accepted = prepare_library(&library);
        assert_eq!(ingot_errors(&accepted, "app/"), "", "{body}");
        assert_eq!(eval(&accepted, "app/src/lib.fe", "result"), "3", "{body}");
    }
}

#[test]
fn quoted_trait_references_check_visibility_from_the_producer() {
    let library = r#"
use core::meta::{Code, Item}

pub mod helpers {
    pub(ingot) trait Marker {}
}

pub const fn fragment() -> Code<Item> {
    quote {
        const fn score() -> u8 {
            use ingot::helpers as local
            fn check<T: local::Marker>() {}
            7
        }
    }
}
"#;
    let app = r#"
use core::derive::Derive
use core::meta::{Code, Item, Record}

trait Score { const fn score() -> u8 }
struct Target {}
struct Provider {}
impl Derive<trait Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> { lib::fragment() }
}

impl Score for Target with (Provider)
pub const fn result() -> u8 { Target::score() }
"#;
    let prepare_library = |library| {
        let db = project(&[
            ("app/fe.toml", APP_WITH_LIB_TOML),
            ("app/src/lib.fe", app),
            ("lib/fe.toml", LIB_TOML),
            ("lib/src/lib.fe", library),
        ]);
        prepare(&db, "app/")
    };
    let accepted = prepare_library(library);
    assert_eq!(ingot_errors(&accepted, "app/"), "");
    assert_eq!(eval(&accepted, "app/src/lib.fe", "result"), "7");

    let hidden = library.replace("pub(ingot) trait Marker", "trait Marker");
    let refused = prepare_library(&hidden);
    let diagnostics = ingot_errors(&refused, "app/");
    assert!(
        diagnostics.contains("2-0003") && diagnostics.contains("`Marker` is not visible"),
        "{diagnostics}"
    );
}
