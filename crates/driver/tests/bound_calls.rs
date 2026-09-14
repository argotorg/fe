use common::{
    InputDb,
    file::File,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
use fe_driver::{
    DriverDataBase,
    generation::{
        FunctionTemplate, GeneratedFunction, GenerationBudget, GenerationRequest,
        GenerationSession,
        imports::{FrozenArtifact, ImportError, ImportErrorKind, bind_function},
    },
};
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

fn url(name: &str) -> Url {
    Url::parse(&format!("file:///bound-calls/{name}.fe")).unwrap()
}

fn input(db: &mut DriverDataBase, name: &str, source: &str) -> File {
    db.workspace().touch(db, url(name), Some(source.to_owned()))
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

fn make_artifact(base: u64, context: &str) -> GeneratedFunction {
    let mut db = database();
    let provider_source = format!(
        "use core::meta::FunctionBody\nconst fn provide() -> FunctionBody<u256> {{ FunctionBody {{ value: {base} }} }}\n"
    );
    let provider_file = input(&mut db, &format!("provider-{base}"), &provider_source);
    GenerationSession::new("stable-stage".to_owned(), GenerationBudget::new(1, 65_536))
        .generate_value_function(
            &db,
            named(&db, provider_file, "provide"),
            GenerationRequest {
                key: "stable-request".to_owned(),
                template: FunctionTemplate {
                    url: url(&format!("artifact-{base}")),
                    source: context.to_owned(),
                    function_name: "base".to_owned(),
                },
            },
        )
        .unwrap()
}

fn template(name: &str, source: &str) -> FunctionTemplate {
    FunctionTemplate {
        url: url(name),
        source: source.to_owned(),
        function_name: "generated".to_owned(),
    }
}

fn rejection<T>(result: Result<T, ImportError>, kind: ImportErrorKind) -> ImportError {
    let error = result.err().expect("binding unexpectedly succeeded");
    assert_eq!(error.kind, kind, "{}", error.message);
    error
}

const APPLY_CONTEXT: &str = r#"
const fn base() -> u256 {}
const fn offset(_ value: u256) -> u256 { value + 1 }
mod helpers {
    pub const fn adjust(_ value: u256) -> u256 { super::offset(value) }
}
pub const fn apply(amount: u256) -> u256 { ingot::base() + helpers::adjust(amount) }
"#;

#[test]
fn bound_calls_outlive_artifacts_and_distinguish_same_identity_incarnations() {
    let bind = |base, name: &str| {
        let artifact = make_artifact(base, APPLY_CONTEXT);
        let frozen = FrozenArtifact::new(artifact);
        let selected = frozen.export("apply").unwrap();
        bind_function(
            template(
                name,
                "const fn generated(amount: u256) -> u256 {}\nconst fn consume() -> u256 { generated(amount: 1) }\n",
            ),
            &selected,
            &[0],
            &mut GenerationBudget::new(1, 65_536),
        )
        .unwrap()
    };

    let old = bind(40, "old-bound");
    let new = bind(80, "new-bound");
    assert_eq!(evaluate(old.database(), old.file(), "consume"), "42");
    assert_eq!(evaluate(new.database(), new.file(), "consume"), "82");
    assert_eq!(
        old.receipt().original_provenance.request_identity,
        new.receipt().original_provenance.request_identity
    );
    assert_ne!(old.receipt().original_source, new.receipt().original_source);
}

#[test]
fn selected_source_keeps_root_paths_and_private_helpers() {
    let artifact = make_artifact(40, APPLY_CONTEXT);
    let original_source = artifact.source().to_owned();
    let original_provenance = artifact.provenance().clone();
    let frozen = FrozenArtifact::new(artifact);
    let selected = frozen.export("apply").unwrap();
    let mut budget = GenerationBudget::new(1, 65_536);
    let bound = bind_function(
        template(
            "private-context",
            r#"
const fn generated(amount: u256) -> u256 {}
const fn consume() -> u256 { generated(amount: 1) }
"#,
        ),
        &selected,
        &[0],
        &mut budget,
    )
    .unwrap();
    assert_eq!(evaluate(bound.database(), bound.file(), "consume"), "42");
    assert_eq!(bound.receipt().original_source, original_source);
    assert_eq!(bound.receipt().original_provenance, original_provenance);
    assert_eq!(bound.receipt().selected_name, "apply");
    assert!(
        bound
            .receipt()
            .selected_source
            .contains("pub const fn apply")
    );
    assert!(bound.receipt().selected_span.end <= original_source.len());
    assert_eq!(
        &original_source[bound.receipt().selected_span.clone()],
        bound.receipt().selected_source
    );
    assert!(
        bound
            .receipt()
            .materialized_source_url
            .path()
            .ends_with("/src/lib.fe")
    );
    assert_eq!(bound.receipt().consumer_source, bound.source());
    assert!(
        bound
            .receipt()
            .consumer_source_url
            .path()
            .ends_with("/consumer/src/lib.fe")
    );
    assert_eq!(
        &bound.receipt().consumer_source[bound.receipt().emitted_call.clone()],
        "__frozen_import::apply(amount: amount)"
    );
    assert_eq!(budget.used_functions(), 1);
    assert_eq!(
        budget.used_bytes(),
        original_source.len() + bound.source().len()
    );
}

#[test]
fn parameter_indices_reorder_values_while_export_labels_are_preserved() {
    let artifact = make_artifact(
        0,
        r#"
const fn base() -> u256 {}
pub const fn decimal(tens: u256, ones: u256) -> u256 { tens * 10 + ones }
"#,
    );
    let frozen = FrozenArtifact::new(artifact);
    let selected = frozen.export("decimal").unwrap();
    let bound = bind_function(
        template(
            "reordered",
            r#"
const fn generated(ones: u256, tens: u256) -> u256 {}
const fn consume() -> u256 { generated(ones: 2, tens: 4) }
"#,
        ),
        &selected,
        &[1, 0],
        &mut GenerationBudget::new(1, 65_536),
    )
    .unwrap();
    assert_eq!(evaluate(bound.database(), bound.file(), "consume"), "42");
    let emitted_call = &bound.source()[bound.receipt().emitted_call.clone()];
    assert!(emitted_call.contains("tens:"));
    assert!(emitted_call.contains("ones:"));
}

#[test]
fn repeated_named_parameters_work_but_selected_anonymous_parameters_do_not() {
    let artifact = make_artifact(
        0,
        r#"
const fn base() -> u256 {}
pub const fn add(left: u256, right: u256) -> u256 { left + right }
pub const fn positional(_ value: u256) -> u256 { value }
"#,
    );
    let frozen = FrozenArtifact::new(artifact);

    let add = frozen.export("add").unwrap();
    let repeated = bind_function(
        template(
            "repeated",
            "const fn generated(value: u256) -> u256 {}\nconst fn consume() -> u256 { generated(value: 21) }",
        ),
        &add,
        &[0, 0],
        &mut GenerationBudget::new(1, 65_536),
    )
    .unwrap();
    assert_eq!(
        evaluate(repeated.database(), repeated.file(), "consume"),
        "42"
    );

    let positional = frozen.export("positional").unwrap();
    let suppressed_label = bind_function(
        template(
            "suppressed-label",
            "const fn generated(value: u256) -> u256 {}\nconst fn consume() -> u256 { generated(value: 42) }",
        ),
        &positional,
        &[0],
        &mut GenerationBudget::new(1, 65_536),
    )
    .unwrap();
    let emitted = &suppressed_label.source()[suppressed_label.receipt().emitted_call.clone()];
    assert!(!emitted.contains("value:"));

    rejection(
        bind_function(
            template(
                "selected-anonymous",
                "const fn generated(_: u256) -> u256 {}",
            ),
            &positional,
            &[0],
            &mut GenerationBudget::new(1, 65_536),
        ),
        ImportErrorKind::Template,
    );
    let unselected_anonymous = bind_function(
        template(
            "unselected-anonymous",
            "const fn generated(value: u256, _: bool) -> u256 {}\nconst fn consume() -> u256 { generated(value: 42, false) }",
        ),
        &positional,
        &[0],
        &mut GenerationBudget::new(1, 65_536),
    )
    .unwrap();
    assert_eq!(
        evaluate(
            unselected_anonymous.database(),
            unselected_anonymous.file(),
            "consume"
        ),
        "42"
    );
}

#[test]
fn copy_parameters_bind_across_all_view_and_own_combinations() {
    for (name, export_mode, target_mode) in [
        ("view-view", "", ""),
        ("view-own", "", "own "),
        ("own-view", "own ", ""),
        ("own-own", "own ", "own "),
    ] {
        let source = format!(
            "const fn base() -> u256 {{}}\npub const fn copy(value: {export_mode}u256) -> u256 {{ value }}\n"
        );
        let artifact = make_artifact(0, &source);
        let frozen = FrozenArtifact::new(artifact);
        let selected = frozen.export("copy").unwrap();
        let target = format!(
            "const fn generated(value: {target_mode}u256) -> u256 {{}}\nconst fn consume() -> u256 {{ generated(value: 42) }}\n"
        );
        let bound = bind_function(
            template(name, &target),
            &selected,
            &[0],
            &mut GenerationBudget::new(1, 65_536),
        )
        .unwrap();
        assert_eq!(evaluate(bound.database(), bound.file(), "consume"), "42");
    }
}

#[test]
fn bool_parameters_and_results_bind_without_integer_coercion() {
    let source_artifact = make_artifact(
        0,
        "const fn base() -> u256 {}\npub const fn invert(value: bool) -> bool { !value }\n",
    );
    let frozen = FrozenArtifact::new(source_artifact);
    let selected = frozen.export("invert").unwrap();
    let bound = bind_function(
        template(
            "bool",
            "const fn generated(value: bool) -> bool {}\nconst fn consume() -> bool { generated(value: false) }\n",
        ),
        &selected,
        &[0],
        &mut GenerationBudget::new(1, 65_536),
    )
    .unwrap();
    assert_eq!(evaluate(bound.database(), bound.file(), "consume"), "true");
}

#[test]
fn invalid_parameter_maps_and_incompatible_interfaces_are_rejected() {
    let artifact = make_artifact(
        0,
        r#"
const fn base() -> u256 {}
pub const fn combine(left: u256, right: bool) -> u256 { if right { left } else { 0 } }
"#,
    );
    let frozen = FrozenArtifact::new(artifact);
    let selected = frozen.export("combine").unwrap();
    for (name, source, parameters, kind) in [
        (
            "arity",
            "const fn generated(value: u256) -> u256 {}",
            vec![0],
            ImportErrorKind::Template,
        ),
        (
            "index",
            "const fn generated(left: u256, right: bool) -> u256 {}",
            vec![0, 2],
            ImportErrorKind::Template,
        ),
        (
            "parameter-type",
            "const fn generated(left: bool, right: u256) -> u256 {}",
            vec![0, 1],
            ImportErrorKind::Output,
        ),
        (
            "result-type",
            "const fn generated(left: u256, right: bool) -> bool {}",
            vec![0, 1],
            ImportErrorKind::Output,
        ),
    ] {
        let error = rejection(
            bind_function(
                template(name, source),
                &selected,
                &parameters,
                &mut GenerationBudget::new(1, 65_536),
            ),
            kind,
        );
        assert_eq!(error.receipt.is_some(), kind == ImportErrorKind::Output);
    }
}

#[test]
fn unsupported_exports_and_nonempty_target_bodies_are_rejected() {
    let unsupported_artifact = make_artifact(
        0,
        r#"
const fn base() -> u256 {}
const fn private(value: u256) -> u256 { value }
pub fn runtime(value: u256) -> u256 { value }
pub unsafe const fn unsafe_export(value: u256) -> u256 { value }
pub const fn generic<T>(_ value: T) -> u256 { 1 }
pub const fn empty() {}
"#,
    );
    let frozen = FrozenArtifact::new(unsupported_artifact);
    for name in [
        "private",
        "runtime",
        "unsafe_export",
        "generic",
        "empty",
        "missing",
    ] {
        rejection(frozen.export(name), ImportErrorKind::Export);
    }

    let artifact = make_artifact(0, APPLY_CONTEXT);
    let frozen = FrozenArtifact::new(artifact);
    let selected = frozen.export("apply").unwrap();
    rejection(
        bind_function(
            template(
                "nonempty",
                "const fn generated(amount: u256) -> u256 { amount }",
            ),
            &selected,
            &[0],
            &mut GenerationBudget::new(1, 65_536),
        ),
        ImportErrorKind::Template,
    );
}

#[test]
fn unrelated_names_are_allowed_but_generated_binding_shadow_is_rejected() {
    let artifact = make_artifact(40, APPLY_CONTEXT);
    let frozen = FrozenArtifact::new(artifact);
    let selected = frozen.export("apply").unwrap();
    let allowed = bind_function(
        template(
            "unrelated-collision",
            r#"
const fn apply(amount: u256) -> u256 { 999 }
const fn generated(amount: u256) -> u256 {}
const fn consume() -> u256 { generated(amount: 1) }
"#,
        ),
        &selected,
        &[0],
        &mut GenerationBudget::new(1, 65_536),
    )
    .unwrap();
    assert_eq!(
        evaluate(allowed.database(), allowed.file(), "consume"),
        "42"
    );

    // The binder's reserved dependency alias is intentionally exercised here.
    let shadow = rejection(
        bind_function(
            template(
                "binding-shadow",
                r#"
mod __frozen_import { pub const fn apply(amount: u256) -> u256 { 999 } }
const fn generated(amount: u256) -> u256 {}
"#,
            ),
            &selected,
            &[0],
            &mut GenerationBudget::new(1, 65_536),
        ),
        ImportErrorKind::Binding,
    );
    let shadow_receipt = shadow.receipt.expect("post-emission binding receipt");
    assert_eq!(
        &shadow_receipt.consumer_source[shadow_receipt.emitted_call.clone()],
        "__frozen_import::apply(amount: amount)"
    );
}

#[test]
fn budgets_limit_emission_and_charge_sources_rejected_after_emission() {
    let artifact = make_artifact(40, APPLY_CONTEXT);
    let frozen = FrozenArtifact::new(artifact);
    let selected = frozen.export("apply").unwrap();

    let source = "const fn generated(amount: u256) -> u256 {}";
    let mut probe = GenerationBudget::new(1, 65_536);
    bind_function(
        template("budget-probe", source),
        &selected,
        &[0],
        &mut probe,
    )
    .unwrap();
    let charged_len = probe.used_bytes();
    let mut short = GenerationBudget::new(1, charged_len - 1);
    let limit = rejection(
        bind_function(
            template("budget-short", source),
            &selected,
            &[0],
            &mut short,
        ),
        ImportErrorKind::Limit,
    );
    assert!(limit.receipt.is_none());
    assert_eq!(short.used_functions(), 0);
    assert_eq!(short.used_bytes(), 0);

    let repeated_artifact = make_artifact(
        0,
        r#"
const fn base() -> u256 {}
pub const fn add(first: u256, second: u256) -> u256 { first + second }
"#,
    );
    let repeated_source = "const fn generated(a_very_long_parameter_name: u256) -> u256 {}";
    let minimum_input_bytes = repeated_artifact.source().len() + repeated_source.len();
    let repeated_frozen = FrozenArtifact::new(repeated_artifact);
    let repeated = repeated_frozen.export("add").unwrap();
    let mut render_limited = GenerationBudget::new(1, minimum_input_bytes);
    rejection(
        bind_function(
            template("render-limit", repeated_source),
            &repeated,
            &[0, 0],
            &mut render_limited,
        ),
        ImportErrorKind::Limit,
    );
    assert_eq!(render_limited.used_functions(), 0);
    assert_eq!(render_limited.used_bytes(), 0);

    let mut charged = GenerationBudget::new(1, 65_536);
    let output = rejection(
        bind_function(
            template(
                "bad-output",
                "const fn generated(amount: u256) -> u256 {}\nconst BAD: u256 = true\n",
            ),
            &selected,
            &[0],
            &mut charged,
        ),
        ImportErrorKind::Output,
    );
    let output_receipt = output.receipt.expect("post-emission output receipt");
    assert!(
        output_receipt
            .consumer_source_url
            .path()
            .ends_with("/consumer/src/lib.fe")
    );
    assert_eq!(
        &output_receipt.consumer_source[output_receipt.emitted_call.clone()],
        "__frozen_import::apply(amount: amount)"
    );
    assert_eq!(charged.used_functions(), 1);
    assert!(charged.used_bytes() > 0);
}
