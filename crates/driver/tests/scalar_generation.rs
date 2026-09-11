use common::{
    InputDb,
    file::File,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
use fe_driver::{
    DriverDataBase,
    generation::{
        FunctionTemplate, GenerationBudget, GenerationErrorKind, generate_scalar_function,
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

fn input(db: &mut DriverDataBase, name: &str, source: &str) -> File {
    let url = Url::parse(&format!("file:///scalar-generation/{name}.fe")).unwrap();
    db.workspace().touch(db, url, Some(source.to_owned()))
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

fn target_template(name: &str, source: &str) -> FunctionTemplate {
    FunctionTemplate {
        url: Url::parse(&format!("file:///scalar-generation/{name}.fe")).unwrap(),
        source: source.to_owned(),
        function_name: "generated".to_owned(),
    }
}

fn load_provider(db: &mut DriverDataBase, name: &str, source: &str) -> File {
    input(db, name, source)
}

fn value(db: &DriverDataBase, file: File, function: &str) -> String {
    eval_body_owner_const_with_args(
        db,
        BodyOwner::Func(named(db, file, function)),
        Vec::new(),
        Vec::new(),
    )
    .unwrap_or_else(|error| panic!("failed to evaluate `{function}`: {error:?}"))
    .pretty_print(db)
}

fn assert_error(
    result: Result<
        fe_driver::generation::GeneratedFunction,
        fe_driver::generation::GenerationError,
    >,
    kind: GenerationErrorKind,
    reason: &str,
) {
    let error = match result {
        Ok(_) => panic!("generation unexpectedly succeeded"),
        Err(error) => error,
    };
    assert_eq!(error.kind, kind, "{}", error.message);
    assert!(
        error.message.contains(reason),
        "expected `{reason}` in: {}",
        error.message
    );
}

const U256_TEMPLATE: &str = r#"
const fn generated() -> u256 {}
const fn consume() -> u256 { generated() }
"#;

#[test]
fn scalar_u256_and_bool_outputs_reach_real_callers() {
    let cases = [
        (
            "u256",
            r#"
use core::meta::FunctionBody
const fn provide() -> FunctionBody<u256> {
    let mut result: u256 = 40
    let slot = mut result
    slot += 2
    FunctionBody { value: result }
}
"#,
            U256_TEMPLATE,
            "42",
        ),
        (
            "bool",
            r#"
use core::meta::FunctionBody
const fn provide() -> FunctionBody<bool> {
    FunctionBody { value: true }
}
"#,
            r#"
const fn generated() -> bool {}
const fn consume() -> bool { generated() }
"#,
            "true",
        ),
    ];

    for (name, provider_source, template_source, expected) in cases {
        let mut db = database();
        let provider_file = load_provider(&mut db, name, provider_source);
        let provider = named(&db, provider_file, "provide");
        let mut budget = GenerationBudget::new(1, 4096);
        let artifact = generate_scalar_function(
            &db,
            provider,
            target_template(name, template_source),
            &mut budget,
        )
        .unwrap();
        assert_eq!(
            value(artifact.database(), artifact.file(), "consume"),
            expected
        );
        assert_eq!(
            artifact
                .function()
                .name(artifact.database())
                .unwrap()
                .data(artifact.database()),
            "generated"
        );
    }
}

#[test]
fn descriptor_identity_accepts_core_alias_and_rejects_impostors() {
    let mut db = database();
    let alias_file = load_provider(
        &mut db,
        "alias",
        r#"
use core::meta::FunctionBody
type Descriptor = FunctionBody<u256>
const fn provide() -> Descriptor { FunctionBody { value: 7 } }
"#,
    );
    let alias = named(&db, alias_file, "provide");
    let mut budget = GenerationBudget::new(1, 4096);
    let artifact = generate_scalar_function(
        &db,
        alias,
        target_template("alias-template", U256_TEMPLATE),
        &mut budget,
    )
    .unwrap();
    assert_eq!(value(artifact.database(), artifact.file(), "consume"), "7");

    for (name, source, reason) in [
        (
            "impostor",
            r#"
struct FunctionBody<T> { value: T }
const fn provide() -> FunctionBody<u256> { FunctionBody { value: 7 } }
"#,
            "core::meta::FunctionBody",
        ),
        (
            "shadow",
            r#"
mod fake { pub struct FunctionBody<T> { pub value: T } }
use fake::FunctionBody
const fn provide() -> FunctionBody<u256> { FunctionBody { value: 7 } }
"#,
            "core::meta::FunctionBody",
        ),
        (
            "unsupported-scalar",
            r#"
use core::meta::FunctionBody
const fn provide() -> FunctionBody<u8> { FunctionBody { value: 7 } }
"#,
            "FunctionBody<bool>",
        ),
        (
            "core-shadow",
            r#"
mod core {
    pub mod prelude {}
    pub mod meta { pub struct FunctionBody<T> { pub value: T } }
}
const fn provide() -> core::meta::FunctionBody<u256> {
    core::meta::FunctionBody { value: 7 }
}
"#,
            "core::meta::FunctionBody",
        ),
    ] {
        let mut db = database();
        let provider_file = load_provider(&mut db, name, source);
        let provider = named(&db, provider_file, "provide");
        let mut budget = GenerationBudget::new(1, 4096);
        assert_error(
            generate_scalar_function(
                &db,
                provider,
                target_template(&format!("{name}-template"), U256_TEMPLATE),
                &mut budget,
            ),
            GenerationErrorKind::Protocol,
            reason,
        );
    }
}

#[test]
fn descriptor_and_template_return_types_must_match_exactly() {
    let mut db = database();
    let provider_file = load_provider(
        &mut db,
        "return-type",
        r#"
use core::meta::FunctionBody
const fn provide() -> FunctionBody<u256> { FunctionBody { value: 7 } }
"#,
    );
    let provider = named(&db, provider_file, "provide");
    let mut budget = GenerationBudget::new(1, 4096);
    assert_error(
        generate_scalar_function(
            &db,
            provider,
            target_template(
                "u8-template",
                r#"
const fn generated() -> u8 {}
const fn consume() -> u8 { generated() }
"#,
            ),
            &mut budget,
        ),
        GenerationErrorKind::ReturnType,
        "u256",
    );
}

#[test]
fn provider_analysis_and_execution_fail_at_distinct_stages() {
    for (name, source, kind, reason) in [
        (
            "provider-error",
            r#"
use core::meta::FunctionBody
const fn provide() -> FunctionBody<u256> {
    let invalid: bool = 7
    FunctionBody { value: 7 }
}
"#,
            GenerationErrorKind::Provider,
            "type mismatch",
        ),
        (
            "execution-error",
            r#"
use core::meta::FunctionBody
const fn provide() -> FunctionBody<u256> {
    let zero: u256 = 0
    FunctionBody { value: 1 / zero }
}
"#,
            GenerationErrorKind::Execution,
            "DivisionByZero",
        ),
        (
            "effect-provider",
            r#"
use core::meta::FunctionBody
struct Cap {}
const fn provide() -> FunctionBody<u256> uses (cap: Cap) {
    FunctionBody { value: 1 }
}
"#,
            GenerationErrorKind::Provider,
            "effects are not allowed",
        ),
    ] {
        let mut db = database();
        let provider_file = load_provider(&mut db, name, source);
        let provider = named(&db, provider_file, "provide");
        let mut budget = GenerationBudget::new(1, 4096);
        assert_error(
            generate_scalar_function(
                &db,
                provider,
                target_template(&format!("{name}-template"), U256_TEMPLATE),
                &mut budget,
            ),
            kind,
            reason,
        );
    }

    for (name, source, reason) in [
        (
            "runtime-provider",
            "use core::meta::FunctionBody\nfn provide() -> FunctionBody<u256> { FunctionBody { value: 1 } }",
            "const",
        ),
        (
            "generic-provider",
            "use core::meta::FunctionBody\nconst fn provide<T>() -> FunctionBody<u256> { FunctionBody { value: 1 } }",
            "without generics",
        ),
        (
            "unsafe-provider",
            "use core::meta::FunctionBody\nunsafe const fn provide() -> FunctionBody<u256> { FunctionBody { value: 1 } }",
            "safe top-level",
        ),
        (
            "parameter-provider",
            "use core::meta::FunctionBody\nconst fn provide(value: u256) -> FunctionBody<u256> { FunctionBody { value: value } }",
            "nullary",
        ),
        (
            "where-provider",
            "use core::meta::FunctionBody\ntrait Marker {}\nconst fn provide<T>() -> FunctionBody<u256> where T: Marker { FunctionBody { value: 1 } }",
            "requirements",
        ),
        (
            "nested-provider",
            "mod nested { pub const fn provide() -> core::meta::FunctionBody<u256> { core::meta::FunctionBody { value: 1 } } }",
            "top-level",
        ),
    ] {
        let mut db = database();
        let provider_file = load_provider(&mut db, name, source);
        let provider = named(&db, provider_file, "provide");
        let mut budget = GenerationBudget::new(1, 4096);
        assert_error(
            generate_scalar_function(
                &db,
                provider,
                target_template(&format!("{name}-template"), U256_TEMPLATE),
                &mut budget,
            ),
            GenerationErrorKind::Protocol,
            reason,
        );
    }
}

#[test]
fn template_target_must_be_unique_and_have_an_empty_body() {
    let cases = [
        (
            "missing",
            "const fn other() -> u256 {}",
            "named top-level function",
        ),
        (
            "duplicate",
            "const fn generated() -> u256 {}\nconst fn generated() -> u256 {}",
            "ambiguous",
        ),
        (
            "nonempty",
            "const fn generated() -> u256 { 0 }",
            "body must be empty",
        ),
    ];
    for (name, template_source, reason) in cases {
        let mut db = database();
        let provider_file = load_provider(
            &mut db,
            name,
            r#"
use core::meta::FunctionBody
const fn provide() -> FunctionBody<u256> { FunctionBody { value: 7 } }
"#,
        );
        let provider = named(&db, provider_file, "provide");
        let mut budget = GenerationBudget::new(1, 4096);
        assert_error(
            generate_scalar_function(
                &db,
                provider,
                target_template(&format!("{name}-template"), template_source),
                &mut budget,
            ),
            GenerationErrorKind::Template,
            reason,
        );
    }
}

fn signature_template(caller: &str) -> String {
    format!(
        r#"
trait Marker {{}}
struct Token {{}}
struct Missing {{}}
struct Cap {{}}
impl Marker for Token {{}}

fn generated<T>(token: own T, amount: u256) -> u256 uses (cap: Cap)
    where T: Marker
{{}}

{caller}
"#,
    )
}

#[test]
fn template_signature_is_preserved_and_checked_by_real_consumers() {
    let provider_source = r#"
use core::meta::FunctionBody
const fn provide() -> FunctionBody<u256> { FunctionBody { value: 9 } }
"#;
    let good_caller = r#"
fn consume() -> u256 uses (cap: Cap) {
    generated<Token>(token: Token {}, amount: 1)
}
"#;
    let mut db = database();
    let provider_file = load_provider(&mut db, "signature", provider_source);
    let provider = named(&db, provider_file, "provide");
    let mut budget = GenerationBudget::new(1, 8192);
    let artifact = generate_scalar_function(
        &db,
        provider,
        target_template("signature-template", &signature_template(good_caller)),
        &mut budget,
    )
    .unwrap();
    assert!(artifact.source().contains(
        "fn generated<T>(token: own T, amount: u256) -> u256 uses (cap: Cap)\n    where T: Marker"
    ));

    for (name, caller, reason) in [
        (
            "label",
            "fn consume() -> u256 uses (cap: Cap) { generated<Token>(wrong: Token {}, amount: 1) }",
            "argument label mismatch",
        ),
        (
            "where",
            "fn consume() -> u256 uses (cap: Cap) { generated<Missing>(token: Missing {}, amount: 1) }",
            "doesn't implement `Marker`",
        ),
        (
            "effect",
            "fn consume() -> u256 { generated<Token>(token: Token {}, amount: 1) }",
            "missing effect `Cap`",
        ),
        (
            "mode",
            "fn consume(token: Token) -> u256 uses (cap: Cap) { generated<Token>(token: token, amount: 1) }",
            "`own` argument requires",
        ),
    ] {
        let mut db = database();
        let provider_file = load_provider(&mut db, name, provider_source);
        let provider = named(&db, provider_file, "provide");
        let mut budget = GenerationBudget::new(1, 8192);
        assert_error(
            generate_scalar_function(
                &db,
                provider,
                target_template(&format!("{name}-template"), &signature_template(caller)),
                &mut budget,
            ),
            GenerationErrorKind::Output,
            reason,
        );
    }
}

#[test]
fn artifact_provenance_tracks_both_stages_and_output_errors() {
    let provider_source = r#"
use core::meta::FunctionBody
const fn provide() -> FunctionBody<u256> { FunctionBody { value: 11 } }
"#;
    let template_source = U256_TEMPLATE;
    let mut db = database();
    let provider_file = load_provider(&mut db, "provenance-provider", provider_source);
    let provider = named(&db, provider_file, "provide");
    let template = target_template("provenance-template", template_source);
    let template_url = template.url.clone();
    let mut budget = GenerationBudget::new(2, 8192);
    let artifact = generate_scalar_function(&db, provider, template, &mut budget).unwrap();
    let provenance = artifact.provenance();
    assert_eq!(provenance.provider_url, input_url("provenance-provider"));
    assert_eq!(provenance.provider_source, provider_source);
    assert!(provenance.provider_source[provenance.provider_span.clone()].contains("provide"));
    assert_eq!(provenance.template_url, template_url);
    assert_eq!(provenance.template_source, template_source);
    assert_eq!(
        &provenance.template_source[provenance.template_body.clone()],
        "{}"
    );
    assert!(artifact.source()[provenance.generated_body.clone()].contains("11"));
    assert_eq!(provenance.invocation, 0);

    let mut bad_db = database();
    let bad_provider_file = load_provider(&mut bad_db, "output-provider", provider_source);
    let bad_provider = named(&bad_db, bad_provider_file, "provide");
    let mut bad_budget = GenerationBudget::new(1, 4096);
    let error = generate_scalar_function(
        &bad_db,
        bad_provider,
        target_template(
            "bad-output-template",
            "const fn generated() -> u256 {}\nconst fn consume() -> bool { generated() }",
        ),
        &mut bad_budget,
    )
    .err()
    .expect("invalid caller unexpectedly passed output analysis");
    assert_eq!(error.kind, GenerationErrorKind::Output, "{}", error.message);
    assert!(error.message.contains("type mismatch"), "{}", error.message);
    let error_provenance = error
        .provenance
        .expect("output-stage errors must retain generation provenance");
    assert_eq!(error_provenance.provider_url, input_url("output-provider"));
    assert_eq!(
        error_provenance.template_url,
        input_url("bad-output-template")
    );
    assert!(error_provenance.generated_body.start < error_provenance.generated_body.end);
}

fn input_url(name: &str) -> Url {
    Url::parse(&format!("file:///scalar-generation/{name}.fe")).unwrap()
}

#[test]
fn aggregate_budget_is_independent_of_warm_queries_and_cached_execution() {
    let provider_source = r#"
use core::meta::FunctionBody
const fn provide() -> FunctionBody<u256> { FunctionBody { value: 13 } }
"#;
    let mut db = database();
    let provider_file = load_provider(&mut db, "budget-provider", provider_source);
    let provider = named(&db, provider_file, "provide");
    let mut probe_budget = GenerationBudget::new(1, 4096);
    let probe = generate_scalar_function(
        &db,
        provider,
        target_template("budget-template", U256_TEMPLATE),
        &mut probe_budget,
    )
    .unwrap();
    let source_len = probe.source().len();

    assert!(provider.return_ty(&db).is_struct(&db));
    let mut budget = GenerationBudget::new(2, source_len * 2);
    let first = generate_scalar_function(
        &db,
        provider,
        target_template("budget-template", U256_TEMPLATE),
        &mut budget,
    )
    .unwrap();
    let second = generate_scalar_function(
        &db,
        provider,
        target_template("budget-template", U256_TEMPLATE),
        &mut budget,
    )
    .unwrap();
    assert_eq!(first.source(), second.source());
    assert_eq!(first.provenance().invocation, 0);
    assert_eq!(second.provenance().invocation, 1);
    assert_eq!(budget.used_functions(), 2);
    assert_eq!(budget.used_bytes(), source_len * 2);
    assert_error(
        generate_scalar_function(
            &db,
            provider,
            target_template("budget-template", U256_TEMPLATE),
            &mut budget,
        ),
        GenerationErrorKind::Limit,
        "generation request exceeds its function or source limit",
    );
    assert_eq!(budget.used_functions(), 2);
    assert_eq!(budget.used_bytes(), source_len * 2);

    let mut byte_budget = GenerationBudget::new(1, source_len - 1);
    assert_error(
        generate_scalar_function(
            &db,
            provider,
            target_template("budget-template", U256_TEMPLATE),
            &mut byte_budget,
        ),
        GenerationErrorKind::Limit,
        "source byte limit",
    );
    assert_eq!(byte_budget.used_functions(), 0);
    assert_eq!(byte_budget.used_bytes(), 0);
}

#[test]
fn provider_stage_rejects_unvalidated_project_dependencies() {
    let mut db = database();
    db.workspace().touch(
        &mut db,
        Url::parse("file:///scalar-generation/project/fe.toml").unwrap(),
        Some("[ingot]\nname = \"provider-project\"\nversion = \"0.0.0\"\n".to_owned()),
    );
    let file = load_provider(
        &mut db,
        "project/src/lib",
        r#"
use core::meta::FunctionBody
const fn provide() -> FunctionBody<u256> { FunctionBody { value: 7 } }
"#,
    );
    assert_error(
        generate_scalar_function(
            &db,
            named(&db, file, "provide"),
            target_template("project-output", U256_TEMPLATE),
            &mut GenerationBudget::new(1, 4096),
        ),
        GenerationErrorKind::Protocol,
        "standalone",
    );
}

#[test]
fn provider_edits_match_fresh_stages_and_preserve_prior_artifacts() {
    use salsa::Setter;
    let source = |value| {
        format!(
            "use core::meta::FunctionBody\nconst fn provide() -> FunctionBody<u256> {{ FunctionBody {{ value: {value} }} }}"
        )
    };
    let mut db = database();
    let file = load_provider(&mut db, "edited", &source(1));
    let retained = generate_scalar_function(
        &db,
        named(&db, file, "provide"),
        target_template("edited-output", U256_TEMPLATE),
        &mut GenerationBudget::new(1, 4096),
    )
    .unwrap();
    for expected in [1, 2, 1] {
        file.set_text(&mut db).to(source(expected));
        let provider = named(&db, file, "provide");
        let _ = provider.return_ty(&db);
        let artifact = generate_scalar_function(
            &db,
            provider,
            target_template("edited-output", U256_TEMPLATE),
            &mut GenerationBudget::new(1, 4096),
        )
        .unwrap();
        let mut fresh = database();
        let fresh_file = load_provider(&mut fresh, "edited", &source(expected));
        let fresh_artifact = generate_scalar_function(
            &fresh,
            named(&fresh, fresh_file, "provide"),
            target_template("edited-output", U256_TEMPLATE),
            &mut GenerationBudget::new(1, 4096),
        )
        .unwrap();
        assert_eq!(artifact.source(), fresh_artifact.source());
        assert_eq!(artifact.provenance(), fresh_artifact.provenance());
        assert_eq!(
            value(artifact.database(), artifact.file(), "consume"),
            expected.to_string()
        );
        assert_eq!(value(retained.database(), retained.file(), "consume"), "1");
    }
}
