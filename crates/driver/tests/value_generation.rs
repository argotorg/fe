use std::collections::BTreeMap;

use common::{
    InputDb,
    file::File,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
use fe_driver::{
    DriverDataBase,
    generation::{
        FunctionTemplate, GeneratedFunction, GenerationBudget, GenerationError,
        GenerationErrorKind, GenerationRequest, GenerationSession, RequestIdentity,
        generate_scalar_function,
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

fn input_url(name: &str) -> Url {
    Url::parse(&format!("file:///value-generation/{name}.fe")).unwrap()
}

fn input(db: &mut DriverDataBase, name: &str, source: &str) -> File {
    db.workspace()
        .touch(db, input_url(name), Some(source.to_owned()))
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

fn target_template(name: &str, ty: &str) -> FunctionTemplate {
    FunctionTemplate {
        url: input_url(name),
        source: format!(
            "const fn generated() -> {ty} {{}}\nconst fn consume() -> {ty} {{ generated() }}\n"
        ),
        function_name: "generated".to_owned(),
    }
}

fn raw_template(name: &str, source: &str) -> FunctionTemplate {
    FunctionTemplate {
        url: input_url(name),
        source: source.to_owned(),
        function_name: "generated".to_owned(),
    }
}

fn provider_source(ty: &str, value: &str) -> String {
    format!(
        "use core::meta::FunctionBody\nconst fn provide() -> FunctionBody<{ty}> {{ FunctionBody {{ value: {value} }} }}\n"
    )
}

fn value(db: &DriverDataBase, file: File) -> String {
    eval_body_owner_const_with_args(
        db,
        BodyOwner::Func(named(db, file, "consume")),
        Vec::new(),
        Vec::new(),
    )
    .unwrap_or_else(|error| panic!("failed to evaluate generated caller: {error:?}"))
    .pretty_print(db)
}

fn generate(
    session: &mut GenerationSession,
    db: &DriverDataBase,
    provider_file: File,
    key: &str,
    template: FunctionTemplate,
) -> Result<GeneratedFunction, GenerationError> {
    session.generate_value_function(
        db,
        named(db, provider_file, "provide"),
        GenerationRequest {
            key: key.to_owned(),
            template,
        },
    )
}

fn assert_error(
    result: Result<GeneratedFunction, GenerationError>,
    kind: GenerationErrorKind,
    reason: &str,
) -> GenerationError {
    let error = result.err().expect("generation unexpectedly succeeded");
    assert_eq!(error.kind, kind, "{}", error.message);
    assert!(
        error.message.contains(reason),
        "expected `{reason}` in: {}",
        error.message
    );
    error
}

fn identity(stage: &str, key: &str) -> RequestIdentity {
    RequestIdentity {
        stage: stage.to_owned(),
        key: key.to_owned(),
    }
}

#[test]
fn recursive_tuples_arrays_and_aliases_reach_real_callers() {
    let cases = [
        ("tuple", "(u256, bool)", "(7, true)", "(7, true)"),
        (
            "nested",
            "([u256; 2], (bool, [u256; 2]))",
            "([1, 2], (false, [3, 4]))",
            "([1, 2], (false, [3, 4]))",
        ),
        (
            "array-of-tuples",
            "[(u256, bool); 2]",
            "[(5, true), (6, false)]",
            "[(5, true), (6, false)]",
        ),
    ];

    for (name, ty, provider_value, expected) in cases {
        let mut db = database();
        let provider_file = input(&mut db, name, &provider_source(ty, provider_value));
        let mut session =
            GenerationSession::new(format!("recursive-{name}"), GenerationBudget::new(1, 8192));
        let artifact = generate(
            &mut session,
            &db,
            provider_file,
            name,
            target_template(&format!("{name}-output"), ty),
        )
        .unwrap();
        assert_eq!(value(artifact.database(), artifact.file()), expected);
        assert_eq!(
            artifact.provenance().request_identity,
            Some(identity(&format!("recursive-{name}"), name))
        );
    }

    let alias_source = r#"
use core::meta::FunctionBody
type Payload = (u256, [bool; 2])
type Descriptor = FunctionBody<Payload>
const fn provide() -> Descriptor { FunctionBody { value: (9, [true, false]) } }
"#;
    let mut db = database();
    let provider_file = input(&mut db, "alias", alias_source);
    let mut session =
        GenerationSession::new("alias-stage".to_owned(), GenerationBudget::new(1, 4096));
    let artifact = generate(
        &mut session,
        &db,
        provider_file,
        "alias",
        target_template("alias-output", "(u256, [bool; 2])"),
    )
    .unwrap();
    assert_eq!(
        value(artifact.database(), artifact.file()),
        "(9, [true, false])"
    );
}

#[test]
fn scalar_entry_keeps_rejecting_aggregate_descriptors() {
    let mut db = database();
    let provider_file = input(
        &mut db,
        "scalar-aggregate",
        &provider_source("(u256, bool)", "(7, true)"),
    );
    let provider = named(&db, provider_file, "provide");
    let mut budget = GenerationBudget::new(1, 4096);
    assert_error(
        generate_scalar_function(
            &db,
            provider,
            target_template("scalar-aggregate-output", "(u256, bool)"),
            &mut budget,
        ),
        GenerationErrorKind::Protocol,
        "FunctionBody<bool> or FunctionBody<u256>",
    );
}

#[test]
fn unit_singleton_tuple_and_empty_array_preserve_shape() {
    for (name, ty, provider_value, expected) in [
        ("unit", "()", "()", "()"),
        ("singleton", "(u256,)", "(7,)", "(7,)"),
        ("empty-array", "[u256; 0]", "[]", "[]"),
        ("nested-empty-array", "[[u256; 4096]; 0]", "[]", "[]"),
    ] {
        let mut db = database();
        let provider_file = input(&mut db, name, &provider_source(ty, provider_value));
        let mut session = GenerationSession::new(name.to_owned(), GenerationBudget::new(1, 4096));
        let artifact = generate(
            &mut session,
            &db,
            provider_file,
            name,
            target_template(&format!("{name}-output"), ty),
        )
        .unwrap();
        assert_eq!(value(artifact.database(), artifact.file()), expected);
        assert!(artifact.source().contains(&format!("-> {ty}")));
    }
}

#[test]
fn nested_narrow_nominal_and_pointer_values_are_rejected() {
    let cases = [
        (
            "narrow",
            provider_source("(u256, u8)", "(1, 2)"),
            "is not supported",
        ),
        (
            "nominal",
            "use core::meta::FunctionBody\nstruct Payload { value: u256 }\nconst fn provide() -> FunctionBody<Payload> { FunctionBody { value: Payload { value: 1 } } }\n".to_owned(),
            "is not supported",
        ),
        (
            "zero-nominal-array",
            "use core::meta::FunctionBody\nstruct Payload { value: u256 }\nconst fn provide() -> FunctionBody<[Payload; 0]> { FunctionBody { value: [] } }\n".to_owned(),
            "is not supported",
        ),
        (
            "pointer",
            provider_source("[*u256; 0]", "[]"),
            "is not supported",
        ),
    ];
    for (name, source, reason) in cases {
        let mut db = database();
        let provider_file = input(&mut db, name, &source);
        let mut session = GenerationSession::new(name.to_owned(), GenerationBudget::new(1, 4096));
        let error = assert_error(
            generate(
                &mut session,
                &db,
                provider_file,
                "rejected",
                target_template(&format!("{name}-output"), "u256"),
            ),
            GenerationErrorKind::Protocol,
            reason,
        );
        assert_eq!(error.request_identity, Some(identity(name, "rejected")));
    }
}

#[test]
fn target_return_shape_and_scalar_types_must_match_exactly() {
    let cases = [
        (
            "shape",
            "(u256, u256)",
            "(7, 8)",
            "[u256; 2]",
            GenerationErrorKind::Output,
            "type mismatch",
        ),
        (
            "fitting-element",
            "(u256, bool)",
            "(7, true)",
            "(u8, bool)",
            GenerationErrorKind::ReturnType,
            "target function returns (u8, bool)",
        ),
        (
            "array-length",
            "[u256; 1]",
            "[7]",
            "[u256; 2]",
            GenerationErrorKind::Output,
            "type mismatch",
        ),
        (
            "empty-array-element",
            "[bool; 0]",
            "[]",
            "[u256; 0]",
            GenerationErrorKind::ReturnType,
            "target function returns [u256; 0]",
        ),
    ];
    for (name, provider_ty, provider_value, target_ty, kind, reason) in cases {
        let mut db = database();
        let provider_file = input(&mut db, name, &provider_source(provider_ty, provider_value));
        let mut session = GenerationSession::new(name.to_owned(), GenerationBudget::new(1, 8192));
        let error = assert_error(
            generate(
                &mut session,
                &db,
                provider_file,
                "mismatch",
                target_template(&format!("{name}-output"), target_ty),
            ),
            kind,
            reason,
        );
        assert_eq!(error.request_identity, Some(identity(name, "mismatch")));
    }
}

fn nested_singleton(depth: usize) -> (String, String) {
    let mut ty = "u256".to_owned();
    let mut value = "1".to_owned();
    for _ in 0..depth {
        ty = format!("({ty},)");
        value = format!("({value},)");
    }
    (ty, value)
}

#[test]
fn value_depth_node_and_source_limits_have_passing_boundaries() {
    // The root value is at depth one, so 31 singleton wrappers put the leaf at 32.
    for (depth, succeeds) in [(31, true), (32, false)] {
        let (ty, provider_value) = nested_singleton(depth);
        let mut db = database();
        let provider_file = input(
            &mut db,
            &format!("depth-{depth}"),
            &provider_source(&ty, &provider_value),
        );
        let mut session =
            GenerationSession::new(format!("depth-{depth}"), GenerationBudget::new(1, 32_768));
        let result = generate(
            &mut session,
            &db,
            provider_file,
            "depth",
            target_template(&format!("depth-{depth}-output"), &ty),
        );
        if succeeds {
            let artifact = result.unwrap();
            assert_eq!(value(artifact.database(), artifact.file()), provider_value);
        } else {
            assert_error(result, GenerationErrorKind::Limit, "depth limit");
        }
    }

    // An array root plus 4095 scalar elements consumes the 4096-node allowance.
    for (elements, succeeds) in [(4095, true), (4096, false)] {
        let ty = format!("[u256; {elements}]");
        let provider_value = format!("[1; {elements}]");
        let mut db = database();
        let provider_file = input(
            &mut db,
            &format!("nodes-{elements}"),
            &provider_source(&ty, &provider_value),
        );
        let mut session = GenerationSession::new(
            format!("nodes-{elements}"),
            GenerationBudget::new(1, 1_000_000),
        );
        let result = generate(
            &mut session,
            &db,
            provider_file,
            "nodes",
            target_template(&format!("nodes-{elements}-output"), &ty),
        );
        if succeeds {
            let artifact = result.unwrap();
            assert!(artifact.source().contains("[1, 1"));
        } else {
            assert_error(result, GenerationErrorKind::Limit, "node limit");
        }
    }

    let mut db = database();
    let provider_file = input(&mut db, "source", &provider_source("u256", "7"));
    let template_source = target_template("source-output", "u256");
    let mut probe =
        GenerationSession::new("source-probe".to_owned(), GenerationBudget::new(1, 4096));
    let output_len = generate(&mut probe, &db, provider_file, "source", template_source)
        .unwrap()
        .source()
        .len();
    let mut exact = GenerationSession::new(
        "source-exact".to_owned(),
        GenerationBudget::new(1, output_len),
    );
    generate(
        &mut exact,
        &db,
        provider_file,
        "source",
        target_template("source-output", "u256"),
    )
    .unwrap();
    assert_eq!(exact.budget().used_bytes(), output_len);
    let mut short = GenerationSession::new(
        "source-short".to_owned(),
        GenerationBudget::new(1, output_len - 1),
    );
    assert_error(
        generate(
            &mut short,
            &db,
            provider_file,
            "source",
            target_template("source-output", "u256"),
        ),
        GenerationErrorKind::Limit,
        "source byte limit",
    );
    assert_eq!(short.budget().used_functions(), 0);
}

#[test]
fn request_keys_are_reserved_before_success_or_failure() {
    let mut db = database();
    let provider_file = input(&mut db, "duplicates", &provider_source("u256", "7"));
    let mut session = GenerationSession::new(
        "duplicate-stage".to_owned(),
        GenerationBudget::new(4, 16_384),
    );
    let first = generate(
        &mut session,
        &db,
        provider_file,
        "success",
        target_template("success-output", "u256"),
    )
    .unwrap();
    assert_eq!(
        first.provenance().request_identity,
        Some(identity("duplicate-stage", "success"))
    );
    let first_bytes = session.budget().used_bytes();
    let duplicate = assert_error(
        generate(
            &mut session,
            &db,
            provider_file,
            "success",
            target_template("success-output-again", "u256"),
        ),
        GenerationErrorKind::DuplicateRequest,
        "already attempted in this session",
    );
    assert_eq!(
        duplicate.request_identity,
        Some(identity("duplicate-stage", "success"))
    );

    let failed = assert_error(
        generate(
            &mut session,
            &db,
            provider_file,
            "failed",
            raw_template("bad-template", "const fn other() -> u256 {}"),
        ),
        GenerationErrorKind::Template,
        "named top-level function",
    );
    assert_eq!(
        failed.request_identity,
        Some(identity("duplicate-stage", "failed"))
    );
    let duplicate_failed = assert_error(
        generate(
            &mut session,
            &db,
            provider_file,
            "failed",
            target_template("corrected-template", "u256"),
        ),
        GenerationErrorKind::DuplicateRequest,
        "already attempted in this session",
    );
    assert_eq!(duplicate_failed.request_identity, failed.request_identity);
    assert_eq!(session.budget().used_functions(), 1);
    assert_eq!(session.budget().used_bytes(), first_bytes);

    let output_error = assert_error(
        generate(
            &mut session,
            &db,
            provider_file,
            "output-failure",
            target_template("narrow-output", "u8"),
        ),
        GenerationErrorKind::ReturnType,
        "target function returns u8",
    );
    let receipt = output_error.provenance.as_ref().unwrap();
    assert_eq!(
        output_error.request_identity,
        Some(identity("duplicate-stage", "output-failure"))
    );
    assert_eq!(receipt.request_identity, output_error.request_identity);
    assert_eq!(session.budget().used_functions(), 2);
    assert!(session.budget().used_bytes() > first_bytes);
    let charged = session.budget().used_bytes();
    assert_error(
        generate(
            &mut session,
            &db,
            provider_file,
            "output-failure",
            target_template("corrected-output", "u256"),
        ),
        GenerationErrorKind::DuplicateRequest,
        "already attempted",
    );
    assert_eq!(session.budget().used_functions(), 2);
    assert_eq!(session.budget().used_bytes(), charged);
}

#[test]
fn request_identity_is_independent_of_order_and_other_sessions() {
    fn run(order: [&str; 2]) -> BTreeMap<String, (RequestIdentity, usize, String)> {
        let mut db = database();
        let provider_file = input(&mut db, "ordered", &provider_source("u256", "5"));
        let mut session =
            GenerationSession::new("ordered-stage".to_owned(), GenerationBudget::new(2, 8192));
        let mut observations = BTreeMap::new();
        let mut retained = Vec::new();
        for key in order {
            let artifact = generate(
                &mut session,
                &db,
                provider_file,
                key,
                target_template(&format!("{key}-output"), "u256"),
            )
            .unwrap();
            observations.insert(
                key.to_owned(),
                (
                    artifact.provenance().request_identity.clone().unwrap(),
                    artifact.provenance().invocation,
                    value(artifact.database(), artifact.file()),
                ),
            );
            retained.push(artifact);
        }
        assert_eq!(retained.len(), 2);
        observations
    }

    let forward = run(["a", "b"]);
    let reverse = run(["b", "a"]);
    for key in ["a", "b"] {
        assert_eq!(forward[key].0, identity("ordered-stage", key));
        assert_eq!(reverse[key].0, identity("ordered-stage", key));
        assert_eq!(forward[key].2, "5");
        assert_eq!(reverse[key].2, "5");
        assert_ne!(forward[key].1, reverse[key].1);
    }

    let mut db = database();
    let provider_file = input(&mut db, "stage", &provider_source("u256", "5"));
    let mut left = GenerationSession::new("left".to_owned(), GenerationBudget::new(1, 4096));
    let mut right = GenerationSession::new("right".to_owned(), GenerationBudget::new(1, 4096));
    let left_artifact = generate(
        &mut left,
        &db,
        provider_file,
        "same",
        target_template("left-output", "u256"),
    )
    .unwrap();
    let right_artifact = generate(
        &mut right,
        &db,
        provider_file,
        "same",
        target_template("right-output", "u256"),
    )
    .unwrap();
    assert_eq!(
        left_artifact.provenance().request_identity,
        Some(identity("left", "same"))
    );
    assert_eq!(
        right_artifact.provenance().request_identity,
        Some(identity("right", "same"))
    );
}

#[test]
fn same_identity_fresh_sessions_recompute_edits_and_retain_old_artifacts() {
    use salsa::Setter;

    let mut db = database();
    let provider_file = input(&mut db, "edited", &provider_source("u256", "1"));
    let mut retained = Vec::new();
    for expected in [1, 2, 1] {
        let source = provider_source("u256", &expected.to_string());
        provider_file.set_text(&mut db).to(source.clone());
        let mut session =
            GenerationSession::new("edit-stage".to_owned(), GenerationBudget::new(1, 4096));
        let artifact = generate(
            &mut session,
            &db,
            provider_file,
            "stable-key",
            target_template("edited-output", "u256"),
        )
        .unwrap();

        let mut fresh = database();
        let fresh_file = input(&mut fresh, "edited", &source);
        let mut fresh_session =
            GenerationSession::new("edit-stage".to_owned(), GenerationBudget::new(1, 4096));
        let fresh_artifact = generate(
            &mut fresh_session,
            &fresh,
            fresh_file,
            "stable-key",
            target_template("edited-output", "u256"),
        )
        .unwrap();

        assert_eq!(
            artifact.provenance().request_identity,
            Some(identity("edit-stage", "stable-key"))
        );
        assert_eq!(artifact.provenance(), fresh_artifact.provenance());
        assert_eq!(artifact.source(), fresh_artifact.source());
        assert_eq!(
            value(artifact.database(), artifact.file()),
            expected.to_string()
        );
        retained.push(artifact);
    }
    assert_eq!(value(retained[0].database(), retained[0].file()), "1");
    assert_eq!(value(retained[1].database(), retained[1].file()), "2");
    assert_eq!(value(retained[2].database(), retained[2].file()), "1");
    assert_eq!(
        retained[0].provenance().provider_source,
        retained[2].provenance().provider_source
    );
    assert_ne!(
        retained[0].provenance().provider_source,
        retained[1].provenance().provider_source
    );
}
