use super::*;
use fe_driver::generation::{GenerationErrorKind, imports::ImportErrorKind};

fn validator(provider_source: &str, package: &str, consumer: &str) -> BoundFunction {
    let mut db = database();
    let provider_file = provider(&mut db, provider_source);
    let generated = generate(&db, provider_file, package).unwrap();
    let frozen = FrozenArtifact::new(generated);
    let within = frozen.export("within").unwrap();
    bind(&within, consumer).unwrap()
}

fn generation_error(result: Result<GeneratedFunction, GenerationError>) -> GenerationError {
    match result {
        Ok(_) => panic!("generation unexpectedly succeeded"),
        Err(error) => error,
    }
}

fn import_error(result: Result<BoundFunction, ImportError>) -> ImportError {
    match result {
        Ok(_) => panic!("binding unexpectedly succeeded"),
        Err(error) => error,
    }
}

#[test]
fn generated_bounds_drive_inclusive_validator_calls() {
    let bound = validator(PROVIDER, PACKAGE, CONSUMER);
    for (name, expected) in [
        ("below", "false"),
        ("lower", "true"),
        ("inside", "true"),
        ("upper", "true"),
        ("above", "false"),
    ] {
        assert_eq!(evaluate(&bound, name), expected, "caller `{name}`");
    }
    assert_eq!(bound.receipt().selected_name, "within");
    assert_eq!(
        &bound.receipt().consumer_source[bound.receipt().emitted_call.clone()],
        "__frozen_import::within(value: value)"
    );
}

#[test]
fn same_urls_recompute_provider_edits_and_retained_artifacts_keep_old_results() {
    let mut db = database();
    let old_file = provider(&mut db, PROVIDER);
    let old_generated = generate(&db, old_file, PACKAGE).unwrap();
    let old_frozen = FrozenArtifact::new(old_generated);
    let old_within = old_frozen.export("within").unwrap();
    let old_bound = bind(&old_within, CONSUMER).unwrap();

    let edited_source = PROVIDER.replace("let lower: u256 = 2 * 5", "let lower: u256 = 6 * 5");
    assert_ne!(edited_source, PROVIDER, "provider bounds fixture changed");
    let edited_consumer = CONSUMER.replace("validate(value: 15)", "validate(value: 35)");
    assert_ne!(edited_consumer, CONSUMER, "consumer probe fixture changed");
    let edited_file = provider(&mut db, &edited_source);
    assert_eq!(edited_file, old_file);
    assert_eq!(edited_file.text(&db), &edited_source);
    let edited_generated = generate(&db, edited_file, PACKAGE).unwrap();
    let edited_frozen = FrozenArtifact::new(edited_generated);
    let edited_within = edited_frozen.export("within").unwrap();
    let edited_bound = bind(&edited_within, &edited_consumer).unwrap();

    let mut fresh_db = database();
    let fresh_file = provider(&mut fresh_db, &edited_source);
    let fresh_generated = generate(&fresh_db, fresh_file, PACKAGE).unwrap();
    let fresh_frozen = FrozenArtifact::new(fresh_generated);
    let fresh_within = fresh_frozen.export("within").unwrap();
    let fresh_bound = bind(&fresh_within, &edited_consumer).unwrap();

    let retained = bind(&old_within, &edited_consumer).unwrap();
    assert_eq!(evaluate(&retained, "inside"), "false");
    assert_eq!(evaluate(&retained, "lower"), "true");
    assert_eq!(
        old_bound.receipt().original_provenance.request_identity,
        edited_bound.receipt().original_provenance.request_identity
    );
    assert_eq!(edited_bound.source(), fresh_bound.source());
    for name in ["below", "lower", "inside", "upper", "above"] {
        assert_eq!(evaluate(&edited_bound, name), evaluate(&fresh_bound, name));
    }

    assert_eq!(evaluate(&old_bound, "inside"), "true");
    assert_eq!(evaluate(&edited_bound, "inside"), "true");
    assert_eq!(evaluate(&fresh_bound, "inside"), "true");
    assert_eq!(evaluate(&edited_bound, "lower"), "false");
    assert_eq!(evaluate(&fresh_bound, "lower"), "false");
    assert_eq!(
        edited_bound.receipt().original_source,
        fresh_bound.receipt().original_source
    );
    assert_ne!(
        old_bound.receipt().original_source,
        edited_bound.receipt().original_source
    );
    assert_eq!(evaluate(&old_bound, "inside"), "true");

    let restored_file = provider(&mut db, PROVIDER);
    assert_eq!(restored_file, old_file);
    let restored_generated = generate(&db, restored_file, PACKAGE).unwrap();
    let restored_frozen = FrozenArtifact::new(restored_generated);
    let restored_within = restored_frozen.export("within").unwrap();
    let restored_bound = bind(&restored_within, CONSUMER).unwrap();
    assert_eq!(evaluate(&restored_bound, "inside"), "true");
    assert_eq!(
        restored_bound.receipt().original_source,
        old_bound.receipt().original_source
    );
}

#[test]
fn descriptor_shape_and_helper_requirements_are_checked_before_export() {
    let wrong_shape = PROVIDER
        .replace("FunctionBody<(u256, u256)>", "FunctionBody<(u256, bool)>")
        .replace("(lower, lower + width)", "(lower, true)");
    assert_ne!(wrong_shape, PROVIDER, "provider shape fixture changed");
    let mut db = database();
    let wrong_file = provider(&mut db, &wrong_shape);
    let wrong = generation_error(generate(&db, wrong_file, PACKAGE));
    assert!(
        matches!(wrong.kind, GenerationErrorKind::Output),
        "{}",
        wrong.message
    );

    const REQUIREMENT: &str = r#"
trait Ready {}
struct Proof {}
const fn require<T: Ready>(_ proof: T) {}

const fn bounds() -> (u256, u256) {}
pub const fn within(value: u256) -> bool {
    require(Proof {})
    let limits = bounds()
    value >= limits.0 && value <= limits.1
}
"#;
    let missing_file = provider(&mut db, PROVIDER);
    let missing = generation_error(generate(&db, missing_file, REQUIREMENT));
    assert_eq!(
        missing.kind,
        GenerationErrorKind::Output,
        "{}",
        missing.message
    );

    let satisfied = REQUIREMENT.replace(
        "struct Proof {}",
        "struct Proof {}\nimpl Ready for Proof {} ",
    );
    let generated = generate(&db, missing_file, &satisfied).unwrap();
    let frozen = FrozenArtifact::new(generated);
    let within = frozen.export("within").unwrap();
    let bound = bind(&within, CONSUMER).unwrap();
    assert_eq!(evaluate(&bound, "lower"), "true");
    assert_eq!(evaluate(&bound, "above"), "false");
}

#[test]
fn recipient_shadowing_and_wrong_argument_types_are_rejected() {
    let mut db = database();
    let provider_file = provider(&mut db, PROVIDER);
    let generated = generate(&db, provider_file, PACKAGE).unwrap();
    let frozen = FrozenArtifact::new(generated);
    let within = frozen.export("within").unwrap();

    let shadowed = r#"
mod __frozen_import {
    pub const fn within(value: u256) -> bool { true }
}
const fn validate(value: u256) -> bool {}
"#;
    let shadow = import_error(bind(&within, shadowed));
    assert_eq!(shadow.kind, ImportErrorKind::Binding, "{}", shadow.message);
    let receipt = shadow
        .receipt
        .expect("shadow rejection should retain receipt");
    assert_eq!(
        &receipt.consumer_source[receipt.emitted_call.clone()],
        "__frozen_import::within(value: value)"
    );

    let wrong_type = "const fn validate(value: bool) -> bool {}";
    let mismatch = import_error(bind(&within, wrong_type));
    assert_eq!(
        mismatch.kind,
        ImportErrorKind::Output,
        "{}",
        mismatch.message
    );
    assert!(mismatch.receipt.is_some());
}
