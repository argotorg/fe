//! Executable packaging probe, not a public or hygienic reference protocol.
#[path = "support/frozen_imports.rs"]
mod frozen;

use common::InputDb;
use fe_driver::generation::{
    FunctionTemplate, GeneratedFunction, GenerationBudget, GenerationRequest, GenerationSession,
};
use frozen::{FrozenExport, database, evaluate, import_stage, named};
use url::Url;

const CONTEXT: &str = r#"
const fn base() -> u256 {}
const fn offset(_ value: u256) -> u256 { value + 1 }
mod helpers {
    pub const fn adjust(_ value: u256) -> u256 { super::offset(value) }
}
pub const fn apply(amount: u256) -> u256 { ingot::base() + helpers::adjust(amount) }
pub const fn other() -> u256 { 17 }
"#;

fn artifact(base: u64, context: &str) -> GeneratedFunction {
    let mut db = database();
    let provider_source = format!(
        "use core::meta::FunctionBody\nconst fn provide() -> FunctionBody<u256> {{ FunctionBody {{ value: {base} }} }}"
    );
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///frozen-provider.fe").unwrap(),
        Some(provider_source),
    );
    GenerationSession::new("same-stage".to_owned(), GenerationBudget::new(1, 65536))
        .generate_value_function(
            &db,
            named(&db, file, "provide"),
            GenerationRequest {
                key: "same-request".to_owned(),
                template: FunctionTemplate {
                    url: Url::parse("file:///frozen-artifact.fe").unwrap(),
                    source: context.to_owned(),
                    function_name: "base".to_owned(),
                },
            },
        )
        .unwrap()
}

fn rejection<T>(result: Result<T, String>, needle: &str) -> String {
    let error = match result {
        Ok(_) => panic!("expected rejection containing {needle}"),
        Err(error) => error,
    };
    assert!(error.contains(needle), "expected {needle}: {error}");
    error
}

#[test]
fn isolated_ingot_preserves_root_paths_private_helpers_and_parameter_calls() {
    let source = artifact(40, CONTEXT);
    let export = FrozenExport::new(&source, "apply").unwrap();
    let stage = import_stage(
        &[("chosen", &export)],
        r#"
const fn base() -> u256 { 1000 }
const fn offset(_ value: u256) -> u256 { 2000 }
const fn consume() -> u256 { chosen::apply(amount: 1) }
"#,
    )
    .unwrap();
    assert_eq!(evaluate(&stage.db, stage.file, "consume"), "42");
    assert_eq!(stage.receipts.len(), 1);
    let receipt = &stage.receipts[0];
    assert_eq!(receipt.alias, "chosen");
    assert_eq!(receipt.export_name, "apply");
    assert_eq!(receipt.original_source, source.source());
    assert_eq!(&receipt.original_provenance, source.provenance());
    assert!(
        receipt
            .materialized_source_url
            .path()
            .ends_with("/src/lib.fe")
    );
}

#[test]
fn same_logical_request_revisions_coexist_and_imports_outlive_originals() {
    let stage = {
        let old = artifact(40, CONTEXT);
        let newer = artifact(80, CONTEXT);
        assert_eq!(
            old.provenance().request_identity,
            newer.provenance().request_identity
        );
        let left = FrozenExport::new(&old, "apply").unwrap();
        let right = FrozenExport::new(&newer, "apply").unwrap();
        let stage = import_stage(
            &[("older", &left), ("newer", &right)],
            "const fn consume() -> u256 { older::apply(amount: 1) + newer::apply(amount: 1) }",
        )
        .unwrap();
        assert_ne!(
            stage.receipts[0].original_source,
            stage.receipts[1].original_source
        );
        let again = import_stage(
            &[("older", &left)],
            "const fn consume() -> u256 { older::apply(amount: 1) }",
        )
        .unwrap();
        assert_eq!(evaluate(&again.db, again.file, "consume"), "42");
        stage
    };
    assert_eq!(evaluate(&stage.db, stage.file, "consume"), "124");
}

#[test]
fn ordinary_dependency_aliases_are_shadowable_and_are_not_bound_references() {
    let source = artifact(40, CONTEXT);
    let export = FrozenExport::new(&source, "apply").unwrap();
    for recipient in [
        r#"
mod chosen { pub const fn apply(amount: u256) -> u256 { 999 } }
const fn consume() -> u256 { chosen::apply(amount: 1) }
"#,
        r#"
mod wrapper {
    mod chosen { pub const fn apply(amount: u256) -> u256 { 999 } }
    pub const fn run() -> u256 { chosen::apply(amount: 1) }
}
const fn consume() -> u256 { wrapper::run() }
"#,
    ] {
        let stage = import_stage(&[("chosen", &export)], recipient).unwrap();
        assert_eq!(evaluate(&stage.db, stage.file, "consume"), "999");
        assert_eq!(stage.receipts[0].export_name, "apply");
        // A receipt of the installed dependency cannot prove which item a
        // particular authored path selected in a nested lexical scope.
    }
}

#[test]
fn callers_keep_label_type_visibility_and_dependency_requirements() {
    let source = artifact(40, CONTEXT);
    let export = FrozenExport::new(&source, "apply").unwrap();
    rejection(
        import_stage(
            &[("chosen", &export)],
            "const fn consume() -> u256 { chosen::apply(wrong: 1) }",
        ),
        "label",
    );
    rejection(
        import_stage(
            &[("chosen", &export)],
            "const fn consume() -> u256 { chosen::apply(amount: true) }",
        ),
        "type mismatch",
    );
    rejection(
        import_stage(
            &[("chosen", &export)],
            "const fn consume() -> u256 { chosen::offset(1) }",
        ),
        "not visible",
    );
    rejection(
        import_stage(
            &[],
            "const fn consume() -> u256 { chosen::apply(amount: 1) }",
        ),
        "chosen",
    );
}

#[test]
fn whole_source_packaging_exposes_other_public_items() {
    let source = artifact(40, CONTEXT);
    let export = FrozenExport::new(&source, "apply").unwrap();
    let stage = import_stage(
        &[("chosen", &export)],
        "const fn consume() -> u256 { chosen::other() }",
    )
    .unwrap();
    assert_eq!(evaluate(&stage.db, stage.file, "consume"), "17");
    // The selected export is provenance, not an export whitelist for an ingot.
}

#[test]
fn export_protocol_rejects_unready_interfaces_without_rewriting_them() {
    let source = artifact(
        40,
        &format!(
            "{CONTEXT}\n{}",
            r#"
pub const fn generic<T>(_ value: T) -> u256 { 1 }
pub unsafe const fn unsafe_export() -> u256 { 1 }
pub fn runtime_only() -> u256 { 1 }
pub struct Payload { pub value: u256 }
pub const fn nominal(_ value: Payload) -> u256 { 1 }
"#
        ),
    );
    for name in [
        "base",
        "generic",
        "unsafe_export",
        "runtime_only",
        "nominal",
        "missing",
    ] {
        assert!(FrozenExport::new(&source, name).is_err(), "exported {name}");
    }
}

#[test]
fn import_admission_rejects_duplicate_aliases_and_excessive_source() {
    let source = artifact(40, CONTEXT);
    let export = FrozenExport::new(&source, "apply").unwrap();
    let recipient = "const fn consume() -> u256 { 1 }";
    assert!(import_stage(&[("chosen", &export), ("chosen", &export)], recipient).is_err());
    for alias in ["core", "std", "ingot", "bad-alias", "a\"\n[evil]"] {
        assert!(
            import_stage(&[(alias, &export)], recipient).is_err(),
            "accepted {alias}"
        );
    }
    assert!(import_stage(&[("chosen", &export)], &" ".repeat(65537)).is_err());
    assert!(import_stage(&[], &" ".repeat(65537)).is_err());
    let aliases = (0..17)
        .map(|index| format!("dep_{index}"))
        .collect::<Vec<_>>();
    let imports = aliases
        .iter()
        .map(|alias| (alias.as_str(), &export))
        .collect::<Vec<_>>();
    assert!(import_stage(&imports, recipient).is_err());
}

fn designated_call(source: &str, spelling: &str) -> std::ops::Range<usize> {
    let mut offsets = source.match_indices(spelling).map(|(offset, _)| offset);
    let start = offsets.next().expect("designated call spelling");
    assert!(offsets.next().is_none(), "ambiguous fixture call slot");
    start..start + spelling.len()
}

#[test]
fn explicit_call_binding_rejects_shadows_without_banning_unrelated_names() {
    let source = artifact(40, CONTEXT);
    let export = FrozenExport::new(&source, "apply").unwrap();
    for (recipient, call, accepts) in [
        (
            "const fn consume() -> u256 { chosen::apply(amount: 1) }",
            "chosen::apply(amount: 1)",
            true,
        ),
        (
            r#"
use chosen::apply as selected
mod unrelated { mod chosen { pub const fn apply(amount: u256) -> u256 { 999 } } }
const fn consume() -> u256 { selected(amount: 1) }
"#,
            "selected(amount: 1)",
            true,
        ),
        (
            r#"
mod chosen { pub const fn apply(amount: u256) -> u256 { 999 } }
const fn consume() -> u256 { chosen::apply(amount: 1) }
"#,
            "chosen::apply(amount: 1)",
            false,
        ),
        (
            r#"
mod wrapper {
    mod chosen { pub const fn apply(amount: u256) -> u256 { 999 } }
    pub const fn run() -> u256 { chosen::apply(amount: 1) }
}
const fn consume() -> u256 { wrapper::run() }
"#,
            "chosen::apply(amount: 1)",
            false,
        ),
    ] {
        let stage = import_stage(&[("chosen", &export)], recipient).unwrap();
        let checked = frozen::validate_call_binding(&stage, designated_call(recipient, call), 0);
        if accepts {
            checked.unwrap();
            assert_eq!(evaluate(&stage.db, stage.file, "consume"), "42");
        } else {
            rejection(checked, "binding does not match");
            // No evaluation follows a failed explicit binding check.
        }
    }
}

#[test]
fn explicit_binding_distinguishes_same_identity_and_same_signature_revisions() {
    let old = artifact(40, CONTEXT);
    let new = artifact(80, CONTEXT);
    let left = FrozenExport::new(&old, "apply").unwrap();
    let right = FrozenExport::new(&new, "apply").unwrap();
    let recipient = "const fn consume() -> u256 { newer::apply(amount: 1) }";
    let stage = import_stage(&[("older", &left), ("newer", &right)], recipient).unwrap();
    let call = designated_call(recipient, "newer::apply(amount: 1)");
    rejection(
        frozen::validate_call_binding(&stage, call.clone(), 0),
        "binding does not match",
    );
    frozen::validate_call_binding(&stage, call.clone(), 1).unwrap();
    assert_eq!(evaluate(&stage.db, stage.file, "consume"), "82");
    rejection(
        frozen::validate_call_binding(&stage, call.start..call.end - 1, 1),
        "exactly one",
    );
}
