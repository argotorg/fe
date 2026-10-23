use common::InputDb;
use fe_driver::{DriverDataBase, generation::prepare_file};
use hir::analysis::{
    semantic::{GenericSubst, SemConstScalar, SemConstValue, eval_body_owner_const},
    ty::ty_check::BodyOwner,
};
use url::Url;

fn prepare(source: &str) -> Result<DriverDataBase, String> {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///tmp/derive-request-visibility.fe").unwrap(),
        Some(source.into()),
    );
    prepare_file(&db, file)
        .map_err(|error| error.cause)?
        .ok_or_else(|| "expected a generated implementation".into())
}

fn assert_value(db: &DriverDataBase, expected: &str) {
    let file = db
        .workspace()
        .get(
            db,
            &Url::parse("file:///tmp/derive-request-visibility.fe").unwrap(),
        )
        .unwrap();
    let module = db.top_mod(file);
    let function = module
        .all_funcs(db)
        .iter()
        .copied()
        .find(|function| {
            function
                .name(db)
                .to_opt()
                .is_some_and(|name| name.data(db) == "run")
        })
        .unwrap();
    let owner = BodyOwner::Func(function, None);
    let value = eval_body_owner_const(db, owner, GenericSubst::for_body_owner(db, owner, vec![]))
        .into_ready()
        .expect("generated implementation should evaluate");
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = value.value(db)
    else {
        panic!("expected an integer result");
    };
    assert_eq!(value.to_string(), expected);
}

const PREFIX: &str = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item, field_count}
pub trait Score { const fn score() -> u256 }
pub struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        let count = field_count(record) as u256
        quote { const fn score() -> u256 { ${count} } }
    }
}
"#;

#[test]
fn explicit_private_fields_refuse_before_provider_execution() {
    for request in [
        "impl Score for owner::Secret with (Provider)\nconst fn run() -> u256 { owner::Secret::score() }",
    ] {
        let prefix = PREFIX.replace(
            "let count = field_count(record) as u256",
            "let zero: u256 = 0\nlet count = 1 / zero",
        );
        let source =
            format!("{prefix}\nmod owner {{ pub struct Secret {{ hidden: u256 }} }}\n{request}");
        let cause = prepare(&source)
            .err()
            .expect("private reflection must be refused");
        assert!(
            cause.contains("visible from the requesting scope"),
            "{cause}"
        );
        assert!(!cause.contains("division by zero"), "{cause}");
    }
}

#[test]
fn requesters_with_private_field_access_remain_lawful() {
    let explicit = format!(
        "{PREFIX}\nstruct Secret {{ hidden: u256 }}\nimpl Score for Secret with (Provider)\nconst fn run() -> u256 {{ Secret::score() }}"
    );
    let output = prepare(&explicit).unwrap_or_else(|cause| panic!("{cause}"));
    assert_value(&output, "1");
}

#[test]
fn sibling_requests_can_reflect_public_fields() {
    for request in [
        "impl Score for owner::Public with (Provider)\nconst fn run() -> u256 { owner::Public::score() }",
    ] {
        let source = format!(
            "{PREFIX}\nmod owner {{ pub struct Public {{ pub exposed: u256 }} }}\n{request}"
        );
        let output = prepare(&source).unwrap_or_else(|cause| panic!("{cause}"));
        assert_value(&output, "1");
    }
}

#[test]
fn enum_payload_fields_follow_the_same_requester_visibility_rule() {
    for request in [
        "impl Score for owner::Payload with (Provider)\nconst fn run() -> u256 { owner::Payload::score() }",
    ] {
        let faulting = PREFIX.replace(
            "let count = field_count(record) as u256",
            "let zero: u256 = 0\nlet count = 1 / zero",
        );
        let source = format!(
            "{faulting}\nmod owner {{ pub enum Payload {{ Case {{ hidden: u256 }} }} }}\n{request}"
        );
        let cause = prepare(&source).err().expect("private payload field");
        assert!(
            cause.contains("visible from the requesting scope"),
            "{cause}"
        );
        assert!(!cause.contains("division by zero"), "{cause}");

        let reflected = PREFIX
            .replace("Item, field_count", "Item, field_count, variant")
            .replace("field_count(record)", "field_count(variant(record, 0))");
        let source = format!(
            "{reflected}\nmod owner {{ pub enum Payload {{ Case {{ pub exposed: u256 }} }} }}\n{request}"
        );
        let output = prepare(&source).unwrap_or_else(|cause| panic!("{cause}"));
        assert_value(&output, "1");
    }
}
