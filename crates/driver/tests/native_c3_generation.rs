use common::InputDb;
use fe_driver::{DriverDataBase, generation::prepare_file};
use hir::analysis::{
    semantic::{EvalOutcome, GenericSubst, SemConstScalar, SemConstValue, eval_body_owner_const},
    ty::ty_check::BodyOwner,
};
use url::Url;

#[path = "support/generation.rs"]
mod support;

const LAWFUL: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\ntrait Value { const fn value() -> u256 }\npub struct Key {}\nstruct Provider {}\nimpl Derive<trait Value> for Provider {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote { const fn value() -> u256 { 7 } }\n    }\n}\nimpl Value for Key with (Provider)\n#[test]\nfn independent_expected_seven_and_repeat_use() {\n    assert!(Key::value() == 7)\n    assert!(Key::value() + Key::value() == 14)\n}\n";

const HANDWRITTEN: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\ntrait Value { const fn value() -> u256 }\npub struct Key {}\nstruct Provider {}\nimpl Derive<trait Value> for Provider {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote { const fn value() -> u256 { 7 } }\n    }\n}\nimpl Value for Key { const fn value() -> u256 { 7 } }\n#[test]\nfn independent_expected_seven_and_repeat_use() {\n    assert!(Key::value() == 7)\n    assert!(Key::value() + Key::value() == 14)\n}\n";

const PRODUCER_SCOPE: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\ntrait Value { const fn value() -> u256 }\npub struct Key {}\nmod producer {\n    use super::Value\n    use core::derive::Derive\n    use core::meta::{Record, Code, Item}\n    const fn helper() -> u256 { 7 }\n    pub struct Provider {}\n    impl Derive<trait Value> for Provider {\n        const fn derive(_ record: Record) -> Code<Item> {\n            quote { const fn value() -> u256 { helper() } }\n        }\n    }\n}\nimpl Value for Key with (producer::Provider)\n#[test]\nfn independent_expected_seven_and_repeat_use() {\n    assert!(Key::value() == 7)\n    assert!(Key::value() + Key::value() == 14)\n}\n";

const REFUSED_REQUEST: &str = "use core::default::Default\nuse core::derive::Derive\nuse core::meta::{Record, Code, Item}\nmod m {\n    pub struct Key { x: u256 }\n}\nstruct Provider {}\nimpl Derive<trait Default> for Provider {\n    const fn derive(_ record: Record) -> Code<Item> {\n        let zero: u256 = 0\n        let _ = 1 / zero\n        quote { fn default() -> Self { m::Key { x: 0 } } }\n    }\n}\nimpl Default for m::Key with (Provider)\n";

const DUPLICATE_ORDINARY: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\ntrait Value { const fn value() -> u256 }\npub struct Key {}\nstruct Provider {}\nimpl Derive<trait Value> for Provider {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote { const fn value() -> u256 { 7 } }\n    }\n}\nimpl Value for Key with (Provider)\nimpl Value for Key { const fn value() -> u256 { 7 } }\n";

const WRONG_OUTPUT_ORDINARY: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\ntrait Value { const fn value() -> u256 }\npub struct Key {}\nstruct Provider {}\nimpl Derive<trait Value> for Provider {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote { const fn value() -> bool { true } }\n    }\n}\nimpl Value for Key with (Provider)\n";

#[test]
fn refused_request_keeps_the_authored_database_and_cannot_evaluate_fault() {
    let source = format!(
        "{}\n{}",
        LAWFUL.replace(
            "#[test]\nfn independent_expected_seven_and_repeat_use()",
            "fn first_use()",
        ),
        REFUSED_REQUEST
            .replace(
                "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\n",
                "",
            )
            .replace("Key", "OtherKey")
            .replace("Provider", "FaultProvider")
    );
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///tmp/native-c3-atomic-refusal.fe").unwrap(),
        Some(source.clone()),
    );
    let err = prepare_file(&db, file)
        .err()
        .expect("a request for a target with invisible fields must refuse");
    assert!(
        err.cause.contains(
            "a derive header requires every field of its target to be visible where the header is written"
        ),
        "{}",
        err.cause
    );
    assert!(!err.cause.contains("division by zero"));
    assert_eq!(file.text(&db), &source);
    assert!(db.generation_inputs().functions(&db).is_empty());
    let request_start = source.find("impl Default for m::OtherKey with").unwrap();
    assert!(u32::from(err.range.start()) as usize <= request_start);
    assert!(u32::from(err.range.end()) as usize > request_start);
}

#[test]
fn explicit_derive_positive_and_negative_matrix() {
    for source in [LAWFUL, HANDWRITTEN, PRODUCER_SCOPE] {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///tmp/native-c3-positive.fe").unwrap();
        let file = db
            .workspace()
            .touch(&mut db, url.clone(), Some(source.into()));
        let prepared = prepare_file(&db, file).expect("lawful explicit derive setup");
        let check_db = prepared.as_ref().unwrap_or(&db);
        let check_file = check_db.workspace().get(check_db, &url).unwrap();
        let diags = check_db.run_on_top_mod(check_db.top_mod(check_file));
        assert!(diags.is_empty(), "{}", diags.format_diags(check_db));
    }

    for (source, expected_diag) in [
        (DUPLICATE_ORDINARY, "5-0001"),
        (WRONG_OUTPUT_ORDINARY, "6-0008"),
    ] {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///tmp/native-c3-diag.fe").unwrap();
        let file = db
            .workspace()
            .touch(&mut db, url.clone(), Some(source.into()));
        let prepared = prepare_file(&db, file);
        if let Err(err) = prepared {
            assert!(err.cause.contains(expected_diag), "{err}");
        } else {
            let check_db = prepared.unwrap().unwrap_or(db);
            let check_file = check_db.workspace().get(&check_db, &url).unwrap();
            let diags = check_db
                .run_on_top_mod(check_db.top_mod(check_file))
                .format_diags(&check_db);
            assert!(diags.contains(expected_diag), "{diags}");
        }
    }
}

#[test]
fn explicit_derive_and_named_choice_preserve_unified_evidence() {
    let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item, field_count}

trait Score {
    const fn score() -> usize
}
struct Fields {}
impl Derive<trait Score> for Fields {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        quote {
            const fn score() -> usize {
                ${n} + 40
            }
        }
    }
}
pub struct Pair { a: u256, b: bool }
impl Score for Pair with (Fields)

struct ZeroFields {}
impl Derive<trait Score> for ZeroFields {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn score() -> usize {
                0
            }
        }
    }
}
impl Score for Pair as Zero with (ZeroFields)

const fn observed<T: Score>() -> usize {
    T::score()
}

const fn check_ordinary_and_named() -> usize {
    let a = observed<Pair>()
    let b = with (Zero) { observed<Pair>() }
    let c = observed<Pair>()
    a * 1000 + b * 100 + c
}
"#;
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///tmp/native-derive-named.fe").unwrap();
    let file = db
        .workspace()
        .touch(&mut db, url.clone(), Some(source.into()));
    let output = prepare_file(&db, file).unwrap().unwrap();
    let output_file = output.workspace().get(&output, &url).unwrap();
    let module = output.top_mod(output_file);
    let diags = output.run_on_top_mod(module);
    assert!(diags.is_empty(), "{}", diags.format_diags(&output));
    let func = module
        .all_funcs(&output)
        .iter()
        .find(|f| {
            f.name(&output)
                .to_opt()
                .is_some_and(|n| n.data(&output) == "check_ordinary_and_named")
        })
        .unwrap();
    let owner = BodyOwner::Func(*func, None);
    let EvalOutcome::Ready(val) = eval_body_owner_const(
        &output,
        owner,
        GenericSubst::for_body_owner(&output, owner, vec![]),
    ) else {
        panic!("expected ready CTFE outcome");
    };
    assert!(
        matches!(
            val.value(&output),
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } if value.to_string() == "42042"
        ),
        "unexpected value: {val:?}"
    );

    for (bad_source, expected_cause) in [
        (
            "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\ntrait Other {}\ntrait Score { const fn score() -> usize }\nstruct Fields {}\nimpl Derive<trait Other> for Fields { const fn derive(_ r: Record) -> Code<Item> { quote { const fn score() -> usize { 1 } } } }\npub struct Pair { a: u256 }\nimpl Score for Pair with (Fields)\n",
            "Derive<trait Score>",
        ),
        (
            "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\ntrait Score { const fn score() -> usize }\npub struct Pair { a: u256 }\nimpl Score for Pair with (missing_entry)\n",
            "cannot resolve the derive provider type",
        ),
        (
            "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\ntrait Score { const fn score() -> usize }\nstruct Fields {}\nimpl Derive<trait Score> for Fields { const fn derive(_ r: Record) -> Code<Item> { quote { const fn unrelated() -> usize { 1 } } } }\npub struct Pair { a: u256 }\nimpl Score for Pair with (Fields)\n",
            "method not defined in trait",
        ),
        (
            "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\ntrait Score { const fn score() -> usize }\nstruct Fields {}\nimpl Derive<trait Score> for Fields { const fn derive(_ r: Record) -> Code<Item> { let n = Pair::score()\n quote { const fn score() -> usize { ${n} } } } }\npub struct Pair { a: u256 }\nimpl Score for Pair with (Fields)\n",
            "only declared by a written header",
        ),
        (
            "mod fake { use core::meta::{Record, Code, Item}\n pub trait Derive<G> { const fn derive(_ r: Record) -> Code<Item> } }\nuse fake::Derive\nuse core::meta::{Record, Code, Item}\ntrait Score { const fn score() -> usize }\nstruct Fields {}\nimpl Derive<u256> for Fields { const fn derive(_ r: Record) -> Code<Item> { quote { const fn score() -> usize { 1 } } } }\npub struct Pair { a: u256 }\nimpl Score for Pair with (Fields)\n",
            "Derive<trait Score>",
        ),
    ] {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///tmp/native-derive-bad.fe").unwrap();
        let file = db
            .workspace()
            .touch(&mut db, url.clone(), Some(bad_source.into()));
        let errors = support::program_errors(&db, file);
        assert!(errors.contains(expected_cause), "{errors}");
    }
}

#[test]
fn explicit_derive_refuses_sibling_item_holes_without_dropping_output() {
    let source = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Value { const fn value() -> u256 }
struct Key {}
struct Provider {}
impl Derive<trait Value> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        let extra = quote { const fn ignored() -> bool { missing } }
        quote {
            const fn value() -> u256 { 7 }
            ${extra}
        }
    }
}
impl Value for Key with (Provider)
const RESULT: u256 = Key::value()
"#;
    for rejected in [
        source.to_owned(),
        source.replace("${extra}", "const fn ignored() -> bool { missing }"),
    ] {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///tmp/explicit-derive-item-shape.fe").unwrap(),
            Some(rejected.clone()),
        );
        // A sibling is lowered as a member of the header, never dropped, and
        // ordinary impl conformance refuses it like a handwritten extra method.
        let error = prepare_file(&db, file)
            .err()
            .expect("every sibling output node must be admitted or refused");
        assert!(
            error
                .cause
                .contains("method `ignored` is not defined in trait `Value`"),
            "{error}"
        );
        assert!(db.generation_inputs().functions(&db).is_empty());
        assert!(db.generation_inputs().requests(&db).is_empty());
        assert_eq!(file.text(&db).as_str(), rejected);
    }
    let source = source
        .replace(
            "        let extra = quote { const fn ignored() -> bool { missing } }\n",
            "        let answer: u256 = 7\n",
        )
        .replace("            ${extra}\n", "")
        .replace(
            "const fn value() -> u256 { 7 }",
            "const fn value() -> u256 { ${answer} }",
        );
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///tmp/explicit-derive-one-item.fe").unwrap();
    let file = db.workspace().touch(&mut db, url.clone(), Some(source));
    let output = prepare_file(&db, file).unwrap().unwrap();
    let output_file = output.workspace().get(&output, &url).unwrap();
    let diags = output.run_on_top_mod(output.top_mod(output_file));
    assert!(diags.is_empty(), "{}", diags.format_diags(&output));
}

#[test]
fn explicit_derive_accepts_root_item_splice_and_preserves_helper_hygiene() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}

pub trait Score {
    const fn score() -> usize
}

mod helpers {
    use core::meta::{Code, Item, Record, field_count}

    const fn private_offset() -> usize {
        100
    }

    pub const fn make_score_method(_ target: Record) -> Code<Item> {
        let count: usize = field_count(target)
        quote {
            const fn score() -> usize {
                private_offset() + ${count}
            }
        }
    }
}

const fn private_offset() -> usize {
    999
}

pub struct Provider {}

impl Derive<trait Score> for Provider {
    const fn derive(_ target: Record) -> Code<Item> {
        let method: Code<Item> = helpers::make_score_method(target)
        quote {
            ${method}
        }
    }
}

pub struct Pair {
    pub a: u256,
    pub b: u256,
}

impl Score for Pair with (Provider)

pub const CHECK: [u8; 1] = [0; Pair::score() - 101]
"#;
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///tmp/explicit-derive-root-splice.fe").unwrap();
    let file = db
        .workspace()
        .touch(&mut db, url.clone(), Some(source.into()));
    let output = prepare_file(&db, file)
        .expect("root item splice in derive must be admitted")
        .expect("generated output");
    let output_file = output.workspace().get(&output, &url).unwrap();
    let diags = output.run_on_top_mod(output.top_mod(output_file));
    assert!(diags.is_empty(), "{}", diags.format_diags(&output));

    let two_items_spliced = source.replace(
        "            const fn score() -> usize {\n                private_offset() + ${count}\n            }",
        "            const fn score() -> usize { 1 }\n            const fn extra() -> usize { 2 }",
    );
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///tmp/explicit-derive-root-splice-two.fe").unwrap(),
        Some(two_items_spliced),
    );
    let error = support::program_errors(&db, file);
    // Every member of an explicit header is lowered, so the second function is
    // refused by ordinary impl conformance like a handwritten extra method.
    assert!(
        error.contains("method `extra` is not defined in trait `Score`"),
        "{error}"
    );
}
