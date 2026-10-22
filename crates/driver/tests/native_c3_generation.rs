use common::InputDb;
use fe_driver::{DriverDataBase, generation::prepare_file};
use hir::analysis::{
    semantic::{EvalOutcome, GenericSubst, SemConstScalar, SemConstValue, eval_body_owner_const},
    ty::ty_check::BodyOwner,
};
use url::Url;

const LAWFUL: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\nuse core::permits::PermitAuthority\n#[canonical]\ntrait Value { const fn value() -> u256 }\npub struct Key {}\n#[permit_root]\nconst ROOT: PermitAuthority<Value<Key>> = PermitAuthority {}\nstruct Provider {}\nimpl Derive<Value> for Provider {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote { const fn value() -> u256 { 7 } }\n    }\n}\n#[permit(ROOT)]\nimpl Value for Key with (Provider)\n#[test]\nfn independent_expected_seven_and_repeat_use() {\n    assert!(Key::value() == 7)\n    assert!(Key::value() + Key::value() == 14)\n}\n";

const HANDWRITTEN: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\nuse core::permits::PermitAuthority\n#[canonical]\ntrait Value { const fn value() -> u256 }\npub struct Key {}\n#[permit_root]\nconst ROOT: PermitAuthority<Value<Key>> = PermitAuthority {}\nstruct Provider {}\nimpl Derive<Value> for Provider {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote { const fn value() -> u256 { 7 } }\n    }\n}\n#[permit(ROOT)]\nimpl Value for Key { const fn value() -> u256 { 7 } }\n#[test]\nfn independent_expected_seven_and_repeat_use() {\n    assert!(Key::value() == 7)\n    assert!(Key::value() + Key::value() == 14)\n}\n";

const PRODUCER_SCOPE: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\nuse core::permits::PermitAuthority\n#[canonical]\ntrait Value { const fn value() -> u256 }\npub struct Key {}\n#[permit_root]\nconst ROOT: PermitAuthority<Value<Key>> = PermitAuthority {}\nmod producer {\n    use super::Value\n    use core::derive::Derive\n    use core::meta::{Record, Code, Item}\n    const ROOT: u256 = 99\n    const fn helper() -> u256 { 7 }\n    pub struct Provider {}\n    impl Derive<Value> for Provider {\n        const fn derive(_ record: Record) -> Code<Item> {\n            quote { const fn value() -> u256 { helper() } }\n        }\n    }\n}\n#[permit(ROOT)]\nimpl Value for Key with (producer::Provider)\n#[test]\nfn independent_expected_seven_and_repeat_use() {\n    assert!(Key::value() == 7)\n    assert!(Key::value() + Key::value() == 14)\n}\n";

const MISSING: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\nuse core::permits::PermitAuthority\n#[canonical]\ntrait Value { const fn value() -> u256 }\npub struct Key {}\n#[permit_root]\nconst ROOT: PermitAuthority<Value<Key>> = PermitAuthority {}\nstruct Provider {}\nimpl Derive<Value> for Provider {\n    const fn derive(_ record: Record) -> Code<Item> {\n        let zero: u256 = 0\n        let fault = 1 / zero\n        quote { const fn value() -> u256 { ${fault} } }\n    }\n}\nimpl Value for Key with (Provider)\n#[test]\nfn independent_expected_seven_and_repeat_use() {\n    assert!(Key::value() == 7)\n    assert!(Key::value() + Key::value() == 14)\n}\n";

const COUNTERFEIT: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\nstruct PermitAuthority<G> {}\n#[canonical]\ntrait Value { const fn value() -> u256 }\npub struct Key {}\n#[permit_root]\nconst ROOT: PermitAuthority<Value<Key>> = PermitAuthority {}\nstruct Provider {}\nimpl Derive<Value> for Provider {\n    const fn derive(_ record: Record) -> Code<Item> {\n        let zero: u256 = 0\n        let fault = 1 / zero\n        quote { const fn value() -> u256 { ${fault} } }\n    }\n}\n#[permit(ROOT)]\nimpl Value for Key with (Provider)\n";

const WRONG_GOAL: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\nuse core::permits::PermitAuthority\n#[canonical]\ntrait Value { const fn value() -> u256 }\npub struct Key {}\nstruct Other {}\n#[permit_root]\nconst ROOT: PermitAuthority<Value<Other>> = PermitAuthority {}\nstruct Provider {}\nimpl Derive<Value> for Provider {\n    const fn derive(_ record: Record) -> Code<Item> {\n        let zero: u256 = 0\n        let fault = 1 / zero\n        quote { const fn value() -> u256 { ${fault} } }\n    }\n}\n#[permit(ROOT)]\nimpl Value for Key with (Provider)\n";

const NESTED_ROOT: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\nuse core::permits::PermitAuthority\n#[canonical]\ntrait Value { const fn value() -> u256 }\npub struct Key {}\nmod nested {\n    use super::{Value, Key}\n    use core::permits::PermitAuthority\n    #[permit_root]\n    pub const ROOT: PermitAuthority<Value<Key>> = PermitAuthority {}\n}\nstruct Provider {}\nimpl Derive<Value> for Provider {\n    const fn derive(_ record: Record) -> Code<Item> {\n        let zero: u256 = 0\n        let fault = 1 / zero\n        quote { const fn value() -> u256 { ${fault} } }\n    }\n}\n#[permit(nested::ROOT)]\nimpl Value for Key with (Provider)\n";

const NAMED_CANONICAL: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\nuse core::permits::PermitAuthority\n#[canonical]\ntrait Value { const fn value() -> u256 }\npub struct Key {}\n#[permit_root]\nconst ROOT: PermitAuthority<Value<Key>> = PermitAuthority {}\nstruct Provider {}\nimpl Derive<Value> for Provider {\n    const fn derive(_ record: Record) -> Code<Item> {\n        let zero: u256 = 0\n        let fault = 1 / zero\n        quote { const fn value() -> u256 { ${fault} } }\n    }\n}\n#[permit(ROOT)]\nimpl Value for Key as Alternative with (Provider)\n";

const FORGED_PERMIT_LITERAL: &str = "use core::permits::PermitAuthority\n#[canonical]\ntrait Value { const fn value() -> u256 }\npub struct Key {}\nconst FORGED: PermitAuthority<Value<Key>> = PermitAuthority {}\n";

const DUPLICATE_CANONICAL: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\nuse core::permits::PermitAuthority\n#[canonical]\ntrait Value { const fn value() -> u256 }\npub struct Key {}\n#[permit_root]\nconst ROOT: PermitAuthority<Value<Key>> = PermitAuthority {}\nstruct Provider {}\nimpl Derive<Value> for Provider {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote { const fn value() -> u256 { 7 } }\n    }\n}\n#[permit(ROOT)]\nimpl Value for Key with (Provider)\n#[permit(ROOT)]\nimpl Value for Key { const fn value() -> u256 { 7 } }\n";

const WRONG_OUTPUT_CANONICAL: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\nuse core::permits::PermitAuthority\n#[canonical]\ntrait Value { const fn value() -> u256 }\npub struct Key {}\n#[permit_root]\nconst ROOT: PermitAuthority<Value<Key>> = PermitAuthority {}\nstruct Provider {}\nimpl Derive<Value> for Provider {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote { const fn value() -> bool { true } }\n    }\n}\n#[permit(ROOT)]\nimpl Value for Key with (Provider)\n";

#[test]
fn refused_authority_keeps_the_authored_database_and_cannot_evaluate_fault() {
    let lawful = LAWFUL;
    let missing = MISSING;
    // A prior valid request must not publish even when a later request fails.
    let source = format!(
        "{}\n{}",
        lawful.replace("#[test]\nfn independent_expected_seven_and_repeat_use()", "fn first_use()"),
        missing.replace("Key", "OtherKey").replace("Provider", "FaultProvider").replace(
            "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\nuse core::permits::PermitAuthority\n#[canonical]\ntrait Value { const fn value() -> u256 }\n", ""
        ).replace("const ROOT:", "const OTHER_ROOT:")
            .replace("independent_expected_seven_and_repeat_use", "second_use")
    );
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///tmp/native-c3-atomic-refusal.fe").unwrap(),
        Some(source.clone()),
    );
    let err = prepare_file(&db, file)
        .err()
        .expect("missing authority must refuse");
    assert!(
        err.cause.contains("exactly one root authority"),
        "{}",
        err.cause
    );
    assert!(!err.cause.contains("division by zero"));
    assert_eq!(file.text(&db), &source);
    assert!(db.generation_inputs().functions(&db).is_empty());
    let request_start = source.find("impl Value for OtherKey with").unwrap();
    assert!(u32::from(err.range.start()) as usize <= request_start);
    assert!(u32::from(err.range.end()) as usize > request_start);
}

#[test]
fn canonical_authority_positive_and_negative_matrix() {
    for source in [LAWFUL, HANDWRITTEN, PRODUCER_SCOPE] {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///tmp/native-c3-positive.fe").unwrap();
        let file = db
            .workspace()
            .touch(&mut db, url.clone(), Some(source.into()));
        let prepared = prepare_file(&db, file).expect("lawful canonical setup");
        let check_db = prepared.as_ref().unwrap_or(&db);
        let check_file = check_db.workspace().get(check_db, &url).unwrap();
        let diags = check_db.run_on_top_mod(check_db.top_mod(check_file));
        assert!(diags.is_empty(), "{}", diags.format_diags(check_db));
    }

    for (source, cause) in [
        (MISSING, "exactly one root authority"),
        (
            COUNTERFEIT,
            "compiler-designated core::permits::PermitAuthority",
        ),
        (WRONG_GOAL, "different resolved trait goal"),
        (NESTED_ROOT, "foreign or nested root"),
        (NAMED_CANONICAL, "only its ordinary home provision"),
    ] {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///tmp/native-c3-refusal.fe").unwrap(),
            Some(source.into()),
        );
        let err = prepare_file(&db, file)
            .err()
            .expect("canonical violation must refuse");
        assert!(
            err.cause.contains("canonical root establishment failed") && err.cause.contains(cause),
            "{err}"
        );
        assert!(!err.cause.contains("division by zero"), "{err}");
    }

    for (source, expected_diag) in [
        (
            FORGED_PERMIT_LITERAL,
            "PermitAuthority construction is restricted to a designated program-root constant",
        ),
        (DUPLICATE_CANONICAL, "5-0001"),
        (WRONG_OUTPUT_CANONICAL, "6-0008"),
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
impl Derive<Score> for Fields {
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
impl Derive<Score> for ZeroFields {
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
            "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\ntrait Other {}\ntrait Score { const fn score() -> usize }\nstruct Fields {}\nimpl Derive<Other> for Fields { const fn derive(_ r: Record) -> Code<Item> { quote { const fn score() -> usize { 1 } } } }\npub struct Pair { a: u256 }\nimpl Score for Pair with (Fields)\n",
            "no ordinary Derive<requested goal>",
        ),
        (
            "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\ntrait Score { const fn score() -> usize }\npub struct Pair { a: u256 }\nimpl Score for Pair with (missing_entry)\n",
            "cannot resolve native derive provider type",
        ),
        (
            "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\ntrait Score { const fn score() -> usize }\nstruct Fields {}\nimpl Derive<Score> for Fields { const fn derive(_ r: Record) -> Code<Item> { quote { const fn unrelated() -> usize { 1 } } } }\npub struct Pair { a: u256 }\nimpl Score for Pair with (Fields)\n",
            "method not defined in trait",
        ),
        (
            "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\ntrait Score { const fn score() -> usize }\nstruct Fields {}\nimpl Derive<Score> for Fields { const fn derive(_ r: Record) -> Code<Item> { let n = Pair::score()\n quote { const fn score() -> usize { ${n} } } } }\npub struct Pair { a: u256 }\nimpl Score for Pair with (Fields)\n",
            "authored provider body diagnostics",
        ),
        (
            "mod fake { use core::meta::{Record, Code, Item}\n pub trait Derive<G> { const fn derive(_ r: Record) -> Code<Item> } }\nuse fake::Derive\nuse core::meta::{Record, Code, Item}\ntrait Score { const fn score() -> usize }\nstruct Fields {}\nimpl Derive<Score> for Fields { const fn derive(_ r: Record) -> Code<Item> { quote { const fn score() -> usize { 1 } } } }\npub struct Pair { a: u256 }\nimpl Score for Pair with (Fields)\n",
            "no ordinary Derive<requested goal>",
        ),
    ] {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///tmp/native-derive-bad.fe").unwrap();
        let file = db
            .workspace()
            .touch(&mut db, url.clone(), Some(bad_source.into()));
        let err = prepare_file(&db, file).err().expect("expected refusal");
        assert!(err.cause.contains(expected_cause), "{err}");
    }
}
