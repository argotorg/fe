//! Real audited causal twins through the native pre-provider boundary.
use common::InputDb;
use fe_driver::{
    DriverDataBase,
    generation::{GenerationError, prepare_file},
};
use url::Url;

const BOUND_IMPLEMENTATION_FALSE: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item, field_count, variant, variant_count}\ntrait Score { const fn score() -> usize }\ntrait Marker {}\nimpl Marker for u256 {}\nimpl Marker for u8 where false {}\ntrait Check { const OK: bool }\nimpl Check for u256 { const OK: bool = true }\nimpl Check for u8 { const OK: bool = false }\npub struct Target<T: Marker> { item: T }\nstruct Provider {}\nimpl Derive<Score> for Provider {\n const fn derive(_ r: Record) -> Code<Item> {\n  let zero: u256 = 0\n  let fault = 1 / zero\n  quote { const fn score() -> usize { 77 } }\n }\n}\nimpl Score for Target<u8> with (Provider)\n#[test]\nfn handwritten_seventy_seven() { assert(<Target<u8> as Score>::score() == 77) }\n";

const NESTED_GROUND_FALSE: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item, field_count, variant, variant_count}\ntrait Score { const fn score() -> usize }\ntrait Marker {}\nimpl Marker for u256 {}\ntrait Check { const OK: bool }\nimpl Check for u256 { const OK: bool = true }\nimpl Check for u8 { const OK: bool = false }\npub struct Poison where false {}\npub struct Target<T> { item: T }\nstruct Provider {}\nimpl Derive<Score> for Provider {\n const fn derive(_ r: Record) -> Code<Item> {\n  let zero: u256 = 0\n  let fault = 1 / zero\n  quote { const fn score() -> usize { 77 } }\n }\n}\nimpl Score for Target<Poison> with (Provider)\n#[test]\nfn handwritten_seventy_seven() { assert(<Target<Poison> as Score>::score() == 77) }\n";

const BOUND_IMPLEMENTATION_TRUE_FAULT: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item, field_count, variant, variant_count}\ntrait Score { const fn score() -> usize }\ntrait Marker {}\nimpl Marker for u256 {}\nimpl Marker for u8 where true {}\ntrait Check { const OK: bool }\nimpl Check for u256 { const OK: bool = true }\nimpl Check for u8 { const OK: bool = false }\npub struct Target<T: Marker> { item: T }\nstruct Provider {}\nimpl Derive<Score> for Provider {\n const fn derive(_ r: Record) -> Code<Item> {\n  let zero: u256 = 0\n  let fault = 1 / zero\n  quote { const fn score() -> usize { 77 } }\n }\n}\nimpl Score for Target<u8> with (Provider)\n#[test]\nfn handwritten_seventy_seven() { assert(<Target<u8> as Score>::score() == 77) }\n";

const NESTED_GROUND_TRUE_FAULT: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item, field_count, variant, variant_count}\ntrait Score { const fn score() -> usize }\ntrait Marker {}\nimpl Marker for u256 {}\ntrait Check { const OK: bool }\nimpl Check for u256 { const OK: bool = true }\nimpl Check for u8 { const OK: bool = false }\npub struct Poison where true {}\npub struct Target<T> { item: T }\nstruct Provider {}\nimpl Derive<Score> for Provider {\n const fn derive(_ r: Record) -> Code<Item> {\n  let zero: u256 = 0\n  let fault = 1 / zero\n  quote { const fn score() -> usize { 77 } }\n }\n}\nimpl Score for Target<Poison> with (Provider)\n#[test]\nfn handwritten_seventy_seven() { assert(<Target<Poison> as Score>::score() == 77) }\n";

const RECORD_BOUNDS_VALID: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item, field_count, variant, variant_count}\ntrait Score { const fn score() -> usize }\ntrait Marker {}\nimpl Marker for u256 {}\ntrait Check { const OK: bool }\nimpl Check for u256 { const OK: bool = true }\nimpl Check for u8 { const OK: bool = false }\npub struct Target<T: Marker> { item: T }\nstruct Provider {}\nimpl Derive<Score> for Provider {\n const fn derive(_ r: Record) -> Code<Item> {\n  let n = field_count(r)\n  quote { const fn score() -> usize { ${n} + 76 } }\n }\n}\nimpl Score for Target<u256> with (Provider)\n#[test]\nfn handwritten_seventy_seven() { assert(<Target<u256> as Score>::score() == 77) }\n";

const RECORD_CONDITION_VALID: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item, field_count, variant, variant_count}\ntrait Score { const fn score() -> usize }\ntrait Marker {}\nimpl Marker for u256 {}\ntrait Check { const OK: bool }\nimpl Check for u256 { const OK: bool = true }\nimpl Check for u8 { const OK: bool = false }\npub struct Target<T: Check> where <T as Check>::OK { item: T }\nstruct Provider {}\nimpl Derive<Score> for Provider {\n const fn derive(_ r: Record) -> Code<Item> {\n  let n = field_count(r)\n  quote { const fn score() -> usize { ${n} + 76 } }\n }\n}\nimpl Score for Target<u256> with (Provider)\n#[test]\nfn handwritten_seventy_seven() { assert(<Target<u256> as Score>::score() == 77) }\n";

const ENUM_BOUNDS_VALID: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item, field_count, variant, variant_count}\ntrait Score { const fn score() -> usize }\ntrait Marker {}\nimpl Marker for u256 {}\ntrait Check { const OK: bool }\nimpl Check for u256 { const OK: bool = true }\nimpl Check for u8 { const OK: bool = false }\npub enum Target<T: Marker> { Empty, One(T) }\nstruct Provider {}\nimpl Derive<Score> for Provider {\n const fn derive(_ r: Record) -> Code<Item> {\n  let n = field_count(variant(r, 1))\n  quote { const fn score() -> usize { ${n} + 76 } }\n }\n}\nimpl Score for Target<u256> with (Provider)\n#[test]\nfn handwritten_seventy_seven() { assert(<Target<u256> as Score>::score() == 77) }\n";

const ENUM_CONDITION_VALID: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item, field_count, variant, variant_count}\ntrait Score { const fn score() -> usize }\ntrait Marker {}\nimpl Marker for u256 {}\ntrait Check { const OK: bool }\nimpl Check for u256 { const OK: bool = true }\nimpl Check for u8 { const OK: bool = false }\npub enum Target<T: Check> where <T as Check>::OK { Empty, One(T) }\nstruct Provider {}\nimpl Derive<Score> for Provider {\n const fn derive(_ r: Record) -> Code<Item> {\n  let n = field_count(variant(r, 1))\n  quote { const fn score() -> usize { ${n} + 76 } }\n }\n}\nimpl Score for Target<u256> with (Provider)\n#[test]\nfn handwritten_seventy_seven() { assert(<Target<u256> as Score>::score() == 77) }\n";

const RECORDS_VALID: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item, field, field_count, field_type, type_is, variant, variant_count}\nuse core::permits::PermitAuthority\n#[canonical]\ntrait Score { const fn score() -> usize }\npub struct Box<T> { item: T, flag: bool }\npub enum Choice<T> { Empty, One(T), Named { item: T, flag: bool } }\nstruct Provider {}\nimpl Derive<Score> for Provider {\n const fn derive(_ r: Record) -> Code<Item> {\n  let a = field_type(field(r, 0))\n  let b = field_type(field(r, 1))\n  let n = if type_is<u256>(a) && !type_is<u8>(a) && type_is<bool>(b) { 256 } else { if type_is<u8>(a) && !type_is<u256>(a) && type_is<bool>(b) { 8 } else { 9999 } }\n  let answer = field_count(r) * 1000 + n\n  quote { const fn score() -> usize { ${answer} } }\n }\n}\n#[permit_root]\nconst ROOT0: PermitAuthority<Score<Box<u256>>> = PermitAuthority {}\n#[permit(ROOT0)]\nimpl Score for Box<u256> with (Provider)\nstruct Hand0 {}\n#[permit_root]\nconst HAND_ROOT0: PermitAuthority<Score<Hand0>> = PermitAuthority {}\n#[permit(HAND_ROOT0)]\nimpl Score for Hand0 { const fn score() -> usize { 2256 } }\n#[test]\nfn ground_0() {\n assert(<Box<u256> as Score>::score() == 2256)\n assert(<Box<u256> as Score>::score() == Hand0::score())\n}\n#[permit_root]\nconst ROOT1: PermitAuthority<Score<Box<u8>>> = PermitAuthority {}\n#[permit(ROOT1)]\nimpl Score for Box<u8> with (Provider)\nstruct Hand1 {}\n#[permit_root]\nconst HAND_ROOT1: PermitAuthority<Score<Hand1>> = PermitAuthority {}\n#[permit(HAND_ROOT1)]\nimpl Score for Hand1 { const fn score() -> usize { 2008 } }\n#[test]\nfn ground_1() {\n assert(<Box<u8> as Score>::score() == 2008)\n assert(<Box<u8> as Score>::score() == Hand1::score())\n}\n";

const ENUMS_VALID: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item, field, field_count, field_type, type_is, variant, variant_count}\nuse core::permits::PermitAuthority\n#[canonical]\ntrait Score { const fn score() -> usize }\npub struct Box<T> { item: T, flag: bool }\npub enum Choice<T> { Empty, One(T), Named { item: T, flag: bool } }\nstruct Provider {}\nimpl Derive<Score> for Provider {\n const fn derive(_ r: Record) -> Code<Item> {\n  let a = field_type(field(variant(r, 1), 0))\n  let b = field_type(field(variant(r, 2), 1))\n  let n = if type_is<u256>(a) && !type_is<u8>(a) && type_is<bool>(b) { 256 } else { if type_is<u8>(a) && !type_is<u256>(a) && type_is<bool>(b) { 8 } else { 9999 } }\n  let answer = variant_count(r) * 10000 + field_count(variant(r, 0)) * 1000 + field_count(variant(r, 1)) * 100 + field_count(variant(r, 2)) * 10 + n\n  quote { const fn score() -> usize { ${answer} } }\n }\n}\n#[permit_root]\nconst ROOT0: PermitAuthority<Score<Choice<u256>>> = PermitAuthority {}\n#[permit(ROOT0)]\nimpl Score for Choice<u256> with (Provider)\nstruct Hand0 {}\n#[permit_root]\nconst HAND_ROOT0: PermitAuthority<Score<Hand0>> = PermitAuthority {}\n#[permit(HAND_ROOT0)]\nimpl Score for Hand0 { const fn score() -> usize { 30376 } }\n#[test]\nfn ground_0() {\n assert(<Choice<u256> as Score>::score() == 30376)\n assert(<Choice<u256> as Score>::score() == Hand0::score())\n}\n#[permit_root]\nconst ROOT1: PermitAuthority<Score<Choice<u8>>> = PermitAuthority {}\n#[permit(ROOT1)]\nimpl Score for Choice<u8> with (Provider)\nstruct Hand1 {}\n#[permit_root]\nconst HAND_ROOT1: PermitAuthority<Score<Hand1>> = PermitAuthority {}\n#[permit(HAND_ROOT1)]\nimpl Score for Hand1 { const fn score() -> usize { 30128 } }\n#[test]\nfn ground_1() {\n assert(<Choice<u8> as Score>::score() == 30128)\n assert(<Choice<u8> as Score>::score() == Hand1::score())\n}\n";

const RECORD_BOUNDS_REFUSED: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item, field_count, variant, variant_count}\ntrait Score { const fn score() -> usize }\ntrait Marker {}\nimpl Marker for u256 {}\ntrait Check { const OK: bool }\nimpl Check for u256 { const OK: bool = true }\nimpl Check for u8 { const OK: bool = false }\npub struct Target<T: Marker> { item: T }\nstruct Provider {}\nimpl Derive<Score> for Provider {\n const fn derive(_ r: Record) -> Code<Item> {\n  let zero: u256 = 0\n  let fault = 1 / zero\n  quote { const fn score() -> usize { 77 } }\n }\n}\nimpl Score for Target<u8> with (Provider)\n#[test]\nfn handwritten_seventy_seven() { assert(<Target<u8> as Score>::score() == 77) }\n";

const RECORD_CONDITION_REFUSED: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item, field_count, variant, variant_count}\ntrait Score { const fn score() -> usize }\ntrait Marker {}\nimpl Marker for u256 {}\ntrait Check { const OK: bool }\nimpl Check for u256 { const OK: bool = true }\nimpl Check for u8 { const OK: bool = false }\npub struct Target<T: Check> where <T as Check>::OK { item: T }\nstruct Provider {}\nimpl Derive<Score> for Provider {\n const fn derive(_ r: Record) -> Code<Item> {\n  let zero: u256 = 0\n  let fault = 1 / zero\n  quote { const fn score() -> usize { 77 } }\n }\n}\nimpl Score for Target<u8> with (Provider)\n#[test]\nfn handwritten_seventy_seven() { assert(<Target<u8> as Score>::score() == 77) }\n";

const ENUM_BOUNDS_REFUSED: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item, field_count, variant, variant_count}\ntrait Score { const fn score() -> usize }\ntrait Marker {}\nimpl Marker for u256 {}\ntrait Check { const OK: bool }\nimpl Check for u256 { const OK: bool = true }\nimpl Check for u8 { const OK: bool = false }\npub enum Target<T: Marker> { Empty, One(T) }\nstruct Provider {}\nimpl Derive<Score> for Provider {\n const fn derive(_ r: Record) -> Code<Item> {\n  let zero: u256 = 0\n  let fault = 1 / zero\n  quote { const fn score() -> usize { 77 } }\n }\n}\nimpl Score for Target<u8> with (Provider)\n#[test]\nfn handwritten_seventy_seven() { assert(<Target<u8> as Score>::score() == 77) }\n";

const ENUM_CONDITION_REFUSED: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item, field_count, variant, variant_count}\ntrait Score { const fn score() -> usize }\ntrait Marker {}\nimpl Marker for u256 {}\ntrait Check { const OK: bool }\nimpl Check for u256 { const OK: bool = true }\nimpl Check for u8 { const OK: bool = false }\npub enum Target<T: Check> where <T as Check>::OK { Empty, One(T) }\nstruct Provider {}\nimpl Derive<Score> for Provider {\n const fn derive(_ r: Record) -> Code<Item> {\n  let zero: u256 = 0\n  let fault = 1 / zero\n  quote { const fn score() -> usize { 77 } }\n }\n}\nimpl Score for Target<u8> with (Provider)\n#[test]\nfn handwritten_seventy_seven() { assert(<Target<u8> as Score>::score() == 77) }\n";

fn prepare(source: &str) -> Result<(), GenerationError> {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///tmp/ground-type-admission.fe").unwrap(),
        Some(source.into()),
    );
    let result = prepare_file(&db, file).map(|_| ());
    if result.is_err() {
        assert!(db.generation_inputs().functions(&db).is_empty());
    }
    assert_eq!(file.text(&db).as_str(), source);
    result
}

#[test]
fn audited_false_targets_refuse_before_faulting_provider_with_original_cause() {
    for source in [BOUND_IMPLEMENTATION_FALSE, NESTED_GROUND_FALSE] {
        let error = prepare(source).expect_err("false target cannot execute provider");
        assert!(error.cause.contains("target admission failed"), "{error}");
        assert!(!error.cause.contains("division by zero"), "{error}");
        assert_eq!(
            &source[usize::from(error.range.start())..usize::from(error.range.end())],
            "false",
            "{error}"
        );
    }
}

#[test]
fn audited_true_twins_reach_the_intentional_provider_fault() {
    for (source, expected) in [
        (BOUND_IMPLEMENTATION_TRUE_FAULT, 511..519),
        (NESTED_GROUND_TRUE_FAULT, 502..510),
    ] {
        let error = prepare(source).expect_err("lawful provider must execute its fault");
        assert!(error.cause.contains("division by zero"), "{error}");
        assert!(!error.cause.contains("target admission failed"), "{error}");
        assert_eq!(
            usize::from(error.range.start())..usize::from(error.range.end()),
            expected
        );
    }
}

#[test]
fn lawful_concrete_and_parametric_applications_publish_normally() {
    for source in [
        RECORD_BOUNDS_VALID,
        RECORD_CONDITION_VALID,
        ENUM_BOUNDS_VALID,
        ENUM_CONDITION_VALID,
        RECORDS_VALID,
        ENUMS_VALID,
    ] {
        prepare(source).expect("lawful ground application must publish");
    }
}

#[test]
fn ordinary_missing_bound_and_parametric_failures_keep_their_cause() {
    for source in [
        RECORD_BOUNDS_REFUSED,
        RECORD_CONDITION_REFUSED,
        ENUM_BOUNDS_REFUSED,
        ENUM_CONDITION_REFUSED,
    ] {
        let error = prepare(source).expect_err("invalid application must refuse");
        assert!(error.cause.contains("target admission failed"), "{error}");
        assert!(!error.cause.contains("division by zero"), "{error}");
    }
}
