use common::InputDb;
use fe_driver::{DriverDataBase, generation::prepare_file};
use url::Url;

const PROVIDER_CONDITION_FALSE: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\ntrait Value { const fn value() -> u256 }\npub struct Key {}\nstruct Provider {}\nimpl Derive<trait Value> for Provider where false {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote { const fn value() -> u256 { 7 } }\n    }\n}\nimpl Value for Key with (Provider)\n#[test]\nfn independent_expected_seven_and_repeat_use() {\n    assert!(Key::value() == 7)\n    assert!(Key::value() + Key::value() == 14)\n}\n";

const PROVIDER_CONDITION_FALSE_FAULT: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\ntrait Value { const fn value() -> u256 }\npub struct Key {}\nstruct Provider {}\nimpl Derive<trait Value> for Provider where false {\n    const fn derive(_ record: Record) -> Code<Item> {\n        let zero: u256 = 0\n        let fault = 1 / zero\n        quote { const fn value() -> u256 { 7 } }\n    }\n}\nimpl Value for Key with (Provider)\n#[test]\nfn independent_expected_seven_and_repeat_use() {\n    assert!(Key::value() == 7)\n    assert!(Key::value() + Key::value() == 14)\n}\n";

fn prepare(source: &str) -> Result<(), String> {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///tmp/native-provider-admission.fe").unwrap(),
        Some(source.into()),
    );
    let result = prepare_file(&db, file).map(|_| ()).map_err(|e| e.cause);
    assert!(db.generation_inputs().functions(&db).is_empty());
    assert_eq!(file.text(&db).as_str(), source);
    result
}

#[test]
fn false_provider_condition_refuses_before_body_execution() {
    for source in [PROVIDER_CONDITION_FALSE, PROVIDER_CONDITION_FALSE_FAULT] {
        let error = prepare(source).expect_err("false declaration cannot enter CTFE");
        assert!(
            error.contains("provider application not admitted"),
            "{error}"
        );
        assert!(error.contains("const where predicate failed"), "{error}");
        assert!(!error.contains("division by zero"), "{error}");
    }
}

#[test]
fn true_provider_condition_does_not_hide_body_fault() {
    let source = PROVIDER_CONDITION_FALSE_FAULT.replace("where false", "where true");
    let error = prepare(&source).expect_err("lawful provider must execute its fault");
    assert!(error.contains("division by zero"), "{error}");
    assert!(!error.contains("not admitted"), "{error}");
}

#[test]
fn true_provider_condition_publishes_normally() {
    let source = PROVIDER_CONDITION_FALSE.replace("where false", "where true");
    prepare(&source).expect("ground true provider must be admitted");
}

#[test]
fn ill_formed_provider_nominal_refuses_before_provider_ctfe() {
    let bad_provider_nominal = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Value { const fn value() -> u256 }
pub struct Key {}

struct BadProvider where false {}
impl Derive<trait Value> for BadProvider {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: u256 = 0
        let fault = 1 / zero
        quote { const fn value() -> u256 { ${fault} } }
    }
}

impl Value for Key with (BadProvider)
"#;
    let err =
        prepare(bad_provider_nominal).expect_err("BadProvider where false must fail admission");
    assert!(
        err.contains("provider application not admitted") && !err.contains("division by zero"),
        "{err}"
    );
}

#[test]
fn derive_unsafe_block_hygiene_and_quoted_attrs_and_quote_fast_exit() {
    // `unsafe { quote { ... } }` inside `Derive::derive` preserves producer-block hygiene.
    let unsafe_derive = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item, field_count}

trait Score {
    const fn score() -> usize
}

mod author {
    use super::Score
    use core::derive::Derive
    use core::meta::{Record, Code, Item, field_count}

    pub struct Provider {}
    impl Derive<trait Score> for Provider {
        const fn derive(_ r: Record) -> Code<Item> {
            let n = field_count(r)
            unsafe {
                const fn local_boost(_ x: usize) -> usize {
                    x + 100
                }
                quote {
                    const fn score() -> usize {
                        local_boost(${n})
                    }
                }
            }
        }
    }
}

const fn local_boost(_ x: usize) -> usize {
    999
}

pub struct Pair {
    pub a: u256,
    pub b: u256,
}

impl Score for Pair with (author::Provider)
"#;
    prepare(unsafe_derive).expect("unsafe block inside Derive::derive must preserve hygiene");

    // Derived method carrying `#[event]` is diagnosed by `validate_func_attrs`.
    let bad_derive_attr = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score {
    const fn score() -> usize
}

struct Provider {}
impl Derive<trait Score> for Provider {
    const fn derive(_ r: Record) -> Code<Item> {
        quote {
            #[event]
            const fn score() -> usize {
                1
            }
        }
    }
}

pub struct Pair {
    pub a: u256,
}

impl Score for Pair with (Provider)
"#;
    let attr_err = prepare(bad_derive_attr).expect_err("derived method with #[event] must fail");
    assert!(
        attr_err.contains("`#[event]`") && attr_err.contains("structs"),
        "{attr_err}"
    );

    // `prepare_file` fast-exit ignores `impl Trait for Target with (Provider)` inside `quote { ... }`.
    let quoted_derive_only = r#"
use core::meta::{Code, Item}

const fn quoted_impl() -> Code<Item> {
    quote {
        impl Trait for Target with (Provider)
    }
}
"#;
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///tmp/quoted-derive-fast-exit.fe").unwrap(),
        Some(quoted_derive_only.into()),
    );
    assert!(
        prepare_file(&db, file)
            .expect("quoted derive without top-level generation must fast-exit")
            .is_none()
    );
}

#[test]
fn concrete_type_alias_to_generic_provider_is_admitted_and_evaluates() {
    let source = r#"
use core::derive::Derive
use core::meta::{Code, Item, Record, field_count}

pub trait Score {
    const fn score() -> usize
}

pub trait Gate {
    const BONUS: usize
}
impl Gate for u8 {
    const BONUS: usize = 40
}

pub struct GenericProvider<T> {}

impl<T: Gate> Derive<trait Score> for GenericProvider<T> {
    const fn derive(_ target: Record) -> Code<Item> {
        let count: usize = field_count(target)
        let bonus: usize = <T as Gate>::BONUS
        let total: usize = count + bonus
        quote {
            const fn score() -> usize {
                ${total}
            }
        }
    }
}

pub type AdmittedProvider = GenericProvider<u8>

pub struct Pair {
    pub a: u256,
    pub b: u256,
}

impl Score for Pair with (AdmittedProvider)

pub const CHECK: bool = Pair::score() == 42
"#;
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///tmp/generic-provider-alias.fe").unwrap();
    let file = db
        .workspace()
        .touch(&mut db, url.clone(), Some(source.into()));
    let output = prepare_file(&db, file)
        .expect("admitted concrete alias of generic provider must evaluate")
        .expect("generated output");
    let output_file = output.workspace().get(&output, &url).unwrap();
    let diags = output.run_on_top_mod(output.top_mod(output_file));
    assert!(diags.is_empty(), "{}", diags.format_diags(&output));

    let unsatisfied = source.replace(
        "pub type AdmittedProvider = GenericProvider<u8>",
        "pub type AdmittedProvider = GenericProvider<bool>",
    );
    let err =
        prepare(&unsatisfied).expect_err("unsatisfied generic provider alias must fail admission");
    assert!(
        err.contains("`GenericProvider<bool>` has no `impl Derive<trait Score>` that applies"),
        "{err}"
    );
}
