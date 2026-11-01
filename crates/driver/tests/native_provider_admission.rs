use common::InputDb;
use fe_driver::{DriverDataBase, generation::prepare_file};
use url::Url;

const PROVIDER_CONDITION_FALSE: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\nuse core::permits::PermitAuthority\n#[canonical]\ntrait Value { const fn value() -> u256 }\npub struct Key {}\n#[permit_root]\nconst ROOT: PermitAuthority<Value<Key>> = PermitAuthority {}\nstruct Provider {}\nimpl Derive<Value> for Provider where false {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote { const fn value() -> u256 { 7 } }\n    }\n}\n#[permit(ROOT)]\nimpl Value for Key with (Provider)\n#[test]\nfn independent_expected_seven_and_repeat_use() {\n    assert!(Key::value() == 7)\n    assert!(Key::value() + Key::value() == 14)\n}\n";

const PROVIDER_CONDITION_FALSE_FAULT: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\nuse core::permits::PermitAuthority\n#[canonical]\ntrait Value { const fn value() -> u256 }\npub struct Key {}\n#[permit_root]\nconst ROOT: PermitAuthority<Value<Key>> = PermitAuthority {}\nstruct Provider {}\nimpl Derive<Value> for Provider where false {\n    const fn derive(_ record: Record) -> Code<Item> {\n        let zero: u256 = 0\n        let fault = 1 / zero\n        quote { const fn value() -> u256 { 7 } }\n    }\n}\n#[permit(ROOT)]\nimpl Value for Key with (Provider)\n#[test]\nfn independent_expected_seven_and_repeat_use() {\n    assert!(Key::value() == 7)\n    assert!(Key::value() + Key::value() == 14)\n}\n";

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
fn ill_formed_provider_nominal_and_invalid_permit_root_refuse_before_provider_ctfe() {
    // Finding 2b: Ill-formed provider struct `where false` refuses before CTFE.
    let bad_provider_nominal = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
use core::permits::PermitAuthority

#[canonical]
trait Value { const fn value() -> u256 }
pub struct Key {}

#[permit_root]
const ROOT: PermitAuthority<Value<Key>> = PermitAuthority {}

struct BadProvider where false {}
impl Derive<Value> for BadProvider {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: u256 = 0
        let fault = 1 / zero
        quote { const fn value() -> u256 { ${fault} } }
    }
}

#[permit(ROOT)]
impl Value for Key with (BadProvider)
"#;
    let err =
        prepare(bad_provider_nominal).expect_err("BadProvider where false must fail admission");
    assert!(
        err.contains("native derive target admission failed") && !err.contains("division by zero"),
        "{err}"
    );

    // Finding 2c: Ill-typed `#[permit_root] const ROOT` refuses in preflight before provider CTFE.
    let bad_permit_root = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
use core::permits::PermitAuthority

#[canonical]
trait Value { const fn value() -> u256 }
pub struct Key {}

#[permit_root]
const ROOT: PermitAuthority<Value<Key>> = false

struct FaultingProvider {}
impl Derive<Value> for FaultingProvider {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: u256 = 0
        let fault = 1 / zero
        quote { const fn value() -> u256 { ${fault} } }
    }
}

#[permit(ROOT)]
impl Value for Key with (FaultingProvider)
"#;
    let root_err =
        prepare(bad_permit_root).expect_err("ill-typed #[permit_root] must fail preflight");
    assert!(
        root_err.contains("canonical root establishment failed")
            && root_err.contains("root authority constant must be a valid compile-time value")
            && !root_err.contains("division by zero"),
        "{root_err}"
    );
}

#[test]
fn derive_unsafe_block_hygiene_and_quoted_attrs_and_quote_fast_exit() {
    // Finding 5: `unsafe { quote { ... } }` inside `Derive::derive` preserves producer-block hygiene.
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
    impl Derive<Score> for Provider {
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

    // Finding 6a: Derived method carrying `#[event]` is diagnosed by `validate_func_attrs`.
    let bad_derive_attr = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score {
    const fn score() -> usize
}

struct Provider {}
impl Derive<Score> for Provider {
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

    // Finding 6b: `prepare_file` fast-exit ignores `impl Trait for Target with (Provider)` inside `quote { ... }`.
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
