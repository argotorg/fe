use common::InputDb;
use fe_driver::{DriverDataBase, generation::prepare_file};
use url::Url;

const SOURCE: &str = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> usize }
pub struct Pair {}
struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn score() -> usize { 42 } }
    }
}
struct Sized<const N: usize = { with (Provider) { Pair::score() } }> {}
"#;

fn prepare(source: &str) -> Result<DriverDataBase, String> {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///tmp/scoped-default-owner.fe").unwrap(),
        Some(source.into()),
    );
    prepare_file(&db, file)
        .map_err(|error| error.cause)?
        .ok_or_else(|| "expected a generated implementation for the default".into())
}

#[test]
fn scoped_default_uses_the_checked_anonymous_owner() {
    let output = prepare(SOURCE).expect("an authored default must retain its checked opening");
    let file = output
        .workspace()
        .get(
            &output,
            &Url::parse("file:///tmp/scoped-default-owner.fe").unwrap(),
        )
        .unwrap();
    let module = output.top_mod(file);
    assert_eq!(
        module
            .all_impl_traits(&output)
            .iter()
            .filter(|implementation| implementation.selection_name(&output).is_some())
            .count(),
        1,
    );
    assert!(!output.run_on_top_mod(module).has_errors(&output));
}

#[test]
fn scoped_default_keeps_its_declared_expected_type() {
    let cause = match prepare(&SOURCE.replace("const N: usize", "const N: bool")) {
        Ok(_) => panic!("a usize-valued default cannot establish a bool parameter"),
        Err(cause) => cause,
    };
    assert!(
        !cause.contains("no unique checked authored provider opening"),
        "{cause}"
    );
    assert!(cause.contains("bool") && cause.contains("usize"), "{cause}");
}
