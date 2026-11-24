//! Inside the package that generates them, generated items only fill gaps:
//! authored code keeps the meaning of every name and member it uses, and a
//! provider that read that meaning agrees with the prepared program. A
//! generated member of an inherent header that an authored member would hide
//! is refused at its request. Each refused program has an accepted control
//! that differs only in the part the rule is about.
use common::InputDb;
use fe_driver::{DriverDataBase, generation::prepare_file};
use hir::analysis::{
    semantic::{EvalOutcome, GenericSubst, SemConstScalar, SemConstValue, eval_body_owner_const},
    ty::ty_check::BodyOwner,
};
use url::Url;

const URL: &str = "file:///tmp/conservative-extension.fe";
/// Every refusal of this file says that the generated item would be hidden by
/// something the package already binds.
const RULE: &str = "would be hidden";

fn prepare(source: &str) -> Result<DriverDataBase, String> {
    let mut db = DriverDataBase::default();
    let file = db
        .workspace()
        .touch(&mut db, Url::parse(URL).unwrap(), Some(source.into()));
    prepare_file(&db, file)
        .map_err(|error| error.cause)?
        .ok_or_else(|| "expected generation output".to_string())
}

fn refused(source: &str) -> String {
    match prepare(source) {
        Ok(_) => panic!("expected the program to be refused"),
        Err(cause) => {
            assert!(cause.contains(RULE), "{cause}");
            cause
        }
    }
}

fn value_of(db: &DriverDataBase, name: &str) -> String {
    let file = db
        .workspace()
        .get(db, &Url::parse(URL).unwrap())
        .expect("prepared file");
    let function = db
        .top_mod(file)
        .all_funcs(db)
        .iter()
        .copied()
        .find(|f| f.name(db).to_opt().is_some_and(|n| n.data(db) == name))
        .unwrap_or_else(|| panic!("missing function `{name}`"));
    let owner = BodyOwner::Func(function, None);
    let EvalOutcome::Ready(value) = eval_body_owner_const(
        db,
        owner,
        GenericSubst::for_body_owner(db, owner, Vec::new()),
    ) else {
        panic!("`{name}` did not evaluate");
    };
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = value.value(db)
    else {
        panic!("`{name}` is not an integer");
    };
    value.to_string()
}

const HEAD: &str =
    "use core::derive::{Derive, DeriveInherent}\nuse core::meta::{Record, Code, Item}\n";

/// E5b: a generated inherent method, a member of an inherent header, would
/// outrank an authored trait method of the same name, so it is hidden by it
/// and refused.
#[test]
fn generated_inherent_method_cannot_outrank_an_authored_trait_method() {
    let source = format!(
        "{HEAD}trait Seed {{ const fn seed() -> u256 }}\nstruct Token {{}}\n\
         impl Seed for Token {{ const fn seed() -> u256 {{ 1 }} }}\n\
         struct Make {{}}\nimpl DeriveInherent for Make {{\n    const fn derive(_ record: Record) -> Code<Item> {{\n        let seen = Token::seed()\n        \
         quote {{ const fn seed() -> u256 {{ 2 }}\n const fn seen_by_provider() -> u256 {{ ${{seen}} }} }}\n    }}\n}}\n\
         struct Token2 {{}}\nimpl Token2 with (Make)\n"
    );
    // The generated inherent members belong to `Token2`, a different type, so
    // nothing changes for `Token::seed()`.
    let output = prepare(&format!(
        "{source}const fn read() -> u256 {{ Token::seed() }}\n"
    ))
    .expect("an inherent method of another type changes nothing");
    assert_eq!(value_of(&output, "read"), "1");
    // Aimed at `Token`, the generated method outranks the trait method.
    let aimed = format!(
        "{HEAD}trait Seed {{ const fn seed() -> u256 }}\nstruct Token {{}}\n\
         impl Seed for Token {{ const fn seed() -> u256 {{ 1 }} }}\n\
         struct Make {{}}\nimpl DeriveInherent for Make {{ const fn derive(_ record: Record) -> Code<Item> {{ quote {{ const fn seed() -> u256 {{ 2 }} }} }} }}\n\
         impl Token with (Make)\nconst fn read() -> u256 {{ Token::seed() }}\n"
    );
    let cause = refused(&aimed);
    assert!(cause.contains("the generated `seed`"), "{cause}");
    assert!(
        cause.contains("`Token` already has the method `seed` of the trait `Seed`"),
        "{cause}"
    );
}

/// A fresh generated name, a member of an inherent header, is lawful and
/// authored code may use it.
#[test]
fn a_fresh_generated_name_is_lawful() {
    let output = prepare(&format!(
        "{HEAD}struct Make {{}}\nimpl DeriveInherent for Make {{ const fn derive(_ record: Record) -> Code<Item> {{ quote {{ const fn fresh() -> u256 {{ 3 }} }} }} }}\n\
         struct S {{}}\nimpl S with (Make)\nconst fn read() -> u256 {{ S::fresh() }}\n"
    ))
    .expect("a new name changes no authored meaning");
    assert_eq!(value_of(&output, "read"), "3");
}

/// The cause of a refusal that is not the hiding rule of `refused`.
fn refused_for(source: &str, rule: &str) -> String {
    match prepare(source) {
        Ok(_) => panic!("expected the program to be refused"),
        Err(cause) => {
            assert!(cause.contains(rule), "{cause}");
            cause
        }
    }
}

/// A quotation that lowers the block of a generated constant lowers generated
/// code, including the items in the block. A deciding implementation there is
/// refused as it is in a generated function.
#[test]
fn a_deciding_implementation_inside_a_generated_constant_is_refused() {
    let cause = refused_for(
        &format!(
            "{HEAD}use core::marker::Copy\nstruct MyType {{ a: u256 }}\n\
             struct Smuggle {{}}\n\
             impl DeriveInherent for Smuggle {{ const fn derive(_ record: Record) -> Code<Item> {{ quote {{ pub const SMUGGLE: u256 = {{ impl Copy for MyType {{}}\n 0 }} }} }} }}\n\
             impl MyType with (Smuggle)\n"
        ),
        "is refused",
    );
    assert!(
        cause.contains("the generated implementation of `Copy` for `MyType`"),
        "{cause}"
    );
}

/// An inherent method in the block of a generated constant is generated code
/// too: it would hide the authored trait method of the same name.
#[test]
fn an_inherent_method_inside_a_generated_constant_does_not_hide_a_written_one() {
    let cause = refused(&format!(
        "{HEAD}mod a {{ pub trait Tr1 {{ fn m(self) -> u256 }} }}\n\
         struct X {{ a: u256 }}\nimpl a::Tr1 for X {{ fn m(self) -> u256 {{ 1 }} }}\n\
         struct Gen {{}}\n\
         impl DeriveInherent for Gen {{ const fn derive(_ record: Record) -> Code<Item> {{ quote {{ pub const CARRIER: u256 = {{ impl X {{ pub fn m(self) -> u256 {{ 2 }} }}\n 0 }} }} }} }}\n\
         struct G {{ a: u256 }}\nimpl G with (Gen)\n"
    ));
    assert!(cause.contains("would be hidden"), "{cause}");
}

/// A generated constant with no items in its block is lawful.
#[test]
fn a_generated_constant_without_items_is_lawful() {
    let output = prepare(&format!(
        "{HEAD}struct Make {{}}\n\
         impl DeriveInherent for Make {{ const fn derive(_ record: Record) -> Code<Item> {{ quote {{ pub const K: u256 = 7 }} }} }}\n\
         struct G {{}}\nimpl G with (Make)\npub const fn run() -> u256 {{ G::K }}\n"
    ))
    .expect("a constant with no items changes no authored meaning");
    assert_eq!(value_of(&output, "run"), "7");
}
