//! Generation staging. Every provider, whichever request channel names it,
//! runs against the input of its package's preparation: authored text, and
//! nothing that generation produced. These tests state what follows from that
//! one view: outcomes do not depend on declaration order or on the database's
//! history, and a program that needs one provider to read another's output is
//! refused with a message that names the rule.
use common::InputDb;
use fe_driver::{DriverDataBase, generation::prepare_file};
use hir::analysis::{
    semantic::{EvalOutcome, GenericSubst, SemConstScalar, SemConstValue, eval_body_owner_const},
    ty::ty_check::BodyOwner,
};
use url::Url;

const URL: &str = "file:///tmp/staging-discriminators.fe";

/// The observable outcome of preparing a program: the value of `run`, or the
/// refusal's cause.
fn outcome(db: &mut DriverDataBase, source: &str) -> Result<String, String> {
    let url = Url::parse(URL).unwrap();
    db.workspace().update(db, url.clone(), source.into());
    let db = &*db;
    let file = db.workspace().get(db, &url).unwrap();
    let output = prepare_file(db, file)
        .map_err(|error| error.cause)?
        .ok_or("nothing was generated")?;
    let file = output.workspace().get(&output, &url).unwrap();
    let function = output
        .top_mod(file)
        .all_funcs(&output)
        .iter()
        .copied()
        .find(|function| {
            function
                .name(&output)
                .to_opt()
                .is_some_and(|ident| ident.data(&output) == "run")
        })
        .ok_or("missing `run`")?;
    let owner = BodyOwner::Func(function, None);
    match eval_body_owner_const(
        &output,
        owner,
        GenericSubst::for_body_owner(&output, owner, vec![]),
    ) {
        EvalOutcome::Ready(value) => match value.value(&output) {
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } => Ok(value.to_string()),
            other => Err(format!("`run` returned {other:?}")),
        },
        other => Err(format!("`run` did not evaluate: {other:?}")),
    }
}

fn fresh(source: &str) -> Result<String, String> {
    outcome(&mut DriverDataBase::default(), source)
}

const PRELUDE: &str = "use core::derive::Derive\nuse core::meta::{Record, Code, Item}\ntrait Score { const fn score() -> u256 }\ntrait Seed { const fn seed() -> u256 }\nstruct Pair {}\nstruct Token {}\n";

/// Both providers read the same authored helper, which is the way two
/// providers share a value under the one-view rule.
const A_USES_HELPER: &str = "struct A {}\nimpl Derive<trait Score> for A {\n    const fn derive(_ record: Record) -> Code<Item> {\n        let n = helper()\n        quote { const fn score() -> u256 { ${n} } }\n    }\n}\nconst fn helper() -> u256 { 42 }\n";
const B_USES_HELPER: &str = "struct B {}\nimpl Derive<trait Seed> for B {\n    const fn derive(_ record: Record) -> Code<Item> {\n        let n = helper()\n        quote { const fn seed() -> u256 { ${n} } }\n    }\n}\n";
const RUN_BOTH: &str =
    "const fn run() -> u256 { with (A) { Pair::score() } + with (B) { Token::seed() } }\n";

/// The text a refusal of a provider that reads generated output must carry.
const RULE: &str = "what generation produces in its own package is not visible to it";

/// S-T3: declaration order is not an input. Every permutation of the program
/// in which two providers share a helper has the same value.
#[test]
fn shared_helper_outcome_is_independent_of_declaration_order() {
    let parts = [A_USES_HELPER, B_USES_HELPER, RUN_BOTH];
    let orders = [
        [0, 1, 2],
        [0, 2, 1],
        [1, 0, 2],
        [1, 2, 0],
        [2, 0, 1],
        [2, 1, 0],
    ];
    for order in orders {
        let source = std::iter::once(PRELUDE)
            .chain(order.iter().map(|index| parts[*index]))
            .collect::<String>();
        assert_eq!(fresh(&source), Ok("84".to_string()), "{order:?}");
    }
}

/// S-T8: an authored database reused across edits prepares exactly what a
/// fresh database prepares, for each revision, including the restore.
#[test]
fn shared_helper_edits_agree_with_fresh_preparation() {
    let original = [PRELUDE, A_USES_HELPER, B_USES_HELPER, RUN_BOTH].concat();
    let edited = original.replace("{ 42 }", "{ 43 }");
    assert_eq!(fresh(&original), Ok("84".to_string()));
    assert_eq!(fresh(&edited), Ok("86".to_string()));
    let mut warm = DriverDataBase::default();
    for source in [&original, &edited, &original] {
        assert_eq!(outcome(&mut warm, source), fresh(source));
    }
}

/// A provider that opens another provider's implementation reads generated
/// output. It is refused in every declaration order, and the refusal names
/// the rule; the provider body is never run.
#[test]
fn provider_chain_is_refused_in_every_declaration_order() {
    const A_USES_B: &str = "struct A {}\nimpl Derive<trait Score> for A {\n    const fn derive(_ record: Record) -> Code<Item> {\n        let n = helper()\n        let zero: u256 = 0\n        let fault = 1 / zero\n        quote { const fn score() -> u256 { ${n} + ${fault} } }\n    }\n}\nconst fn helper() -> u256 { with (B) { Token::seed() } }\n";
    const B: &str = "struct B {}\nimpl Derive<trait Seed> for B {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote { const fn seed() -> u256 { 42 } }\n    }\n}\n";
    const RUN: &str = "const fn run() -> u256 { with (A) { Pair::score() } }\n";
    let parts = [A_USES_B, B, RUN];
    for order in [
        [0, 1, 2],
        [0, 2, 1],
        [1, 0, 2],
        [1, 2, 0],
        [2, 0, 1],
        [2, 1, 0],
    ] {
        let source = std::iter::once(PRELUDE)
            .chain(order.iter().map(|index| parts[*index]))
            .collect::<String>();
        let cause = fresh(&source).expect_err("a provider chain must be refused");
        assert!(cause.contains(RULE), "{order:?}: {cause}");
        assert!(!cause.contains("division by zero"), "{order:?}: {cause}");
    }
}

/// S-T4: the request channels see the same thing. A scoped provider reading
/// the members of an inherent header, the provider of an inherent header
/// reading output of a scoped opening, and the provider of an inherent header
/// reading the members of another inherent header are all refused, and every
/// refusal names the rule.
#[test]
fn generation_channels_see_each_other_symmetrically() {
    let scoped_reads_inherent = "use core::derive::{Derive, DeriveInherent}\nuse core::meta::{Record, Code, Item}\ntrait Score { const fn score() -> u256 }\nstruct SeedP {}\nimpl DeriveInherent for SeedP {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote { pub const fn seed() -> u256 { 42 } }\n    }\n}\nstruct Token {}\nimpl Token with (SeedP)\nstruct Pair {}\nstruct A {}\nimpl Derive<trait Score> for A {\n    const fn derive(_ record: Record) -> Code<Item> {\n        let n = Token::seed()\n        quote { const fn score() -> u256 { ${n} } }\n    }\n}\nconst fn run() -> u256 { with (A) { Pair::score() } }\n";
    let inherent_reads_scoped = "use core::derive::{Derive, DeriveInherent}\nuse core::meta::{Record, Code, Item}\ntrait Seed { const fn seed() -> u256 }\nstruct Token {}\nstruct B {}\nimpl Derive<trait Seed> for B {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote { const fn seed() -> u256 { 42 } }\n    }\n}\nstruct Make {}\nimpl DeriveInherent for Make {\n    const fn derive(_ record: Record) -> Code<Item> {\n        let n = with (B) { Token::seed() }\n        quote { pub const fn run() -> u256 { ${n} } }\n    }\n}\nstruct Pair {}\nimpl Pair with (Make)\n";
    let inherent_reads_inherent = "use core::derive::{Derive, DeriveInherent}\nuse core::meta::{Record, Code, Item}\nstruct SeedP {}\nimpl DeriveInherent for SeedP {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote { pub const fn seed() -> u256 { 42 } }\n    }\n}\nstruct Token {}\nimpl Token with (SeedP)\nstruct Make {}\nimpl DeriveInherent for Make {\n    const fn derive(_ record: Record) -> Code<Item> {\n        let n = Token::seed()\n        quote { pub const fn run() -> u256 { ${n} } }\n    }\n}\nstruct Pair {}\nimpl Pair with (Make)\n";
    for (name, source) in [
        (
            "scoped provider reads inherent header output",
            scoped_reads_inherent,
        ),
        (
            "inherent header provider reads scoped output",
            inherent_reads_scoped,
        ),
        (
            "inherent header provider reads inherent header output",
            inherent_reads_inherent,
        ),
    ] {
        let cause = fresh(source).expect_err(name);
        assert!(cause.contains(RULE), "{name}: {cause}");
    }
}
