//! A module's `with` declaration parses and lowers to one item. Its meaning is
//! not available yet, so the checker refuses it instead of ignoring it.
use fe_hir::{hir_def::ItemKind, test_db::HirAnalysisTestDb};

const PICKS: &str = r#"
pub struct Item {}
pub trait Pick { const fn value() -> u256 }
impl Pick for Item { const fn value() -> u256 { 100 } }
pub impl Pick for Item as Discount { const fn value() -> u256 { 70 } }
pub impl Pick for Item as Tiny { const fn value() -> u256 { 3 } }
"#;

fn with_picks(rest: &str) -> String {
    [PICKS, rest].concat()
}

fn diagnostics(source: &str) -> Vec<String> {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("module_with.fe".into(), source);
    let (module, _) = db.top_mod(file);
    fe_hir::analysis::initialize_analysis_pass()
        .run_on_module(&db, module)
        .into_iter()
        .map(|diagnostic| diagnostic.to_complete(&db).error_code.to_string())
        .collect()
}

#[test]
fn a_declaration_lowers_to_one_item_with_its_operands() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "module_with.fe".into(),
        &with_picks("with (Discount, <Item as Pick>)\n"),
    );
    let (module, _) = db.top_mod(file);
    let declarations: Vec<_> = module
        .children_non_nested(&db)
        .filter_map(|item| match item {
            ItemKind::With(declaration) => Some(declaration),
            _ => None,
        })
        .collect();
    assert_eq!(declarations.len(), 1);
    assert_eq!(declarations[0].operands(&db).len(), 2);
    assert_eq!(
        declarations[0].pretty_print(&db),
        "with (Discount, <Item as Pick>)"
    );
}

#[test]
fn a_declaration_is_refused_until_its_meaning_is_available() {
    assert_eq!(
        diagnostics(&with_picks("with (Discount)\n")),
        vec!["8-0171"]
    );
}

#[test]
fn a_declaration_in_a_child_module_is_refused_once() {
    assert_eq!(
        diagnostics(&with_picks(
            "mod m {\n    use super::*\n    with (Discount)\n    pub fn f() -> u256 { 1 }\n}\n"
        )),
        vec!["8-0171"]
    );
}

#[test]
fn a_program_without_a_declaration_is_unaffected() {
    assert_eq!(diagnostics(&with_picks("")), Vec::<String>::new());
    assert_eq!(
        diagnostics(&with_picks(
            "pub fn f() -> u256 { with (Discount) { <Item as Pick>::value() } }\n"
        )),
        Vec::<String>::new()
    );
}

/// Names an operand can resolve to, shared by the operand tests.
const OPERANDS: &str = r#"
pub trait Show { fn show() -> u256 }
pub struct Foo {}
impl Show for Foo { fn show() -> u256 { 100 } }
pub impl Show for Foo as Loud { fn show() -> u256 { 70 } }
pub impl Show for Foo as Quiet { fn show() -> u256 { 1 } }
pub fn Loud2() -> u256 { 3 }
pub impl Show for Foo as Loud2 { fn show() -> u256 { 71 } }
pub struct Item {}
pub trait Missing {}
pub trait Sub {}
impl<T: Missing> Sub for T as Broken {}
pub struct NeedsSub<T: Sub> {}
mod hidden { use super::*
  impl Show for Item as Secret { fn show() -> u256 { 5 } } }
pub struct G<T> {}
pub impl<T> Show for G<T> as GenLoud { fn show() -> u256 { 9 } }
pub struct G2<T, U> {}
pub impl<T, U> Show for G2<T, U> as Two { fn show() -> u256 { 9 } }
pub struct Restricted<const N: u256> where { N > 0 } {}
pub struct Gx<T> {}
pub impl<T> Show for Gx<T> as Choice { fn show() -> u256 { 9 } }
pub fn val() -> u256 { 3 }
"#;

const PROVIDER_AND_IMPL: &str = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Tr { fn op(self) -> u256 }
struct R { a: u256 }
struct P {}
impl Derive<trait Tr> for P {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { fn op(self) -> u256 { 1 } }
    }
}
impl Tr for R { fn op(self) -> u256 { 5 } }
impl Tr for R as P { fn op(self) -> u256 { 6 } }
"#;

fn sorted_codes(source: &str) -> Vec<String> {
    let mut codes = diagnostics(source);
    codes.sort();
    codes
}

/// The operands of a block `with`, and the codes each one is refused with.
/// These pin what the block says about an operand, whatever code decides it.
#[test]
fn block_operand_codes() {
    let rows: &[(&str, &str, &[&str])] = &[
        ("Nope", "fn f() -> u256 { with (Nope) { 1 } }", &["8-0012"]),
        (
            "nope::Loud",
            "fn f() -> u256 { with (nope::Loud) { 1 } }",
            &["2-0002"],
        ),
        (
            "hidden::Secret",
            "fn f() -> u256 { with (hidden::Secret) { 1 } }",
            &["2-0003"],
        ),
        (
            "Loud2",
            "fn f() -> u256 { with (Loud2) { 1 } }",
            &["2-0004"],
        ),
        (
            "Loud<u8>",
            "fn f() -> u256 { with (Loud<u8>) { 1 } }",
            &["3-0016"],
        ),
        (
            "Two<u8>",
            "fn f() -> u256 { with (Two<u8>) { 1 } }",
            &["8-0163"],
        ),
        (
            "Loud, Quiet",
            "fn f() -> u256 { with (Loud, Quiet) { Foo::show() } }",
            &["8-0160"],
        ),
        (
            "<Item as Show>",
            "fn f() -> u256 { with (<Item as Show>) { 1 } }",
            &["6-0003"],
        ),
        (
            "Choice<Restricted<0>>",
            "fn f() -> u256 { with (Choice<Restricted<0>>) { 1 } }",
            &["8-0093"],
        ),
        (
            "Broken",
            "fn f() { with (Broken) { let _: NeedsSub<Item> } }",
            &["6-0003"],
        ),
    ];
    for (name, source, codes) in rows {
        assert_eq!(
            sorted_codes(&[OPERANDS, source].concat()),
            *codes,
            "operand {name}"
        );
    }
    assert_eq!(
        sorted_codes(
            &[
                PROVIDER_AND_IMPL,
                "fn f() -> u256 { with (P) { R { a: 0 }.op() } }"
            ]
            .concat()
        ),
        ["2-0004"],
        "operand P, a provider type and a named implementation"
    );
}
