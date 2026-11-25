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
