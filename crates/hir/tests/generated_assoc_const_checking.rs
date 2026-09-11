use fe_hir::{
    analysis::ty::{
        diagnostics::{BodyDiag, FuncBodyDiag},
        ty_check::check_impl_trait_const_bodies,
    },
    hir_def::{AssocConstBodyCheckPolicy, ImplTrait, TopLevelMod},
    span::{DesugaredOrigin, HirOrigin, impl_trait_ast},
    test_db::{HirAnalysisTestDb, format_diagnostics},
};

fn generated_event_impl<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod<'db>,
) -> ImplTrait<'db> {
    let generated = top_mod
        .all_impl_traits(db)
        .iter()
        .copied()
        .filter(|item| {
            matches!(
                impl_trait_ast(db, *item),
                HirOrigin::Desugared(DesugaredOrigin::Event(_))
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(generated.len(), 1, "expected one generated event impl");
    generated[0]
}

fn is_mismatch_between<'db>(
    db: &'db HirAnalysisTestDb,
    diagnostic: &FuncBodyDiag<'db>,
    expected_name: &str,
    actual_name: &str,
) -> bool {
    let (expected, actual) = match diagnostic {
        FuncBodyDiag::Body(BodyDiag::TypeMismatch {
            expected, given, ..
        }) => (expected, given),
        FuncBodyDiag::Body(BodyDiag::ReturnedTypeMismatch {
            expected, actual, ..
        }) => (expected, actual),
        _ => return false,
    };
    expected.pretty_print(db) == expected_name && actual.pretty_print(db) == actual_name
}

#[test]
fn generated_default_policy_checks_the_trait_expected_type() {
    for expected in ["bool", "u256"] {
        let mut db = HirAnalysisTestDb::default();
        // The local trait changes the expected constant type while the real
        // event producer still emits keccak's u256 body. This query deliberately
        // isolates body checking from separate impl-header/method conformance.
        let source = format!(
            r#"
mod std {{
    pub mod evm {{
        pub trait Event {{ const TOPIC0: {expected} }}
    }}
}}
#[event]
struct Empty {{}}
"#
        );
        let file = db.new_stand_alone("generated_const_expected_type.fe".into(), &source);
        let (top_mod, _) = db.top_mod(file);
        let generated = generated_event_impl(&db, top_mod);
        assert_eq!(generated.hir_consts(&db).len(), 1);
        assert_eq!(
            generated.hir_consts(&db)[0].body_check_policy,
            AssocConstBodyCheckPolicy::default()
        );
        let diagnostics = check_impl_trait_const_bodies(&db, generated);
        if expected == "bool" {
            assert_eq!(diagnostics.len(), 1, "{diagnostics:#?}");
            assert!(
                is_mismatch_between(&db, &diagnostics[0], "bool", "u256"),
                "{diagnostics:#?}"
            );
        } else {
            assert!(diagnostics.is_empty(), "{diagnostics:#?}");
        }
    }
}

#[test]
fn standard_fieldless_event_remains_clean_with_body_checking() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "generated_const_standard_event.fe".into(),
        "#[event]\nstruct Empty {}\n",
    );
    let (top_mod, _) = db.top_mod(file);
    let generated = generated_event_impl(&db, top_mod);
    assert_eq!(
        generated.hir_consts(&db)[0].body_check_policy,
        AssocConstBodyCheckPolicy::BodyAnalysis
    );
    db.assert_no_diags(top_mod);
}

#[test]
fn ordinary_associated_const_body_checking_is_preserved() {
    for value in ["true", "7"] {
        let mut db = HirAnalysisTestDb::default();
        let source = format!(
            r#"
trait Sample {{ const VALUE: u256 }}
struct Target {{}}
impl Sample for Target {{ const VALUE: u256 = {value} }}
"#
        );
        let file = db.new_stand_alone("ordinary_const_body_policy.fe".into(), &source);
        let (top_mod, _) = db.top_mod(file);
        let impls = top_mod.all_impl_traits(&db);
        assert_eq!(impls.len(), 1);
        assert_eq!(
            impls[0].hir_consts(&db)[0].body_check_policy,
            AssocConstBodyCheckPolicy::BodyAnalysis
        );
        let diagnostics = check_impl_trait_const_bodies(&db, impls[0]);
        if value == "true" {
            assert_eq!(diagnostics.len(), 1, "{diagnostics:#?}");
            assert!(
                is_mismatch_between(&db, &diagnostics[0], "u256", "bool"),
                "{diagnostics:#?}"
            );
        } else {
            assert!(diagnostics.is_empty(), "{diagnostics:#?}");
        }
    }
}

#[test]
fn message_selector_body_diagnostic_has_one_owner() {
    for (selector_type, selector_value) in [("bool", "true"), ("u32", "1")] {
        let mut db = HirAnalysisTestDb::default();
        let source = format!(
            r#"
const SELECTOR_VALUE: {selector_type} = {selector_value}

msg Calls {{
    #[selector = ingot::SELECTOR_VALUE]
    Ping {{}} -> bool,
}}
"#
        );
        let file = db.new_stand_alone("message_const_body_owner.fe".into(), &source);
        let (top_mod, _) = db.top_mod(file);
        let policies = top_mod
            .all_impl_traits(&db)
            .iter()
            .flat_map(|item| item.hir_consts(&db))
            .map(|constant| constant.body_check_policy)
            .collect::<Vec<_>>();
        assert_eq!(
            policies
                .iter()
                .filter(|&&policy| policy == AssocConstBodyCheckPolicy::MsgSelectorAnalysis)
                .count(),
            1
        );
        assert!(policies.contains(&AssocConstBodyCheckPolicy::ExpansionSourceCompatibility));
        let diagnostics = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
        if selector_type == "bool" {
            assert_eq!(diagnostics.matches("error[").count(), 1, "{diagnostics}");
            assert!(diagnostics.contains("error[8-0000]"), "{diagnostics}");
            assert!(diagnostics.contains("u32"), "{diagnostics}");
            assert!(diagnostics.contains("bool"), "{diagnostics}");
        } else {
            assert!(diagnostics.is_empty(), "{diagnostics}");
        }
    }
}
