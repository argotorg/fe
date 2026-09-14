use fe_hir::{
    analysis::{
        analysis_pass::ModuleAnalysisPass,
        ty::{
            BodyAnalysisPass,
            diagnostics::{BodyDiag, FuncBodyDiag},
            ty_check::check_impl_trait_const_bodies,
        },
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

fn generated_error_impl_with_const<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod<'db>,
    const_name: &str,
) -> ImplTrait<'db> {
    let generated = top_mod
        .all_impl_traits(db)
        .iter()
        .copied()
        .filter(|item| {
            matches!(
                impl_trait_ast(db, *item),
                HirOrigin::Desugared(DesugaredOrigin::Error(_))
            ) && item.hir_consts(db).iter().any(|constant| {
                constant
                    .name
                    .to_opt()
                    .is_some_and(|name| name.data(db) == const_name)
            })
        })
        .collect::<Vec<_>>();
    assert_eq!(
        generated.len(),
        1,
        "expected one generated error impl containing `{const_name}`"
    );
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
fn generated_abi_record_and_metadata_consts_use_body_analysis() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "generated_abi_record_const_policies.fe".into(),
        "#[error]\npub struct Failure { pub code: u256 }\n",
    );
    let (top_mod, _) = db.top_mod(file);

    for const_name in ["LAYOUT", "HEAD_SIZE", "IS_DYNAMIC"] {
        let generated = generated_error_impl_with_const(&db, top_mod, const_name);
        let constant = generated
            .hir_consts(&db)
            .iter()
            .find(|constant| {
                constant
                    .name
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == const_name)
            })
            .expect("selected impl contains the constant");
        assert_eq!(
            constant.body_check_policy,
            AssocConstBodyCheckPolicy::BodyAnalysis,
            "generated `{const_name}` must retain ordinary body checking"
        );
    }

    db.assert_no_diags(top_mod);
}

#[test]
fn generated_abi_record_layout_checks_the_trait_expected_type() {
    let mut db = HirAnalysisTestDb::default();
    // The local ABI trait deliberately changes LAYOUT's expected type while
    // the real error producer still emits an AbiRecordLayout body. Querying
    // the generated impl directly isolates body checking from the separate
    // impl-header conformance diagnostic.
    let file = db.new_stand_alone(
        "generated_abi_record_expected_type.fe".into(),
        r#"
mod core {
    pub mod abi {
        pub struct AbiRecordLayout<const N: usize> {
            pub offsets: [u256; N],
            pub head_size: u256,
        }
        pub trait AbiRecord<const N: usize> { const LAYOUT: bool }
        pub const fn abi_record_layout<const N: usize>(
            _ sizes: [u256; N],
        ) -> AbiRecordLayout<N> {
            AbiRecordLayout { offsets: [0; N], head_size: 0 }
        }
    }
}

#[error]
pub struct Empty {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let generated = generated_error_impl_with_const(&db, top_mod, "LAYOUT");
    assert_eq!(generated.hir_consts(&db).len(), 1);
    assert_eq!(
        generated.hir_consts(&db)[0].body_check_policy,
        AssocConstBodyCheckPolicy::BodyAnalysis
    );

    let diagnostics = check_impl_trait_const_bodies(&db, generated);
    assert_eq!(diagnostics.len(), 1, "{diagnostics:#?}");
    assert!(
        is_mismatch_between(&db, &diagnostics[0], "bool", "AbiRecordLayout<0>"),
        "{diagnostics:#?}"
    );
}

#[test]
fn generated_abi_record_does_not_hide_an_unrelated_helper_bound() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "generated_abi_record_unrelated_bound.fe".into(),
        r#"
mod core {
    pub mod abi {
        pub trait Unexpected {}
        pub struct AbiRecordLayout<const N: usize> {
            pub offsets: [u256; N],
            pub head_size: u256,
        }
        pub trait AbiRecord<const N: usize> {
            const LAYOUT: AbiRecordLayout<N>
        }
        pub const fn abi_record_layout<const N: usize>(
            _ sizes: [u256; N],
        ) -> AbiRecordLayout<N> where bool: Unexpected {
            AbiRecordLayout { offsets: [0; N], head_size: 0 }
        }
    }
}

#[error]
pub struct Empty {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diagnostics = BodyAnalysisPass {}.run_on_module(&db, top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);
    assert!(rendered.contains("Unexpected"), "{rendered}");
    assert!(
        rendered.contains("trait bound is not satisfied"),
        "{rendered}"
    );
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
        assert_eq!(
            policies
                .iter()
                .filter(|&&policy| policy == AssocConstBodyCheckPolicy::BodyAnalysis)
                .count(),
            3
        );
        assert!(
            !policies.contains(&AssocConstBodyCheckPolicy::ExpansionSourceCompatibility),
            "message ABI constants must not escape ordinary body checking: {policies:?}"
        );
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
