use fe_hir::{
    analysis::ty::{
        diagnostics::{BodyDiag, FuncBodyDiag},
        ty_check::check_impl_trait_const_bodies,
    },
    hir_def::{AssocConstBodyCheckPolicy, ImplTrait, TopLevelMod},
    span::{DesugaredOrigin, HirOrigin, impl_trait_ast},
    test_db::HirAnalysisTestDb,
};

/// Reads a fixture from `test_files/assoc_const_body_policy`, returning its
/// real path (so the file gets a real `file:` URL) and its text.
fn fixture(name: &str) -> (camino::Utf8PathBuf, String) {
    let path = camino::Utf8PathBuf::from(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/test_files/assoc_const_body_policy"
    ))
    .join(name);
    let text = std::fs::read_to_string(&path).expect("fixture should be readable");
    (path, text)
}

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
    // These fixtures isolate body checking from the separate impl-header and
    // method conformance checks by querying the generated impl directly.
    for (name, mismatch) in [
        ("generated_topic_expects_bool.fe", Some(("bool", "u256"))),
        ("generated_topic_expects_u256.fe", None),
    ] {
        let mut db = HirAnalysisTestDb::default();
        let (path, text) = fixture(name);
        let file = db.new_stand_alone(path, &text);
        let (top_mod, _) = db.top_mod(file);
        let generated = generated_event_impl(&db, top_mod);
        assert_eq!(generated.hir_consts(&db).len(), 1);
        assert_eq!(
            generated.hir_consts(&db)[0].body_check_policy,
            AssocConstBodyCheckPolicy::default()
        );
        let diagnostics = check_impl_trait_const_bodies(&db, generated);
        match mismatch {
            Some((expected, actual)) => {
                assert_eq!(diagnostics.len(), 1, "{name}: {diagnostics:#?}");
                assert!(
                    is_mismatch_between(&db, &diagnostics[0], expected, actual),
                    "{name}: {diagnostics:#?}"
                );
            }
            None => assert!(diagnostics.is_empty(), "{name}: {diagnostics:#?}"),
        }
    }
}

#[test]
fn standard_fieldless_event_remains_clean_with_body_checking() {
    let mut db = HirAnalysisTestDb::default();
    let (path, text) = fixture("fieldless_event.fe");
    let file = db.new_stand_alone(path, &text);
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
    for (name, mismatch) in [
        ("ordinary_value_mismatch.fe", Some(("u256", "bool"))),
        ("ordinary_value.fe", None),
    ] {
        let mut db = HirAnalysisTestDb::default();
        let (path, text) = fixture(name);
        let file = db.new_stand_alone(path, &text);
        let (top_mod, _) = db.top_mod(file);
        let impls = top_mod.all_impl_traits(&db);
        assert_eq!(impls.len(), 1);
        assert_eq!(
            impls[0].hir_consts(&db)[0].body_check_policy,
            AssocConstBodyCheckPolicy::BodyAnalysis
        );
        let diagnostics = check_impl_trait_const_bodies(&db, impls[0]);
        match mismatch {
            Some((expected, actual)) => {
                assert_eq!(diagnostics.len(), 1, "{name}: {diagnostics:#?}");
                assert!(
                    is_mismatch_between(&db, &diagnostics[0], expected, actual),
                    "{name}: {diagnostics:#?}"
                );
            }
            None => assert!(diagnostics.is_empty(), "{name}: {diagnostics:#?}"),
        }
    }
}

/// The rendered diagnostic for a mistyped selector is the uitest fixture
/// `ty_check/msg_selector_const_type_mismatch.fe`. This checks which analysis
/// owns each generated message constant.
#[test]
fn message_selector_body_diagnostic_has_one_owner() {
    let mut db = HirAnalysisTestDb::default();
    let (path, text) = fixture("selector_value.fe");
    let file = db.new_stand_alone(path, &text);
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
    db.assert_no_diags(top_mod);
}
