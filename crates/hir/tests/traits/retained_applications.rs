use common::indexmap::IndexMap;
use fe_hir::{
    analysis::ty::{
        trait_def::{CheckedApplicationError, TraitInstId, prove_selected_hir_application},
        trait_resolution::TraitSolveCx,
        ty_def::TyId,
    },
    hir_def::IdentId,
    test_db::{HirAnalysisTestDb, find_func},
};

#[test]
fn public_checked_application_preserves_scope_and_revalidates_full_goal() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "public_checked_application.fe".into(),
        r#"
trait Produce { type Output }
struct Subject {}
impl Produce for Subject { type Output = u256 }
fn request(_ subject: Subject) {}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let request = find_func(&db, module, "request");
    let subject = request.params(&db).next().unwrap().ty(&db);
    let subject = subject.as_view(&db).unwrap_or(subject);
    let trait_ = module.all_traits(&db)[0];
    let goal = TraitInstId::new_simple(&db, trait_, vec![subject]);
    let cx = TraitSolveCx::new(&db, request.scope());
    let checked = prove_selected_hir_application(&db, cx, module.all_impl_traits(&db)[0], goal)
        .expect("checked ordinary source must expose its application");
    assert_eq!(checked.goal(), goal);
    assert_eq!(checked.origin_context().scope(), request.scope());
    assert_ne!(checked.origin_context().scope(), module.scope());
    let output = IdentId::new(&db, "Output");
    assert_eq!(checked.associated_type(output), Some(TyId::u256(&db)));
    let mut bindings = IndexMap::new();
    bindings.insert(output, TyId::bool(&db));
    let wrong = TraitInstId::new(&db, trait_, vec![subject], bindings);
    assert_eq!(
        checked.revalidate(cx, wrong).unwrap_err(),
        CheckedApplicationError::NotProven
    );
    let moved = checked
        .revalidate(TraitSolveCx::new(&db, module.scope()), goal)
        .expect("an unconditional application needs no inherited caller premise");
    assert_eq!(moved.origin_context().scope(), module.scope());
    assert_eq!(moved.associated_type(output), Some(TyId::u256(&db)));
}

#[test]
fn public_checked_application_rejects_unrequested_conditional_associated_output() {
    for (name, output, source_valid) in [
        ("plain", "u256", true),
        ("conditional_good", "Bounded<1>", true),
        ("conditional_bad", "Bounded<0>", false),
    ] {
        let source = r#"
trait Pick { type Out }
struct Key {}
struct Bounded<const N: usize> where { N > 0 } {}
impl Pick for Key { type Out = __OUT__ }
fn probe(_ key: Key) {}
"#
        .replace("__OUT__", output);
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(format!("{name}.fe").into(), &source);
        let (module, _) = db.top_mod(file);
        if source_valid {
            db.assert_no_diags(module);
        }
        let probe = find_func(&db, module, "probe");
        let subject = probe.params(&db).next().unwrap().ty(&db);
        let subject = subject.as_view(&db).unwrap_or(subject);
        // No output equality in the requested goal: the candidate's own
        // associated definition must still enter the admission receipt.
        let goal = TraitInstId::new_simple(&db, module.all_traits(&db)[0], vec![subject]);
        let result = prove_selected_hir_application(
            &db,
            TraitSolveCx::new(&db, probe.scope()),
            module.all_impl_traits(&db)[0],
            goal,
        );
        if name == "plain" {
            assert!(result.is_ok());
        } else {
            assert_eq!(
                result.unwrap_err(),
                CheckedApplicationError::UnsupportedConditions
            );
        }
    }
}

#[test]
fn normalized_prerequisite_cannot_hide_conditional_nominal_admission() {
    for output in ["u256", "Bounded<1>"] {
        let source = format!(
            "trait Supply {{ type Out }}\n\
             trait Marker<A> {{}}\n\
             trait Run {{}}\n\
             struct Key {{}}\n\
             struct Bounded<const N: usize> where {{ N > 0 }} {{}}\n\
             impl Supply for Key {{ type Out = {output} }}\n\
             impl<A> Marker<A> for Key {{}}\n\
             impl Run for Key where Key: Marker<<Key as Supply>::Out> {{}}\n\
             fn probe(_ key: Key) {{}}"
        );
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("normalized-prerequisite.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        // Both declarations are valid source. The checked application API has
        // a narrower admission boundary for conditional nominal applications.
        db.assert_no_diags(module);
        let probe = find_func(&db, module, "probe");
        let ty = probe.params(&db).next().unwrap().ty(&db);
        let ty = ty.as_view(&db).unwrap_or(ty);
        let goal = TraitInstId::new_simple(&db, module.all_traits(&db)[2], vec![ty]);
        let result = prove_selected_hir_application(
            &db,
            TraitSolveCx::new(&db, probe.scope()),
            module.all_impl_traits(&db)[2],
            goal,
        );
        if output == "u256" {
            result.expect("plain normalized prerequisite remains admitted");
        } else {
            assert_eq!(
                result.unwrap_err(),
                CheckedApplicationError::UnsupportedConditions
            );
        }
    }
}
