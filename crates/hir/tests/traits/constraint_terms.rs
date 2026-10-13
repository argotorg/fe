use fe_hir::{analysis::ty::ty_def::Kind, test_db::HirAnalysisTestDb};

#[test]
fn constraint_terms_retain_subject_arguments_and_equalities_without_proving() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "constraint_terms.fe".into(),
        r#"
use core::obligations::Constraint
trait Produce<Mode> { type Output }
struct Subject {}
struct Number {}
type Request = Produce<Subject, Number, Output = u64>
type Bare = Produce<Subject, Number>
struct Envelope<G: Constraint> { value: u64 }
fn accepts<G: Constraint>() {}
fn describe() { accepts<Request>() }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let aliases = module.all_type_aliases(&db);
    let request = aliases[0].ty(&db);
    let bare = aliases[1].ty(&db);
    assert_eq!(*request.kind(&db), Kind::Constraint);
    assert_eq!(*bare.kind(&db), Kind::Constraint);
    assert!(request.pretty_print(&db).contains("Subject"));
    assert!(request.pretty_print(&db).contains("Number"));
    assert!(request.pretty_print(&db).contains("Output = u64"));
    assert_ne!(request, bare, "binding omission changes goal identity");
}

#[test]
fn designated_constraint_kind_follows_import_identity() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "constraint_kind_alias.fe".into(),
        r#"
use core::obligations::Constraint as GoalKind
struct Constraint {}
trait Produce { type Output }
struct Subject {}
fn accepts<G: GoalKind>() {}
fn describe() { accepts<Produce<Subject, Output = u64>>() }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
}

#[test]
fn malformed_kind_marker_is_not_admitted_as_a_constraint_bound() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "constraint_kind_arity.fe".into(),
        r#"
use core::obligations::Constraint
fn invalid<G: Constraint<u64>>() {}
"#,
    );
    let (module, _) = db.top_mod(file);
    let mut manager = fe_hir::test_db::initialize_test_analysis_pass();
    let diags = manager.run_on_module(&db, module);
    assert!(
        !diags.is_empty(),
        "marker arguments must not disappear during kind recognition"
    );
}

#[test]
fn an_equality_cannot_be_added_or_removed_when_passing_a_goal_carrier() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "constraint_equality_mismatch.fe".into(),
        r#"
use core::obligations::Constraint
trait Produce { type Output }
struct Subject {}
struct Envelope<G: Constraint> { value: u64 }
fn consume(_ value: Envelope<Produce<Subject, Output = u64>>) {}
fn mismatch(_ value: Envelope<Produce<Subject>>) { consume(value) }
"#,
    );
    let (module, _) = db.top_mod(file);
    let mut manager = fe_hir::test_db::initialize_test_analysis_pass();
    let diags = manager.run_on_module(&db, module);
    let complete: Vec<_> = diags
        .into_iter()
        .map(|diag| diag.to_complete(&db))
        .collect();
    assert!(
        complete.iter().any(|diag| diag.message == "type mismatch"),
        "{complete:#?}"
    );
}
