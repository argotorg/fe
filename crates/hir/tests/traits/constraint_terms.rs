use fe_hir::{analysis::ty::ty_def::Kind, test_db::HirAnalysisTestDb};

#[test]
fn evidence_introduction_proves_the_concrete_goal_and_preserves_equalities() {
    use fe_hir::analysis::ty::{
        trait_def::{CheckedApplicationError, introduce_obligation},
        trait_resolution::TraitSolveCx,
    };
    use fe_hir::test_db::find_func;
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "evidence_introduction.fe".into(),
        r#"
trait Produce { type Output }
struct Subject {}
struct Missing {}
impl Produce for Subject { type Output = u64 }
type Good = Produce<Subject, Output = u64>
type Wrong = Produce<Subject, Output = bool>
type Absent = Produce<Missing>
fn request() {}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let aliases = module.all_type_aliases(&db);
    let context = TraitSolveCx::new(&db, find_func(&db, module, "request").scope());
    let application = introduce_obligation(&db, context, aliases[0].ty(&db)).unwrap();
    assert_eq!(application.resolved().trait_inst(), application.goal());
    for alias in &aliases[1..] {
        assert!(matches!(
            introduce_obligation(&db, context, alias.ty(&db)),
            Err(CheckedApplicationError::NotProven)
        ));
    }
}

#[test]
fn source_prove_accepts_a_concrete_goal() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "source_prove.fe".into(),
        r#"
use core::obligations::{Evidence, prove}
trait Produce { type Output }
struct Subject {}
impl Produce for Subject { type Output = u64 }
fn witness() -> Evidence<Produce<Subject, Output = u64>> {
    prove<Produce<Subject, Output = u64>>()
}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let witness = fe_hir::test_db::find_func(&db, module, "witness");
    let (_, typed) = fe_hir::analysis::ty::ty_check::check_func_body(&db, witness);
    let body = witness.body(&db).unwrap();
    let records: Vec<_> = body
        .exprs(&db)
        .keys()
        .filter_map(|expr| typed.callable_expr(expr))
        .filter_map(|call| call.evidence_introduction())
        .collect();
    assert_eq!(
        records.len(),
        1,
        "prove retains one checked use-site decision"
    );
    let record = records[0];
    let context = fe_hir::analysis::ty::trait_resolution::TraitSolveCx::new(&db, witness.scope());
    let checked = record.revalidate(&db, context, record.goal()).unwrap();
    assert_eq!(checked.resolved().selected(), record.resolved().selected());
}

#[test]
fn source_prove_rejects_missing_and_generic_goals_for_the_actual_cause() {
    for (name, source, reason) in [
        (
            "missing",
            r#"
use core::obligations::prove
trait Produce {}
struct Subject {}
fn witness() { prove<Produce<Subject>>() }
"#,
            "the requested trait goal was not proved",
        ),
        (
            "generic",
            r#"
use core::obligations::prove
trait Produce {}
fn witness<T: Produce>() { prove<Produce<T>>() }
"#,
            "expected one concrete trait goal",
        ),
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(format!("source_prove_{name}.fe").into(), source);
        let (module, _) = db.top_mod(file);
        let mut manager = fe_hir::test_db::initialize_test_analysis_pass();
        let diags: Vec<_> = manager
            .run_on_module(&db, module)
            .into_iter()
            .map(|diag| diag.to_complete(&db))
            .collect();
        assert!(
            diags.iter().any(|diag| {
                diag.message == "trait evidence introduction failed"
                    && format!("{diag:?}").contains(reason)
            }),
            "{name}: {diags:#?}"
        );
    }
}

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
