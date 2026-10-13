use fe_hir::{
    analysis::{
        semantic::{EvalOutcome, GenericSubst, SemConstValue, eval_body_owner_const},
        ty::{
            trait_def::prove_selected_hir_application, trait_resolution::TraitSolveCx,
            ty_check::BodyOwner,
        },
    },
    test_db::{HirAnalysisTestDb, find_func},
};

#[test]
fn named_opening_captures_the_inner_choice_and_restores_the_outer_choice() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "named_openings.fe".into(),
        r#"
use core::obligations::{Evidence, prove}
trait Pick {}
struct Subject {}
impl Pick for Subject {}
impl Pick for Subject as First {}
impl Pick for Subject as Second {}
type Goal = Pick<Subject>
const fn answer() -> (Evidence<Goal>, Evidence<Goal>, Evidence<Goal>) {
    with (First) {
        let before = prove<Goal>()
        let inner = with (Second) { prove<Goal>() }
        let after = prove<Goal>()
        (before, inner, after)
    }
}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let answer = find_func(&db, module, "answer");
    let context = TraitSolveCx::new(&db, answer.scope());
    let goal = fe_hir::analysis::ty::trait_def::concrete_obligation_goal(
        &db,
        module.all_type_aliases(&db)[0].ty(&db),
    )
    .unwrap();
    let implementations = module.all_impl_traits(&db);
    let first = prove_selected_hir_application(&db, context, implementations[1], goal)
        .unwrap()
        .resolved()
        .selected();
    let second = prove_selected_hir_application(&db, context, implementations[2], goal)
        .unwrap()
        .resolved()
        .selected();
    let owner = BodyOwner::Func(answer);
    let outcome =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    let EvalOutcome::Ready(value) = outcome else {
        panic!("{outcome:?}")
    };
    let SemConstValue::Tuple { elems, .. } = value.value(&db) else {
        panic!("{value:?}")
    };
    let choices: Vec<_> = elems
        .iter()
        .map(|value| match value.value(&db) {
            SemConstValue::Evidence { application, .. } => application.resolved().selected(),
            other => panic!("{other:?}"),
        })
        .collect();
    assert_eq!(choices, [first, second, first]);
}

#[test]
fn one_frame_rejects_two_named_choices_for_the_same_slot() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "duplicate_openings.fe".into(),
        r#"
trait Pick {}
struct Subject {}
impl Pick for Subject {}
impl Pick for Subject as First {}
impl Pick for Subject as Second {}
fn duplicate() { with (First, Second) {} }
"#,
    );
    let (module, _) = db.top_mod(file);
    let mut manager = fe_hir::test_db::initialize_test_analysis_pass();
    let diags: Vec<_> = manager
        .run_on_module(&db, module)
        .into_iter()
        .map(|diag| diag.to_complete(&db))
        .collect();
    assert!(
        diags
            .iter()
            .any(|diag| diag.message == "trait evidence opening failed"
                && format!("{diag:?}")
                    .contains("one frame cannot provide the same trait goal twice")),
        "{diags:#?}"
    );
}

#[test]
fn a_named_declaration_cannot_change_an_ordinary_associated_output() {
    for (named_type, accepted) in [("u64", true), ("bool", false)] {
        let mut db = HirAnalysisTestDb::default();
        let source = format!(
            "trait Pick {{ type Output }}\nstruct Subject {{}}\nimpl Pick for Subject {{ type Output = u64 }}\nimpl Pick for Subject as Alternate {{ type Output = {named_type} }}\n"
        );
        let file = db.new_stand_alone("named_output_agreement.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        let mut manager = fe_hir::test_db::initialize_test_analysis_pass();
        let diags: Vec<_> = manager
            .run_on_module(&db, module)
            .into_iter()
            .map(|diag| diag.to_complete(&db))
            .collect();
        if accepted {
            assert!(diags.is_empty(), "{diags:#?}");
        } else {
            assert!(
                diags.iter().any(|diag| diag.message
                    == "named implementation changes an ordinary associated type"),
                "{diags:#?}"
            );
        }
    }
}

#[test]
fn evidence_operand_reuses_the_issued_choice_and_source_provenance() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "evidence_operand.fe".into(),
        r#"
use core::obligations::{Evidence, prove}
trait Pick {}
struct Subject {}
impl Pick for Subject {}
impl Pick for Subject as First {}
type Goal = Pick<Subject>
const fn issued() -> Evidence<Goal> { with (First) { prove<Goal>() } }
const fn reopen<T>(_ witness: Evidence<Pick<T>>) -> Evidence<Pick<T>> {
    with (witness) { prove<Pick<T>>() }
}
const fn answer() -> (Evidence<Goal>, Evidence<Goal>) {
    let witness = issued()
    (witness, reopen<Subject>(witness))
}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&db, module, "answer"));
    let outcome =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    let EvalOutcome::Ready(value) = outcome else {
        panic!("{outcome:?}")
    };
    let SemConstValue::Tuple { elems, .. } = value.value(&db) else {
        panic!("tuple required")
    };
    let records: Vec<_> = elems
        .iter()
        .map(|value| match value.value(&db) {
            SemConstValue::Evidence { application, .. } => *application,
            other => panic!("actual issued payload required: {other:?}"),
        })
        .collect();
    assert_eq!(
        records[0], records[1],
        "opening must preserve the chosen source application"
    );
    let context = TraitSolveCx::new(&db, find_func(&db, module, "answer").scope());
    let expected = prove_selected_hir_application(
        &db,
        context,
        module.all_impl_traits(&db)[1],
        records[0].goal(),
    )
    .unwrap();
    assert_eq!(
        records[0].resolved().selected(),
        expected.resolved().selected()
    );
    let reopen = find_func(&db, module, "reopen");
    let (_, typed) = fe_hir::analysis::ty::ty_check::check_func_body(&db, reopen);
    let body = reopen.body(&db).unwrap();
    let captured: Vec<_> = body
        .exprs(&db)
        .keys()
        .filter_map(|expr| typed.callable_expr(expr))
        .filter(|call| call.evidence_opening().is_some())
        .collect();
    assert_eq!(captured.len(), 1);
    assert!(
        captured[0].evidence_introduction().is_none(),
        "an operand is not a static introduction record"
    );
}

#[test]
fn reopened_choice_needs_independent_destination_premises() {
    let source = r#"
use core::obligations::{Evidence, prove}
trait Leaf {}
trait Pick {}
struct Subject {}
impl Leaf for Subject as NamedLeaf {}
impl<T: Leaf> Pick for T {}
type Goal = Pick<Subject>
const fn leaf() -> Evidence<Leaf<Subject>> { with (NamedLeaf) { prove<Leaf<Subject>>() } }
const fn issued() -> Evidence<Goal> { with (leaf()) { prove<Goal>() } }
const fn rejected(_ witness: Evidence<Goal>) -> Evidence<Goal> { with (witness) { prove<Goal>() } }
const fn admitted(_ witness: Evidence<Goal>) -> Evidence<Goal> { with (leaf(), witness) { prove<Goal>() } }
const fn bad() -> Evidence<Goal> { rejected(issued()) }
const fn good() -> (Evidence<Goal>, Evidence<Goal>) {
    let witness = issued()
    (witness, admitted(witness))
}
"#;
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("destination_premises.fe".into(), source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&db, module, "bad"));
    let rejected =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    assert!(
        format!("{rejected:?}")
            .contains("opened evidence application was not admitted at this use"),
        "source premises must not be copied to the destination: {rejected:?}"
    );
    let owner = BodyOwner::Func(find_func(&db, module, "good"));
    let outcome =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    let EvalOutcome::Ready(value) = outcome else {
        panic!("independent destination leaf should admit the same choice: {outcome:?}")
    };
    let SemConstValue::Tuple { elems, .. } = value.value(&db) else {
        panic!("tuple required")
    };
    assert_eq!(
        elems[0], elems[1],
        "admission must preserve the actual source record"
    );
    let SemConstValue::Evidence { application, .. } = elems[0].value(&db) else {
        panic!("issued record required")
    };
    let source = application.origin_context();
    assert!(source.lexical_premises().is_some());
    let ordinary = TraitSolveCx::new(&db, source.scope()).with_assumptions(source.assumptions());
    assert!(
        application
            .revalidate(&db, ordinary, application.goal())
            .is_err(),
        "copying the same ground assumptions must not manufacture opened-witness authority or reuse its cached proof"
    );
}
