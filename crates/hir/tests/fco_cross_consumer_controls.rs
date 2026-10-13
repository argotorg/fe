use fe_hir::{
    analysis::{
        semantic::{EvalOutcome, GenericSubst, SemConstId, SemConstValue, eval_body_owner_const},
        ty::ty_check::BodyOwner,
    },
    test_db::{HirAnalysisTestDb, find_func, initialize_test_analysis_pass},
};
use std::path::PathBuf;
fn source(name: &str) -> String {
    std::fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fco_cross_consumer_controls")
            .join(format!("{name}.fe")),
    )
    .unwrap()
}
fn flatten<'db>(db: &'db HirAnalysisTestDb, value: SemConstId<'db>, out: &mut Vec<String>) {
    if let SemConstValue::Tuple { elems, .. } = value.value(db) {
        for &v in elems.iter() {
            flatten(db, v, out);
        }
    } else {
        out.push(value.pretty_print(db));
    }
}
fn values(name: &str, expected: &[&str]) {
    let mut db = HirAnalysisTestDb::default();
    let text = source(name);
    let file = db.new_stand_alone(format!("{name}.fe").into(), &text);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&db, module, "answer"));
    let result =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    println!("CONTROL {name} EXPECTED {expected:?} OUTCOME {result:?}");
    let EvalOutcome::Ready(value) = result else {
        panic!("actual CTFE required: {result:?}")
    };
    let mut actual = vec![];
    flatten(&db, value, &mut actual);
    println!("CONTROL {name} ACTUAL {actual:?}");
    assert_eq!(actual, expected);
}
#[test]
fn direct_restoration() {
    values("direct", &["1", "42", "7", "42", "1"]);
}
#[test]
fn ordinary_bound_helper_restoration() {
    values("helper", &["1", "42", "7", "42", "1"]);
}
#[test]
fn evidence_bound_helper() {
    values("evidence-helper", &["1", "42", "7", "42"]);
}
#[test]
fn ordinary_prerequisite_dispatch_restoration() {
    values("prerequisite", &["11", "43", "19", "43", "11"]);
}
#[test]
fn named_operator_restoration() {
    values("operator", &["5", "47", "23", "47", "5"]);
}
#[test]
fn evidence_operator() {
    values("evidence-operator", &["5", "47", "23", "47"]);
}
#[test]
fn missing_destination_prerequisite_and_valid_twin() {
    let mut db = HirAnalysisTestDb::default();
    let text = source("destination");
    let file = db.new_stand_alone("destination.fe".into(), &text);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&db, module, "rejected"));
    let result =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    println!("CONTROL destination rejected {result:?}");
    assert!(matches!(result, EvalOutcome::Failed(_)));
    assert!(
        format!("{result:?}").contains("opened evidence application was not admitted at this use")
    );
}
#[test]
fn destination_prerequisite_valid_twin() {
    values("destination", &["43"]);
}
#[test]
fn fixed_associated_output_refusal() {
    let mut db = HirAnalysisTestDb::default();
    let text = source("fixed-output");
    let file = db.new_stand_alone("fixed-output.fe".into(), &text);
    let (module, _) = db.top_mod(file);
    let diags: Vec<_> = initialize_test_analysis_pass()
        .run_on_module(&db, module)
        .into_iter()
        .map(|d| d.to_complete(&db))
        .collect();
    println!("CONTROL fixed-output DIAGNOSTICS {diags:#?}");
    let start = text.find("impl Add for Subject as First").unwrap();
    let end = text[start..]
        .find("impl Add for Subject as Second")
        .unwrap()
        + start
        - 1;
    assert!(diags.iter().any(|d| {
        d.message == "named implementation changes an ordinary associated type"
            && d.sub_diagnostics.iter().any(|sub| {
                sub.span.as_ref().is_some_and(|span| {
                    u32::from(span.range.start()) as usize == start
                        && u32::from(span.range.end()) as usize == end
                })
            })
    }));
    println!("CONTROL fixed-output expected cause span {start}..{end}");
}

#[test]
fn ground_prerequisite_valid_twin() {
    values("ground-prerequisite-call-pending", &["43"]);
}
#[test]
fn ground_prerequisite_missing_destination() {
    let mut db = HirAnalysisTestDb::default();
    let text = source("ground-prerequisite-call-pending");
    let file = db.new_stand_alone("ground-prerequisite.fe".into(), &text);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&db, module, "rejected"));
    let result =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    println!("GROUND DESTINATION {result:?}");
    assert!(matches!(result, EvalOutcome::Failed(_)));
    assert!(
        format!("{result:?}").contains("opened evidence application was not admitted at this use")
    );
}

#[test]
fn ordinary_ground_declaration_keeps_master_assumption_policy() {
    let mut db = HirAnalysisTestDb::default();
    let text = r#"
trait Leaf { const fn leaf() -> u256 }
trait Pick { const fn value() -> u256 }
struct Subject {}
impl Pick for Subject where Subject: Leaf {
    const fn value() -> u256 { <Subject as Leaf>::leaf() }
}
"#;
    let file = db.new_stand_alone("ordinary-ground.fe".into(), text);
    let (module, _) = db.top_mod(file);
    let diags: Vec<_> = initialize_test_analysis_pass()
        .run_on_module(&db, module)
        .into_iter()
        .map(|d| d.to_complete(&db))
        .collect();
    assert!(
        diags
            .iter()
            .any(|d| d.message == "trait bound is not satisfied")
    );
}

#[test]
fn prerequisite_constant_reachability_and_declaration_freezing() {
    values(
        "prerequisite-constant-reachability",
        &["2", "43", "8", "43", "2", "1"],
    );
}

#[test]
fn direct_associated_constant_restoration() {
    values("direct-associated-constant", &["1", "42", "7", "42", "1"]);
}

#[test]
fn rd71_parameter_and_concrete_method_reachability() {
    values("rd71-method-reachability", &["42", "1", "1"]);
}
