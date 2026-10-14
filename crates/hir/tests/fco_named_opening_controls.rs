use fe_hir::{
    analysis::{
        semantic::{EvalOutcome, GenericSubst, SemConstId, SemConstValue, eval_body_owner_const},
        ty::{
            trait_def::prove_selected_hir_application, trait_resolution::TraitSolveCx,
            ty_check::BodyOwner,
        },
    },
    test_db::{HirAnalysisTestDb, find_func, initialize_test_analysis_pass},
};
use std::path::PathBuf;
fn source(name: &str) -> String {
    std::fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fco_named_opening_controls")
            .join(format!("{name}.fe")),
    )
    .unwrap()
}
fn flatten<'db>(
    db: &'db HirAnalysisTestDb,
    value: SemConstId<'db>,
    out: &mut Vec<SemConstId<'db>>,
) {
    if let SemConstValue::Tuple { elems, .. } = value.value(db) {
        for &elem in elems.iter() {
            flatten(db, elem, out)
        }
    } else {
        out.push(value)
    }
}
fn choices(name: &str, expected: &[&str], equalities: usize) {
    let mut db = HirAnalysisTestDb::default();
    let text = source(name);
    let file = db.new_stand_alone(format!("{name}.fe").into(), &text);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&db, module, "answer"));
    let result =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    println!("CONTROL {name} CTFE {result:?}");
    let EvalOutcome::Ready(value) = result else {
        panic!("{name}: actual CTFE evidence required: {result:?}")
    };
    let mut values = vec![];
    flatten(&db, value, &mut values);
    let actual: Vec<_> = values
        .iter()
        .map(|v| {
            let SemConstValue::Evidence { application, .. } = v.value(&db) else {
                panic!("issued evidence required")
            };
            assert_eq!(
                application.goal().assoc_type_bindings(&db).len(),
                equalities
            );
            if equalities > 0 {
                for ty in application.goal().assoc_type_bindings(&db).values() {
                    assert_eq!(ty.pretty_print(&db), "u64")
                }
            }
            module
                .all_impl_traits(&db)
                .iter()
                .copied()
                .find_map(|item| {
                    let checked = prove_selected_hir_application(
                        &db,
                        TraitSolveCx::new(&db, find_func(&db, module, "answer").scope()),
                        item,
                        application.goal(),
                    )
                    .ok()?;
                    (checked.resolved().selected() == application.resolved().selected()).then(
                        || {
                            item.selection_name(&db)
                                .map(|n| n.data(&db).to_string())
                                .unwrap_or("ordinary".into())
                        },
                    )
                })
                .expect("retained source choice must equal a checked source declaration")
        })
        .collect();
    println!("CONTROL {name} EXPECTED {expected:?} ACTUAL {actual:?}");
    assert_eq!(actual, expected);
}
fn negative(name: &str, message: &str, reason: Option<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let text = source(name);
    let file = db.new_stand_alone(format!("{name}.fe").into(), &text);
    let (module, _) = db.top_mod(file);
    let diags: Vec<_> = initialize_test_analysis_pass()
        .run_on_module(&db, module)
        .into_iter()
        .map(|d| d.to_complete(&db))
        .collect();
    println!("CONTROL {name} DIAGNOSTICS {diags:#?}");
    assert!(
        diags
            .iter()
            .any(|d| d.message == message && reason.is_none_or(|r| format!("{d:?}").contains(r))),
        "{name}: exact rejection required {diags:#?}"
    );
    let fault = match name {
        "duplicate-two" => "Second",
        "duplicate-same" => "First",
        "associated-change" => "impl Pick for Subject as First { type Output = bool }",
        "associated-use-wrong" => "prove<Goal>()",
        "prerequisite-absent" => "NeedsLeaf",
        _ => panic!("undefined independent fault locator"),
    };
    let start = text.rfind(fault).unwrap();
    let end = start + fault.len();
    assert!(
        diags
            .iter()
            .filter(|d| d.message == message)
            .any(|d| d.sub_diagnostics.iter().any(|sub| sub
                .span
                .as_ref()
                .is_some_and(|span| u32::from(span.range.start()) as usize == start
                    && u32::from(span.range.end()) as usize == end))),
        "{name}: exact authored fault range {start}..{end} required"
    );
    println!("CONTROL {name} EXPECTED_CAUSE_AND_RANGE={start}..{end}");
}
#[test]
fn precedence_and_both_restorations() {
    choices(
        "precedence-restoration",
        &["ordinary", "First", "Second", "First", "ordinary"],
        0,
    )
}
#[test]
fn associated_output_agreement() {
    choices("associated-good", &["First"], 1)
}
#[test]
fn named_prerequisite_valid_control() {
    choices("prerequisite-valid", &["NeedsLeaf"], 0)
}
#[test]
fn two_choices_same_frame_reject() {
    negative(
        "duplicate-two",
        "trait evidence opening failed",
        Some("one frame cannot provide the same trait goal twice"),
    )
}
#[test]
fn same_choice_twice_same_frame_reject() {
    negative(
        "duplicate-same",
        "trait evidence opening failed",
        Some("one frame cannot provide the same trait goal twice"),
    )
}
#[test]
fn named_associated_output_cannot_change() {
    negative(
        "associated-change",
        "named implementation changes an ordinary associated type",
        None,
    )
}
#[test]
fn requested_associated_output_cannot_change() {
    negative(
        "associated-use-wrong",
        "trait evidence introduction failed",
        Some("not proved"),
    )
}
#[test]
fn ordinary_root_cannot_rescue_named_missing_prerequisite() {
    negative(
        "prerequisite-absent",
        "trait evidence opening failed",
        Some("named implementation prerequisites were not proved"),
    )
}
#[test]
fn const_dispatch_obeys_the_same_ratified_law() {
    let mut db = HirAnalysisTestDb::default();
    let text = source("const-dispatch");
    let file = db.new_stand_alone("const-dispatch.fe".into(), &text);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&db, module, "answer"));
    let result =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    println!("CONTROL const-dispatch OUTCOME {result:?}");
    let EvalOutcome::Ready(value) = result else {
        panic!("expected selected CTFE dispatch, got {result:?}")
    };
    let mut values = vec![];
    flatten(&db, value, &mut values);
    let actual: Vec<_> = values.iter().map(|v| v.pretty_print(&db)).collect();
    println!("CONTROL const-dispatch EXPECTED [1,2,3,2,1] ACTUAL {actual:?}");
    assert_eq!(actual, ["1", "2", "3", "2", "1"]);
}
