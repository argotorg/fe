// Unchanged factory-local law from the frozen capacity continuation.
use hir::{
    analysis::{
        semantic::{EvalOutcome, GenericSubst, SemConstValue, eval_body_owner_const},
        ty::ty_check::BodyOwner,
    },
    test_db::{HirAnalysisTestDb, find_func},
};
#[test]
fn forwarded_opened_payload_dispatch_obeys_unmodified_native_325_law() {
    use hir::analysis::semantic::{SemConstId, SemConstScalar};
    fn flatten<'db>(db: &'db HirAnalysisTestDb, value: SemConstId<'db>, out: &mut Vec<String>) {
        match value.value(db) {
            SemConstValue::Tuple { elems, .. } => {
                for value in elems {
                    flatten(db, *value, out);
                }
            }
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } => out.push(value.to_string()),
            other => panic!("expected authored scalar dispatch result, got {other:?}"),
        }
    }
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "operand_dispatch.fe".into(),
        include_str!("fco_payload_controls/factory_local_dispatch.fe"),
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&db, module, "answer"));
    let result =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    let EvalOutcome::Ready(value) = result else {
        panic!("{result:?}")
    };
    let mut actual = vec![];
    flatten(&db, value, &mut actual);
    eprintln!("OPERAND_DISPATCH_NATIVE_325_LAW expected=[1,2,3,2,1] actual={actual:?}");
    assert_eq!(actual, ["1", "2", "3", "2", "1"]);
}

#[test]
fn factory_choices_agree_across_warm_fresh_and_restored_edits() {
    use salsa::Setter;
    let original = include_str!("fco_payload_controls/factory_local_dispatch.fe");
    let changed = original.replace(
        "const fn first() -> Evidence<Goal> { with (First)",
        "const fn first() -> Evidence<Goal> { with (Second)",
    );
    assert_ne!(
        changed, original,
        "the selected declaration must actually change"
    );
    fn answer<'db>(db: &'db HirAnalysisTestDb, module: hir::hir_def::TopLevelMod<'db>) -> String {
        let owner = BodyOwner::Func(find_func(db, module, "answer"));
        let EvalOutcome::Ready(value) =
            eval_body_owner_const(db, owner, GenericSubst::for_body_owner(db, owner, vec![]))
        else {
            panic!("checked edited factory must evaluate")
        };
        value.pretty_print(db)
    }
    let mut warm = HirAnalysisTestDb::default();
    let file = warm.new_stand_alone("factory-cache.fe".into(), original);
    let mut observed = Vec::new();
    for source in [original, changed.as_str(), original] {
        file.set_text(&mut warm).to(source.into());
        let (module, _) = warm.top_mod(file);
        warm.assert_no_diags(module);
        let current = answer(&warm, module);
        assert_eq!(answer(&warm, module), current, "warm repeat must agree");
        let mut fresh = HirAnalysisTestDb::default();
        let fresh_file = fresh.new_stand_alone("factory-cache.fe".into(), source);
        let (fresh_module, _) = fresh.top_mod(fresh_file);
        fresh.assert_no_diags(fresh_module);
        assert_eq!(
            answer(&fresh, fresh_module),
            current,
            "fresh database must agree"
        );
        observed.push(current);
    }
    assert_ne!(
        observed[0], observed[1],
        "edited named choice cannot reuse an old answer"
    );
    assert_eq!(
        observed[0], observed[2],
        "restored source must restore its answer"
    );
}
