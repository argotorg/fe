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
