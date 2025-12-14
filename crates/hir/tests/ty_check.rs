use std::path::Path;

use dir_test::{Fixture, dir_test};
use fe_hir::analysis::ty::ty_check::{check_contract_recv_arm_body, check_func_body};
use fe_hir::test_db::HirAnalysisTestDb;
use test_utils::snap_test;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/ty_check",
    glob: "**/*.fe"
)]
fn ty_check_standalone(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, mut prop_formatter) = db.top_mod(file);

    db.assert_no_diags(top_mod);

    for &func in top_mod.all_funcs(&db) {
        if let Some(body) = func.body(&db) {
            let typed_body = &check_func_body(&db, func).1;
            collect_body_props(&db, body, typed_body, &mut prop_formatter);
        }
    }

    for &contract in top_mod.all_contracts(&db) {
        let recvs = contract.recvs(&db);
        for (recv_idx, recv) in recvs.data(&db).iter().enumerate() {
            for (arm_idx, arm) in recv.arms.data(&db).iter().enumerate() {
                let typed_body =
                    &check_contract_recv_arm_body(&db, contract, recv_idx as u32, arm_idx as u32).1;
                collect_body_props(&db, arm.body, typed_body, &mut prop_formatter);
            }
        }
    }

    let res = prop_formatter.finish(&db);
    snap_test!(res, fixture.path());
}

fn collect_body_props<'db>(
    db: &'db HirAnalysisTestDb,
    body: fe_hir::hir_def::Body<'db>,
    typed_body: &fe_hir::analysis::ty::ty_check::TypedBody<'db>,
    prop_formatter: &mut fe_hir::test_db::HirPropertyFormatter<'db>,
) {
    for expr in body.exprs(db).keys() {
        let ty = typed_body.expr_ty(db, expr);
        prop_formatter.push_prop(
            body.top_mod(db),
            expr.span(body).into(),
            ty.pretty_print(db).to_string(),
        );
    }

    for pat in body.pats(db).keys() {
        let ty = typed_body.pat_ty(db, pat);
        prop_formatter.push_prop(
            body.top_mod(db),
            pat.span(body).into(),
            ty.pretty_print(db).to_string(),
        );
    }
}
