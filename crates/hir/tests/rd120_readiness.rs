use fe_hir::test_db::{HirAnalysisTestDb, initialize_test_analysis_pass};
fn check(name: &str, source: &str, reject: bool) {
    let mut db = HirAnalysisTestDb::default();
    let f = db.new_stand_alone(name.into(), source);
    let (m, _) = db.top_mod(f);
    let mut manager = initialize_test_analysis_pass();
    let ds: Vec<_> = manager
        .run_on_module(&db, m)
        .into_iter()
        .map(|d| d.to_complete(&db))
        .collect();
    println!("CASE {name} {ds:#?}");
    if reject {
        assert_eq!(ds.len(), 1, "one cause-specific escape error required");
        assert!(
            ds[0].message.contains("cannot leave this `with` block"),
            "unrelated error does not prove RD120"
        );
        assert!(
            ds[0].sub_diagnostics.iter().any(|l| l.span.is_some()),
            "authored source span required"
        );
    } else {
        assert!(ds.is_empty(), "within-block valid source rejected: {ds:#?}");
    }
}
#[test]
fn selected_aggregate_escape() {
    check(
        "signature_const_aggregate_leak.fe",
        include_str!("rd120_controls/signature_const_aggregate_leak.fe"),
        true,
    );
}
#[test]
fn selected_aggregate_inside() {
    check(
        "signature_const_aggregate_inside.fe",
        include_str!("rd120_controls/signature_const_aggregate_inside.fe"),
        false,
    );
}

#[test]
fn equal_selected_layout_may_leave() {
    let source = include_str!("rd120_controls/signature_const_aggregate_leak.fe")
        .replace("const N: usize = 2", "const N: usize = 1")
        .replace("[7,8]", "[7]");
    check("equal-selected-layout.fe", &source, false);
}
