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

#[test]
fn explicit_return_cannot_bypass_selection_boundary() {
    let prefix = include_str!("rd120_controls/signature_const_aggregate_leak.fe")
        .split("fn leak()")
        .next()
        .unwrap();
    let source = format!(
        "{prefix}\nfn returned() -> Buf<Subject> {{ with (Other) {{ return make<Subject>([7,8]) }} }}\n"
    );
    check("explicit-return-escape.fe", &source, true);
}

#[test]
fn assignment_cannot_export_selected_layout() {
    let prefix = include_str!("rd120_controls/signature_const_aggregate_leak.fe")
        .split("fn leak()")
        .next()
        .unwrap();
    let source = format!(
        "{prefix}\nfn assigned() -> u256 {{ let mut b = make<Subject>([7])\nwith (Other) {{ b = make<Subject>([7,8]) }}\nb.a[0] }}\n"
    );
    check("assignment-escape.fe", &source, true);
}

#[test]
fn same_context_assignment_preserves_selected_layout() {
    let prefix = include_str!("rd120_controls/signature_const_aggregate_leak.fe")
        .split("fn leak()")
        .next()
        .unwrap();
    let source = format!(
        "{prefix}\nfn assigned() -> u256 {{ with (Other) {{ let mut b = make<Subject>([7,8])\nb = make<Subject>([9,10])\nb.a[1] }} }}\n"
    );
    check("same-context-assignment.fe", &source, false);
}

#[test]
fn field_assignment_cannot_export_selected_layout() {
    let prefix = include_str!("rd120_controls/signature_const_aggregate_leak.fe")
        .split("fn leak()")
        .next()
        .unwrap();
    let source = format!(
        "{prefix}\nfn assigned() -> u256 {{ let mut b = make<Subject>([7])\nwith (Other) {{ b.a = [7,8] }}\nb.a[0] }}\n"
    );
    check("field-assignment-escape.fe", &source, true);
}

#[test]
fn same_context_field_assignment_preserves_selected_layout() {
    let prefix = include_str!("rd120_controls/signature_const_aggregate_leak.fe")
        .split("fn leak()")
        .next()
        .unwrap();
    let source = format!(
        "{prefix}\nfn assigned() -> u256 {{ with (Other) {{ let mut b = make<Subject>([7,8])\nb.a = [9,10]\nb.a[1] }} }}\n"
    );
    check("same-context-field-assignment.fe", &source, false);
}
#[test]
fn indexed_assignment_cannot_export_selected_layout() {
    let prefix = include_str!("rd120_controls/signature_const_aggregate_leak.fe")
        .split("fn leak()")
        .next()
        .unwrap();
    let source = format!(
        "{prefix}\nfn assigned() -> u256 {{ let mut bs = [make<Subject>([7])]\nwith (Other) {{ bs[0] = make<Subject>([7,8]) }}\nbs[0].a[0] }}\n"
    );
    check("indexed-assignment-escape.fe", &source, true);
}

#[test]
fn ordinary_capability_assignment_keeps_master_behavior() {
    check(
        "ordinary-capability-assignment.fe",
        "fn bump(_ x: mut u256) { x = 7 }",
        false,
    );
}
#[test]
fn same_context_indexed_assignment_preserves_selected_layout() {
    let prefix = include_str!("rd120_controls/signature_const_aggregate_leak.fe")
        .split("fn leak()")
        .next()
        .unwrap();
    let source = format!(
        "{prefix}\nfn assigned() -> u256 {{ with (Other) {{ let mut bs = [make<Subject>([7,8])]\nbs[0] = make<Subject>([9,10])\nbs[0].a[1] }} }}\n"
    );
    check("same-context-indexed-assignment.fe", &source, false);
}
