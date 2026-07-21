#[test]
fn hir_expr_and_stmt_origins_do_not_cross() {
    let tests = trybuild::TestCases::new();
    tests.compile_fail("tests/ui/hir_origin_expr_stmt_mismatch.rs");
}

#[test]
fn hir_export_keys_reject_raw_owner_strings() {
    let tests = trybuild::TestCases::new();
    tests.compile_fail("tests/ui/hir_origin_export_key_raw_strings.rs");
}
