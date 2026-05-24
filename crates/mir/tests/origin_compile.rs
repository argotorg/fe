#[test]
fn runtime_stmt_and_terminator_origins_do_not_cross() {
    let tests = trybuild::TestCases::new();
    tests.compile_fail("tests/ui/runtime_origin_stmt_terminator_mismatch.rs");
}

#[test]
fn runtime_export_keys_reject_raw_owner_strings() {
    let tests = trybuild::TestCases::new();
    tests.compile_fail("tests/ui/runtime_origin_export_key_raw_strings.rs");
}
