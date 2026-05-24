#[test]
fn runtime_stmt_and_terminator_origins_do_not_cross() {
    let tests = trybuild::TestCases::new();
    tests.compile_fail("tests/ui/runtime_origin_stmt_terminator_mismatch.rs");
}
