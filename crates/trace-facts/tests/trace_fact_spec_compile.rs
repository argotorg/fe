#[test]
fn trace_fact_spec_rejects_invalid_annotations() {
    let tests = trybuild::TestCases::new();
    tests.compile_fail("tests/ui/trace_fact_spec_missing_fact_attr.rs");
    tests.compile_fail("tests/ui/trace_fact_spec_duplicate_key.rs");
}
