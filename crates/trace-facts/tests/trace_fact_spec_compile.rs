#[test]
fn trace_fact_spec_rejects_invalid_annotations() {
    let tests = trybuild::TestCases::new();
    tests.compile_fail("tests/ui/trace_fact_spec_missing_fact_attr.rs");
    tests.compile_fail("tests/ui/trace_fact_spec_duplicate_key.rs");
    tests.compile_fail("tests/ui/trace_fact_spec_optional_ref_requires_option.rs");
    tests.compile_fail("tests/ui/trace_fact_spec_ref_requires_origin_key.rs");
    tests.compile_fail("tests/ui/trace_fact_spec_skip_conflict.rs");
    tests.compile_fail("tests/ui/trace_fact_spec_unsupported_column_type.rs");
}
