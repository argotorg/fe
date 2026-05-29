#[test]
fn shape_describe_rejects_invalid_annotations() {
    let tests = trybuild::TestCases::new();
    tests.compile_fail("tests/ui/shape_describe_missing_kind.rs");
    tests.compile_fail("tests/ui/shape_describe_missing_field_policy.rs");
    tests.compile_fail("tests/ui/shape_describe_conflicting_dimensions.rs");
    tests.compile_fail("tests/ui/shape_describe_skip_without_reason.rs");
}
