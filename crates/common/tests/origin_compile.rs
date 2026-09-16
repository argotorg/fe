#[test]
fn origin_export_key_constructor_rejects_raw_owner_and_local_strings() {
    let tests = trybuild::TestCases::new();
    tests.compile_fail("tests/ui/origin_export_key_raw_strings.rs");
}
