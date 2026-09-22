// All compile-fail cases run from one test. trybuild shares a scratch project
// per crate, and separate test processes (as under nextest) can clobber it when
// its lockfile heartbeat goes stale, which has been flaky on Windows CI.
#[test]
fn ui() {
    let tests = trybuild::TestCases::new();
    tests.compile_fail("tests/ui/*.rs");
}
