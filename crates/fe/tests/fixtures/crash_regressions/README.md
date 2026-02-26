# Crash Regressions

These fixtures are minimal inputs that previously triggered compiler panics/ICEs found by fuzzing.

They are used by `crates/fe/tests/crash_regressions.rs` to ensure `fe check --standalone` never
panics on them (it should emit diagnostics and exit normally).

