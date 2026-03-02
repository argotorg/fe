# Crash Regressions

These fixtures are minimal inputs that previously triggered compiler panics/ICEs or hangs found by
fuzzing.

They are used by `crates/fe/tests/crash_regressions.rs` to ensure `fe check --standalone` never
panics or times out on them (it should emit diagnostics and exit normally).
