# Crash Regressions

These fixtures are minimal inputs that previously triggered compiler panics/ICEs or hangs found by
fuzzing.

Keep each fixture focused on the smallest construct that still reaches the former panic path.

They are used by `crates/fe/tests/crash_regressions.rs` to ensure `fe check --standalone` never
panics or times out on them (it should emit diagnostics and exit normally).

Fixtures in `recovery_mode/` are checked with `fe check --standalone --recovery-mode`. They are
inputs whose syntax errors made parser recovery loop without end, which only happens when recovery
is on.
