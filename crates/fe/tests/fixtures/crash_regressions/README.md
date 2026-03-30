# Crash Regressions

These fixtures are minimal inputs that previously triggered compiler panics/ICEs or hangs found by
fuzzing.

Top-level `.fe` files are checked in standalone mode. Top-level directories with `fe.toml` are
checked as projects so multi-file repros can stay intact.

Keep each fixture focused on the smallest construct that still reaches the former panic path.

They are used by `crates/fe/tests/crash_regressions.rs` to ensure `fe check` never panics or times
out on them (it should emit diagnostics and exit normally).
