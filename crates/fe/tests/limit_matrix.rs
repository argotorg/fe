//! A normalization limit decides a program only when the answer depends on
//! it (law 5 of the limits design), through the whole pipeline that
//! `fe check` runs, instance building and borrow checking included. The
//! families are shared with the analysis test in
//! `crates/hir/tests/traits/limit_families.rs`.

use std::process::Command;

include!("../../hir/tests/traits/limit_families.rs");

/// The error codes `fe check` reports for `src`, sorted.
fn codes(src: &str) -> Vec<String> {
    use std::sync::atomic::{AtomicUsize, Ordering};
    static NEXT: AtomicUsize = AtomicUsize::new(0);
    let dir = std::env::temp_dir().join(format!("fe-limit-matrix-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join(format!("p{}.fe", NEXT.fetch_add(1, Ordering::Relaxed)));
    std::fs::write(&path, src).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_fe"))
        .args(["check", "--color", "never", "--standalone"])
        .arg(&path)
        .output()
        .expect("fe runs");
    let _ = std::fs::remove_file(&path);
    let text = String::from_utf8_lossy(&output.stdout).into_owned()
        + &String::from_utf8_lossy(&output.stderr);
    let mut codes: Vec<String> = text
        .lines()
        .filter_map(|line| line.strip_prefix("error["))
        .filter_map(|rest| rest.split_once(']'))
        .map(|(code, _)| code.to_string())
        .collect();
    if text.contains("panicked") || text.contains("overflowed its stack") {
        codes.push("crash".to_string());
    }
    codes.sort();
    codes
}

#[test]
fn a_limit_decides_only_answers_that_depend_on_it_through_the_pipeline() {
    check_all(&codes, 8);
}
