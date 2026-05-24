use std::path::Path;
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=tests/fixtures");
    println!("cargo:rerun-if-changed=tests/doc_fixtures");
    println!("cargo:rerun-if-env-changed=FE_GIT_COMMIT");

    if let Some(commit) = std::env::var("FE_GIT_COMMIT")
        .ok()
        .or_else(current_git_commit)
    {
        println!("cargo:rustc-env=FE_GIT_COMMIT={commit}");
    }
}

fn current_git_commit() -> Option<String> {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").ok()?;
    let repo_root = Path::new(&manifest_dir).join("../..");
    let output = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(repo_root)
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let commit = String::from_utf8(output.stdout).ok()?.trim().to_string();
    (!commit.is_empty()).then_some(commit)
}
