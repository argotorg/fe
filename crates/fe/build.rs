use std::{
    env,
    path::{Path, PathBuf},
};

fn main() {
    println!("cargo:rerun-if-changed=tests/fixtures");
    println!("cargo:rerun-if-changed=tests/doc_fixtures");
    println!("cargo:rerun-if-env-changed=FE_GIT_HASH");

    if let Ok(override_hash) = env::var("FE_GIT_HASH")
        && !override_hash.trim().is_empty()
    {
        println!("cargo:rustc-env=FE_GIT_HASH={}", override_hash.trim());
        return;
    }

    let Some(repo) = git_repo() else {
        return;
    };

    let Ok(head_id) = repo.head_id() else {
        return;
    };

    emit_git_rerun_paths(&repo);
    let hash = head_id.shorten_or_id();
    println!("cargo:rustc-env=FE_GIT_HASH={hash}");
}

fn git_repo() -> Option<gix::Repository> {
    let manifest_dir = env::var_os("CARGO_MANIFEST_DIR").map(PathBuf::from)?;
    let workspace_root = manifest_dir.parent()?.parent()?;

    // Open the expected Fe workspace root directly so source exports nested in
    // another checkout do not inherit that checkout's HEAD.
    gix::open(workspace_root).ok()
}

fn emit_git_rerun_paths(repo: &gix::Repository) {
    emit_rerun_path(repo.git_dir().join("HEAD"));
    emit_rerun_path(repo.common_dir().join("packed-refs"));

    if let Ok(Some(head_ref)) = repo.head_ref() {
        emit_rerun_path(repo.common_dir().join(head_ref.name().to_path()));
    }
}

fn emit_rerun_path(path: impl AsRef<Path>) {
    println!("cargo:rerun-if-changed={}", path.as_ref().display());
}
