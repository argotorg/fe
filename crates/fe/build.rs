use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=.git/HEAD");
    println!("cargo:rerun-if-env-changed=FE_GIT_HASH");

    if let Ok(override_hash) = std::env::var("FE_GIT_HASH")
        && !override_hash.trim().is_empty()
    {
        println!("cargo:rustc-env=FE_GIT_HASH={}", override_hash.trim());
        return;
    }

    let output = Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .output();

    let Some(hash) = output
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
        .filter(|s| !s.is_empty())
    else {
        return;
    };

    println!("cargo:rustc-env=FE_GIT_HASH={hash}");
}
