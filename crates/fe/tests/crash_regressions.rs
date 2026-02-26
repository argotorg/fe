use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
    process::Command,
    sync::OnceLock,
};

fn fe_binary() -> PathBuf {
    static BIN: OnceLock<PathBuf> = OnceLock::new();
    BIN.get_or_init(|| {
        if let Some(bin) = std::env::var_os("CARGO_BIN_EXE_fe") {
            return PathBuf::from(bin);
        }

        let cargo_exe = std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_string());

        let mut build = Command::new(&cargo_exe);
        build.args(["build", "--bin", "fe", "--no-default-features"]);

        let mut features = Vec::new();
        if cfg!(feature = "lsp") {
            features.push("lsp");
        }
        if cfg!(feature = "vendored-openssl") {
            features.push("vendored-openssl");
        }
        if !features.is_empty() {
            build.args(["--features", &features.join(",")]);
        }

        build.current_dir(env!("CARGO_MANIFEST_DIR"));
        let output = build.output().expect("failed to build `fe` binary");
        if !output.status.success() {
            panic!(
                "failed to build `fe` binary:\n{}",
                String::from_utf8_lossy(&output.stderr)
            );
        }

        std::env::current_exe()
            .expect("failed to get current exe")
            .parent()
            .expect("failed to get parent")
            .parent()
            .expect("failed to get parent")
            .join(format!("fe{}", std::env::consts::EXE_SUFFIX))
    })
    .clone()
}

fn fixtures_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/crash_regressions")
}

fn is_fe_file(path: &Path) -> bool {
    path.extension().and_then(OsStr::to_str) == Some("fe")
}

fn run_fe_check(path: &Path) -> (i32, String) {
    let output = Command::new(fe_binary())
        .args(["check", "--standalone", "--color", "never"])
        .arg(path)
        .env("NO_COLOR", "1")
        .env("RUST_BACKTRACE", "0")
        .output()
        .unwrap_or_else(|err| panic!("failed to run `fe check` on {path:?}: {err}"));

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{stdout}{stderr}");

    let code = output
        .status
        .code()
        .unwrap_or_else(|| panic!("`fe check` terminated by signal for {path:?}\n{combined}"));
    (code, combined)
}

#[test]
fn crash_regressions_do_not_panic() {
    let dir = fixtures_dir();
    let mut fixtures: Vec<PathBuf> = fs::read_dir(&dir)
        .unwrap_or_else(|err| panic!("failed to read crash regressions dir {dir:?}: {err}"))
        .filter_map(|entry| entry.ok().map(|e| e.path()))
        .filter(|path| is_fe_file(path))
        .collect();
    fixtures.sort();

    assert!(
        !fixtures.is_empty(),
        "no crash regression fixtures found under {dir:?}"
    );

    for fixture in fixtures {
        let (code, combined) = run_fe_check(&fixture);

        assert_ne!(code, 101, "`fe check` panicked on {fixture:?}\n{combined}");
        assert!(
            !combined.contains("panicked at")
                && !combined.contains("thread 'main' panicked")
                && !combined.contains("stack backtrace:"),
            "`fe check` produced panic output on {fixture:?}\n{combined}"
        );
        assert!(
            code == 0 || code == 1,
            "`fe check` returned unexpected exit code {code} on {fixture:?}\n{combined}"
        );
    }
}
