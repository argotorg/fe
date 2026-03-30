use std::{
    ffi::OsStr,
    fs,
    io::Read,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    thread::{self, sleep},
    time::{Duration, Instant},
};

const FE_CHECK_TIMEOUT_STANDALONE: Duration = Duration::from_secs(5);
const FE_CHECK_TIMEOUT_PROJECT: Duration = Duration::from_secs(60);

struct FeCheckRun {
    code: i32,
    combined: String,
    timed_out: bool,
}

fn fixtures_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/crash_regressions")
}

fn is_fe_file(path: &Path) -> bool {
    path.extension().and_then(OsStr::to_str) == Some("fe")
}

fn is_project_fixture(path: &Path) -> bool {
    path.is_dir() && path.join("fe.toml").is_file()
}

fn spawn_reader<R: Read + Send + 'static>(mut pipe: R) -> thread::JoinHandle<Vec<u8>> {
    thread::spawn(move || {
        let mut buf = Vec::new();
        pipe.read_to_end(&mut buf).unwrap_or_else(|err| {
            panic!("failed to read `fe check` output stream: {err}");
        });
        buf
    })
}

fn fe_check_timeout(path: &Path) -> Duration {
    if path.is_file() {
        FE_CHECK_TIMEOUT_STANDALONE
    } else {
        FE_CHECK_TIMEOUT_PROJECT
    }
}

fn run_fe_check(path: &Path) -> FeCheckRun {
    let mut command = Command::new(env!("CARGO_BIN_EXE_fe"));
    command.arg("check");
    if path.is_file() {
        command.arg("--standalone");
    }
    let mut child = command
        .args(["--color", "never"])
        .arg(path)
        .env("NO_COLOR", "1")
        .env("RUST_BACKTRACE", "0")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|err| panic!("failed to run `fe check` on {path:?}: {err}"));

    // Drain both pipes concurrently to avoid deadlocking when diagnostics exceed pipe capacity.
    let stdout_handle = spawn_reader(
        child
            .stdout
            .take()
            .unwrap_or_else(|| panic!("missing stdout pipe for `fe check` on {path:?}")),
    );
    let stderr_handle = spawn_reader(
        child
            .stderr
            .take()
            .unwrap_or_else(|| panic!("missing stderr pipe for `fe check` on {path:?}")),
    );

    let start = Instant::now();
    let timeout = fe_check_timeout(path);
    let timed_out = loop {
        if child
            .try_wait()
            .unwrap_or_else(|err| panic!("failed to poll `fe check` on {path:?}: {err}"))
            .is_some()
        {
            break false;
        }

        if start.elapsed() >= timeout {
            let _ = child.kill();
            break true;
        }

        sleep(Duration::from_millis(10));
    };

    let status = child
        .wait()
        .unwrap_or_else(|err| panic!("failed to wait for `fe check` on {path:?}: {err}"));

    let stdout = stdout_handle
        .join()
        .unwrap_or_else(|_| panic!("stdout reader thread panicked for {path:?}"));
    let stderr = stderr_handle
        .join()
        .unwrap_or_else(|_| panic!("stderr reader thread panicked for {path:?}"));

    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&stdout),
        String::from_utf8_lossy(&stderr)
    );

    let code = status.code().unwrap_or(-1);
    FeCheckRun {
        code,
        combined,
        timed_out,
    }
}

#[test]
fn crash_regressions_do_not_panic() {
    let dir = fixtures_dir();
    let mut fixtures: Vec<PathBuf> = fs::read_dir(&dir)
        .unwrap_or_else(|err| panic!("failed to read crash regressions dir {dir:?}: {err}"))
        .filter_map(|entry| entry.ok().map(|e| e.path()))
        .filter(|path| is_fe_file(path) || is_project_fixture(path))
        .collect();
    fixtures.sort();

    assert!(
        !fixtures.is_empty(),
        "no crash regression fixtures found under {dir:?}"
    );

    for fixture in fixtures {
        let run = run_fe_check(&fixture);
        let code = run.code;
        let combined = run.combined;

        assert!(
            !run.timed_out,
            "`fe check` timed out after {:?} on {fixture:?}\n{combined}",
            fe_check_timeout(&fixture)
        );
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
