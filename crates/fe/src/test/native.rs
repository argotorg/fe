use std::{fmt::Write, fs, sync::Arc, time::Instant};

#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::{
    io::{self, Read},
    os::fd::AsRawFd,
    os::unix::process::CommandExt,
    process::{Command, Stdio},
    time::Duration,
};

use camino::Utf8PathBuf;
use codegen::{ExpectedRevert, OptLevel, emit_test_suite_native};
use driver::DriverDataBase;
use hir::hir_def::TopLevelMod;
use tempfile::{Builder, TempDir};

use crate::native::link_executable;

use super::{
    CompiledTest, NativeTestLimits, ReportContext, SingleTestJob, SuitePreparation,
    TestEmitSelection, TestOutcome, TestResult, default_test_emit_dir, emit_with_catch_unwind,
    revert_mismatch, test_emit_stem, write_test_emit_artifact,
};

#[cfg(target_os = "linux")]
const NATIVE_TEST_ADDRESS_SPACE_LIMIT: libc::rlim_t = 2 * 1024 * 1024 * 1024;
#[cfg(any(target_os = "linux", target_os = "macos"))]
const NATIVE_TEST_DIAGNOSTIC_LIMIT: usize = 4096;
/// The launcher's exit status after it records a test's revert data.
const NATIVE_TEST_REVERT_STATUS: i32 = 101;

#[derive(Debug)]
struct NativeExecutable {
    directory: TempDir,
}

#[derive(Debug, Clone)]
pub(super) struct NativeTestCase {
    name: String,
    entry_index: usize,
    expected_revert: Option<ExpectedRevert>,
    executable: Arc<NativeExecutable>,
}

impl NativeTestCase {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    pub(super) fn run(
        &self,
        report: Option<&ReportContext>,
        limits: NativeTestLimits,
    ) -> TestOutcome {
        let started = Instant::now();
        let revert_path = self
            .executable
            .directory
            .path()
            .join(format!("revert-{}", self.entry_index));
        let result = (|| -> io::Result<_> {
            let mut command = Command::new(self.executable.directory.path().join("tests"));
            command
                .arg(self.entry_index.to_string())
                .arg(&revert_path)
                .stdin(Stdio::null())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .process_group(0);
            // Set resource limits in the child, before it runs generated code.
            // macOS cannot reliably lower RLIMIT_AS below its mapped shared cache;
            // memory quotas there must be enforced by the caller's OS isolation.
            unsafe {
                command.pre_exec(move || {
                    let limits = [
                        (
                            libc::RLIMIT_CPU,
                            limits.timeout.as_secs().saturating_add(1) as libc::rlim_t,
                        ),
                        // A core limit of 1 writes no core file and, unlike 0, keeps
                        // the kernel from piping each trap's core to a crash reporter
                        // such as apport, which takes about a second per trap.
                        (libc::RLIMIT_CORE, 1),
                        #[cfg(target_os = "linux")]
                        (libc::RLIMIT_AS, NATIVE_TEST_ADDRESS_SPACE_LIMIT),
                    ];
                    for (resource, limit) in limits {
                        let mut value = libc::rlimit {
                            rlim_cur: 0,
                            rlim_max: 0,
                        };
                        if libc::getrlimit(resource, &mut value) != 0 {
                            return Err(io::Error::last_os_error());
                        }
                        value.rlim_max = value.rlim_max.min(limit);
                        // Raise a lower soft core limit, such as 0, to exactly 1.
                        value.rlim_cur = if resource == libc::RLIMIT_CORE {
                            value.rlim_max
                        } else {
                            value.rlim_cur.min(limit)
                        };
                        if libc::setrlimit(resource, &value) != 0 {
                            return Err(io::Error::last_os_error());
                        }
                    }
                    Ok(())
                });
            }
            let mut child = command
                .spawn()
                .map_err(|err| io::Error::new(err.kind(), format!("spawn native test: {err}")))?;
            let child_pid = child.id() as i32;
            let mut stdout = child.stdout.take().expect("piped stdout");
            let mut stderr = child.stderr.take().expect("piped stderr");
            let mut stdout_bytes = Vec::new();
            let mut stderr_bytes = Vec::new();
            let mut pipes = [
                libc::pollfd {
                    fd: stdout.as_raw_fd(),
                    events: libc::POLLIN | libc::POLLHUP | libc::POLLERR,
                    revents: 0,
                },
                libc::pollfd {
                    fd: stderr.as_raw_fd(),
                    events: libc::POLLIN | libc::POLLHUP | libc::POLLERR,
                    revents: 0,
                },
            ];
            for pipe in &pipes {
                let flags = unsafe { libc::fcntl(pipe.fd, libc::F_GETFL) };
                if flags < 0
                    || unsafe { libc::fcntl(pipe.fd, libc::F_SETFL, flags | libc::O_NONBLOCK) } < 0
                {
                    let error = io::Error::last_os_error();
                    let _ = unsafe { libc::killpg(child.id() as i32, libc::SIGKILL) };
                    let _ = child.wait();
                    return Err(error);
                }
            }
            let deadline = started + limits.timeout;
            let execution = (|| -> io::Result<Option<&'static str>> {
                let mut exited = false;
                let mut captured = 0;
                loop {
                    if !exited && child.try_wait()?.is_some() {
                        exited = true;
                        // Descendants can outlive the launcher and hold pipes open.
                        let _ = unsafe { libc::killpg(child_pid, libc::SIGKILL) };
                    }
                    if exited && pipes.iter().all(|pipe| pipe.fd == -1) {
                        break Ok(None);
                    }
                    let remaining = deadline.saturating_duration_since(Instant::now());
                    if remaining.is_zero() {
                        break Ok(Some("native test exceeded the time limit"));
                    }
                    let timeout = remaining.min(Duration::from_millis(10)).as_millis() as i32;
                    let ready =
                        unsafe { libc::poll(pipes.as_mut_ptr(), pipes.len() as _, timeout) };
                    if ready < 0 {
                        let error = io::Error::last_os_error();
                        if error.kind() == io::ErrorKind::Interrupted {
                            continue;
                        }
                        return Err(error);
                    }
                    for (pipe, (reader, bytes)) in pipes.iter_mut().zip([
                        (&mut stdout as &mut dyn Read, &mut stdout_bytes),
                        (&mut stderr as &mut dyn Read, &mut stderr_bytes),
                    ]) {
                        if pipe.fd == -1 || pipe.revents == 0 {
                            continue;
                        }
                        let mut buffer = [0_u8; 8192];
                        loop {
                            match reader.read(&mut buffer) {
                                Ok(0) => {
                                    pipe.fd = -1;
                                    break;
                                }
                                Ok(n) => {
                                    let remaining = limits.output_bytes - captured;
                                    bytes.extend_from_slice(&buffer[..n.min(remaining)]);
                                    captured += n.min(remaining);
                                    if n > remaining {
                                        return Ok(Some("native test exceeded the output limit"));
                                    }
                                }
                                Err(err) if err.kind() == io::ErrorKind::WouldBlock => break,
                                Err(err) if err.kind() == io::ErrorKind::Interrupted => continue,
                                Err(err) => return Err(err),
                            }
                        }
                    }
                }
            })();
            // The launcher can exit while descendants still run. Stop the whole
            // process group on every path, then reap the launcher.
            let kill_result = unsafe { libc::killpg(child_pid, libc::SIGKILL) };
            let kill_error = io::Error::last_os_error();
            let status = child.wait()?;
            if kill_result != 0 && !process_group_is_gone(&kill_error) {
                let group_probe = unsafe { libc::killpg(child_pid, 0) };
                let group_probe_error = io::Error::last_os_error();
                if group_probe == 0 || !process_group_is_gone(&group_probe_error) {
                    return Err(io::Error::new(
                        kill_error.kind(),
                        format!("terminate native test process group: {kill_error}"),
                    ));
                }
            }
            let reason = execution?;
            Ok((status, stdout_bytes, stderr_bytes, reason))
        })();
        let error_message = match result {
            Ok((status, stdout, stderr, reason)) => {
                if let Some(report) = report {
                    let dir = report
                        .root_dir
                        .join(format!("artifacts/native/test-{}", self.entry_index));
                    let _ = fs::create_dir_all(&dir);
                    let _ = fs::write(dir.join("stdout.txt"), &stdout);
                    let _ = fs::write(dir.join("stderr.txt"), &stderr);
                    let _ = fs::write(dir.join("status.txt"), status.to_string());
                }
                let describe = |summary: String| {
                    let stdout_preview = String::from_utf8_lossy(
                        &stdout[..stdout.len().min(NATIVE_TEST_DIAGNOSTIC_LIMIT)],
                    );
                    let stderr_preview = String::from_utf8_lossy(
                        &stderr[..stderr.len().min(NATIVE_TEST_DIAGNOSTIC_LIMIT)],
                    );
                    format!(
                        "{summary}\nstdout:\n{stdout_preview}{}\nstderr:\n{stderr_preview}{}",
                        if stdout.len() > NATIVE_TEST_DIAGNOSTIC_LIMIT {
                            "\n[output truncated]"
                        } else {
                            ""
                        },
                        if stderr.len() > NATIVE_TEST_DIAGNOSTIC_LIMIT {
                            "\n[output truncated]"
                        } else {
                            ""
                        },
                    )
                };
                let failure = || {
                    describe(format!(
                        "native test failed ({status}){}",
                        reason
                            .map(|reason| format!(": {reason}"))
                            .unwrap_or_default()
                    ))
                };
                // A test that exits with the revert status without the
                // launcher's record of its data failed otherwise.
                let revert = (reason.is_none() && status.code() == Some(NATIVE_TEST_REVERT_STATUS))
                    .then(|| fs::read(&revert_path).ok())
                    .flatten();
                let succeeded = status.success() && reason.is_none();
                match (revert, &self.expected_revert) {
                    (Some(data), Some(expected)) => revert_mismatch(expected, &data),
                    (Some(data), None) => {
                        Some(describe(format!("Test reverted: 0x{}", hex::encode(data))))
                    }
                    (None, Some(_)) if succeeded => {
                        Some("Expected test to revert, but it succeeded".to_string())
                    }
                    (None, Some(_)) => Some(format!(
                        "Expected test to revert, but it failed with: {}",
                        failure()
                    )),
                    (None, None) => (!succeeded).then(failure),
                }
            }
            Err(err) => Some(format!("failed to execute native test: {err}")),
        };
        let passed = error_message.is_none();
        TestOutcome {
            result: TestResult {
                name: self.name.clone(),
                passed,
                error_message,
            },
            logs: Vec::new(),
            trace: None,
            elapsed: started.elapsed(),
        }
    }

    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    pub(super) fn run(
        &self,
        _report: Option<&ReportContext>,
        _limits: NativeTestLimits,
    ) -> TestOutcome {
        TestOutcome {
            result: TestResult {
                name: self.name.clone(),
                passed: false,
                error_message: Some("native tests require Linux or macOS".to_string()),
            },
            logs: Vec::new(),
            trace: None,
            elapsed: Default::default(),
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn prepare_tests(
    db: &DriverDataBase,
    top_mods: Vec<TopLevelMod<'_>>,
    source_path: &Utf8PathBuf,
    suite: &str,
    suite_key: &str,
    filter: Option<&str>,
    opt_level: OptLevel,
    emit: TestEmitSelection,
    report: Option<&ReportContext>,
    output: &mut String,
) -> SuitePreparation {
    let mut artifact_output = String::new();
    let prepared = emit_with_catch_unwind(
        || -> Result<Vec<SingleTestJob>, String> {
            if !cfg!(any(
                all(target_arch = "x86_64", target_os = "linux"),
                all(target_arch = "aarch64", target_os = "macos")
            )) {
                return Err("native test execution supports x86-64 Linux and AArch64 macOS".into());
            }
            let out_dir = (!emit.is_empty())
                .then(|| default_test_emit_dir(source_path))
                .transpose()?;
            let Some(module) = emit_test_suite_native(
                db,
                &top_mods,
                opt_level,
                filter,
                emit.ir || report.is_some(),
                emit.rmir || report.is_some(),
            )
            .map_err(|err| err.to_string())?
            else {
                return Ok(Vec::new());
            };
            let directory = Builder::new()
                .prefix("fe-native-test-")
                .tempdir()
                .map_err(|err| format!("failed to create native test directory: {err}"))?;
            let object = directory.path().join("tests.o");
            let launcher = directory.path().join("launcher.c");
            let executable_path = directory.path().join("tests");
            fs::write(&object, &module.object)
                .map_err(|err| format!("failed to write native test object: {err}"))?;

            // Each test starts in its own process; every test in this suite
            // shares the same compiled object and linked dispatcher. A revert
            // ends the test in `__fe_revert`, which records the revert data
            // in the file the runner names.
            let mut source = format!(
                r#"#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
static const char *revert_path;
void __fe_revert(const unsigned char *data, int64_t len) {{
fflush(NULL);
FILE *file = fopen(revert_path, "wb");
if (!file || fwrite(data, 1, (size_t)len, file) != (size_t)len || fclose(file) != 0) abort();
_exit({NATIVE_TEST_REVERT_STATUS});
}}
"#
            );
            for test in &module.tests {
                let symbol = &test.symbol;
                let _ = writeln!(source, "extern void {symbol}(void);");
            }
            source.push_str(
                "int main(int argc, char **argv) {\nif (argc != 3) return 2;\nrevert_path = argv[2];\n",
            );
            for (index, test) in module.tests.iter().enumerate() {
                let symbol = &test.symbol;
                let _ = writeln!(
                    source,
                    "if (strcmp(argv[1], \"{index}\") == 0) {{ {symbol}(); return 0; }}"
                );
            }
            source.push_str("return 2;\n}\n");
            fs::write(&launcher, source)
                .map_err(|err| format!("failed to write native test launcher: {err}"))?;
            link_executable(&[&launcher, &object], &executable_path)?;

            if let Some(out_dir) = &out_dir {
                let stem = format!("{}.native", test_emit_stem(suite_key));
                if emit.ir
                    && let Some(ir) = &module.ir
                {
                    write_test_emit_artifact(out_dir, &stem, "sona", ir, &mut artifact_output)?;
                }
                if emit.rmir
                    && let Some(rmir) = &module.rmir
                {
                    write_test_emit_artifact(out_dir, &stem, "rmir", rmir, &mut artifact_output)?;
                }
            }
            if let Some(report) = report {
                let dir = report.root_dir.join("artifacts/native");
                fs::create_dir_all(&dir).map_err(|err| err.to_string())?;
                for filename in ["tests.o", "launcher.c", "tests"] {
                    fs::copy(directory.path().join(filename), dir.join(filename))
                        .map_err(|err| format!("failed to retain native test {filename}: {err}"))?;
                }
                if let Some(ir) = &module.ir {
                    fs::write(dir.join("tests.sona"), ir).map_err(|err| err.to_string())?;
                }
                if let Some(rmir) = &module.rmir {
                    fs::write(dir.join("tests.rmir"), rmir).map_err(|err| err.to_string())?;
                }
            }
            let executable = Arc::new(NativeExecutable { directory });
            Ok(module
                .tests
                .into_iter()
                .enumerate()
                .map(|(entry_index, test)| SingleTestJob {
                    suite_key: suite_key.to_string(),
                    case: CompiledTest::Native(NativeTestCase {
                        name: test.name,
                        entry_index,
                        expected_revert: test.expected_revert,
                        executable: Arc::clone(&executable),
                    }),
                    evm_trace: None,
                    report_root: report.map(|ctx| ctx.root_dir.clone()),
                })
                .collect())
        },
        "native",
        suite,
        report,
        output,
    );
    output.push_str(&artifact_output);
    match prepared {
        Ok(single_jobs) => SuitePreparation {
            results: Vec::new(),
            single_jobs,
        },
        Err(results) => SuitePreparation {
            results,
            single_jobs: Vec::new(),
        },
    }
}

/// Whether a failed `killpg` means the test's process group has no member left to stop.
/// Every member runs as our user, so a permission error cannot mean a live process we may
/// not signal. Linux reports `ESRCH` once the group is empty; macOS can report `EPERM`
/// while exited members are still waiting to be reaped.
#[cfg(any(target_os = "linux", target_os = "macos"))]
fn process_group_is_gone(error: &io::Error) -> bool {
    let code = error.raw_os_error();
    code == Some(libc::ESRCH) || (cfg!(target_os = "macos") && code == Some(libc::EPERM))
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
mod tests {
    use std::{os::unix::fs::PermissionsExt, thread, time::Duration};

    use super::*;

    #[test]
    fn process_group_cleanup_accepts_only_errors_that_mean_the_group_is_gone() {
        assert!(process_group_is_gone(&io::Error::from_raw_os_error(
            libc::ESRCH
        )));
        assert_eq!(
            process_group_is_gone(&io::Error::from_raw_os_error(libc::EPERM)),
            cfg!(target_os = "macos")
        );
        assert!(!process_group_is_gone(&io::Error::from_raw_os_error(
            libc::EINVAL
        )));
    }

    #[test]
    fn native_runner_stops_descendants_after_launcher_exits() {
        let directory = tempfile::tempdir().unwrap();
        let marker = directory.path().join("child-survived");
        let executable = directory.path().join("tests");
        fs::write(
            &executable,
            format!(
                "#!/bin/sh\nnohup sh -c 'sleep 2; echo survived > \"$1\"' sh '{}' >/dev/null 2>&1 &\n",
                marker.display()
            ),
        )
        .unwrap();
        fs::set_permissions(&executable, fs::Permissions::from_mode(0o700)).unwrap();
        let case = NativeTestCase {
            name: "forking".to_string(),
            entry_index: 0,
            expected_revert: None,
            executable: Arc::new(NativeExecutable { directory }),
        };
        let outcome = case.run(
            None,
            NativeTestLimits {
                timeout: Duration::from_secs(5),
                output_bytes: 1024,
            },
        );
        assert!(outcome.result.passed, "{:?}", outcome.result);
        thread::sleep(Duration::from_secs(3));
        assert!(!marker.exists(), "native test descendant survived cleanup");
    }
}
