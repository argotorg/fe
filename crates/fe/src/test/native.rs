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
use codegen::{OptLevel, emit_test_module_native};
use driver::DriverDataBase;
use hir::hir_def::TopLevelMod;
use tempfile::{Builder, TempDir};

use crate::native::link_executable;

use super::{
    CompiledTest, NativeTestLimits, ReportContext, SingleTestJob, SuitePreparation,
    TestEmitSelection, TestOutcome, TestResult, default_test_emit_dir, emit_with_catch_unwind,
    has_test_functions, test_emit_stem, write_test_emit_artifact,
};

#[cfg(target_os = "linux")]
const NATIVE_TEST_ADDRESS_SPACE_LIMIT: libc::rlim_t = 2 * 1024 * 1024 * 1024;
#[cfg(any(target_os = "linux", target_os = "macos"))]
const NATIVE_TEST_DIAGNOSTIC_LIMIT: usize = 4096;

#[derive(Debug)]
struct NativeExecutable {
    directory: TempDir,
}

#[derive(Debug, Clone)]
pub(super) struct NativeTestCase {
    name: String,
    module_index: usize,
    entry_index: usize,
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
        let result = (|| -> io::Result<_> {
            let mut command = Command::new(self.executable.directory.path().join("tests"));
            command
                .arg(self.entry_index.to_string())
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
                        (libc::RLIMIT_CORE, 0),
                    ];
                    for (resource, limit) in limits {
                        let mut value = libc::rlimit {
                            rlim_cur: 0,
                            rlim_max: 0,
                        };
                        if libc::getrlimit(resource, &mut value) != 0 {
                            return Err(io::Error::last_os_error());
                        }
                        value.rlim_cur = value.rlim_cur.min(limit);
                        value.rlim_max = value.rlim_max.min(limit);
                        if libc::setrlimit(resource, &value) != 0 {
                            return Err(io::Error::last_os_error());
                        }
                    }
                    #[cfg(target_os = "linux")]
                    {
                        let mut value = libc::rlimit {
                            rlim_cur: 0,
                            rlim_max: 0,
                        };
                        if libc::getrlimit(libc::RLIMIT_AS, &mut value) != 0 {
                            return Err(io::Error::last_os_error());
                        }
                        value.rlim_cur = value.rlim_cur.min(NATIVE_TEST_ADDRESS_SPACE_LIMIT);
                        value.rlim_max = value.rlim_max.min(NATIVE_TEST_ADDRESS_SPACE_LIMIT);
                        if libc::setrlimit(libc::RLIMIT_AS, &value) != 0 {
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
        let (passed, error_message) = match result {
            Ok((status, stdout, stderr, reason)) => {
                if let Some(report) = report {
                    let dir = report.root_dir.join(format!(
                        "artifacts/native/module-{}/test-{}",
                        self.module_index, self.entry_index
                    ));
                    let _ = fs::create_dir_all(&dir);
                    let _ = fs::write(dir.join("stdout.txt"), &stdout);
                    let _ = fs::write(dir.join("stderr.txt"), &stderr);
                    let _ = fs::write(dir.join("status.txt"), status.to_string());
                }
                (
                    status.success() && reason.is_none(),
                    (!status.success() || reason.is_some()).then(|| {
                        let stdout_preview = String::from_utf8_lossy(
                            &stdout[..stdout.len().min(NATIVE_TEST_DIAGNOSTIC_LIMIT)],
                        );
                        let stderr_preview = String::from_utf8_lossy(
                            &stderr[..stderr.len().min(NATIVE_TEST_DIAGNOSTIC_LIMIT)],
                        );
                        format!(
                            "native test failed ({status}){}\nstdout:\n{stdout_preview}{}\nstderr:\n{stderr_preview}{}",
                            reason
                                .map(|reason| format!(": {reason}"))
                                .unwrap_or_default(),
                            if stdout.len() > NATIVE_TEST_DIAGNOSTIC_LIMIT { "\n[output truncated]" } else { "" },
                            if stderr.len() > NATIVE_TEST_DIAGNOSTIC_LIMIT { "\n[output truncated]" } else { "" },
                        )
                    }),
                )
            }
            Err(err) => (false, Some(format!("failed to execute native test: {err}"))),
        };
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
            let mut jobs = Vec::new();
            for (module_index, top_mod) in top_mods.into_iter().enumerate() {
                if !has_test_functions(db, top_mod) {
                    continue;
                }
                let Some(module) = emit_test_module_native(
                    db,
                    top_mod,
                    opt_level,
                    filter,
                    emit.ir || report.is_some(),
                    emit.rmir || report.is_some(),
                )
                .map_err(|err| err.to_string())?
                else {
                    continue;
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

                // Each test starts in its own process; every test in this module
                // shares the same compiled object and linked dispatcher.
                let mut source = String::from("#include <string.h>\n");
                for test in &module.tests {
                    let symbol = &test.symbol;
                    let _ = writeln!(source, "extern void {symbol}(void);");
                }
                source.push_str("int main(int argc, char **argv) {\nif (argc != 2) return 2;\n");
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
                    let stem = format!("{}.native-{module_index}", test_emit_stem(suite_key));
                    if emit.ir
                        && let Some(ir) = &module.ir
                    {
                        write_test_emit_artifact(out_dir, &stem, "sona", ir, &mut artifact_output)?;
                    }
                    if emit.rmir
                        && let Some(rmir) = &module.rmir
                    {
                        write_test_emit_artifact(
                            out_dir,
                            &stem,
                            "rmir",
                            rmir,
                            &mut artifact_output,
                        )?;
                    }
                }
                if let Some(report) = report {
                    let dir = report
                        .root_dir
                        .join(format!("artifacts/native/module-{module_index}"));
                    fs::create_dir_all(&dir).map_err(|err| err.to_string())?;
                    for filename in ["tests.o", "launcher.c", "tests"] {
                        fs::copy(directory.path().join(filename), dir.join(filename)).map_err(
                            |err| format!("failed to retain native test {filename}: {err}"),
                        )?;
                    }
                    if let Some(ir) = &module.ir {
                        fs::write(dir.join("tests.sona"), ir).map_err(|err| err.to_string())?;
                    }
                    if let Some(rmir) = &module.rmir {
                        fs::write(dir.join("tests.rmir"), rmir).map_err(|err| err.to_string())?;
                    }
                }
                let executable = Arc::new(NativeExecutable { directory });
                jobs.extend(
                    module
                        .tests
                        .into_iter()
                        .enumerate()
                        .map(|(entry_index, test)| SingleTestJob {
                            suite_key: suite_key.to_string(),
                            case: CompiledTest::Native(NativeTestCase {
                                name: test.name,
                                module_index,
                                entry_index,
                                executable: Arc::clone(&executable),
                            }),
                            evm_trace: None,
                            report_root: report.map(|ctx| ctx.root_dir.clone()),
                        }),
                );
            }
            Ok(jobs)
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
    match error.raw_os_error() {
        Some(libc::ESRCH) => true,
        Some(libc::EPERM) => cfg!(target_os = "macos"),
        _ => false,
    }
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
            module_index: 0,
            entry_index: 0,
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
