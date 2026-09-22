use std::{fmt::Write, fs, process::Command, sync::Arc, time::Instant};

use camino::Utf8PathBuf;
use codegen::{OptLevel, emit_test_module_native};
use driver::DriverDataBase;
use hir::hir_def::TopLevelMod;
use tempfile::{Builder, TempDir};

use crate::native::link_executable;

use super::{
    CompiledTest, ReportContext, SingleTestJob, SuitePreparation, TestEmitSelection, TestOutcome,
    TestResult, default_test_emit_dir, emit_with_catch_unwind, has_test_functions, test_emit_stem,
    write_test_emit_artifact,
};

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
    pub(super) fn run(&self, report: Option<&ReportContext>) -> TestOutcome {
        let started = Instant::now();
        let result = Command::new(self.executable.directory.path().join("tests"))
            .arg(self.entry_index.to_string())
            .output();
        let (passed, error_message) = match result {
            Ok(output) => {
                if let Some(report) = report {
                    let dir = report.root_dir.join(format!(
                        "artifacts/native/module-{}/test-{}",
                        self.module_index, self.entry_index
                    ));
                    let _ = fs::create_dir_all(&dir);
                    let _ = fs::write(dir.join("stdout.txt"), &output.stdout);
                    let _ = fs::write(dir.join("stderr.txt"), &output.stderr);
                    let _ = fs::write(dir.join("status.txt"), output.status.to_string());
                }
                (
                    output.status.success(),
                    (!output.status.success()).then(|| {
                        format!(
                            "native test failed ({})\nstdout:\n{}\nstderr:\n{}",
                            output.status,
                            String::from_utf8_lossy(&output.stdout),
                            String::from_utf8_lossy(&output.stderr)
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
