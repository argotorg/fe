use camino::Utf8PathBuf;
use std::{cell::RefCell, sync::OnceLock};

pub fn sanitize_filename(component: &str) -> String {
    component
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect()
}

pub fn normalize_report_out_path(out: &Utf8PathBuf) -> Utf8PathBuf {
    let s = out.as_str();
    if !s.ends_with(".tar.gz") {
        eprintln!("Error: report output path must end with `.tar.gz`: `{out}`");
        std::process::exit(1);
    }

    if !out.exists() {
        return out.clone();
    }

    let base = s.strip_suffix(".tar.gz").expect("checked .tar.gz suffix");
    for idx in 1.. {
        let candidate = Utf8PathBuf::from(format!("{base}-{idx}.tar.gz"));
        if !candidate.exists() {
            return candidate;
        }
    }
    unreachable!()
}

pub fn create_dir_all_utf8(path: &Utf8PathBuf) {
    if let Err(err) = std::fs::create_dir_all(path) {
        eprintln!("Error: failed to create dir `{path}`: {err}");
        std::process::exit(1);
    }
}

pub fn create_report_staging_dir(base: &str) -> Utf8PathBuf {
    let base = Utf8PathBuf::from(base);
    let _ = std::fs::create_dir_all(&base);
    let pid = std::process::id();
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let dir = base.join(format!("report-{pid}-{nanos}"));
    create_dir_all_utf8(&dir);
    dir
}

#[derive(Debug, Clone)]
pub struct ReportStaging {
    pub root_dir: Utf8PathBuf,
    pub temp_dir: Utf8PathBuf,
}

pub fn create_report_staging_root(base: &str, root_name: &str) -> ReportStaging {
    let temp_dir = create_report_staging_dir(base);
    let root_dir = temp_dir.join(root_name);
    create_dir_all_utf8(&root_dir);
    ReportStaging { root_dir, temp_dir }
}

pub fn tar_gz_dir(staging: &Utf8PathBuf, out: &Utf8PathBuf) -> Result<(), String> {
    let parent = staging
        .parent()
        .ok_or_else(|| "missing staging parent".to_string())?;
    let name = staging
        .file_name()
        .ok_or_else(|| "missing staging basename".to_string())?;

    let status = std::process::Command::new("tar")
        .arg("-czf")
        .arg(out.as_str())
        .arg("-C")
        .arg(parent.as_str())
        .arg(name)
        .status()
        .map_err(|err| format!("failed to run tar: {err}"))?;

    if !status.success() {
        return Err(format!("tar exited with status {status}"));
    }
    Ok(())
}

pub fn copy_input_into_report(input: &Utf8PathBuf, inputs_dir: &Utf8PathBuf) {
    if input.is_file() {
        let name = input
            .file_name()
            .map(|s| s.to_string())
            .unwrap_or_else(|| "input.fe".to_string());
        let dest = inputs_dir.join(name);
        if let Err(err) = std::fs::copy(input, &dest) {
            eprintln!("Error: failed to copy `{input}` to `{dest}`: {err}");
            std::process::exit(1);
        }
        return;
    }

    if !input.is_dir() {
        return;
    }

    // Keep the report small but useful: include `fe.toml` and all `.fe` sources under `src/`.
    let fe_toml = input.join("fe.toml");
    if fe_toml.is_file() {
        let dest = inputs_dir.join("fe.toml");
        let _ = std::fs::copy(fe_toml, dest);
    }

    let src_dir = input.join("src");
    if !src_dir.is_dir() {
        return;
    }

    let dest_src = inputs_dir.join("src");
    create_dir_all_utf8(&dest_src);

    for entry in walkdir::WalkDir::new(src_dir.as_std_path())
        .follow_links(false)
        .into_iter()
        .filter_map(Result::ok)
    {
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        if path.extension().and_then(|s| s.to_str()) != Some("fe") {
            continue;
        }
        let rel = match path.strip_prefix(src_dir.as_std_path()) {
            Ok(rel) => rel,
            Err(_) => continue,
        };
        let rel = match Utf8PathBuf::from_path_buf(rel.to_path_buf()) {
            Ok(p) => p,
            Err(_) => continue,
        };
        let dest = dest_src.join(rel);
        if let Some(parent) = dest.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        let _ = std::fs::copy(path, dest);
    }
}

pub fn panic_payload_to_string(payload: &(dyn std::any::Any + Send)) -> String {
    if let Some(s) = payload.downcast_ref::<&'static str>() {
        (*s).to_string()
    } else if let Some(s) = payload.downcast_ref::<String>() {
        s.clone()
    } else {
        "panic payload is not a string".to_string()
    }
}

thread_local! {
    static PANIC_REPORT_PATH: RefCell<Option<Utf8PathBuf>> = const { RefCell::new(None) };
}

static PANIC_REPORT_INSTALL: OnceLock<()> = OnceLock::new();

fn install_panic_reporter_once() {
    let _ = PANIC_REPORT_INSTALL.get_or_init(|| {
        let old = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            // Never panic inside a panic hook: that would abort the process and can prevent reports
            // from being written.
            let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let path = PANIC_REPORT_PATH.with(|p| p.borrow().clone());
                if let Some(path) = path {
                    let bt = std::backtrace::Backtrace::force_capture();
                    let mut msg = String::new();
                    msg.push_str("panic while running `fe`\n\n");
                    msg.push_str(&format!("{info}\n\n"));
                    msg.push_str(&format!("backtrace:\n{bt:?}\n"));
                    let _ = std::fs::write(&path, msg);
                }

                // Keep the default stderr output for interactive runs.
                (old)(info);
            }));
        }));
    });
}

pub struct PanicReportGuard {
    prev: Option<Utf8PathBuf>,
}

impl Drop for PanicReportGuard {
    fn drop(&mut self) {
        let prev = self.prev.take();
        PANIC_REPORT_PATH.with(|p| {
            *p.borrow_mut() = prev;
        });
    }
}

pub fn enable_panic_report(path: Utf8PathBuf) -> PanicReportGuard {
    install_panic_reporter_once();
    let prev = PANIC_REPORT_PATH.with(|p| p.borrow().clone());
    PANIC_REPORT_PATH.with(|p| {
        *p.borrow_mut() = Some(path);
    });
    PanicReportGuard { prev }
}

fn find_git_repo_root(start: &Utf8PathBuf) -> Option<Utf8PathBuf> {
    let mut dir = start.clone();
    loop {
        if dir.join(".git").exists() {
            return Some(dir);
        }
        let parent = dir.parent()?.to_owned();
        if parent == dir {
            return None;
        }
        dir = parent;
    }
}

fn capture_cmd(cwd: &Utf8PathBuf, program: &str, args: &[&str]) -> Option<String> {
    let output = std::process::Command::new(program)
        .args(args)
        .current_dir(cwd.as_std_path())
        .output()
        .ok()?;
    let mut text = String::new();
    text.push_str(&String::from_utf8_lossy(&output.stdout));
    text.push_str(&String::from_utf8_lossy(&output.stderr));
    Some(text.trim().to_string())
}

fn write_best_effort(path: &Utf8PathBuf, contents: impl AsRef<[u8]>) {
    let _ = std::fs::write(path, contents);
}

pub fn write_report_meta(root: &Utf8PathBuf, kind: &str, suite: Option<&str>) {
    let meta = root.join("meta");
    let _ = std::fs::create_dir_all(meta.as_std_path());

    write_best_effort(&meta.join("kind.txt"), format!("{kind}\n"));
    if let Some(suite) = suite {
        write_best_effort(&meta.join("suite.txt"), format!("{suite}\n"));
    }

    if let Ok(cwd) = std::env::current_dir() {
        if let Ok(cwd) = Utf8PathBuf::from_path_buf(cwd) {
            write_best_effort(&meta.join("cwd.txt"), format!("{cwd}\n"));
        }
    }

    let mut args = String::new();
    for a in std::env::args() {
        args.push_str(&a);
        args.push('\n');
    }
    write_best_effort(&meta.join("args.txt"), args);

    let keys = [
        "RUST_BACKTRACE",
        "FE_TRACE_EVM",
        "FE_TRACE_EVM_KEEP",
        "FE_TRACE_EVM_STACK_N",
        "FE_TRACE_EVM_OUT",
        "FE_TRACE_EVM_STDERR",
        "FE_SONATINA_DUMP_SYMTAB",
        "FE_SONATINA_DUMP_SYMTAB_OUT",
        "SONATINA_STACKIFY_TRACE",
        "SONATINA_STACKIFY_TRACE_FUNC",
        "SONATINA_STACKIFY_TRACE_OUT",
        "SONATINA_TRANSIENT_MALLOC_TRACE",
        "SONATINA_TRANSIENT_MALLOC_TRACE_FUNC",
        "SONATINA_TRANSIENT_MALLOC_TRACE_OUT",
    ];
    let mut env_txt = String::new();
    for k in keys {
        if let Ok(v) = std::env::var(k) {
            env_txt.push_str(k);
            env_txt.push('=');
            env_txt.push_str(&v);
            env_txt.push('\n');
        }
    }
    if !env_txt.is_empty() {
        write_best_effort(&meta.join("env.txt"), env_txt);
    }

    if let Ok(manifest_dir) =
        Utf8PathBuf::from_path_buf(std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")))
    {
        if let Some(repo) = find_git_repo_root(&manifest_dir) {
            let mut git_txt = String::new();
            git_txt.push_str(&format!("fe_repo: {repo}\n"));
            if let Some(head) = capture_cmd(&repo, "git", &["rev-parse", "HEAD"]) {
                git_txt.push_str(&format!("fe_head: {head}\n"));
            }
            if let Some(status) = capture_cmd(&repo, "git", &["status", "--porcelain=v1"]) {
                let dirty = if status.trim().is_empty() {
                    "no"
                } else {
                    "yes"
                };
                git_txt.push_str(&format!("fe_dirty: {dirty}\n"));
            }

            let sonatina_guess = repo.join("../sonatina");
            if sonatina_guess.exists() {
                if let Some(sonatina_repo) = find_git_repo_root(&sonatina_guess) {
                    git_txt.push_str(&format!("\nsonatina_repo: {sonatina_repo}\n"));
                    if let Some(head) = capture_cmd(&sonatina_repo, "git", &["rev-parse", "HEAD"]) {
                        git_txt.push_str(&format!("sonatina_head: {head}\n"));
                    }
                    if let Some(status) =
                        capture_cmd(&sonatina_repo, "git", &["status", "--porcelain=v1"])
                    {
                        let dirty = if status.trim().is_empty() {
                            "no"
                        } else {
                            "yes"
                        };
                        git_txt.push_str(&format!("sonatina_dirty: {dirty}\n"));
                    }
                }
            }

            write_best_effort(&meta.join("git.txt"), git_txt);
        }
    }

    if let Ok(out) = std::process::Command::new("rustc").arg("-Vv").output() {
        if out.status.success() {
            let mut txt = String::new();
            txt.push_str(&String::from_utf8_lossy(&out.stdout));
            txt.push_str(&String::from_utf8_lossy(&out.stderr));
            write_best_effort(&meta.join("rustc.txt"), txt);
        }
    }
}
