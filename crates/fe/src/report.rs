use camino::Utf8PathBuf;

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

    let base = s
        .strip_suffix(".tar.gz")
        .expect("checked .tar.gz suffix");
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

pub fn tar_gz_dir(staging: &Utf8PathBuf, out: &Utf8PathBuf) -> Result<(), String> {
    let parent = staging.parent().ok_or_else(|| "missing staging parent".to_string())?;
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

pub struct PanicHookGuard {
    old: Option<Box<dyn Fn(&std::panic::PanicHookInfo) + Send + Sync + 'static>>,
}

impl Drop for PanicHookGuard {
    fn drop(&mut self) {
        if let Some(old) = self.old.take() {
            std::panic::set_hook(old);
        }
    }
}

pub fn install_panic_hook(path: Utf8PathBuf) -> PanicHookGuard {
    let old = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let bt = std::backtrace::Backtrace::force_capture();
        let mut msg = String::new();
        msg.push_str("panic while running `fe`\n\n");
        msg.push_str(&format!("{info}\n\n"));
        msg.push_str(&format!("backtrace:\n{bt:?}\n"));
        let _ = std::fs::write(&path, msg);
    }));

    PanicHookGuard { old: Some(old) }
}
