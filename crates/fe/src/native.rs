use std::{path::Path, process::Command};

pub(crate) fn link_executable(inputs: &[&Path], executable: &Path) -> Result<(), String> {
    // The C compiler driver supplies the platform startup objects and linker
    // configuration for both ordinary executables and test dispatchers.
    let output = Command::new("cc")
        .args(inputs)
        .arg("-o")
        .arg(executable)
        .output()
        .map_err(|err| format!("failed to run host linker `cc`: {err}"))?;
    if !output.status.success() {
        return Err(format!(
            "host linker failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    Ok(())
}
