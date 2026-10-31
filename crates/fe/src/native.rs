use std::{path::Path, process::Command};

/// Links `inputs` and the `libraries` named as `-l<name>` into `executable`.
pub(crate) fn link_executable(
    inputs: &[&Path],
    libraries: &[String],
    executable: &Path,
) -> Result<(), String> {
    // The C compiler driver supplies the platform startup objects and linker
    // configuration for both ordinary executables and test dispatchers.
    run_host_tool(
        Command::new("cc")
            .args(inputs)
            .args(libraries.iter().map(|library| format!("-l{library}")))
            .arg("-o")
            .arg(executable),
        "linker `cc`",
    )
}

/// Archives `objects` into the static library `archive`, with a symbol index.
pub(crate) fn archive_objects(objects: &[&Path], archive: &Path) -> Result<(), String> {
    run_host_tool(
        Command::new("ar").arg("rcs").arg(archive).args(objects),
        "archiver `ar`",
    )
}

fn run_host_tool(command: &mut Command, tool: &str) -> Result<(), String> {
    let output = command
        .output()
        .map_err(|err| format!("failed to run host {tool}: {err}"))?;
    if !output.status.success() {
        return Err(format!(
            "host {tool} failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    Ok(())
}
