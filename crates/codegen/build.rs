use std::path::Path;

fn main() {
    // Ensure fixture edits rerun codegen tests by explicitly watching each file.
    watch_dir_recursively(Path::new("tests/fixtures"));
}

fn watch_dir_recursively(dir: &Path) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            watch_dir_recursively(&path);
            continue;
        }
        if let Some(path_str) = path.to_str() {
            println!("cargo:rerun-if-changed={path_str}");
        }
    }
}
