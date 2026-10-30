#![cfg(feature = "wasm")]

//! Two builds of one program give the same bytes.
//!
//! Each program is built at the four optimization levels (0, 1, 2 and s), twice:
//! once in a directory with a relative source path, and once from another working
//! directory, through a source path that differs in length and in every
//! directory name, with an old modification time on the source, a different
//! output directory, an environment that holds only `PATH`, `HOME`, `TZ` and
//! `LC_ALL`, and an output directory that did not exist. The module (`.wasm`),
//! its IR (`.wasm.sona`) and its manifest (`.manifest.json`) of the two builds
//! must be equal. A difference fails the test and names the file, the sizes and
//! the first byte that differs.
//!
//! The same check, over a larger corpus and with the clock moved, is
//! `examples/wasm/reproducible.sh`.

mod wasm_support;

use std::{
    env, fs,
    path::{Path, PathBuf},
    process::Command,
    time::{Duration, SystemTime},
};

use tempfile::tempdir;
use wasm_support::render_flavour;

const LEVELS: [&str; 4] = ["0", "1", "2", "s"];
const OUTPUTS: [&str; 3] = ["wasm", "wasm.sona", "manifest.json"];

fn manifest_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn repository_file(relative: &str) -> PathBuf {
    manifest_dir().join("../..").join(relative)
}

fn fixture(directory: &str, name: &str) -> String {
    let path = manifest_dir().join(format!("tests/fixtures/{directory}/{name}.fe"));
    fs::read_to_string(&path).unwrap_or_else(|err| panic!("{}: {err}", path.display()))
}

/// What `fe build --backend wasm` writes for one program, by extension.
type Outputs = Vec<(&'static str, Vec<u8>)>;

fn read_outputs(out: &Path, stem: &str) -> Outputs {
    OUTPUTS
        .iter()
        .map(|ext| {
            let path = out.join(format!("{stem}.{ext}"));
            let bytes = fs::read(&path).unwrap_or_else(|err| panic!("{}: {err}", path.display()));
            (*ext, bytes)
        })
        .collect()
}

fn run(command: &mut Command, what: &str) {
    let output = command.output().expect("fe should run");
    assert!(
        output.status.success(),
        "{what} failed:\n{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

/// Build 1: the source is in `root/src`, the working directory is `root`.
fn build_plain(source: &str, stem: &str, level: &str, root: &Path) -> Outputs {
    fs::create_dir_all(root.join("src")).unwrap();
    fs::write(root.join("src").join(format!("{stem}.fe")), source).unwrap();
    run(
        Command::new(env!("CARGO_BIN_EXE_fe"))
            .current_dir(root)
            .args([
                "build",
                "--backend",
                "wasm",
                "-O",
                level,
                "--emit",
                "wasm,ir",
            ])
            .args(["--out-dir", "out"])
            .arg(format!("src/{stem}.fe")),
        "the first build",
    );
    read_outputs(&root.join("out"), stem)
}

/// Build 2: other directories, an absolute and longer path, an old modification
/// time on the source, another environment.
fn build_varied(source: &str, stem: &str, level: &str, root: &Path) -> Outputs {
    let source_dir = root.join("another-directory-with-a-longer-name/and/deeper/still");
    fs::create_dir_all(&source_dir).unwrap();
    let file = source_dir.join(format!("{stem}.fe"));
    fs::write(&file, source).unwrap();
    fs::File::options()
        .write(true)
        .open(&file)
        .unwrap()
        .set_modified(SystemTime::UNIX_EPOCH + Duration::from_secs(981_173_106))
        .unwrap();
    let out = root.join("elsewhere/output");
    let working = root.join("working");
    fs::create_dir_all(&working).unwrap();
    run(
        Command::new(env!("CARGO_BIN_EXE_fe"))
            .current_dir(&working)
            .env_clear()
            .env("PATH", env::var_os("PATH").unwrap_or_default())
            .env("HOME", &working)
            .env("TZ", "Pacific/Kiritimati")
            .env("LC_ALL", "C")
            .args([
                "build",
                "--backend",
                "wasm",
                "-O",
                level,
                "--emit",
                "wasm,ir",
            ])
            .arg("--out-dir")
            .arg(&out)
            .arg(&file),
        "the second build",
    );
    read_outputs(&out, stem)
}

/// A description of the first difference between two sets of outputs.
fn difference(first: &Outputs, second: &Outputs) -> Option<String> {
    for ((ext, a), (_, b)) in first.iter().zip(second) {
        if a != b {
            let at = a
                .iter()
                .zip(b)
                .position(|(x, y)| x != y)
                .unwrap_or(a.len().min(b.len()));
            return Some(format!(
                ".{ext} differs: {} and {} bytes, the first difference is at byte {at}",
                a.len(),
                b.len()
            ));
        }
    }
    None
}

fn assert_reproducible(source: &str, stem: &str) {
    for level in LEVELS {
        let temp = tempdir().unwrap();
        let first = build_plain(source, stem, level, &temp.path().join("one"));
        let second = build_varied(source, stem, level, &temp.path().join("two"));
        if let Some(difference) = difference(&first, &second) {
            panic!("{stem} at -O{level}: {difference}");
        }
        // The module must not be empty, or the comparison says nothing.
        assert!(first[0].1.len() > 8, "{stem} at -O{level} wrote no module");
    }
}

macro_rules! reproducible {
    ($name:ident, $source:expr, $stem:literal) => {
        #[test]
        fn $name() {
            assert_reproducible(&$source, $stem);
        }
    };
}

reproducible!(
    struct_complex,
    fixture("wasm", "struct_complex"),
    "struct_complex"
);
reproducible!(array_sort, fixture("wasm", "array_sort"), "array_sort");
reproducible!(vec_arena, fixture("wasm", "vec_arena"), "vec_arena");
reproducible!(const_crc, fixture("wasm", "const_crc"), "const_crc");
reproducible!(
    string_bytes,
    fixture("wasm", "string_bytes"),
    "string_bytes"
);
reproducible!(stack_vm, fixture("wasm", "stack_vm"), "stack_vm");
reproducible!(trap_assert, fixture("wasm", "trap_assert"), "trap_assert");
reproducible!(
    imports_with_the_host,
    render_flavour(
        &fixture("wasm_imports", "log_basic"),
        "//host: ",
        "//mock: "
    ),
    "log_basic"
);
reproducible!(
    imports_with_a_mock,
    render_flavour(
        &fixture("wasm_imports", "log_basic"),
        "//mock: ",
        "//host: "
    ),
    "log_basic"
);
reproducible!(
    files_with_the_host,
    render_flavour(&fixture("wasm_files", "file_ops"), "//host: ", "//mock: "),
    "file_ops"
);
reproducible!(
    files_with_a_mock,
    render_flavour(&fixture("wasm_files", "file_ops"), "//mock: ", "//host: "),
    "file_ops"
);
reproducible!(forged_handles, fixture("wasm_files", "forge"), "forge");
reproducible!(
    example_lines,
    fs::read_to_string(repository_file("examples/wasm/lines.fe")).unwrap(),
    "lines"
);
reproducible!(
    example_host,
    fs::read_to_string(repository_file("examples/wasm_host.fe")).unwrap(),
    "wasm_host"
);

/// The comparison must be able to fail: modules built at different levels
/// differ, and `difference` says so.
#[test]
fn the_comparison_detects_a_difference() {
    let source = fixture("wasm", "struct_complex");
    let temp = tempdir().unwrap();
    let at_zero = build_plain(&source, "struct_complex", "0", &temp.path().join("zero"));
    let at_two = build_plain(&source, "struct_complex", "2", &temp.path().join("two"));
    let found = difference(&at_zero, &at_two).expect("-O0 and -O2 give different modules");
    assert!(found.starts_with(".wasm differs"), "{found}");
    assert_eq!(difference(&at_zero, &at_zero), None);
}
