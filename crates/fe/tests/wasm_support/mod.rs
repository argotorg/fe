//! Shared machinery for the programs of `tests/fixtures/wasm`.
//!
//! A Rust reference gives the expected result of every call, or says that the
//! call traps: the reference computes the exact integer result and the call
//! traps when the result does not fit the declared type or the operation is
//! undefined (division by zero). Every program is built at the four
//! optimization levels (0, 1, 2 and s) and run with wasmtime. On x86-64 Linux
//! with the `cranelift` feature, the same functions are also built for the host
//! and the native executable is checked against the wasm results.
//!
//! A module that was built with its manifest (`Built`) is checked against the
//! manifest as wasmtime reads it, and run in Node.js with the host of
//! `examples/wasm/host.mjs`.

#![allow(dead_code)]

use std::{
    fs,
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Output, Stdio},
    sync::OnceLock,
};

use serde_json::{Value, json};
use sha2::{Digest, Sha256};
use tempfile::tempdir;
use wasmtime::{Engine, ExternType, FuncType, Instance, Module, Store, Trap, Val, ValType};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ty {
    Bool,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
}

pub use Ty::*;

impl Ty {
    pub fn bits(self) -> u32 {
        match self {
            Bool => 1,
            U8 | I8 => 8,
            U16 | I16 => 16,
            U32 | I32 => 32,
            U64 | I64 => 64,
        }
    }

    pub fn signed(self) -> bool {
        matches!(self, I8 | I16 | I32 | I64)
    }

    pub fn range(self) -> (i128, i128) {
        match self {
            Bool => (0, 1),
            _ if self.signed() => (
                -(1i128 << (self.bits() - 1)),
                (1i128 << (self.bits() - 1)) - 1,
            ),
            _ => (0, (1i128 << self.bits()) - 1),
        }
    }

    /// `value` if it is representable in the type, which is what checked
    /// arithmetic requires of every intermediate result.
    pub fn chk(self, value: i128) -> Option<i128> {
        let (lo, hi) = self.range();
        (lo..=hi).contains(&value).then_some(value)
    }

    /// `value` modulo 2^bits, read in the type's signedness.
    pub fn wrap(self, value: i128) -> i128 {
        let modulus = 1i128 << self.bits();
        let mut v = value.rem_euclid(modulus);
        if self.signed() && v >= modulus / 2 {
            v -= modulus;
        }
        v
    }

    /// The value held by `raw`, a WebAssembly result widened to 64 bits.
    pub fn decode_raw(self, raw: u64) -> i128 {
        match self {
            Bool => {
                assert!(raw <= 1, "a bool result must be 0 or 1, got {raw}");
                i128::from(raw)
            }
            _ => self.wrap(i128::from(raw)),
        }
    }

    pub fn to_val(self, value: i128) -> Val {
        if self.bits() <= 32 {
            Val::I32(value as i32)
        } else {
            Val::I64(value as i64)
        }
    }

    pub fn literal(self, value: i128) -> String {
        match self {
            Bool => (value != 0).to_string(),
            _ => value.to_string(),
        }
    }

    /// Values at and around the limits of the type, with a few small ones.
    pub fn edges(self) -> Vec<i128> {
        let (lo, hi) = self.range();
        let mut values = vec![
            0,
            1,
            2,
            3,
            7,
            10,
            100,
            hi,
            hi - 1,
            lo,
            lo + 1,
            -1,
            -2,
            -100,
            hi / 2,
            hi / 2 + 1,
        ];
        values.retain(|value| (lo..=hi).contains(value));
        values.sort_unstable();
        values.dedup();
        values
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Want {
    Value(i128),
    Trap,
}

#[derive(Clone, Debug)]
pub struct Case {
    pub args: Vec<i128>,
    pub want: Want,
}

pub struct Export {
    pub name: &'static str,
    pub params: Vec<(&'static str, Ty)>,
    pub ret: Ty,
    pub cases: Vec<Case>,
}

impl Export {
    pub fn new(name: &'static str, params: &[(&'static str, Ty)], ret: Ty) -> Self {
        Self {
            name,
            params: params.to_vec(),
            ret,
            cases: Vec::new(),
        }
    }

    /// Adds a call per combination of `inputs`, one list per parameter. The
    /// reference returns the exact result, or `None` where the call traps.
    pub fn product(
        mut self,
        inputs: &[Vec<i128>],
        reference: impl Fn(&[i128]) -> Option<i128>,
    ) -> Self {
        assert_eq!(inputs.len(), self.params.len());
        let mut combos: Vec<Vec<i128>> = vec![Vec::new()];
        for list in inputs {
            combos = combos
                .iter()
                .flat_map(|prefix| {
                    list.iter().map(move |value| {
                        let mut next = prefix.clone();
                        next.push(*value);
                        next
                    })
                })
                .collect();
        }
        for args in combos {
            let want = match reference(&args).and_then(|value| self.ret.chk(value)) {
                Some(value) => Want::Value(value),
                None => Want::Trap,
            };
            self.cases.push(Case { args, want });
        }
        self
    }

    /// `product` over the edge values of each parameter type.
    pub fn edges(self, reference: impl Fn(&[i128]) -> Option<i128>) -> Self {
        let inputs: Vec<_> = self.params.iter().map(|(_, ty)| ty.edges()).collect();
        self.product(&inputs, reference)
    }

    pub fn calls(self, inputs: &[&[i128]], reference: impl Fn(&[i128]) -> Option<i128>) -> Self {
        let mut export = self;
        for args in inputs {
            let lists: Vec<Vec<i128>> = args.iter().map(|value| vec![*value]).collect();
            export = export.product(&lists, &reference);
        }
        export
    }
}

pub fn fixture_path(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/wasm")
        .join(format!("{name}.fe"))
}

pub fn fe(args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_fe"))
        .args(args)
        .output()
        .expect("fe should run")
}

/// Runs `fe`, which must exit with status 1 and every needle in its standard
/// error, without a panic. Returns the standard error.
pub fn expect_failure(args: &[&str], needles: &[&str]) -> String {
    let output = fe(args);
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    assert!(
        !output.status.success(),
        "the build should fail: {output:?}"
    );
    assert_eq!(output.status.code(), Some(1), "{stderr}");
    assert!(
        !stderr.contains("panicked"),
        "the compiler panicked:\n{stderr}"
    );
    for needle in needles {
        assert!(stderr.contains(needle), "missing `{needle}` in:\n{stderr}");
    }
    stderr
}

pub fn build_wasm(source: &Path, level: &str, out: &Path) -> Vec<u8> {
    let output = fe(&[
        "build",
        "--backend",
        "wasm",
        "-O",
        level,
        "--out-dir",
        out.to_str().unwrap(),
        source.to_str().unwrap(),
    ]);
    assert!(
        output.status.success(),
        "wasm build of {} at O{level} failed:\n{}\n{}",
        source.display(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let stem = source.file_stem().unwrap().to_str().unwrap();
    fs::read(out.join(format!("{stem}.wasm"))).expect("the module should be written")
}

pub fn engine() -> &'static Engine {
    static ENGINE: OnceLock<Engine> = OnceLock::new();
    ENGINE.get_or_init(Engine::default)
}

/// A module that `fe build` wrote to `dir` under the name `stem`, with its
/// manifest.
pub struct Built {
    pub dir: PathBuf,
    pub stem: String,
}

impl Built {
    pub fn wasm(&self) -> PathBuf {
        self.dir.join(format!("{}.wasm", self.stem))
    }

    pub fn manifest_path(&self) -> PathBuf {
        self.dir.join(format!("{}.manifest.json", self.stem))
    }

    pub fn manifest(&self) -> Value {
        let text = fs::read_to_string(self.manifest_path()).expect("the manifest is written");
        serde_json::from_str(&text).expect("the manifest is JSON")
    }
}

fn wasm_name(ty: &ValType) -> &'static str {
    match ty {
        ValType::I32 => "i32",
        ValType::I64 => "i64",
        other => panic!("unexpected value type {other:?}"),
    }
}

fn signature(ty: &FuncType) -> (Vec<&'static str>, Vec<&'static str>) {
    (
        ty.params().map(|t| wasm_name(&t)).collect(),
        ty.results().map(|t| wasm_name(&t)).collect(),
    )
}

/// The import and export sections of the module as an engine reads them must
/// agree with the manifest: the same names, in the same order for imports, and
/// the same WebAssembly types.
pub fn assert_matches_the_engine(built: &Built) {
    let bytes = fs::read(built.wasm()).unwrap();
    let manifest = built.manifest();
    let module = Module::new(engine(), &bytes).expect("the module validates");

    let imports = manifest["imports"].as_array().unwrap();
    let engine_imports: Vec<_> = module.imports().collect();
    assert_eq!(imports.len(), engine_imports.len());
    for (entry, import) in imports.iter().zip(&engine_imports) {
        assert_eq!(entry["module"], import.module());
        assert_eq!(entry["name"], import.name());
        assert_eq!(entry["kind"], "function");
        let ExternType::Func(ty) = import.ty() else {
            panic!("import {} is not a function", import.name());
        };
        let (params, results) = signature(&ty);
        assert_eq!(entry["type"]["params"], json!(params), "{}", import.name());
        assert_eq!(
            entry["type"]["results"],
            json!(results),
            "{}",
            import.name()
        );
    }

    let exports = manifest["exports"].as_array().unwrap();
    let engine_exports: Vec<_> = module.exports().collect();
    assert_eq!(exports.len(), engine_exports.len());
    let names: Vec<_> = exports
        .iter()
        .map(|e| e["name"].as_str().unwrap())
        .collect();
    let mut sorted = names.clone();
    sorted.sort_unstable();
    assert_eq!(names, sorted, "exports are listed in name order");
    for entry in exports {
        let name = entry["name"].as_str().unwrap();
        let export = module
            .get_export(name)
            .unwrap_or_else(|| panic!("no export {name}"));
        match export {
            ExternType::Memory(_) => assert_eq!(entry["kind"], "memory"),
            ExternType::Global(_) => {
                assert_eq!(name, "stack_top");
                assert_eq!(
                    (&entry["kind"], &entry["role"]),
                    (&json!("global"), &json!("stack"))
                );
            }
            ExternType::Func(ty) => {
                assert_eq!(entry["kind"], "function");
                let (params, results) = signature(&ty);
                let flat: Vec<_> = entry["params"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .flat_map(|p| {
                        p["wasm"]
                            .as_array()
                            .unwrap()
                            .iter()
                            .map(|t| t.as_str().unwrap().to_string())
                    })
                    .collect();
                assert_eq!(flat, params, "parameters of {name}");
                let result: Vec<_> = entry["result"]
                    .as_object()
                    .map(|r| {
                        r["wasm"]
                            .as_array()
                            .unwrap()
                            .iter()
                            .map(|t| t.as_str().unwrap().to_string())
                            .collect()
                    })
                    .unwrap_or_default();
                assert_eq!(result, results, "result of {name}");
            }
            other => panic!("unexpected export {name}: {other:?}"),
        }
    }

    // The hash is the SHA-256 of the file, written as 64 lowercase digits.
    assert_eq!(
        manifest["module"]["sha256"],
        hex::encode(Sha256::digest(&bytes))
    );
    assert_eq!(manifest["module"]["size"], bytes.len());
    assert_eq!(manifest["module"]["file"], format!("{}.wasm", built.stem));
}

/// Instantiates a module with the Node host (`examples/wasm/host.mjs`) and
/// makes the calls of `request`, which `wasm_support/manifest_driver.mjs`
/// describes with the answer. Node must be installed.
pub fn node_driver(request: &Value) -> Value {
    let driver =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/wasm_support/manifest_driver.mjs");
    let mut child = Command::new("node")
        .arg(driver)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("node must be installed to run these tests");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(request.to_string().as_bytes())
        .unwrap();
    let output = child.wait_with_output().unwrap();
    assert!(
        output.status.success(),
        "node failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).expect("the driver prints JSON")
}

pub struct Running {
    pub store: Store<()>,
    pub instance: Instance,
}

impl Running {
    pub fn new(bytes: &[u8]) -> Self {
        let module = Module::new(engine(), bytes).expect("the module should validate");
        let mut store = Store::new(engine(), ());
        let instance = Instance::new(&mut store, &module, &[]).expect("no imports are needed");
        Self { store, instance }
    }

    /// The result of the call, or the trap it ended in.
    pub fn call(&mut self, export: &Export, args: &[i128]) -> Result<i128, Trap> {
        let func = self
            .instance
            .get_func(&mut self.store, export.name)
            .unwrap_or_else(|| panic!("the module should export `{}`", export.name));
        let vals: Vec<Val> = export
            .params
            .iter()
            .zip(args)
            .map(|((_, ty), value)| ty.to_val(*value))
            .collect();
        let mut results = [Val::I32(0)];
        match func.call(&mut self.store, &vals, &mut results) {
            Ok(()) => {
                let raw = match &results[0] {
                    Val::I32(value) => u64::from(*value as u32),
                    Val::I64(value) => *value as u64,
                    other => panic!("unexpected result {other:?}"),
                };
                Ok(export.ret.decode_raw(raw))
            }
            Err(error) => Err(*error
                .downcast_ref::<Trap>()
                .unwrap_or_else(|| panic!("`{}` failed without a trap: {error}", export.name))),
        }
    }
}

pub fn describe(export: &Export, case: &Case) -> String {
    let args = export
        .params
        .iter()
        .zip(&case.args)
        .map(|((name, ty), value)| format!("{name}: {}", ty.literal(*value)))
        .collect::<Vec<_>>()
        .join(", ");
    format!("{}({args})", export.name)
}

/// Runs every case of `exports` against the module and returns the observed
/// results in the same order.
pub fn run_cases(bytes: &[u8], exports: &[Export], context: &str) -> Vec<Vec<Result<i128, Trap>>> {
    let mut running = Running::new(bytes);
    let mut observed = Vec::new();
    for export in exports {
        let mut results = Vec::new();
        for case in &export.cases {
            let got = running.call(export, &case.args);
            let ok = match (&case.want, &got) {
                (Want::Value(want), Ok(got)) => want == got,
                (Want::Trap, Err(_)) => true,
                _ => false,
            };
            assert!(
                ok,
                "{context}: {} gave {got:?}, expected {:?}",
                describe(export, case),
                case.want
            );
            results.push(got);
        }
        observed.push(results);
    }
    observed
}

pub fn check_program(file: &str, exports: Vec<Export>) {
    let calls: usize = exports.iter().map(|export| export.cases.len()).sum();
    let traps = exports
        .iter()
        .flat_map(|export| &export.cases)
        .filter(|case| case.want == Want::Trap)
        .count();
    println!(
        "{file}: {} exports, {calls} calls, {traps} expected traps",
        exports.len()
    );
    let source = fixture_path(file);
    let temp = tempdir().unwrap();
    let mut observed_results = Vec::new();
    for level in ["0", "1", "2", "s"] {
        let out = temp.path().join(format!("o{level}"));
        let bytes = build_wasm(&source, level, &out);
        let observed = run_cases(&bytes, &exports, &format!("{file} at O{level}"));
        if level == "2" {
            observed_results = observed;
        }
    }
    #[cfg(all(feature = "cranelift", target_os = "linux", target_arch = "x86_64"))]
    native::agree(file, &source, &exports, &observed_results, temp.path());
    #[cfg(not(all(feature = "cranelift", target_os = "linux", target_arch = "x86_64")))]
    let _ = observed_results;
}

#[cfg(all(feature = "cranelift", target_os = "linux", target_arch = "x86_64"))]
pub mod native {
    use std::os::unix::process::ExitStatusExt;

    use super::*;

    /// Cases per export that are also run natively.
    pub const NATIVE_CASES_PER_EXPORT: usize = 24;

    /// Calls that trap in the module and are also run natively, per program:
    /// the first one of each export, up to this many.
    pub const NATIVE_TRAPS_PER_PROGRAM: usize = 6;

    pub fn build_native(source: &Path, out: &Path) -> PathBuf {
        let output = fe(&[
            "build",
            "--backend",
            "native",
            "--emit",
            "executable",
            "-O",
            "1",
            "--out-dir",
            out.to_str().unwrap(),
            source.to_str().unwrap(),
        ]);
        assert!(
            output.status.success(),
            "native build of {} failed:\n{}\n{}",
            source.display(),
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        out.join(source.file_stem().unwrap())
    }

    pub fn call_source(export: &Export, case: &Case) -> String {
        let args = export
            .params
            .iter()
            .zip(&case.args)
            .map(|((name, ty), value)| format!("{name}: {}", ty.literal(*value)))
            .collect::<Vec<_>>()
            .join(", ");
        format!("{}({args})", export.name)
    }

    /// Builds the program for the host with a `main` that calls the functions
    /// with the same arguments as the module saw. With no arguments, `main`
    /// returns 0 only if every result equals the one the module returned, and
    /// otherwise the number of the first call that differs. With k arguments,
    /// it makes the k-th call that trapped in the module and exits with 3 if
    /// the call returns, so a trap must end the process with a signal.
    pub fn agree(
        file: &str,
        source: &Path,
        exports: &[Export],
        observed: &[Vec<Result<i128, Trap>>],
        temp: &Path,
    ) {
        let text = fs::read_to_string(source).unwrap();
        let mut checks = Vec::new();
        let mut traps = Vec::new();
        for (export, results) in exports.iter().zip(observed) {
            let mut taken = 0;
            let mut trapped = false;
            for (case, result) in export.cases.iter().zip(results) {
                match result {
                    Ok(value) if taken < NATIVE_CASES_PER_EXPORT => {
                        taken += 1;
                        checks.push(format!(
                            "if {} != {} {{ return {} }}",
                            call_source(export, case),
                            export.ret.literal(*value),
                            checks.len() + 1
                        ));
                    }
                    Err(_) if !trapped && traps.len() < NATIVE_TRAPS_PER_PROGRAM => {
                        trapped = true;
                        traps.push(call_source(export, case));
                    }
                    _ => {}
                }
            }
        }
        let trap_arms = traps
            .iter()
            .enumerate()
            .map(|(idx, call)| {
                format!(
                    "if argc == {} {{\n        let r = {call}\n        return if r == r {{ 3 }} else {{ 4 }}\n    }}",
                    idx + 2
                )
            })
            .collect::<Vec<_>>()
            .join("\n    ");
        println!(
            "{file}: {} calls and {} traps also run natively",
            checks.len(),
            traps.len()
        );
        let driver = temp.join(format!("{file}_native.fe"));
        fs::write(
            &driver,
            format!(
                "{text}\npub fn main() -> i32 uses (args: std::env::Args) {{\n    let argc = args.count()\n    if argc == 1 {{\n        {}\n        return 0\n    }}\n    {trap_arms}\n    5\n}}\n",
                checks.join("\n        ")
            ),
        )
        .unwrap();
        let exe = build_native(&driver, &temp.join("native"));

        let status = Command::new(&exe).status().unwrap();
        assert_eq!(
            status.code(),
            Some(0),
            "{file}: the native build disagrees with the module at check {:?} of:\n{}",
            status.code(),
            checks.join("\n")
        );
        for (idx, call) in traps.iter().enumerate() {
            let status = Command::new(&exe)
                .args(vec!["trap"; idx + 1])
                .status()
                .unwrap();
            assert!(
                status.signal().is_some(),
                "{file}: native `{call}` exited with {status:?} where the module traps"
            );
        }
    }
}

pub fn small(ty: Ty, values: &[i128]) -> Vec<i128> {
    values
        .iter()
        .copied()
        .filter(|value| ty.chk(*value).is_some())
        .collect()
}

/// A stretch of consecutive values.
pub fn upto(count: i128) -> Vec<i128> {
    (0..count).collect()
}

/// `source` with the lines of one flavour kept (without their marker) and the
/// lines of the other flavour removed. A fixture marks a line `//host: ` or
/// `//mock: ` when only one of its two builds contains it.
pub fn render_flavour(source: &str, keep: &str, drop: &str) -> String {
    let mut out = String::new();
    for line in source.lines() {
        if line.starts_with(drop) {
            continue;
        }
        out.push_str(line.strip_prefix(keep).unwrap_or(line));
        out.push('\n');
    }
    out
}
