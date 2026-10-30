#![cfg(feature = "wasm")]

//! `fe build --backend wasm` on programs that call the host.
//!
//! Each program in `tests/fixtures/wasm_imports` is built twice from one file.
//! In the host build the operations are imports of the module `env`. In the mock
//! build the same operations are Fe code (`std::wasm::mock`, or a Fe function in
//! place of an `extern` declaration), and the module has no imports. The two
//! builds must give the same exported results at the four optimization levels
//! (0, 1, 2 and s).
//!
//! The host here is a Rust program that supplies the imports and records every
//! call. A Rust reference states the result of each exported call, the calls
//! that the host must see, in order, and whether the call traps. The host's
//! clock is the one the mock has: it reads 1000 first and advances by 10 at each
//! reading. A line of a fixture that starts with `//host:` or `//mock:` belongs
//! to that build only.

mod wasm_support;

use std::{fmt, fs, path::Path};

use tempfile::tempdir;
use wasm_support::{Ty, Ty::*, build_wasm, engine};
use wasmtime::{Caller, FuncType, Linker, Module, Store, Trap, Val, ValType};

const START: u64 = 1000;
const STEP: u64 = 10;

// ---------------------------------------------------------------------------
// The host.
// ---------------------------------------------------------------------------

/// A call that the host saw: the import's name and its arguments, as the host
/// received them (an `i32` that holds a narrow integer is zero-extended).
type Event = (String, Vec<i128>);

fn ev(name: &str, args: &[i128]) -> Event {
    (name.to_string(), args.to_vec())
}

#[derive(Default)]
struct State {
    now: u64,
    trace: Vec<Event>,
    /// When set, `sys_wasm_abort` returns instead of trapping.
    abort_returns: bool,
}

/// The error the host raises in `sys_wasm_abort`.
#[derive(Debug, PartialEq, Eq)]
struct HostAbort(u32);

impl fmt::Display for HostAbort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "the program aborted with code {}", self.0)
    }
}

impl std::error::Error for HostAbort {}

/// The operations of `std::wasm`.
fn define_std(linker: &mut Linker<State>) {
    linker
        .func_wrap(
            "env",
            "sys_wasm_log_i32",
            |mut c: Caller<'_, State>, v: i32| {
                c.data_mut().trace.push(ev("log_i32", &[i128::from(v)]));
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "sys_wasm_log_u64",
            |mut c: Caller<'_, State>, v: i64| {
                c.data_mut()
                    .trace
                    .push(ev("log_u64", &[i128::from(v as u64)]));
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "sys_wasm_now_ms",
            |mut c: Caller<'_, State>| -> i64 {
                let state = c.data_mut();
                let reading = state.now;
                state.now += STEP;
                state.trace.push(ev("now_ms", &[]));
                reading as i64
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "sys_wasm_abort",
            |mut c: Caller<'_, State>, code: i32| -> wasmtime::Result<()> {
                let state = c.data_mut();
                state.trace.push(ev("abort", &[i128::from(code as u32)]));
                if state.abort_returns {
                    Ok(())
                } else {
                    Err(wasmtime::Error::new(HostAbort(code as u32)))
                }
            },
        )
        .unwrap();
}

/// The `extern` functions that a fixture declares itself.
fn define_extern_basic(linker: &mut Linker<State>) {
    linker
        .func_wrap(
            "env",
            "host_mul",
            |mut c: Caller<'_, State>, a: i32, b: i32| -> i32 {
                c.data_mut()
                    .trace
                    .push(ev("host_mul", &[i128::from(a), i128::from(b)]));
                a.wrapping_mul(b)
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "host_neg",
            |mut c: Caller<'_, State>, a: i64| -> i64 {
                c.data_mut().trace.push(ev("host_neg", &[i128::from(a)]));
                a.wrapping_neg()
            },
        )
        .unwrap();
    linker
        .func_wrap("env", "host_zero", |mut c: Caller<'_, State>| -> i32 {
            c.data_mut().trace.push(ev("host_zero", &[]));
            0
        })
        .unwrap();
}

/// The host returns bits above the width of a narrow result. The module must
/// ignore them.
fn define_extern_narrow(linker: &mut Linker<State>) {
    linker
        .func_wrap(
            "env",
            "host_inc_u8",
            |mut c: Caller<'_, State>, a: i32| -> i32 {
                c.data_mut().trace.push(ev("host_inc_u8", &[i128::from(a)]));
                i32::from((a as u8).wrapping_add(1)) | 0x7100
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "host_negate_i8",
            |mut c: Caller<'_, State>, a: i32| -> i32 {
                c.data_mut()
                    .trace
                    .push(ev("host_negate_i8", &[i128::from(a)]));
                i32::from((a as u8 as i8).wrapping_neg() as u8) | 0xAB00
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "host_swap_bool",
            |mut c: Caller<'_, State>, a: i32| -> i32 {
                c.data_mut()
                    .trace
                    .push(ev("host_swap_bool", &[i128::from(a)]));
                1 - a
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "host_add_u16_i16",
            |mut c: Caller<'_, State>, a: i32, b: i32| -> i32 {
                c.data_mut()
                    .trace
                    .push(ev("host_add_u16_i16", &[i128::from(a), i128::from(b)]));
                let sum = (b as u16 as i16).wrapping_add(a as u16 as i16);
                i32::from(sum as u16) | 0x5A0000
            },
        )
        .unwrap();
}

fn define_extern_i64(linker: &mut Linker<State>) {
    linker
        .func_wrap(
            "env",
            "host_mix",
            |mut c: Caller<'_, State>, a: i64, b: i64| -> i64 {
                c.data_mut()
                    .trace
                    .push(ev("host_mix", &[i128::from(a), i128::from(b as u64)]));
                (b as u64).wrapping_mul(31).wrapping_add(a as u64) as i64
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "host_high",
            |mut c: Caller<'_, State>, a: i64| -> i32 {
                c.data_mut()
                    .trace
                    .push(ev("host_high", &[i128::from(a as u64)]));
                ((a as u64) >> 32) as i32
            },
        )
        .unwrap();
}

fn define_extern_many(linker: &mut Linker<State>) {
    linker
        .func_wrap(
            "env",
            "host_note",
            |mut c: Caller<'_, State>, tag: i32, a: i32, b: i32, x: i64, y: i64, flag: i32| {
                c.data_mut().trace.push(ev(
                    "host_note",
                    &[
                        i128::from(tag as u32),
                        i128::from(a),
                        i128::from(b),
                        i128::from(x),
                        i128::from(y),
                        i128::from(flag),
                    ],
                ));
            },
        )
        .unwrap();
    linker
        .func_wrap("env", "host_ping", |mut c: Caller<'_, State>| {
            c.data_mut().trace.push(ev("host_ping", &[]));
        })
        .unwrap();
}

/// The host calls the exported `inner` from inside the import.
fn define_reentrant(linker: &mut Linker<State>) {
    linker
        .func_wrap(
            "env",
            "host_callback",
            |mut c: Caller<'_, State>, x: i32| -> wasmtime::Result<i32> {
                c.data_mut()
                    .trace
                    .push(ev("host_callback", &[i128::from(x)]));
                let inner = c
                    .get_export("inner")
                    .and_then(|export| export.into_func())
                    .expect("the module exports `inner`");
                let mut out = [Val::I32(0)];
                inner.call(&mut c, &[Val::I32(x)], &mut out)?;
                Ok(out[0].unwrap_i32())
            },
        )
        .unwrap();
}

// ---------------------------------------------------------------------------
// The programs.
// ---------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
enum Want {
    Value(i128),
    /// The module executes `unreachable` or another trap of its own.
    Trap,
    /// The host raised `HostAbort(code)`. Without a host, the mock traps.
    Abort(u32),
}

/// What a call must give: its result, and the calls the host must see.
struct Expect {
    want: Want,
    trace: Vec<Event>,
}

fn value(v: i128, trace: Vec<Event>) -> Expect {
    Expect {
        want: Want::Value(v),
        trace,
    }
}

fn traps(trace: Vec<Event>) -> Expect {
    Expect {
        want: Want::Trap,
        trace,
    }
}

struct Case {
    args: Vec<i128>,
    expect: Expect,
}

struct Export {
    name: &'static str,
    params: Vec<Ty>,
    ret: Ty,
    cases: Vec<Case>,
}

impl Export {
    fn new(name: &'static str, params: &[Ty], ret: Ty) -> Self {
        Self {
            name,
            params: params.to_vec(),
            ret,
            cases: Vec::new(),
        }
    }

    fn calls(mut self, inputs: &[Vec<i128>], reference: impl Fn(&[i128]) -> Expect) -> Self {
        for args in inputs {
            let expect = reference(args);
            self.cases.push(Case {
                args: args.clone(),
                expect,
            });
        }
        self
    }

    /// One call per value of the single parameter.
    fn each(self, values: &[i128], reference: impl Fn(i128) -> Expect) -> Self {
        let inputs: Vec<Vec<i128>> = values.iter().map(|v| vec![*v]).collect();
        self.calls(&inputs, |args| reference(args[0]))
    }

    /// One call per pair of values.
    fn pairs(self, a: &[i128], b: &[i128], reference: impl Fn(i128, i128) -> Expect) -> Self {
        let inputs: Vec<Vec<i128>> = a
            .iter()
            .flat_map(|x| b.iter().map(move |y| vec![*x, *y]))
            .collect();
        self.calls(&inputs, |args| reference(args[0], args[1]))
    }
}

/// An import in the module's import section: `(name, "params->results")`.
type Import = (&'static str, &'static str);

struct Program {
    file: &'static str,
    imports: Vec<Import>,
    define: Option<fn(&mut Linker<State>)>,
    exports: Vec<Export>,
}

// ---------------------------------------------------------------------------
// Building and running.
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Flavour {
    Host,
    Mock,
}

impl Flavour {
    fn marker(self) -> &'static str {
        match self {
            Self::Host => "//host: ",
            Self::Mock => "//mock: ",
        }
    }

    fn other(self) -> &'static str {
        match self {
            Self::Host => "//mock: ",
            Self::Mock => "//host: ",
        }
    }
}

fn fixture(file: &str) -> String {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/wasm_imports")
        .join(format!("{file}.fe"));
    fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}

/// The fixture with the lines of the other build removed and the marker of this
/// one taken off.
fn render(source: &str, flavour: Flavour) -> String {
    let mut out = String::new();
    for line in source.lines() {
        if line.starts_with(flavour.other()) {
            continue;
        }
        out.push_str(line.strip_prefix(flavour.marker()).unwrap_or(line));
        out.push('\n');
    }
    out
}

/// Builds the fixture for one flavour and level.
fn build(file: &str, flavour: Flavour, level: &str, temp: &Path) -> Vec<u8> {
    let dir = temp.join(format!("{flavour:?}-{level}"));
    fs::create_dir_all(&dir).unwrap();
    let source = dir.join(format!("{file}.fe"));
    fs::write(&source, render(&fixture(file), flavour)).unwrap();
    build_wasm(&source, level, &dir.join("out"))
}

fn type_name(ty: &ValType) -> &'static str {
    match ty {
        ValType::I32 => "i32",
        ValType::I64 => "i64",
        other => panic!("unexpected value type {other:?}"),
    }
}

fn signature(ty: &FuncType) -> String {
    let join = |types: Vec<ValType>| types.iter().map(type_name).collect::<Vec<_>>().join(",");
    format!(
        "{}->{}",
        join(ty.params().collect()),
        join(ty.results().collect())
    )
}

/// The import section of the module as `(module, name, signature)`, sorted.
fn import_section(module: &Module) -> Vec<(String, String, String)> {
    let mut found: Vec<_> = module
        .imports()
        .map(|import| {
            let func = import
                .ty()
                .func()
                .unwrap_or_else(|| panic!("`{}` is not a function import", import.name()))
                .clone();
            (
                import.module().to_string(),
                import.name().to_string(),
                signature(&func),
            )
        })
        .collect();
    found.sort();
    found
}

#[derive(Debug, PartialEq, Eq)]
enum Got {
    Value(i128),
    Trap(Trap),
    Abort(u32),
}

struct Running {
    store: Store<State>,
    instance: wasmtime::Instance,
}

impl Running {
    fn new(module: &Module, define: &[fn(&mut Linker<State>)]) -> Self {
        let mut linker = Linker::new(engine());
        for define in define {
            define(&mut linker);
        }
        let mut store = Store::new(engine(), State::default());
        let instance = linker
            .instantiate(&mut store, module)
            .unwrap_or_else(|e| panic!("the module should instantiate: {e:?}"));
        Self { store, instance }
    }

    fn call(&mut self, export: &Export, args: &[i128]) -> Got {
        let func = self
            .instance
            .get_func(&mut self.store, export.name)
            .unwrap_or_else(|| panic!("the module should export `{}`", export.name));
        let vals: Vec<Val> = export
            .params
            .iter()
            .zip(args)
            .map(|(ty, v)| ty.to_val(*v))
            .collect();
        let state = self.store.data_mut();
        state.now = START;
        state.trace.clear();
        let mut results = [Val::I32(0)];
        match func.call(&mut self.store, &vals, &mut results) {
            Ok(()) => {
                let raw = match &results[0] {
                    Val::I32(v) => u64::from(*v as u32),
                    Val::I64(v) => *v as u64,
                    other => panic!("unexpected result {other:?}"),
                };
                Got::Value(export.ret.decode_raw(raw))
            }
            Err(error) => {
                if let Some(abort) = error.downcast_ref::<HostAbort>() {
                    Got::Abort(abort.0)
                } else if let Some(trap) = error.downcast_ref::<Trap>() {
                    Got::Trap(*trap)
                } else {
                    panic!("`{}` failed without a trap: {error:?}", export.name)
                }
            }
        }
    }
}

fn describe(export: &Export, args: &[i128]) -> String {
    let args = export
        .params
        .iter()
        .zip(args)
        .map(|(ty, v)| ty.literal(*v))
        .collect::<Vec<_>>()
        .join(", ");
    format!("{}({args})", export.name)
}

/// Runs every case. With `host`, the call and its trace must be as expected;
/// without one the mock stands in and an abort is a trap of the module.
fn run_cases(module: &Module, program: &Program, host: bool, context: &str) -> Vec<Vec<Got>> {
    let mut define: Vec<fn(&mut Linker<State>)> = Vec::new();
    if host {
        define.push(define_std);
        define.extend(program.define);
    }
    let mut running = Running::new(module, &define);
    let mut all = Vec::new();
    for export in &program.exports {
        let mut results = Vec::new();
        for case in &export.cases {
            let got = running.call(export, &case.args);
            let what = describe(export, &case.args);
            let ok = match (&case.expect.want, &got) {
                (Want::Value(want), Got::Value(got)) => want == got,
                (Want::Trap, Got::Trap(_)) => true,
                (Want::Abort(code), Got::Abort(got)) => host && code == got,
                (Want::Abort(_), Got::Trap(trap)) => !host && *trap == Trap::UnreachableCodeReached,
                _ => false,
            };
            assert!(
                ok,
                "{context}: {what} gave {got:?}, expected {:?}",
                case.expect.want
            );
            if host {
                assert_eq!(
                    running.store.data().trace,
                    case.expect.trace,
                    "{context}: {what}: the host saw other calls than expected"
                );
            }
            results.push(got);
        }
        all.push(results);
    }
    all
}

/// The result of a call as the mock gives it: an abort is a trap.
fn as_mock(got: &Got) -> Got {
    match got {
        Got::Abort(_) => Got::Trap(Trap::UnreachableCodeReached),
        Got::Trap(trap) => Got::Trap(*trap),
        Got::Value(v) => Got::Value(*v),
    }
}

fn check_program(program: Program) {
    let calls: usize = program.exports.iter().map(|e| e.cases.len()).sum();
    println!(
        "{}: {} exports, {calls} calls, {} imports",
        program.file,
        program.exports.len(),
        program.imports.len()
    );
    let temp = tempdir().unwrap();
    let expected_imports: Vec<_> = {
        let mut list: Vec<_> = program
            .imports
            .iter()
            .map(|(name, sig)| ("env".to_string(), (*name).to_string(), (*sig).to_string()))
            .collect();
        list.sort();
        list
    };
    for level in ["0", "1", "2", "s"] {
        let context = format!("{} at O{level}", program.file);

        let host_bytes = build(program.file, Flavour::Host, level, temp.path());
        let host_module = Module::new(engine(), &host_bytes).expect("the module should validate");
        assert_eq!(
            import_section(&host_module),
            expected_imports,
            "{context}: the import section"
        );
        let from_host = run_cases(&host_module, &program, true, &format!("{context} (host)"));

        let mock_bytes = build(program.file, Flavour::Mock, level, temp.path());
        let mock_module = Module::new(engine(), &mock_bytes).expect("the module should validate");
        assert_eq!(
            import_section(&mock_module),
            Vec::new(),
            "{context}: the mock build has no imports"
        );
        let from_mock = run_cases(&mock_module, &program, false, &format!("{context} (mock)"));

        for (export, (h, m)) in program.exports.iter().zip(from_host.iter().zip(&from_mock)) {
            for (case, (h, m)) in export.cases.iter().zip(h.iter().zip(m)) {
                assert_eq!(
                    as_mock(h),
                    as_mock(m),
                    "{context}: {} differs between the host and the mock build",
                    describe(export, &case.args)
                );
            }
        }

        if level == "2" {
            // The same source gives the same bytes.
            assert_eq!(
                host_bytes,
                build_again(program.file, Flavour::Host, level, temp.path()),
                "{context}: the host build is not reproducible"
            );
            assert_eq!(
                mock_bytes,
                build_again(program.file, Flavour::Mock, level, temp.path()),
                "{context}: the mock build is not reproducible"
            );
        }
    }
}

/// A second build in a directory of its own, to compare bytes.
fn build_again(file: &str, flavour: Flavour, level: &str, temp: &Path) -> Vec<u8> {
    build(file, flavour, level, &temp.join("again"))
}

// ---------------------------------------------------------------------------
// The reference, in `i128` arithmetic.
// ---------------------------------------------------------------------------

fn checked(ty: Ty, v: i128) -> Option<i128> {
    ty.chk(v)
}

fn clock(k: u64) -> i128 {
    i128::from(START + k * STEP)
}

const I32_EDGES: [i128; 9] = [
    -2_147_483_648,
    -100_000,
    -7,
    -1,
    0,
    1,
    3,
    50_000,
    2_147_483_647,
];

// --- log_basic -------------------------------------------------------------

#[test]
fn log_basic() {
    let log = |v: i128| ev("log_i32", &[v]);
    check_program(Program {
        file: "log_basic",
        imports: vec![("sys_wasm_log_i32", "i32->")],
        define: None,
        exports: vec![
            Export::new("log_three", &[I32, I32], I32).pairs(
                &[-5, 0, 7, 2_147_483_647, -2_147_483_648],
                &[-3, 0, 1, 2_147_483_647, -2_147_483_648],
                |a, b| match checked(I32, a + b) {
                    Some(sum) => value(sum, vec![log(a), log(b), log(sum)]),
                    None => traps(vec![log(a), log(b)]),
                },
            ),
            Export::new("log_squares", &[I32], I32).each(
                &[-3, 0, 1, 2, 5, 10, 30, 1000, 1500, 1861, 1862, 2000],
                |n| {
                    let mut trace = Vec::new();
                    let mut total: i128 = 0;
                    for i in 0..n.max(0) {
                        let Some(square) = checked(I32, i * i) else {
                            return traps(trace);
                        };
                        trace.push(log(square));
                        match checked(I32, total + square) {
                            Some(t) => total = t,
                            None => return traps(trace),
                        }
                    }
                    value(total, trace)
                },
            ),
        ],
    });
}

#[test]
fn the_mock_records_what_the_host_is_given() {
    // `mock_count` and `mock_sum` exist only in the mock build. They report what
    // the mock was given, which must be what the host saw for the same call.
    let temp = tempdir().unwrap();
    let host = Module::new(
        engine(),
        build("log_basic", Flavour::Host, "2", temp.path()),
    )
    .unwrap();
    let mock = Module::new(
        engine(),
        build("log_basic", Flavour::Mock, "2", temp.path()),
    )
    .unwrap();
    let mut host = Running::new(&host, &[define_std]);
    let mut mock = Running::new(&mock, &[]);
    let squares = Export::new("log_squares", &[I32], I32);
    let count = Export::new("mock_count", &[I32], U32);
    let sum = Export::new("mock_sum", &[I32], I64);
    for n in [0, 1, 2, 10, 100, 1000] {
        assert!(matches!(host.call(&squares, &[n]), Got::Value(_)));
        let seen = &host.store.data().trace;
        let want_sum: i128 = seen.iter().map(|(_, args)| args[0]).sum();
        assert_eq!(
            mock.call(&count, &[n]),
            Got::Value(seen.len() as i128),
            "count for {n}"
        );
        assert_eq!(mock.call(&sum, &[n]), Got::Value(want_sum), "sum for {n}");
    }
}

// --- log_u64 ---------------------------------------------------------------

#[test]
fn log_u64() {
    let log = |v: i128| ev("log_u64", &[v]);
    let max = i128::from(u64::MAX);
    check_program(Program {
        file: "log_u64",
        imports: vec![("sys_wasm_log_i32", "i32->"), ("sys_wasm_log_u64", "i64->")],
        define: None,
        exports: vec![
            Export::new("collatz_steps", &[U64], U32).each(
                &[0, 1, 2, 3, 6, 7, 27, 97, 871, 1 << 40, (1 << 63) + 1, max],
                |start| {
                    let mut n = start;
                    let mut steps = 0;
                    let mut trace = Vec::new();
                    while n != 1 && n != 0 {
                        trace.push(log(n));
                        n = if n % 2 == 0 { n / 2 } else { 3 * n + 1 };
                        if checked(U64, n).is_none() {
                            return traps(trace);
                        }
                        steps += 1;
                    }
                    trace.push(log(n));
                    value(steps, trace)
                },
            ),
            Export::new("log_wide", &[U64], U64).each(&[0, 1, 255, 1 << 32, 1 << 63, max], |x| {
                value(
                    x,
                    vec![
                        log(x),
                        log(max - x),
                        ev("log_i32", &[-2_147_483_648]),
                        ev("log_i32", &[2_147_483_647]),
                    ],
                )
            }),
        ],
    });
}

// --- clock_elapsed ---------------------------------------------------------

#[test]
fn clock_elapsed() {
    let now = || ev("now_ms", &[]);
    check_program(Program {
        file: "clock_elapsed",
        imports: vec![("sys_wasm_now_ms", "->i64")],
        define: None,
        exports: vec![
            Export::new("elapsed", &[U32], U64).each(
                &[0, 1, 2, 10, 1000, 92_682, 100_000],
                |work| {
                    let mut acc: i128 = 0;
                    for i in 0..work {
                        acc += i;
                        if checked(U32, acc).is_none() {
                            return traps(vec![now()]);
                        }
                    }
                    value(i128::from(STEP) + acc, vec![now(), now()])
                },
            ),
            Export::new("three_reads", &[], U64).calls(&[vec![]], |_| {
                let (a, b, c) = (clock(0), clock(1), clock(2));
                value(a + b * 2 + c * 3, vec![now(), now(), now()])
            }),
        ],
    });
}

// --- clock_deadline --------------------------------------------------------

#[test]
fn clock_deadline() {
    check_program(Program {
        file: "clock_deadline",
        imports: vec![("sys_wasm_now_ms", "->i64")],
        define: None,
        exports: vec![Export::new("ticks_until", &[U64], U32).each(
            &[0, 1, 999, 1000, 1001, 1010, 1011, 1055, 2000, 5000, 10_000],
            |deadline| {
                let mut n = 0;
                while clock(n) < deadline {
                    n += 1;
                }
                value(i128::from(n), (0..=n).map(|_| ev("now_ms", &[])).collect())
            },
        )],
    });
}

// --- abort_guard -----------------------------------------------------------

#[test]
fn abort_guard() {
    let abort = |code: u32| vec![ev("abort", &[i128::from(code)])];
    check_program(Program {
        file: "abort_guard",
        imports: vec![("sys_wasm_abort", "i32->")],
        define: None,
        exports: vec![
            Export::new("checked_double", &[I32], I32).each(
                &[-2_147_483_648, -100, -1, 0, 1, 50, 100, 101, 2_147_483_647],
                |x| {
                    if x > 100 {
                        Expect {
                            want: Want::Abort(7),
                            trace: abort(7),
                        }
                    } else if x < 0 {
                        Expect {
                            want: Want::Abort(8),
                            trace: abort(8),
                        }
                    } else {
                        value(x * 2, Vec::new())
                    }
                },
            ),
            Export::new("abort_with", &[U32], U32).each(
                &[0, 1, 7, 65_536, 4_294_967_295],
                |code| Expect {
                    want: Want::Abort(code as u32),
                    trace: abort(code as u32),
                },
            ),
        ],
    });
}

#[test]
fn a_host_that_returns_from_abort_does_not_resume_the_program() {
    // `std::wasm::abort` never returns. If the host's import returns anyway, the
    // module traps with `unreachable`.
    let temp = tempdir().unwrap();
    for level in ["0", "2"] {
        let bytes = build("abort_guard", Flavour::Host, level, temp.path());
        let module = Module::new(engine(), &bytes).unwrap();
        let mut running = Running::new(&module, &[define_std]);
        running.store.data_mut().abort_returns = true;
        let export = Export::new("checked_double", &[I32], I32);
        for x in [101, -1, 2_147_483_647] {
            let got = running.call(&export, &[x]);
            assert_eq!(
                got,
                Got::Trap(Trap::UnreachableCodeReached),
                "O{level} x={x}"
            );
            assert_eq!(running.store.data().trace.len(), 1, "O{level} x={x}");
            // A call that does not abort still works.
            assert_eq!(running.call(&export, &[21]), Got::Value(42));
        }
    }
}

#[test]
fn an_error_from_the_host_reaches_the_caller_and_the_instance_survives() {
    let temp = tempdir().unwrap();
    for level in ["0", "1", "2", "s"] {
        let bytes = build("abort_guard", Flavour::Host, level, temp.path());
        let module = Module::new(engine(), &bytes).unwrap();
        let mut running = Running::new(&module, &[define_std]);
        let export = Export::new("checked_double", &[I32], I32);
        for round in 0..500 {
            assert_eq!(
                running.call(&export, &[1000]),
                Got::Abort(7),
                "round {round}"
            );
            assert_eq!(running.call(&export, &[-4]), Got::Abort(8), "round {round}");
            let fine = round % 100;
            assert_eq!(running.call(&export, &[fine]), Got::Value(2 * fine));
        }
    }
}

// --- extern_basic ----------------------------------------------------------

#[test]
fn extern_basic() {
    let mul = |a: i128, b: i128| ev("host_mul", &[a, b]);
    let wrap32 = |v: i128| I32.wrap(v);
    check_program(Program {
        file: "extern_basic",
        imports: vec![
            ("host_mul", "i32,i32->i32"),
            ("host_neg", "i64->i64"),
            ("host_zero", "->i32"),
        ],
        define: Some(define_extern_basic),
        exports: vec![
            Export::new("product", &[I32, I32], I32).pairs(&I32_EDGES, &I32_EDGES, |a, b| {
                value(wrap32(a * b), vec![mul(a, b)])
            }),
            Export::new("negated", &[I64], I64).each(
                &[i128::from(i64::MIN), -5, -1, 0, 1, 99, i128::from(i64::MAX)],
                |a| value(I64.wrap(-a), vec![ev("host_neg", &[a])]),
            ),
            Export::new("combined", &[I32, I32, I32], I32).calls(
                &[
                    vec![2, 3, 4],
                    vec![-7, 6, 5],
                    vec![0, 1, 2],
                    vec![65_536, 65_536, 3],
                    vec![2_147_483_647, 2, 1],
                    vec![-2_147_483_648, -1, 9],
                ],
                |args| {
                    let (a, b, c) = (args[0], args[1], args[2]);
                    let ab = wrap32(a * b);
                    value(
                        wrap32(ab * c),
                        vec![mul(a, b), mul(ab, c), ev("host_zero", &[])],
                    )
                },
            ),
        ],
    });
}

// --- extern_narrow ---------------------------------------------------------

#[test]
fn extern_narrow() {
    // The host receives a narrow integer zero-extended in an `i32`.
    let zext = |ty: Ty, v: i128| v.rem_euclid(1 << ty.bits());
    let u8s = [0, 1, 2, 127, 128, 200, 254, 255];
    let i8s = [-128, -127, -2, -1, 0, 1, 2, 126, 127];
    let u16s = [0, 1, 255, 256, 1000, 32_767, 32_768, 65_535];
    let i16s = [-32_768, -32_767, -256, -1, 0, 1, 255, 32_767];
    check_program(Program {
        file: "extern_narrow",
        imports: vec![
            ("host_add_u16_i16", "i32,i32->i32"),
            ("host_inc_u8", "i32->i32"),
            ("host_negate_i8", "i32->i32"),
            ("host_swap_bool", "i32->i32"),
        ],
        define: Some(define_extern_narrow),
        exports: vec![
            Export::new("inc", &[U8], U8).each(&u8s, |a| {
                value(U8.wrap(a + 1), vec![ev("host_inc_u8", &[a])])
            }),
            Export::new("negate", &[I8], I8).each(&i8s, |a| {
                value(I8.wrap(-a), vec![ev("host_negate_i8", &[zext(I8, a)])])
            }),
            Export::new("swap", &[Bool], Bool)
                .each(&[0, 1], |a| value(1 - a, vec![ev("host_swap_bool", &[a])])),
            Export::new("add_mixed", &[U16, I16], I16).pairs(&u16s, &i16s, |a, b| {
                value(
                    I16.wrap(a + b),
                    vec![ev("host_add_u16_i16", &[a, zext(I16, b)])],
                )
            }),
            Export::new("widened", &[U8], U32).each(&u8s, |a| {
                value(U8.wrap(a + 1) + 1000, vec![ev("host_inc_u8", &[a])])
            }),
        ],
    });
}

// --- extern_i64 ------------------------------------------------------------

#[test]
fn extern_i64() {
    let a_values = [
        i128::from(i64::MIN),
        -1,
        0,
        1,
        1 << 40,
        i128::from(i64::MAX),
    ];
    let b_values = [0, 1, 2, 1 << 32, 1 << 63, i128::from(u64::MAX)];
    let u64s = [
        0,
        1,
        4_294_967_295,
        4_294_967_296,
        1 << 40,
        1 << 63,
        i128::from(u64::MAX),
    ];
    check_program(Program {
        file: "extern_i64",
        imports: vec![("host_high", "i64->i32"), ("host_mix", "i64,i64->i64")],
        define: Some(define_extern_i64),
        exports: vec![
            Export::new("mix", &[I64, U64], U64).pairs(&a_values, &b_values, |a, b| {
                value(U64.wrap(b * 31 + a), vec![ev("host_mix", &[a, b])])
            }),
            Export::new("high_word", &[U64], U32)
                .each(&u64s, |a| value(a >> 32, vec![ev("host_high", &[a])])),
        ],
    });
}

// --- extern_many -----------------------------------------------------------

#[test]
fn extern_many() {
    let ping = || ev("host_ping", &[]);
    check_program(Program {
        file: "extern_many",
        imports: vec![
            ("host_note", "i32,i32,i32,i64,i64,i32->"),
            ("host_ping", "->"),
        ],
        define: Some(define_extern_many),
        exports: vec![
            Export::new("report", &[I32, I32], I32).pairs(
                &[-5, 0, 3, 2_147_483_647],
                &[-2, 0, 3, 9, -2_147_483_648],
                |a, b| {
                    let trace = vec![
                        ping(),
                        ev(
                            "host_note",
                            &[1, a, b, 5_000_000_000, -5_000_000_000, i128::from(a < b)],
                        ),
                        ping(),
                        ev("host_note", &[2, b, a, 0, a, i128::from(a == b)]),
                        ping(),
                    ];
                    match checked(I32, a + b) {
                        Some(sum) => value(sum, trace),
                        None => traps(trace),
                    }
                },
            ),
            Export::new("repeat", &[U32], U32).each(&[0, 1, 2, 7, 100], |n| {
                value(
                    n,
                    (0..n)
                        .map(|i| ev("host_note", &[i, 0, 0, 0, 0, i128::from(i % 2 == 0)]))
                        .collect(),
                )
            }),
        ],
    });
}

// --- mixed_providers -------------------------------------------------------

#[test]
fn mixed_providers() {
    check_program(Program {
        file: "mixed_providers",
        imports: vec![("sys_wasm_now_ms", "->i64")],
        define: None,
        exports: vec![
            Export::new("stamp", &[I32], U64).each(&I32_EDGES, |x| {
                let trace = vec![ev("now_ms", &[])];
                match checked(I32, x + 1) {
                    Some(_) => value(clock(0), trace),
                    None => traps(trace),
                }
            }),
            Export::new("stamp_count", &[I32], U32).each(&I32_EDGES, |x| {
                match checked(I32, x + 1) {
                    Some(_) => value(2, vec![ev("now_ms", &[])]),
                    None => traps(vec![ev("now_ms", &[])]),
                }
            }),
        ],
    });
}

// --- recursion_logs --------------------------------------------------------

#[test]
fn recursion_logs() {
    let log = |v: i128| ev("log_i32", &[v]);
    check_program(Program {
        file: "recursion_logs",
        imports: vec![("sys_wasm_log_i32", "i32->")],
        define: None,
        exports: vec![Export::new("walk_down", &[I32], I32).each(
            &[0, 1, 2, 5, 10, 50, 1000, 5000],
            |depth| {
                let down = (0..=depth).rev().map(log);
                let up = (1..=depth).map(|d| log(-d));
                value(depth * (depth + 1) / 2, down.chain(up).collect())
            },
        )],
    });
}

// --- reentrant -------------------------------------------------------------

#[test]
fn reentrant() {
    check_program(Program {
        file: "reentrant",
        imports: vec![("host_callback", "i32->i32")],
        define: Some(define_reentrant),
        exports: vec![
            Export::new("inner", &[I32], I32).each(&I32_EDGES, |x| {
                match checked(I32, x + 3).zip(checked(I32, x + 15)) {
                    Some((a, b)) => match checked(I32, a + b) {
                        Some(sum) => value(sum, Vec::new()),
                        None => traps(Vec::new()),
                    },
                    None => traps(Vec::new()),
                }
            }),
            Export::new("outer", &[I32], I32).each(
                &[
                    -1000,
                    -3,
                    -1,
                    0,
                    1,
                    7,
                    100,
                    1_000_000,
                    300_000_000,
                    400_000_000,
                    2_147_483_640,
                ],
                |x| {
                    // The loop that fills the table, then the struct, then the call.
                    if (0..8).any(|k| checked(I32, x * k).is_none()) {
                        return traps(Vec::new());
                    }
                    if checked(I32, x + 1).is_none() || checked(I32, x + 2).is_none() {
                        return traps(Vec::new());
                    }
                    let call = vec![ev("host_callback", &[x])];
                    let Some(reply) = checked(I32, x + 3)
                        .zip(checked(I32, x + 15))
                        .and_then(|(a, b)| checked(I32, a + b))
                    else {
                        return traps(call);
                    };
                    // `reply + held.a + held.b + held.c`, then the table.
                    let mut sum = reply;
                    for part in [x, x + 1, x + 2] {
                        match checked(I32, sum + part) {
                            Some(v) => sum = v,
                            None => return traps(call),
                        }
                    }
                    for k in 0..8 {
                        match checked(I32, sum + x * k) {
                            Some(v) => sum = v,
                            None => return traps(call),
                        }
                    }
                    value(sum, call)
                },
            ),
        ],
    });
}

// --- seeded_roll -----------------------------------------------------------

#[test]
fn seeded_roll() {
    let next = |seed: u128| (seed * 1_103_515_245 + 12_345) % 2_147_483_648;
    let roll = |reading: u64, sides: u128| {
        let second = next(next(u128::from(reading)));
        second % sides
    };
    check_program(Program {
        file: "seeded_roll",
        imports: vec![("sys_wasm_now_ms", "->i64")],
        define: None,
        exports: vec![
            Export::new("roll_die", &[U32], U32).each(
                &[0, 1, 2, 6, 20, 1000, 4_294_967_295],
                |sides| {
                    let trace = vec![ev("now_ms", &[])];
                    if sides == 0 {
                        traps(trace)
                    } else {
                        value(roll(START, sides as u128) as i128, trace)
                    }
                },
            ),
            Export::new("two_rolls", &[U32], U32).each(&[0, 1, 6, 20, 99], |sides| {
                if sides == 0 {
                    return traps(vec![ev("now_ms", &[])]);
                }
                let first = roll(START, sides as u128);
                let second = roll(START + STEP, sides as u128);
                value(
                    (first * 100 + second) as i128,
                    vec![ev("now_ms", &[]), ev("now_ms", &[])],
                )
            }),
        ],
    });
}

// --- unused_extern ---------------------------------------------------------

#[test]
fn unused_extern() {
    check_program(Program {
        file: "unused_extern",
        imports: vec![],
        define: None,
        exports: vec![Export::new("double", &[I32], I32).each(&I32_EDGES, |x| {
            match checked(I32, x * 2) {
                Some(v) => value(v, Vec::new()),
                None => traps(Vec::new()),
            }
        })],
    });
}

// ---------------------------------------------------------------------------
// Imports that the host does not supply, and errors at build time.
// ---------------------------------------------------------------------------

#[test]
fn a_missing_import_fails_to_instantiate_with_a_message_that_names_it() {
    let temp = tempdir().unwrap();
    let bytes = build("log_u64", Flavour::Host, "2", temp.path());
    let module = Module::new(engine(), &bytes).unwrap();

    // No host at all.
    let mut store = Store::new(engine(), State::default());
    let error = wasmtime::Instance::new(&mut store, &module, &[]).unwrap_err();
    let text = format!("{error:?}");
    println!("no imports given: {text}");
    assert!(
        text.contains("expected 2 imports, found 0"),
        "the number of imports is named: {text}"
    );

    // A host that supplies all but one.
    let mut linker = Linker::new(engine());
    linker
        .func_wrap("env", "sys_wasm_log_i32", |_: i32| {})
        .unwrap();
    let mut store = Store::new(engine(), State::default());
    let error = linker.instantiate(&mut store, &module).unwrap_err();
    let text = format!("{error:?}");
    println!("one import missing: {text}");
    assert!(
        text.contains("env") && text.contains("sys_wasm_log_u64"),
        "the module and the name are in the message: {text}"
    );

    // A host that supplies an import with the wrong signature.
    let mut linker = Linker::new(engine());
    linker
        .func_wrap("env", "sys_wasm_log_i32", |_: i32| {})
        .unwrap();
    linker
        .func_wrap("env", "sys_wasm_log_u64", |_: i32| {})
        .unwrap();
    let mut store = Store::new(engine(), State::default());
    let error = linker.instantiate(&mut store, &module).unwrap_err();
    let text = format!("{error:?}");
    println!("wrong signature: {text}");
    assert!(
        text.contains("sys_wasm_log_u64"),
        "the import is named: {text}"
    );
}

fn build_error(source: &str) -> String {
    let temp = tempdir().unwrap();
    let path = temp.path().join("bad.fe");
    fs::write(&path, source).unwrap();
    let out = temp.path().join("out");
    let output = wasm_support::fe(&[
        "build",
        "--backend",
        "wasm",
        "--out-dir",
        out.to_str().unwrap(),
        path.to_str().unwrap(),
    ]);
    assert!(!output.status.success(), "the build should fail");
    assert!(
        !out.join("bad.wasm").exists(),
        "nothing is written when the build fails"
    );
    String::from_utf8_lossy(&output.stderr).to_string()
}

#[test]
fn imports_that_are_not_scalar_are_named_errors() {
    let wide = build_error(
        "extern { fn h(_ a: u256) -> i32 }\npub fn run(x: i32) -> i32 { unsafe { h(1) } }\n",
    );
    assert!(
        wide.contains(
            "cannot import `h` from the WebAssembly host: parameter `a: u256` is a 256-bit integer; imports take only `bool` and integers of at most 64 bits"
        ),
        "{wide}"
    );
    let result = build_error(
        "extern { fn h(_ a: i32) -> u128 }\npub fn run(x: i32) -> i32 {\n    let v = unsafe { h(1) }\n    x\n}\n",
    );
    assert!(
        result.contains("cannot import `h` from the WebAssembly host: the result type `u128` is a 128-bit integer"),
        "{result}"
    );
    let aggregate = build_error(
        "struct P { a: i32 }\nextern { fn h(_ p: P) -> i32 }\npub fn run(x: i32) -> i32 { unsafe { h(P { a: x }) } }\n",
    );
    assert!(
        aggregate.contains(
            "cannot import `h` from the WebAssembly host: parameter `p: P` is not a scalar"
        ),
        "{aggregate}"
    );
    let pointer = build_error(
        "use core::ptr::MemArray\nextern { fn h(_ p: *u8) -> i32 }\npub fn run(x: i32) -> i32 {\n    let a: MemArray<u8> = MemArray::new_zeroed(4)\n    unsafe { h(a.ptr()) }\n}\n",
    );
    assert!(
        pointer.contains("cannot import `h` from the WebAssembly host: parameter `p: *u8` is a pointer into Fe memory"),
        "{pointer}"
    );
}

#[test]
fn the_example_builds_and_runs_against_a_host() {
    let example = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../examples/wasm_host.fe");
    let temp = tempdir().unwrap();
    let bytes = build_wasm(&example, "2", temp.path());
    let module = Module::new(engine(), &bytes).unwrap();
    assert_eq!(
        import_section(&module),
        [
            ("env", "sys_wasm_abort", "i32->"),
            ("env", "sys_wasm_log_i32", "i32->"),
            ("env", "sys_wasm_now_ms", "->i64"),
        ]
        .map(|(m, n, s)| (m.to_string(), n.to_string(), s.to_string()))
    );
    let mut running = Running::new(&module, &[define_std]);
    let count_up = Export::new("count_up", &[I32], U64);
    assert_eq!(running.call(&count_up, &[3]), Got::Value(10));
    assert_eq!(
        running.store.data().trace,
        [
            ev("now_ms", &[]),
            ev("log_i32", &[0]),
            ev("log_i32", &[1]),
            ev("log_i32", &[2]),
            ev("now_ms", &[]),
        ]
    );
    let non_negative = Export::new("non_negative", &[I32], I32);
    assert_eq!(running.call(&non_negative, &[5]), Got::Value(5));
    assert_eq!(running.call(&non_negative, &[-1]), Got::Abort(1));
}
