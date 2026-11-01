#![cfg(feature = "wasm")]

//! Programs of the native suites, built for WebAssembly and entered at their
//! `pub fn main() -> i32` export: destruction, owners, raw references,
//! providers, arithmetic at every width and allocation limits. The host
//! supplies the `putchar` and `getchar` imports that the programs declare.

mod wasm_support;

use std::{fs, path::Path};

use tempfile::tempdir;
use wasm_support::{build_wasm, engine, fe};
use wasmtime::{Caller, Linker, Module, Store};

const LEVELS: [&str; 2] = ["0", "1"];

struct Host {
    input: Vec<u8>,
    output: Vec<u8>,
}

/// The result of `main`, or the error that ended it, and what it wrote.
struct Run {
    exit: Result<i32, String>,
    output: String,
}

/// Runs `main` with `input` behind `getchar`.
fn run(bytes: &[u8], input: &[u8]) -> Run {
    let module = Module::new(engine(), bytes).unwrap();
    let mut store = Store::new(
        engine(),
        Host {
            input: input.iter().rev().copied().collect(),
            output: Vec::new(),
        },
    );
    let mut linker = Linker::new(engine());
    linker
        .func_wrap(
            "env",
            "putchar",
            |mut caller: Caller<'_, Host>, c: i32| -> i32 {
                caller.data_mut().output.push(c as u8);
                c & 0xff
            },
        )
        .unwrap();
    linker
        .func_wrap("env", "getchar", |mut caller: Caller<'_, Host>| -> i32 {
            caller.data_mut().input.pop().map_or(-1, i32::from)
        })
        .unwrap();
    let instance = linker.instantiate(&mut store, &module).unwrap();
    let exit = instance
        .get_typed_func::<(), i32>(&mut store, "main")
        .unwrap()
        .call(&mut store, ())
        .map_err(|error| error.to_string());
    Run {
        exit,
        output: String::from_utf8_lossy(&store.data().output).into_owned(),
    }
}

fn fixture(path: &str) -> String {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(path);
    fs::read_to_string(&path).unwrap_or_else(|error| panic!("{}: {error}", path.display()))
}

/// Builds `source` at each level and runs `main` with `input`.
fn runs(source: &str, input: &[u8]) -> Vec<Run> {
    let temp = tempdir().unwrap();
    let path = temp.path().join("program.fe");
    fs::write(&path, source).unwrap();
    LEVELS
        .iter()
        .map(|level| {
            let bytes = build_wasm(&path, level, &temp.path().join(format!("out-{level}")));
            run(&bytes, input)
        })
        .collect()
}

fn assert_runs(name: &str, source: &str, exit: i32, output: &str) {
    for (level, run) in LEVELS.iter().zip(runs(source, &[])) {
        assert_eq!(run.exit, Ok(exit), "{name} at O{level}");
        assert_eq!(run.output, output, "{name} at O{level}");
    }
}

#[test]
fn values_are_destroyed_once_in_scope_order() {
    assert_runs(
        "drop_order",
        &fixture("native/drop_order.fe"),
        0,
        "a.\n.a\n0123\nSs\nN\nA,B\n1;2\ny\nx\nx\ny\noi\nABC\nZ-[-\nH-KLJI\n-l\n!r\nA-B-C--Z\nc-c\n-e-ox\n.-p\n+o\n-e\n-BOIA\nq-k\nkQ\n-Vv-\n-u\nU\n-GF\nu1u2u3u4guuuu\n1a\n-b\n-\n-0-1-.2\nba\n-B\nd\nk",
    );
}

#[test]
fn owners_destroy_what_they_hold() {
    assert_runs(
        "owners",
        &fixture("native/owners.fe"),
        0,
        "de-ab-c-fg\nxy-ABC\n-b\nu-U\n123\n",
    );
}

#[test]
fn raw_references_and_projections_execute() {
    for name in [
        "empty_owners",
        "raw_scalar_reference_accessors",
        "raw_aggregate_reference_accessors",
        "returned_reference_projection",
    ] {
        assert_runs(name, &fixture(&format!("native/{name}.fe")), 0, "");
    }
}

#[test]
fn representative_programs_execute() {
    for name in [
        "arithmetic",
        "control_flow",
        "aggregates",
        "wide_integer",
        "entry_name_collision",
    ] {
        assert_runs(
            name,
            &fixture(&format!("cli_output/native/{name}.fe")),
            0,
            "",
        );
    }
}

#[test]
fn trait_providers_keep_state() {
    for copy_impl in ["impl Copy for Counter {}", ""] {
        let source = format!(
            r#"
trait Tick {{ fn next(mut self) -> i32 }}
struct Counter {{ value: i32 }}
{copy_impl}
impl Tick for Counter {{
    fn next(mut self) -> i32 {{
        self.value += 1
        self.value
    }}
}}
fn counter() -> Counter {{ Counter {{ value: 0 }} }}
fn next() -> i32 uses (tick: mut Tick) {{ tick.next() }}
pub fn main() -> i32 {{
    with (Tick = counter()) {{ next() + next() }}
}}
"#
        );
        assert_runs(copy_impl, &source, 3, "");
    }
    assert_runs(
        "trait_provider_identity",
        &fixture("fe_test/trait_provider_identity.fe"),
        42,
        "",
    );
}

#[test]
fn unchecked_div_rem_match_every_width() {
    for bits in [8, 16, 32, 64, 128, 256] {
        let sign_bit = bits - 1;
        let source = format!(
            r#"
#[arithmetic(unchecked)]
fn signed(value: i{bits}, divisor: i{bits}, quotient: i{bits}, remainder: i{bits}) -> bool {{
    value / divisor == quotient && value % divisor == remainder
}}

#[arithmetic(unchecked)]
fn unsigned(value: u{bits}, divisor: u{bits}, quotient: u{bits}, remainder: u{bits}) -> bool {{
    value / divisor == quotient && value % divisor == remainder
}}

fn checked_rem(value: i{bits}, divisor: i{bits}) -> i{bits} {{
    value % divisor
}}

pub fn main() -> i32 {{
    let min: i{bits} = 1 << {sign_bit}
    if !signed(value: -17, divisor: 0, quotient: 0, remainder: 0) {{ return 1 }}
    if !unsigned(value: 17, divisor: 0, quotient: 0, remainder: 0) {{ return 2 }}
    if !signed(value: min, divisor: -1, quotient: min, remainder: 0) {{ return 3 }}
    if !signed(value: -17, divisor: 5, quotient: -3, remainder: -2) {{ return 4 }}
    if !signed(value: 17, divisor: -5, quotient: -3, remainder: 2) {{ return 5 }}
    if !unsigned(value: 17, divisor: 5, quotient: 3, remainder: 2) {{ return 6 }}
    if checked_rem(value: min, divisor: -1) != 0 {{ return 7 }}
    0
}}
"#
        );
        assert_runs(&format!("{bits}-bit"), &source, 0, "");
    }
}

#[test]
fn checked_arithmetic_traps() {
    let mut cases = Vec::new();
    for bits in [8, 128, 256] {
        let sign_bit = bits - 1;
        let min = format!("1 << {sign_bit}");
        for (prefix, op, value, rhs) in [
            ("u", "/", "17".to_string(), "0".to_string()),
            ("u", "%", "17".to_string(), "0".to_string()),
            ("i", "/", "-17".to_string(), "0".to_string()),
            ("i", "%", "-17".to_string(), "0".to_string()),
            ("i", "/", min.clone(), "-1".to_string()),
            ("u", "*", "1 << 7".to_string(), format!("1 << {}", bits - 7)),
            ("i", "-", min.clone(), "1".to_string()),
            ("u", "**", "2".to_string(), bits.to_string()),
            ("i", "**", "2".to_string(), sign_bit.to_string()),
            ("i", "**", "3".to_string(), "-1".to_string()),
        ] {
            cases.push((format!("{prefix}{bits}"), op, value, rhs));
        }
    }

    // `getchar` selects a case, so one module covers every case.
    let mut source = String::from("extern {\n    fn getchar() -> i32\n}\n");
    for (index, (ty, op, _, _)) in cases.iter().enumerate() {
        source.push_str(&format!(
            "fn calculate_{index}(value: {ty}, rhs: {ty}) -> {ty} {{ value {op} rhs }}\n"
        ));
    }
    source.push_str("pub fn main() -> i32 {\n    let selector = unsafe { getchar() }\n");
    for (index, (_, _, value, rhs)) in cases.iter().enumerate() {
        source.push_str(&format!(
            "    if selector == {index} {{ if calculate_{index}(value: {value}, rhs: {rhs}) == 0 {{ return 0 }} else {{ return 1 }} }}\n"
        ));
    }
    source.push_str("    97\n}\n");

    let temp = tempdir().unwrap();
    let path = temp.path().join("checked.fe");
    fs::write(&path, &source).unwrap();
    for level in LEVELS {
        let bytes = build_wasm(&path, level, &temp.path().join(format!("out-{level}")));
        assert_eq!(run(&bytes, &[]).exit, Ok(97), "control at O{level}");
        for (index, (ty, op, value, rhs)) in cases.iter().enumerate() {
            assert!(
                run(&bytes, &[u8::try_from(index).unwrap()]).exit.is_err(),
                "checked {ty} {value} {op} {rhs} did not trap at O{level}"
            );
        }
    }
}

#[test]
fn allocations_beyond_the_address_space_fail() {
    let source = r#"
use core::Option
use core::ptr

extern {
    fn getchar() -> i32
}

fn fails(_ len: usize) -> bool {
    match ptr::try_alloc_bytes(len) {
        Option::Some(_) => false
        Option::None => true
    }
}

pub fn main() -> i32 {
    if unsafe { getchar() } >= 0 {
        let _ = ptr::alloc_bytes(usize::max())
        return 3
    }
    if !fails(usize::max()) || !fails(usize::max() - 15) { return 1 }
    if fails(64) { return 2 }
    0
}
"#;
    let temp = tempdir().unwrap();
    let path = temp.path().join("oversized.fe");
    fs::write(&path, source).unwrap();
    for level in LEVELS {
        let bytes = build_wasm(&path, level, &temp.path().join(format!("out-{level}")));
        assert_eq!(run(&bytes, &[]).exit, Ok(0), "O{level}");
        assert!(run(&bytes, b"x").exit.is_err(), "O{level}");
    }
}

#[test]
fn the_test_runner_executes_host_fixtures() {
    for name in [
        "aggregate_object_copies",
        "const_aggregate_values",
        "drops",
        "fallible_allocation",
        "mem_drop_and_forget",
    ] {
        let fixture = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures/fe_test")
            .join(format!("{name}.fe"));
        for level in LEVELS {
            let result = fe(&[
                "test",
                "--backend",
                "wasm",
                "-O",
                level,
                fixture.to_str().unwrap(),
            ]);
            assert!(
                result.status.success(),
                "{name} at O{level}:\n{}\n{}",
                String::from_utf8_lossy(&result.stdout),
                String::from_utf8_lossy(&result.stderr)
            );
        }
    }
}
