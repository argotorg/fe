#![cfg(all(
    feature = "cranelift",
    any(
        all(target_arch = "x86_64", target_os = "linux"),
        all(target_arch = "aarch64", target_os = "macos")
    )
))]

use std::{
    array,
    ffi::OsString,
    fs,
    io::Write,
    os::unix::{
        ffi::OsStringExt,
        process::{CommandExt, ExitStatusExt},
    },
    path::Path,
    process::{Command, Output, Stdio},
    thread,
    time::{Duration, Instant},
};

use num_bigint::BigUint;
use tempfile::tempdir;

fn build(source: &Path, out: &Path, level: &str, extra: &[&str]) -> Output {
    let result = Command::new(env!("CARGO_BIN_EXE_fe"))
        .args([
            "build",
            "--backend",
            "native",
            "--emit",
            "ir,executable",
            "-O",
            level,
        ])
        .arg("--out-dir")
        .arg(out)
        .args(extra)
        .arg(source)
        .output()
        .unwrap();
    assert!(
        result.status.success(),
        "native build failed:\n{}\n{}",
        String::from_utf8_lossy(&result.stdout),
        String::from_utf8_lossy(&result.stderr)
    );
    result
}

#[test]
fn native_workspace_build_selects_root_entries_and_reachable_dependencies() {
    let temp = tempdir().unwrap();
    let root = temp.path();
    fs::create_dir_all(root.join("app/src")).unwrap();
    fs::create_dir_all(root.join("math/src")).unwrap();
    fs::write(
        root.join("fe.toml"),
        r#"
[workspace]
name = "native_workspace"
version = "0.1.0"
members = [{ path = "app", name = "app" }, { path = "math", name = "math" }]
"#,
    )
    .unwrap();
    fs::write(
        root.join("app/fe.toml"),
        r#"
[ingot]
name = "app"
version = "0.1.0"
[dependencies]
math = true
"#,
    )
    .unwrap();
    fs::write(
        root.join("math/fe.toml"),
        "[ingot]\nname = \"math\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(
        root.join("math/src/lib.fe"),
        "pub fn answer() -> i32 { 42 }\npub fn unused<T>(x: own T) -> T { x }\n#[test]\nfn math_test() { core::assert(answer() == 42) }\n",
    )
    .unwrap();
    fs::write(
        root.join("app/src/lib.fe"),
        "pub fn main() -> i32 { helper::answer() }\n#[test]\nfn root_test() { core::assert(main() == 42) }\n",
    )
    .unwrap();
    fs::write(
        root.join("app/src/helper.fe"),
        "use std::io::{Write, host, write_char}\npub fn answer() -> i32 {\n with (Write = host()) { write_char(65) }\n math::answer()\n}\n#[test]\nfn nested_test() { core::assert(answer() == 42) }\n",
    )
    .unwrap();
    for level in ["0", "1"] {
        let out = root.join(format!("out-{level}"));
        build(root, &out, level, &["--ingot", "app"]);
        let result = Command::new(out.join("app")).output().unwrap();
        assert_eq!(result.status.code(), Some(42));
        assert_eq!(result.stdout, b"A");
        let ir = fs::read_to_string(out.join("app.native.sona")).unwrap();
        assert!(!ir.contains("unused"));
        build(root, &out, level, &[]);
        assert!(!out.join("math").exists());
        build(&root.join("app"), &out, level, &[]);
        for (selection, expected_count) in [(&["--ingot", "app"][..], 2), (&[][..], 3)] {
            let result = Command::new(env!("CARGO_BIN_EXE_fe"))
                .args(["test", "--backend", "native", "-O", level])
                .args(selection)
                .arg(root)
                .output()
                .unwrap();
            assert!(result.status.success(), "{result:?}");
            assert!(
                String::from_utf8_lossy(&result.stdout)
                    .contains(&format!("{expected_count} passed; 0 failed")),
                "{result:?}"
            );
        }
    }
}

#[test]
fn native_host_io_and_assertions_execute_at_o0_and_o1() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("echo.fe");
    fs::write(
        &source,
        r#"
use std::io::{Read, Write, host, read_char, write_char, writeln}
pub fn main() -> i32 {
    with (Read = host(), Write = host()) {
        writeln("ready")
        let c = read_char()
        core::assert(c == 65)
        write_char(c + 1)
    }
    0
}

"#,
    )
    .unwrap();
    for level in ["0", "1"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let mut child = Command::new(out.join("echo"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();
        child.stdin.take().unwrap().write_all(b"A").unwrap();
        let result = child.wait_with_output().unwrap();
        assert!(result.status.success());
        assert_eq!(result.stdout, b"ready\nB");

        let mut child = Command::new(out.join("echo"))
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .spawn()
            .unwrap();
        child.stdin.take().unwrap().write_all(b"Z").unwrap();
        assert!(
            !child.wait().unwrap().success(),
            "failed assertion must trap"
        );
    }
}

#[test]
fn native_assert_message_and_panic_code_trap() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("traps.fe");
    fs::write(
        &source,
        r#"
fn checked(_ value: u256) -> u256 {
    assert!(value < 10, "value too large")
    value
}
#[test]
fn pass_small() { core::assert(checked(3) == 3) }
#[test]
fn fail_message() { core::assert(checked(11) == 11) }
#[test]
fn fail_code() { core::panic_code(0x32) }
"#,
    )
    .unwrap();
    let result = Command::new(env!("CARGO_BIN_EXE_fe"))
        .args(["test", "--backend", "native", "--jobs", "1"])
        .arg(&source)
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&result.stdout);
    assert!(!result.status.success(), "{result:?}");
    assert!(stdout.contains("1 passed; 2 failed"), "{result:?}");
}

#[test]
fn native_runner_executes_tests_filters_and_retains_only_requested_artifacts() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("cases.fe");
    let scratch = temp.path().join("scratch");
    fs::create_dir(&scratch).unwrap();
    fs::write(
        &source,
        r#"
fn difference(left: u256, right: u256) -> u256 { left - right }
#[test]
fn pass_arithmetic() { core::assert(difference(left: 11, right: 3) == 8) }
#[test]
fn pass_wide() {
    let high: u256 = 1 << 192
    core::assert(difference(left: high + 7, right: high) == 7)
}

#[test]
fn fail_assertion() { core::assert(false) }
"#,
    )
    .unwrap();
    for level in ["0", "1"] {
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["test", "--backend", "native", "--jobs", "2", "-O", level])
            .env("TMPDIR", &scratch)
            .arg(&source)
            .output()
            .unwrap();
        let stdout = String::from_utf8_lossy(&result.stdout);
        assert!(
            !result.status.success(),
            "false assertion must fail the suite"
        );
        assert!(stdout.contains("2 passed; 1 failed"), "{result:?}");
        assert!(stdout.contains("fail_assertion"), "{result:?}");
        assert!(fs::read_dir(&scratch).unwrap().all(|entry| {
            !entry
                .unwrap()
                .file_name()
                .to_string_lossy()
                .starts_with("fe-native-test-")
        }));

        let report = temp.path().join(format!("report-{level}.tar.gz"));
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args([
                "test",
                "--backend",
                "native",
                "--grouped",
                "--filter",
                "pass_",
                "--emit",
                "ir,rmir",
                "--report",
                "--report-out",
            ])
            .arg(&report)
            .args(["-O", level])
            .env("TMPDIR", &scratch)
            .arg(&source)
            .output()
            .unwrap();
        assert!(result.status.success(), "{result:?}");
        assert!(String::from_utf8_lossy(&result.stdout).contains("2 passed; 0 failed"));
        assert!(fs::read_dir(&scratch).unwrap().all(|entry| {
            !entry
                .unwrap()
                .file_name()
                .to_string_lossy()
                .starts_with("fe-native-test-")
        }));
        let ir = fs::read_to_string(temp.path().join("out/cases.native-0.test.sona")).unwrap();
        assert!(
            !ir.contains("fail_assertion"),
            "filtered test must not be compiled"
        );
        assert!(temp.path().join("out/cases.native-0.test.rmir").exists());
        let listing = Command::new("tar")
            .arg("-tzf")
            .arg(&report)
            .output()
            .unwrap();
        assert!(listing.status.success(), "{listing:?}");
        let listing = String::from_utf8_lossy(&listing.stdout);
        assert_eq!(
            listing
                .lines()
                .filter(|line| line.ends_with("/tests.o"))
                .count(),
            1
        );
        assert!(listing.contains("test-0/status.txt"), "{listing}");
        assert!(listing.contains("test-1/status.txt"), "{listing}");
    }
}

#[test]
fn native_runner_bounds_time_and_combined_output() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("limits.fe");
    fs::write(
        &source,
        r#"
use std::io::{Write, host, write_char}
#[test]
fn spin() { while true {} }
#[test]
fn noisy() {
    with (Write = host()) {
        let mut i: u32 = 0
        while i < 2048 {
            write_char(65)
            i += 1
        }
    }
}
"#,
    )
    .unwrap();

    for (filter, limit_args, expected) in [
        (
            "spin",
            &["--native-timeout-secs", "1"][..],
            "exceeded the time limit",
        ),
        (
            "noisy",
            &["--native-output-limit-kib", "1"][..],
            "exceeded the output limit",
        ),
    ] {
        let mut child = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["test", "--backend", "native", "--filter", filter])
            .args(limit_args)
            .arg(&source)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .process_group(0)
            .spawn()
            .unwrap();
        let deadline = Instant::now() + Duration::from_secs(15);
        loop {
            if child.try_wait().unwrap().is_some() {
                break;
            }
            if Instant::now() >= deadline {
                unsafe { libc::killpg(child.id() as i32, libc::SIGKILL) };
                child.wait().unwrap();
                panic!("native {filter} test did not terminate within 15 seconds");
            }
            thread::sleep(Duration::from_millis(10));
        }
        let result = child.wait_with_output().unwrap();
        let output = String::from_utf8_lossy(&result.stdout);
        assert!(!result.status.success(), "{result:?}");
        assert!(output.contains(expected), "{result:?}");
        assert!(result.stdout.len() < 8 * 1024, "{result:?}");
    }
}

#[test]
fn native_runner_rejects_evm_attributes_and_trace_options() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("unsupported.fe");
    for attribute in ["should_revert", "balance = 10"] {
        fs::write(
            &source,
            format!("#[test({attribute})]\nfn evm_test() {{}}\n"),
        )
        .unwrap();
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["test", "--backend", "native"])
            .arg(&source)
            .output()
            .unwrap();
        assert!(!result.status.success());
        assert!(
            String::from_utf8_lossy(&result.stdout).contains("cannot use EVM"),
            "{result:?}"
        );
    }
    for option in ["--trace-evm", "--show-logs", "--call-trace"] {
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["test", "--backend", "native", option])
            .arg(&source)
            .output()
            .unwrap();
        assert!(!result.status.success());
        assert!(
            String::from_utf8_lossy(&result.stderr).contains("EVM logs or tracing"),
            "{result:?}"
        );
    }
}

#[test]
fn native_reference_fields_and_slots_preserve_referent_identity() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("reference_identity.fe");
    fs::write(
        &source,
        r#"
use core::ptr
struct Handle { value: mut i32, calls: i32 }
fn step() uses (handle: mut Handle) {
    handle.value += 1
    handle.calls += 1
}
fn replace(slot: *mut i32, value: mut i32) { *slot = value }
fn increment(slot: *mut i32) { *slot += 1 }
pub fn main() -> i32 {
    let mut first: i32 = 20
    let mut second: i32 = 40
    let mut handle = Handle { value: mut first, calls: 0 }
    with (handle) {
        step()
        step()
    }
    core::assert(handle.calls == 2)
    core::assert(handle.value == 22)
    let slot = ptr::alloc<mut i32>()
    *slot = mut first
    increment(slot)
    replace(slot, value: mut second)
    increment(slot)
    // The heap slot must not retain a reference to a local when main returns.
    let retained = ptr::alloc<i32>()
    *retained = 0
    replace(slot, value: mut *retained)
    core::assert(first == 23)
    core::assert(second == 41)
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        assert!(
            Command::new(out.join("reference_identity"))
                .status()
                .unwrap()
                .success(),
            "reference identity at O{level}"
        );
    }
}

#[test]
fn native_loop_carried_struct_values_reuse_storage_after_reads() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("value_loop.fe");
    fs::write(
        &source,
        r#"
struct Pair { low: u64, high: u64 }
impl Copy for Pair {}
fn next(_ value: Pair) -> Pair {
    Pair { low: value.low + 1, high: value.high + 3 }
}
pub fn main() -> i32 {
    let mut value = Pair { low: 2, high: 7 }
    let mut i: u64 = 0
    while i < 17 {
        value = next(value)
        value.low = value.low + 1
        if (i & 1) == 0 {
            value = next(value)
            value.high = value.high + 1
        }
        i += 1
    }
    core::assert(value.low == 45)
    core::assert(value.high == 94)
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        assert!(
            Command::new(out.join("value_loop"))
                .status()
                .unwrap()
                .success()
        );
    }
}

#[test]
fn native_process_clock_preserves_borrows_and_host_io() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("clock.fe");
    fs::write(
        &source,
        r#"
use std::io::{HostIo, Read, Write, host}
use std::native::cpu_clock_ticks
pub fn main() -> i32 {
    let mut input = host()
    let mut output = host()
    let mut value: i32 = 0
    let borrowed = mut value
    let start = cpu_clock_ticks()
    borrowed = input.read_char()
    let end = cpu_clock_ticks()
    core::assert(start >= 0 && end >= start)
    output.write_char(c: value)
    core::assert(!output.failed())
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let mut child = Command::new(out.join("clock"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();
        child.stdin.take().unwrap().write_all(b"A").unwrap();
        let result = child.wait_with_output().unwrap();
        assert!(result.status.success(), "{result:?}");
        assert_eq!(result.stdout, b"A");
    }
}

#[test]
fn native_arguments_preserve_bytes_bounds_and_internal_entry_calls() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("arguments.fe");
    fs::write(
        &source,
        r#"
use std::io::{Write, host, write_char}
use std::native::Args
fn __fe_native_main() -> i32 { 42 }
pub fn main(argc: i32, argv: **u8) -> i32 {
    if argc == 0 { return __fe_native_main() }
    let args = Args::new(argc, argv)
    core::assert(args.len() == argc.downcast_unchecked())
    if argc == 1 { args.get(args.len()) }
    if argc == 2 {
        let arg = args.get(1)
        arg.byte_at(arg.len())
    }
    core::assert(main(argc: 0, argv) == 42)
    with (Write = host()) {
        let mut i: usize = 1
        while i < args.len() {
            let arg = args.get(i)
            let mut j: usize = 0
            while j < arg.len() {
                write_char(arg.byte_at(j) as i32)
                j += 1
            }
            write_char(124)
            i += 1
        }
    }
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let executable = out.join("arguments");
        let result = Command::new(&executable)
            .args(["", "Fe", "λ"])
            .arg(OsString::from_vec(vec![0x80, 0xff]))
            .output()
            .unwrap();
        assert!(result.status.success(), "{result:?}");
        assert_eq!(result.stdout, b"|Fe|\xce\xbb|\x80\xff|");
        for args in [&[][..], &[""][..], &["bounds"][..]] {
            let result = Command::new(&executable).args(args).output().unwrap();
            assert!(
                !result.status.success(),
                "out-of-bounds argument access must trap"
            );
        }
    }
}

#[test]
fn native_modular_arithmetic_matches_full_precision_runtime_inputs() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("modular.fe");
    fs::write(
        &source,
        r#"
use std::io::{HostIo, Read, Write, host, read_char}
use std::evm::crypto::{addmod, mulmod}
fn read_word() -> u256 uses (input: mut Read) {
    let mut value: u256 = 0
    let mut i: u256 = 0
    while i < 32 {
        let byte = read_char()
        core::assert(byte >= 0)
        let bits: u256 = byte.downcast_unchecked()
        value = value | (bits << (i * 8))
        i += 1
    }
    value
}
fn write_word(_ value: u256) uses (output: mut HostIo) {
    let mut i: u256 = 0
    while i < 32 {
        output.write_char(c: ((value >> (i * 8)) & 255).downcast_unchecked())
        core::assert(!output.failed())
        i += 1
    }
}
pub fn main() -> i32 {
    with (Read = host(), HostIo = host()) {
        let mut marker = read_char()
        while marker != -1 {
            core::assert(marker == 64)
            let lhs = read_word()
            let rhs = read_word()
            let modulus = read_word()
            write_word(addmod(lhs, rhs, modulus))
            write_word(mulmod(lhs, rhs, modulus))
            marker = read_char()
        }
    }
    0
}
"#,
    )
    .unwrap();
    let one = BigUint::from(1u8);
    let max = (&one << 256usize) - &one;
    let boundaries = [
        BigUint::from(0u8),
        one.clone(),
        BigUint::from(2u8),
        (&one << 64usize) - &one,
        &one << 64usize,
        &one << 128usize,
        &one << 255usize,
        &max - &one,
        max,
    ];
    let mut cases = Vec::new();
    for lhs in &boundaries {
        for rhs in &boundaries {
            for modulus in &boundaries {
                cases.push([lhs.clone(), rhs.clone(), modulus.clone()]);
            }
        }
    }
    let mut seed = 0x8932_1227_55ab_011du64;
    for _ in 0..128 {
        cases.push(array::from_fn(|_| {
            let mut bytes = [0u8; 32];
            for chunk in bytes.as_chunks_mut::<8>().0 {
                seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
                chunk.copy_from_slice(&seed.to_le_bytes());
            }
            BigUint::from_bytes_le(&bytes)
        }));
    }
    let mut input = Vec::new();
    let mut expected = Vec::new();
    for [lhs, rhs, modulus] in cases {
        input.push(64);
        for value in [&lhs, &rhs, &modulus] {
            let mut bytes = value.to_bytes_le();
            bytes.resize(32, 0);
            input.extend(bytes);
        }
        for value in [&lhs + &rhs, &lhs * &rhs] {
            let result = if modulus == BigUint::from(0u8) {
                BigUint::from(0u8)
            } else {
                value % &modulus
            };
            let mut bytes = result.to_bytes_le();
            bytes.resize(32, 0);
            expected.extend(bytes);
        }
    }
    let input_path = temp.path().join("input.bin");
    fs::write(&input_path, input).unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("modular"))
            .stdin(fs::File::open(&input_path).unwrap())
            .output()
            .unwrap();
        assert!(result.status.success(), "{result:?}");
        assert_eq!(result.stdout.len(), expected.len());
        for (index, (actual, expected)) in result
            .stdout
            .as_chunks::<64>()
            .0
            .iter()
            .zip(expected.as_chunks::<64>().0)
            .enumerate()
        {
            assert_eq!(actual, expected, "case {index} at O{level}");
        }
    }
}

#[test]
fn native_bit_counts_match_runtime_inputs() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("bits.fe");
    fs::write(
        &source,
        r#"
use std::io::{HostIo, Read, Write, host, read_char}
use core::num::{leading_zeros, trailing_zeros}
fn read_word() -> u256 uses (input: mut Read) {
    let mut value: u256 = 0
    let mut i: u256 = 0
    while i < 32 {
        let byte = read_char()
        core::assert(byte >= 0)
        let bits: u256 = byte.downcast_unchecked()
        value = value | (bits << (i * 8))
        i += 1
    }
    value
}
fn write_word(_ value: u256) uses (output: mut HostIo) {
    let mut i: u256 = 0
    while i < 32 {
        output.write_char(c: ((value >> (i * 8)) & 255).downcast_unchecked())
        core::assert(!output.failed())
        i += 1
    }
}
pub fn main() -> i32 {
    with (Read = host(), HostIo = host()) {
        let mut marker = read_char()
        while marker != -1 {
            core::assert(marker == 64)
            let value = read_word()
            write_word(leading_zeros(value))
            write_word(trailing_zeros(value))
            marker = read_char()
        }
    }
    0
}
"#,
    )
    .unwrap();
    let one = BigUint::from(1u8);
    let max = (&one << 256usize) - &one;
    let mut cases = vec![BigUint::from(0u8), max.clone()];
    for bit in 0..256usize {
        let power = &one << bit;
        cases.push(&power - &one);
        cases.push(&power + &one);
        cases.push(power);
    }
    let mut seed = 0x5eed_c1a2_0000_0001u64;
    for index in 0..128usize {
        let mut bytes = [0u8; 32];
        for chunk in bytes.as_chunks_mut::<8>().0 {
            seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
            chunk.copy_from_slice(&seed.to_le_bytes());
        }
        cases.push(BigUint::from_bytes_le(&bytes) >> (index * 2));
    }
    let mut input = Vec::new();
    let mut expected = Vec::new();
    for value in cases {
        let value = value & &max;
        input.push(64);
        let mut bytes = value.to_bytes_le();
        bytes.resize(32, 0);
        input.extend(bytes);
        let leading = 256 - value.bits();
        let trailing = value.trailing_zeros().unwrap_or(256);
        for count in [leading, trailing] {
            let mut bytes = BigUint::from(count).to_bytes_le();
            bytes.resize(32, 0);
            expected.extend(bytes);
        }
    }
    let input_path = temp.path().join("input.bin");
    fs::write(&input_path, input).unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("bits"))
            .stdin(fs::File::open(&input_path).unwrap())
            .output()
            .unwrap();
        assert!(result.status.success(), "{result:?}");
        assert_eq!(result.stdout.len(), expected.len());
        for (index, (actual, expected)) in result
            .stdout
            .as_chunks::<64>()
            .0
            .iter()
            .zip(expected.as_chunks::<64>().0)
            .enumerate()
        {
            assert_eq!(actual, expected, "case {index} at O{level}");
        }
    }
}

#[test]
fn native_memory_copy_preserves_overlaps_and_reserves_the_host_symbol() {
    let temp = tempdir().unwrap();
    let source =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fe_test/raw_memory_copy.fe");
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("raw_memory_copy")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
        let ir = fs::read_to_string(out.join("raw_memory_copy.native.sona")).unwrap();
        assert!(ir.contains("declare external %memmove"), "{ir}");
        if level == "0" {
            assert!(ir.contains("local__memmove"), "{ir}");
        }
        assert!(!ir.contains("evm_mcopy"), "{ir}");
    }
}

#[test]
fn native_memory_copy_checks_native_ranges_and_ignores_empty_addresses() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("copy_range.fe");
    for (name, operation, success) in [
        (
            "empty",
            "let invalid = ptr::offset(data, 1 << 128)\nptr::copy_raw(dest: invalid, source: invalid, len: 0)",
            true,
        ),
        (
            "wide_length",
            "ptr::copy_raw(dest: data, source: data, len: (1 << 64) + 1)",
            false,
        ),
        (
            "wide_destination",
            "ptr::copy_raw(dest: ptr::offset(data, 1 << 64), source: data, len: 1)",
            false,
        ),
        (
            "wide_source",
            "ptr::copy_raw(dest: data, source: ptr::offset(data, 1 << 64), len: 1)",
            false,
        ),
        (
            "wrapping_range",
            "ptr::copy_raw(dest: ptr::offset(data, 1 << 63), source: data, len: 1 << 63)",
            false,
        ),
    ] {
        fs::write(&source, format!("use core::ptr\npub fn main() -> i32 {{\nlet data = ptr::alloc_bytes(8)\n*data = 42\n{operation}\ncore::assert(*data == 42)\n0\n}}\n")).unwrap();
        for level in ["0", "1", "2"] {
            let out = temp.path().join(format!("{name}-{level}"));
            build(&source, &out, level, &[]);
            let result = Command::new(out.join("copy_range")).output().unwrap();
            assert_eq!(
                result.status.success(),
                success,
                "{name}/O{level}: {result:?}"
            );
            if !success {
                // Unreachable traps are SIGILL/SIGTRAP on the supported hosts;
                // an invalid libc access would instead be SIGSEGV/SIGBUS.
                assert!(
                    matches!(result.status.signal(), Some(4 | 5)),
                    "{name}/O{level}: {result:?}"
                );
            }
        }
    }
}

#[test]
fn native_copy_options_are_read_out_of_views() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("option_copy.fe");
    fs::write(
        &source,
        r#"
use core::Option
struct Slot { value: Option<u64> }
impl Slot {
    fn value(self) -> Option<u64> { self.value }
}
fn or_zero(_ value: Option<u64>) -> u64 {
    match value {
        Option::Some(inner) => inner
        Option::None => 0
    }
}
pub fn main() -> i32 {
    let mut slot = Slot { value: Option::Some(40) }
    let before = slot.value()
    slot.value = Option::None
    let total = or_zero(before) + or_zero(slot.value()) + 2
    total.downcast_unchecked()
}
"#,
    )
    .unwrap();
    for level in ["0", "1"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("option_copy")).output().unwrap();
        assert_eq!(result.status.code(), Some(42), "O{level}: {result:?}");
    }
}

#[test]
fn native_runtime_as_bytes_writes_generic_text() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("as_bytes.fe");
    fs::write(
        &source,
        r#"
use core::AsBytes
use std::io::{Write, host, write, writeln}
fn say<T: AsBytes>(_ text: T) uses (w: mut Write) { write(text) }
fn first<T: AsBytes>(_ value: T) -> u8 {
    let bytes = value.as_bytes()
    bytes[0]
}
pub fn main() -> i32 {
    let padded: String<4> = "ab"
    with (Write = host()) {
        say("xyz|")
        say(("ab", "cd|"))
        say((("a", "b"), ("c", ("d", "e|"))))
        say(padded)
        writeln(("t", "u"))
    }
    let word: u256 = 0x4142
    let bytes = word.as_bytes()
    core::assert(bytes[0] == 0 && bytes[30] == 0x41 && bytes[31] == 0x42)
    first(("q", padded)) as i32
}
"#,
    )
    .unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("as_bytes")).output().unwrap();
        assert_eq!(result.status.code(), Some(113), "O{level}: {result:?}");
        assert_eq!(result.stdout, b"xyz|abcd|abcde|\0\0abtu\n", "O{level}");
    }
}

#[test]
fn native_byte_buffer_preserves_contents_and_reuses_zeroed_storage() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("byte_buffer.fe");
    fs::write(
        &source,
        r#"
use std::native::ByteBuffer
fn consume(buffer: own ByteBuffer) { buffer.release() }
pub fn main() -> i32 {
    let mut empty = ByteBuffer::new()
    core::assert(empty.len() == 0 && empty.capacity() == 0)
    empty.copy_within(dest: 0, source: 0, len: 0)
    empty.release()
    let mut round: u64 = 0
    while round < 8 {
        let mut buffer = ByteBuffer::new()
        core::assert(buffer.try_resize(33))
        let mut i: u64 = 0
        while i < 33 {
            core::assert(buffer.byte_at(i) == 0)
            buffer.set_byte(index: i, value: (i + 1).downcast_truncate())
            i += 1
        }
        core::assert(buffer.try_resize(3000))
        i = 0
        while i < 3000 {
            let expected: u8 = if i < 33 { (i + 1).downcast_truncate() } else { 0 }
            core::assert(buffer.byte_at(i) == expected)
            i += 1
        }
        buffer.copy_within(dest: 5, source: 0, len: 33)
        i = 0
        while i < 33 {
            core::assert(buffer.byte_at(i + 5) == (i + 1).downcast_truncate())
            i += 1
        }
        buffer.copy_within(dest: 0, source: 5, len: 33)
        buffer.copy_within(dest: 0, source: 0, len: 33)
        i = 0
        while i < 33 {
            core::assert(buffer.byte_at(i) == (i + 1).downcast_truncate())
            i += 1
        }
        let capacity = buffer.capacity()
        core::assert(!buffer.try_resize(0xffffffffffffffff))
        core::assert(buffer.len() == 3000 && buffer.capacity() == capacity)
        core::assert(buffer.byte_at(32) == 33)
        core::assert(buffer.try_resize(8))
        core::assert(buffer.try_resize(40))
        i = 0
        while i < 40 {
            let expected: u8 = if i < 8 { (i + 1).downcast_truncate() } else { 0 }
            core::assert(buffer.byte_at(i) == expected)
            i += 1
        }
        buffer.clear()
        core::assert(buffer.len() == 0 && buffer.capacity() == capacity)
        core::assert(buffer.try_resize(3000))
        i = 0
        while i < 3000 {
            core::assert(buffer.byte_at(i) == 0)
            i += 1
        }
        consume(buffer)
        round += 1
    }
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("byte_buffer")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}

#[test]
fn native_byte_buffer_allocation_failure_is_atomic_and_storage_is_released() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("allocation_failure.fe");
    fs::write(
        &source,
        r#"
use std::native::ByteBuffer
pub fn main() -> i32 {
    let mut empty = ByteBuffer::new()
    core::assert(!empty.try_resize(3000))
    core::assert(empty.len() == 0 && empty.capacity() == 0)
    empty.release()
    let mut buffer = ByteBuffer::new()
    core::assert(buffer.try_resize(513))
    core::assert(buffer.capacity() == 1024)
    let mut i: u64 = 0
    while i < 513 {
        core::assert(buffer.byte_at(i) == 0)
        buffer.set_byte(index: i, value: (i % 251).downcast_truncate())
        i += 1
    }
    core::assert(buffer.try_resize(1500))
    core::assert(buffer.capacity() == 2048)
    core::assert(!buffer.try_resize(3000))
    core::assert(buffer.len() == 1500 && buffer.capacity() == 2048)
    i = 0
    while i < 1500 {
        let expected: u8 = if i < 513 { (i % 251).downcast_truncate() } else { 0 }
        core::assert(buffer.byte_at(i) == expected)
        i += 1
    }
    buffer.clear()
    buffer.release()
    0
}
"#,
    )
    .unwrap();
    let allocator =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native/byte_buffer_allocator.c");
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        let report = temp.path().join(format!("report-{level}.tar.gz"));
        build(
            &source,
            &out,
            level,
            &["--report", "--report-out", report.to_str().unwrap()],
        );
        let listing = Command::new("tar")
            .arg("-tzf")
            .arg(&report)
            .output()
            .unwrap();
        assert!(listing.status.success(), "{listing:?}");
        let listing = String::from_utf8(listing.stdout).unwrap();
        let objects: Vec<_> = listing
            .lines()
            .filter(|name| name.ends_with("/allocation_failure.o"))
            .collect();
        assert_eq!(objects.len(), 1, "{listing}");
        let extracted = Command::new("tar")
            .arg("-xOf")
            .arg(&report)
            .arg(objects[0])
            .output()
            .unwrap();
        assert!(extracted.status.success(), "{extracted:?}");
        let object = out.join("allocation_failure.o");
        fs::write(&object, extracted.stdout).unwrap();
        let executable = out.join("controlled_allocator");
        let linked = Command::new("cc")
            .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-fno-builtin"])
            .arg(&object)
            .arg(&allocator)
            .arg("-o")
            .arg(&executable)
            .output()
            .unwrap();
        assert!(linked.status.success(), "{linked:?}");
        let result = Command::new(&executable).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}

#[test]
fn native_input_reborrows_preserve_independent_buffer_and_scalar_updates() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("forwarded_buffers.fe");
    fs::write(
        &source,
        r#"
use std::native::ByteBuffer
struct Frame { data: ByteBuffer, value: u256 }
fn mixed(_ buffer: mut ByteBuffer, _ value: mut u256) {
    value += 1
    core::assert(buffer.try_resize(33))
    let scalar: u256 = value
    buffer.set_byte(index: 32, value: scalar.downcast_unchecked())
    buffer.copy_within(dest: 0, source: 32, len: 1)
    value += buffer.byte_at(0) as u256
}
fn forward(_ buffer: mut ByteBuffer, _ value: mut u256) {
    mixed(mut buffer, mut value)
}
fn run(_ frame: mut Frame) { forward(mut frame.data, mut frame.value) }
pub fn main() -> i32 {
    let mut frame = Frame { data: ByteBuffer::new(), value: 20 }
    run(mut frame)
    core::assert(frame.value == 42 && frame.data.byte_at(32) == 21)
    run(mut frame)
    core::assert(frame.value == 86 && frame.data.byte_at(0) == 43)
    frame.data.release()
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("forwarded_buffers"))
            .output()
            .unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}

#[test]
fn native_callee_separation_preserves_fresh_buffer_callers() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("independent_frames.fe");
    fs::write(
        &source,
        r#"
use std::native::ByteBuffer
struct Journal { bytes: ByteBuffer }
struct Frame { data: Journal, value: u256 }
impl Frame {
    fn new() -> Self { Self { data: Journal { bytes: ByteBuffer::new() }, value: 0 } }
    fn run(mut self, state: mut Frame) {
        self.data.bytes.clear()
        self.value = 1
        core::assert(self.data.bytes.try_resize(32))
        core::assert(state.data.bytes.try_resize(32))
        self.data.bytes.set_byte(index: 0, value: 2)
        let mut i: u64 = 0
        while i < 32 {
            state.data.bytes.set_byte(index: i, value: 3)
            i += 1
        }
        state.value += 1
    }
    fn release(own self) { self.data.bytes.release() }
}
fn execute(state: mut Frame) -> Frame {
    let mut vm = Frame::new()
    vm.run(state)
    vm
}
fn finish(state: mut Frame) {
    let mut vm = Frame::new()
    vm.run(state)
    vm.release()
}
pub fn main() -> i32 {
    let mut state = Frame::new()
    let vm = execute(state: mut state)
    core::assert(vm.value == 1 && state.value == 1)
    core::assert(vm.data.bytes.byte_at(0) == 2 && state.data.bytes.byte_at(31) == 3)
    vm.release()
    finish(state: mut state)
    core::assert(state.value == 2 && state.data.bytes.byte_at(31) == 3)
    state.release()
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("independent_frames"))
            .output()
            .unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}

#[test]
fn native_recursive_receiver_authority_converges() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("recursive_buffers.fe");
    fs::write(
        &source,
        r#"
use std::native::ByteBuffer
fn write_at_depth(_ buffer: mut ByteBuffer, depth: usize, value: u8) {
    if depth == 0 {
        buffer.set_byte(index: 31, value)
    } else {
        write_at_depth(mut buffer, depth: depth - 1, value)
    }
}
fn write_alternating(_ first: mut ByteBuffer, _ second: mut ByteBuffer, depth: usize, value: u8) {
    if depth == 0 {
        first.set_byte(index: 31, value)
    } else {
        write_alternating(mut second, mut first, depth: depth - 1, value)
    }
}
pub fn main() -> i32 {
    let mut buffer = ByteBuffer::new()
    core::assert(buffer.try_resize(32))
    write_at_depth(mut buffer, depth: 0, value: 19)
    core::assert(buffer.byte_at(31) == 19)
    write_at_depth(mut buffer, depth: 32, value: 42)
    core::assert(buffer.byte_at(31) == 42)
    let mut other = ByteBuffer::new()
    core::assert(other.try_resize(32))
    other.set_byte(index: 31, value: 7)
    write_alternating(mut buffer, mut other, depth: 32, value: 19)
    core::assert(buffer.byte_at(31) == 19 && other.byte_at(31) == 7)
    write_alternating(mut buffer, mut other, depth: 33, value: 42)
    core::assert(buffer.byte_at(31) == 19 && other.byte_at(31) == 42)
    other.release()
    buffer.release()
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("recursive_buffers"))
            .output()
            .unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}

#[test]
fn native_recursive_buffer_poststates_converge() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("recursive_poststates.fe");
    fs::write(
        &source,
        r#"
use std::native::ByteBuffer
fn grow(_ buffer: mut ByteBuffer, depth: usize, length: u64, value: u8) {
    if depth == 0 {
        let mut size: u64 = 1
        while size <= length {
            core::assert(buffer.try_resize(size))
            buffer.set_byte(index: size - 1, value)
            size += 1
        }
    } else { grow(mut buffer, depth: depth - 1, length, value) }
}
fn clear(_ buffer: mut ByteBuffer, depth: usize) {
    if depth == 0 { buffer.clear() }
    else { clear(mut buffer, depth: depth - 1) }
}
pub fn main() -> i32 {
    let mut buffer = ByteBuffer::new()
    grow(mut buffer, depth: 32, length: 32, value: 19)
    core::assert(buffer.len() == 32 && buffer.byte_at(31) == 19)
    clear(mut buffer, depth: 32)
    core::assert(buffer.len() == 0)
    grow(mut buffer, depth: 16, length: 513, value: 42)
    core::assert(buffer.len() == 513)
    for index in 0..513 { core::assert(buffer.byte_at(index.downcast_unchecked()) == 42) }
    buffer.release()
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("recursive_poststates"))
            .output()
            .unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}

#[test]
fn native_raw_scalar_reference_accessors_at_o1() {
    let temp = tempdir().unwrap();
    let source = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/native/raw_scalar_reference_accessors.fe");
    build(&source, temp.path(), "1", &["--standalone"]);
    let result = Command::new(temp.path().join("raw_scalar_reference_accessors"))
        .output()
        .unwrap();
    assert!(result.status.success(), "{result:?}");
}

#[test]
fn native_raw_aggregate_reference_accessors_at_o1() {
    let temp = tempdir().unwrap();
    let source = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/native/raw_aggregate_reference_accessors.fe");
    build(&source, temp.path(), "1", &["--standalone"]);
    let result = Command::new(temp.path().join("raw_aggregate_reference_accessors"))
        .output()
        .unwrap();
    assert!(result.status.success(), "{result:?}");
}

#[test]
fn native_returned_reference_projections_mutate_the_original_at_o1() {
    let temp = tempdir().unwrap();
    let source = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/native/returned_reference_projection.fe");
    build(&source, temp.path(), "1", &["--standalone"]);
    let result = Command::new(temp.path().join("returned_reference_projection"))
        .output()
        .unwrap();
    assert!(result.status.success(), "{result:?}");
}
