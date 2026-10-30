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
    path::{Path, PathBuf},
    process::{Command, Output, Stdio},
    thread,
    time::{Duration, Instant},
};

use num_bigint::BigUint;
use tempfile::tempdir;

fn build(source: &Path, out: &Path, level: &str, extra: &[&str]) -> Output {
    // Several tests run built programs that trap.
    test_utils::skip_crash_reports();
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
fn native_usize_and_pointers_are_64_bits() {
    let temp = tempdir().unwrap();
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native/word_widths.fe");
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let executable = out.join("word_widths");
        let result = Command::new(&executable).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
        let overflow = Command::new(&executable).arg("x").output().unwrap();
        assert_eq!(
            overflow.status.signal(),
            Some(libc::SIGABRT),
            "O{level}: {overflow:?}"
        );
        assert_eq!(
            overflow.stderr, b"panicked: arithmetic overflow\n",
            "O{level}: {overflow:?}"
        );
    }
}

#[test]
fn native_reverts_report_their_data_in_hex() {
    let temp = tempdir().unwrap();
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native/revert_hex.fe");
    for level in ["0", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        // The report writes 32 bytes of data at a time.
        for len in [1, 32, 33, 65] {
            let result = Command::new(out.join("revert_hex"))
                .args(vec!["x"; len])
                .output()
                .unwrap();
            let digits: String = (0..len).map(|byte| format!("{byte:02x}")).collect();
            assert_eq!(
                String::from_utf8_lossy(&result.stderr),
                format!("panicked: reverted with 0x{digits}\n"),
                "O{level}/{len}"
            );
        }
    }
}

#[test]
fn native_isize_abi_words_must_fit_64_bits() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("isize_words.fe");
    fs::write(
        &source,
        r#"
use core::abi::{Decode, store_word}
use core::ptr::{self, MemSpan}
use std::abi::Sol

fn decode(_ word: u256) -> isize {
    let data = ptr::alloc_bytes(32)
    unsafe { store_word(ptr: data, value: word) }
    Decode<Sol>::decode_from(unsafe { MemSpan::from_raw_parts(ptr: data, len: 32) }, 0)
}

#[test]
fn extremes_decode() {
    assert!(decode(0x7fffffffffffffff) == isize::max())
    assert!(decode(0xffffffffffffffffffffffffffffffffffffffffffffffff8000000000000000) == isize::min())
}

#[test(should_revert)]
fn above_max_reverts() {
    let _ = decode(0x8000000000000000)
}

#[test(should_revert)]
fn below_min_reverts() {
    let _ = decode(0xffffffffffffffffffffffffffffffffffffffffffffffff7fffffffffffffff)
}
"#,
    )
    .unwrap();
    test_utils::skip_crash_reports();
    for level in ["0", "2"] {
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["test", "--backend", "native", "-O", level])
            .arg(&source)
            .output()
            .unwrap();
        assert!(
            String::from_utf8_lossy(&result.stdout).contains("3 passed; 0 failed"),
            "O{level}: {result:?}"
        );
    }
}

#[test]
fn native_word_sized_allocations_beyond_usize_run_out_of_memory() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("oversized.fe");
    fs::write(
        &source,
        r#"
use core::abi::DynString

// The string's 32-byte header and padded payload need more than 2**64 bytes.
#[test(should_revert, panic = 0x41)]
fn oversized_string() {
    let _ = DynString::zeroed(usize::max())
}
"#,
    )
    .unwrap();
    test_utils::skip_crash_reports();
    for level in ["0", "2"] {
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["test", "--backend", "native", "-O", level])
            .arg(&source)
            .output()
            .unwrap();
        assert!(
            String::from_utf8_lossy(&result.stdout).contains("1 passed; 0 failed"),
            "O{level}: {result:?}"
        );
    }
}

#[test]
fn native_panics_report_on_stderr_after_flushing_stdout() {
    let temp = tempdir().unwrap();
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native/panics.fe");
    let reports = [
        "panicked\n",
        "panicked: index out of bounds\n",
        "panicked: arithmetic overflow\n",
        "panicked: custom message\n",
        "panicked: reverted with 0x4e487b710000000000000000000000000000000000000000000000000000000000000077\n",
        "panicked: division by zero\n",
        "panicked: reverted without data\n",
    ];
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let executable = out.join("panics");
        let result = Command::new(&executable).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
        for (kind, report) in reports.iter().enumerate() {
            // Piped stdout is fully buffered; the panic flushes it.
            let result = Command::new(&executable)
                .args(vec!["x"; kind + 1])
                .output()
                .unwrap();
            assert_eq!(
                result.status.signal(),
                Some(libc::SIGABRT),
                "O{level}/{kind}: {result:?}"
            );
            assert_eq!(result.stdout, b"A\n", "O{level}/{kind}: {result:?}");
            assert_eq!(
                String::from_utf8_lossy(&result.stderr),
                *report,
                "O{level}/{kind}"
            );
        }
    }
}

#[test]
fn native_tests_check_expected_reverts() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("reverts.fe");
    fs::write(
        &source,
        r#"
fn bump(_ x: u8) -> u8 {
    x + 1
}

#[test(should_revert, panic = 0x11)]
fn panic_code_matches() {
    let _ = bump(255)
}

#[test(should_revert, panic = 0x32)]
fn panic_code_differs() {
    let _ = bump(255)
}

#[test(should_revert, selector = 0x08c379a0)]
fn selector_matches() {
    assert!(false, "boom")
}

#[test(should_revert)]
fn any_revert() {
    core::panic_code(0x01)
}

#[test(should_revert)]
fn trap_is_not_a_revert() {
    core::panic()
}

#[test(should_revert)]
fn no_revert() {}

#[test]
fn unexpected_revert() {
    let _ = bump(255)
}
"#,
    )
    .unwrap();
    for level in ["0", "2"] {
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["test", "--backend", "native", "-O", level])
            .arg(&source)
            .output()
            .unwrap();
        let output = String::from_utf8_lossy(&result.stdout);
        assert!(!result.status.success(), "O{level}: {output}");
        for passing in ["panic_code_matches", "selector_matches", "any_revert"] {
            assert!(
                output.contains(&format!("] {passing}\n")),
                "O{level}: {output}"
            );
            assert!(
                !output.contains(&format!("    {passing}\n")),
                "O{level}: {output}"
            );
        }
        for failure in [
            "Expected revert data 0x4e487b710000000000000000000000000000000000000000000000000000000000000032, but got: 0x4e487b710000000000000000000000000000000000000000000000000000000000000011",
            "Expected test to revert, but it failed with: native test failed (signal: 6 (SIGABRT)",
            "Expected test to revert, but it succeeded",
            "Test reverted: 0x4e487b710000000000000000000000000000000000000000000000000000000000000011",
            "test result: FAILED. 3 passed; 4 failed",
        ] {
            assert!(output.contains(failure), "O{level}: {output}");
        }
    }
}

#[test]
fn native_buffer_word_writes_preserve_unaligned_values_and_zero_gaps() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("word_writes.fe");
    fs::write(
        &source,
        r#"
use core::abi::ByteSource
use core::ptr::MemBuffer

pub fn main() -> i32 {
    var buffer = MemBuffer::with_capacity(100)
    buffer.write_word(offset: 4, value: 0x1234)
    buffer.write_word(offset: 68, value: 0x5678)
    assert!(buffer.len() == 100)
    assert!(buffer.span().word_at(4) == 0x1234)
    assert!(buffer.span().word_at(36) == 0)
    assert!(buffer.span().word_at(68) == 0x5678)
    // Words are big-endian, as in EVM memory.
    assert!(buffer[34] == 0x12 && buffer[35] == 0x34 && buffer[4] == 0)
    buffer[99] = 0x9a
    assert!(buffer.read_word(68) == 0x569a && buffer.span().read_word(68) == 0x569a)
    buffer.span().write_word(offset: 36, value: 0xbc << 248)
    assert!(buffer[36] == 0xbc && buffer[67] == 0)
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "1"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("word_writes")).output().unwrap();
        assert!(result.status.success(), "{result:?}");
    }
}

#[test]
fn native_records_and_enums_take_aligned_layout() {
    // Native memory lays fields out as a Sonatina object does: each at its
    // alignment, little-endian, and an enum's variants from one payload
    // offset after the tag.
    let temp = tempdir().unwrap();
    let source = temp.path().join("aligned.fe");
    fs::write(
        &source,
        r#"
use core::ptr::MemArray

struct Inner { code: u16, ready: bool }
struct Packet { tag: u8, inner: Inner, samples: [u16; 3], tail: u8 }
enum Choice { Small(u8), Wide(u64, u16) }

impl Copy for Inner {}
impl Copy for Packet {}
impl Copy for Choice {}

pub fn main() -> i32 {
    assert!(core::size_of<Inner>() == 4)
    assert!(core::size_of<Packet>() == 14)
    assert!(core::size_of<Choice>() == 24)
    var packets = MemArray<Packet>::new_zeroed(2)
    assert!(packets.byte_len() == 2 * core::size_of<Packet>())
    packets[1] = Packet {
        tag: 0xa1,
        inner: Inner { code: 0x1234, ready: true },
        samples: [0x5678, 0x9abc, 0xdef0],
        tail: 0x7e,
    }
    let bytes = packets.span()
    // The tag, then `inner` at its two-byte alignment, `samples` and `tail`.
    let offsets: [usize; 9] = [0, 2, 3, 4, 6, 7, 8, 11, 12]
    let expected: [u8; 9] = [0xa1, 0x34, 0x12, 1, 0x78, 0x56, 0xbc, 0xde, 0x7e]
    var i: usize = 0
    while i < 9 {
        assert!(bytes[14 + offsets[i]] == expected[i])
        i += 1
    }
    var choices = MemArray<Choice>::new_zeroed(2)
    assert!(choices.byte_len() == 2 * core::size_of<Choice>())
    choices[0] = Choice::Wide(0x0102030405060708, 0x0a0b)
    choices[1] = Choice::Small(0x5c)
    let bytes = choices.span()
    // Both payloads start at the u64's eight-byte alignment.
    assert!(bytes[0] == 1 && bytes[8] == 0x08 && bytes[15] == 0x01 && bytes[16] == 0x0b)
    assert!(bytes[24] == 0 && bytes[32] == 0x5c)
    match choices[0] {
        Choice::Wide(word, half) => assert!(word == 0x0102030405060708 && half == 0x0a0b)
        Choice::Small(_) => assert!(false)
    }
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("aligned")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}

#[test]
fn native_boxed_structs_move_whole() {
    // A box stores its value in the aligned layout of a Sonatina object, so
    // the frame moves in one load and store despite the padding after `flag`.
    let temp = tempdir().unwrap();
    let source = temp.path().join("boxed.fe");
    fs::write(
        &source,
        r#"
use core::Box

struct Pair { a: u64, b: u64 }

struct Frame {
    flag: bool,
    words: [u256; 64],
    count: u64,
    small: [u8; 3],
    pair: Pair,
}

pub fn main() -> i32 {
    var words: [u256; 64] = [0; 64]
    var i: usize = 0
    while i < 64 {
        words[i] = (i as u256 + 1) * 0x0101010101010101010101010101010101010101010101010101010101010101
        i += 1
    }
    let frame = Frame { flag: true, words, count: 7, small: [1, 2, 3], pair: Pair { a: 5, b: 6 } }
    let moved = Box::new(frame).into_inner()
    assert!(moved.flag && moved.count == 7 && moved.pair.a == 5 && moved.pair.b == 6)
    assert!(moved.small[0] == 1 && moved.small[1] == 2 && moved.small[2] == 3)
    i = 0
    while i < 64 {
        assert!(moved.words[i] == (i as u256 + 1) * 0x0101010101010101010101010101010101010101010101010101010101010101)
        i += 1
    }
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("boxed")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
        if level == "0" {
            let ir = fs::read_to_string(out.join("boxed.native.sona")).unwrap();
            let frame = ir
                .lines()
                .find_map(|line| {
                    line.strip_prefix("type ")?
                        .strip_suffix(" = {i1, [i256; 64], i64, [i8; 3], @layout_1};")
                })
                .unwrap_or_else(|| panic!("no frame type\n{ir}"));
            for op in ["= mload", "mstore"] {
                assert!(
                    ir.lines()
                        .any(|line| line.contains(op) && line.ends_with(&format!(" {frame};"))),
                    "{op} {frame}\n{ir}"
                );
            }
        }
    }
}

#[test]
fn native_long_string_literals_hold_their_bytes() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("long_strings.fe");
    fs::write(
        &source,
        r#"
fn check(_ text: std::abi::DynString) {
    assert!(text.len() == 65)
    assert!(text.byte_at(0) == 0x61 && text.byte_at(31) == 0x35)
    assert!(text.byte_at(32) == 0x36 && text.byte_at(63) == 0x2d && text.byte_at(64) == 0x21)
}

pub fn main() -> i32 {
    check("abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-!")
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "1"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("long_strings")).output().unwrap();
        assert!(result.status.success(), "{result:?}");
    }
}

#[test]
fn native_packed_bytes_write_runtime_words_strings_and_nested_tuples() {
    let source =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native/packed_bytes_io.fe");
    let temp = tempdir().unwrap();
    for level in ["1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        for arg_count in 0..4 {
            let output = Command::new(out.join("packed_bytes_io"))
                .args(vec!["input"; arg_count])
                .output()
                .unwrap();
            assert!(output.status.success(), "O{level}: {output:?}");
            assert!(output.stderr.is_empty(), "{output:?}");
            let mut expected = b"xyz|abcd|abcde|\0\0abtu\n".to_vec();
            let word = match arg_count {
                0 => [0; 32],
                1 => {
                    let mut word = [0; 32];
                    word[31] = 1;
                    word
                }
                2 => {
                    let mut word = [0; 32];
                    word[0] = 128;
                    word
                }
                _ => [255; 32],
            };
            expected.extend(word);
            if arg_count == 0 {
                expected.extend(b"A1234567890123456789012345678901");
            } else {
                expected.push(b'B');
                expected.extend([0; 30]);
                expected.push(b'x');
            }
            expected.extend([1, 2, 3]);
            for width in [0, 1, 31, 32, 33, 63, 64, 65] {
                expected.extend((0..width).map(|i| (arg_count + 1 + i) as u8));
            }
            assert_eq!(output.stdout, expected, "O{level}, argc={}", arg_count + 1);
        }
    }
}

#[test]
fn native_packed_bytes_index_checks_empty_and_nonempty_bounds() {
    let source =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native/packed_bytes_bounds.fe");
    let temp = tempdir().unwrap();
    for level in ["1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        for arg_count in 0..3 {
            let output = Command::new(out.join("packed_bytes_bounds"))
                .args(vec!["input"; arg_count])
                .output()
                .unwrap();
            assert_eq!(
                output.status.success(),
                arg_count == 1,
                "O{level}: {output:?}"
            );
        }
    }
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
fn aggregate_fixtures_run_at_every_level() {
    for (name, expected_count) in [
        ("aggregate_object_copies.fe", 3),
        ("const_aggregate_values.fe", 5),
    ] {
        let fixture = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures/fe_test")
            .join(name);
        for backend in ["sonatina", "native"] {
            for level in ["0", "1", "2"] {
                let result = Command::new(env!("CARGO_BIN_EXE_fe"))
                    .args(["test", "--backend", backend, "--jobs", "1", "-O", level])
                    .arg(&fixture)
                    .output()
                    .unwrap();
                assert!(
                    String::from_utf8_lossy(&result.stdout)
                        .contains(&format!("{expected_count} passed; 0 failed")),
                    "{name} {backend} -O{level}: {result:?}"
                );
            }
        }
    }
}

#[test]
fn reference_holders_and_joins_run_natively_at_every_level() {
    // Products and shapes hold references to frame objects in other objects,
    // and joins of raw-pointer and object places export an object's address.
    for (name, expected_count) in [
        ("absent_projection_payloads.fe", 6),
        ("associated_shape_signatures.fe", 1),
        ("callable_shape_table.fe", 2),
        ("closure_product_params.fe", 2),
        ("loop_steps.fe", 7),
        ("nonreturning_borrow_joins.fe", 1),
        ("projection_temporaries.fe", 1),
        ("shape_equalities.fe", 1),
        ("spread_params.fe", 3),
    ] {
        let fixture = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures/fe_test")
            .join(name);
        for level in ["0", "1", "2"] {
            let result = Command::new(env!("CARGO_BIN_EXE_fe"))
                .args(["test", "--backend", "native", "-O", level])
                .arg(&fixture)
                .output()
                .unwrap();
            assert!(
                String::from_utf8_lossy(&result.stdout)
                    .contains(&format!("{expected_count} passed; 0 failed")),
                "{name} -O{level}: {result:?}"
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
        let ir = fs::read_to_string(temp.path().join("out/cases.native.test.sona")).unwrap();
        assert!(
            !ir.contains("fail_assertion"),
            "filtered test must not be compiled"
        );
        assert!(temp.path().join("out/cases.native.test.rmir").exists());
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
        var i: u32 = 0
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
    fs::write(&source, "#[test(balance = 10)]\nfn evm_test() {}\n").unwrap();
    let result = Command::new(env!("CARGO_BIN_EXE_fe"))
        .args(["test", "--backend", "native"])
        .arg(&source)
        .output()
        .unwrap();
    assert!(!result.status.success());
    assert!(
        String::from_utf8_lossy(&result.stdout).contains("cannot use the EVM `balance` attribute"),
        "{result:?}"
    );
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
fn native_effect_places_and_projection_grants_preserve_referent_identity() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("referent_identity.fe");
    fs::write(
        &source,
        r#"
struct Handle { value: i32, calls: i32 }
struct Pair { first: i32, second: i32 }
impl Pair {
    fn pick(mut self, _ first: bool) -> mut i32 {
        if first { mut self.first } else { mut self.second }
    }
}
fn step() uses (handle: mut Handle) {
    handle.value += 1
    handle.calls += 1
}
fn increment(_ target: mut i32) { target += 1 }
pub fn main() -> i32 {
    var handle = Handle { value: 20, calls: 0 }
    with (handle) {
        step()
        step()
    }
    core::assert(handle.calls == 2)
    core::assert(handle.value == 22)
    var pair = Pair { first: 20, second: 40 }
    increment(mut pair.pick(true))
    increment(mut pair.pick(false))
    increment(mut pair.pick(true))
    let second = mut pair.pick(false)
    second += 1
    core::assert(pair.first == 22)
    core::assert(pair.second == 42)
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        assert!(
            Command::new(out.join("referent_identity"))
                .status()
                .unwrap()
                .success(),
            "referent identity at O{level}"
        );
    }
}

#[test]
fn native_code_after_partially_diverging_else_if_chain_runs() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("else_if_chain.fe");
    fs::write(
        &source,
        r#"
fn after_else_if(_ x: u64) -> u64 {
    var r: u64 = 0
    if x == 0 {
        return 1
    } else if x == 5 {
        r = 3
    } else {
        return 2
    }
    r + 4
}
pub fn main() -> i32 {
    var i: u64 = 0
    while i < 4 {
        if i != 1 {
            i += 1
            continue
        } else if i == 1 {
            i += 0
        } else {
            return 100
        }
        i += 2
    }
    core::assert(i == 4)
    core::assert(after_else_if(5) == 7)
    core::assert(after_else_if(6) == 2)
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let status = Command::new(out.join("else_if_chain")).status().unwrap();
        assert_eq!(status.code(), Some(0), "O{level}: {status:?}");
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
    var value = Pair { low: 2, high: 7 }
    var i: u64 = 0
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
    var input = host()
    var output = host()
    var value: i32 = 0
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
    let args = unsafe { Args::new(argc, argv) }
    core::assert(args.len() == argc.downcast_unchecked())
    if argc == 1 { args.get(args.len()) }
    if argc == 2 {
        let arg = args.get(1)
        arg.byte_at(arg.len())
    }
    core::assert(main(argc: 0, argv) == 42)
    with (Write = host()) {
        var i: usize = 1
        while i < args.len() {
            let arg = args.get(i)
            var j: usize = 0
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
    var value: u256 = 0
    var i: u256 = 0
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
    var i: u256 = 0
    while i < 32 {
        output.write_char(c: ((value >> (i * 8)) & 255).downcast_unchecked())
        core::assert(!output.failed())
        i += 1
    }
}
pub fn main() -> i32 {
    with (Read = host(), HostIo = host()) {
        var marker = read_char()
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
    var value: u256 = 0
    var i: u256 = 0
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
    var i: u256 = 0
    while i < 32 {
        output.write_char(c: ((value >> (i * 8)) & 255).downcast_unchecked())
        core::assert(!output.failed())
        i += 1
    }
}
pub fn main() -> i32 {
    with (Read = host(), HostIo = host()) {
        var marker = read_char()
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
fn native_fallible_allocation_reports_failure_and_releases_blocks() {
    let temp = tempdir().unwrap();
    let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
    let source = fixtures.join("fe_test/fallible_allocation.fe");
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        let object = build_native_object(&source, &out, level);
        let ir = fs::read_to_string(out.join("fallible_allocation.native.sona")).unwrap();
        for symbol in ["malloc", "free"] {
            assert!(ir.contains(&format!("declare external %{symbol}")), "{ir}");
            if level == "0" {
                assert!(ir.contains(&format!("local__{symbol}")), "{ir}");
            }
        }
        let executable = out.join("controlled_allocator");
        let linked = Command::new("cc")
            .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-fno-builtin"])
            .arg(&object)
            .arg(fixtures.join("native/fallible_allocator.c"))
            .arg("-o")
            .arg(&executable)
            .output()
            .unwrap();
        assert!(linked.status.success(), "{linked:?}");
        let result = Command::new(&executable).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}

/// Runs a native fixture under an allocator that requires every allocation to
/// be freed exactly once, and reports its counts on stderr. `defines`
/// configure the allocator.
fn run_balanced(fixture: &str, out: &Path, level: &str, defines: &[&str]) -> Output {
    let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native");
    let object = build_native_object(&fixtures.join(fixture), out, level);
    let executable = out.join("balanced");
    let linked = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-fno-builtin"])
        .args(defines)
        .arg(&object)
        .arg(fixtures.join("balanced_allocator.c"))
        .arg("-o")
        .arg(&executable)
        .output()
        .unwrap();
    assert!(linked.status.success(), "{linked:?}");
    let result = Command::new(&executable).output().unwrap();
    assert!(result.status.success(), "O{level}: {result:?}");
    result
}

#[test]
fn native_owners_destroy_what_they_hold_and_free_it_once() {
    let temp = tempdir().unwrap();
    for level in ["0", "1", "2"] {
        let result = run_balanced(
            "owners.fe",
            &temp.path().join(format!("out-{level}")),
            level,
            &[],
        );
        assert_eq!(
            String::from_utf8_lossy(&result.stdout),
            "de-ab-c-fg\nxy-ABC\n-b\nu-U\n123\n",
            "O{level}"
        );
    }
}

#[test]
fn native_failed_allocations_hand_values_back() {
    let temp = tempdir().unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        let result = run_balanced("handed_back.fe", &out, level, &["-DREFUSE_FROM=1024"]);
        assert_eq!(String::from_utf8_lossy(&result.stdout), "-A-B", "O{level}");
    }
}

#[test]
fn native_empty_owners_take_nothing_from_the_host() {
    let temp = tempdir().unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        let result = run_balanced("empty_owners.fe", &out, level, &[]);
        assert_eq!(
            String::from_utf8_lossy(&result.stderr),
            "allocations=1 releases=1\n",
            "O{level}"
        );
    }
}

#[test]
fn native_values_are_destroyed_once_in_scope_order() {
    let temp = tempdir().unwrap();
    let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&fixtures.join("native/drop_order.fe"), &out, level, &[]);
        let result = Command::new(out.join("drop_order")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
        assert_eq!(
            String::from_utf8_lossy(&result.stdout),
            "a.\n.a\n0123\nSs\nN\nA,B\n1;2\ny\nx\nx\ny\noi\nABC\nZ-[-\nH-KLJI\n-l\n!r\nA-B-C--Z\nc-c\n-e-ox\n.-p\n+o\n-e\n-BOIA\nq-k\nkQ\n-Vv-\n-u\nU\n-GF\nu1u2u3u4guuuu\n1a\n-b\n-\n-0-1-.2\nba\n-B\nd\nk",
            "O{level}"
        );

        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["test", "--backend", "native", "-O", level])
            .arg(fixtures.join("fe_test/drops.fe"))
            .output()
            .unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}

#[test]
fn native_out_of_bounds_index_store_destroys_nothing() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("index_store.fe");
    fs::write(
        &source,
        r#"
use core::ops::Drop
extern {
    fn exit(_ code: i32)
}
struct Exits {
    code: i32,
}
impl Drop for Exits {
    fn drop(own self) {
        unsafe { exit(self.code) }
    }
}
#[inline(never)]
fn index() -> usize {
    2
}
pub fn main() -> i32 {
    var exits = [Exits { code: 3 }, Exits { code: 4 }]
    exits[index()] = Exits { code: 5 }
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("index_store")).output().unwrap();
        assert!(result.status.signal().is_some(), "O{level}: {result:?}");
    }
}

#[test]
fn native_extern_declarations_share_runtime_imports() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("shared.fe");
    // The runtime's allocation, copy, release and panic report call the same
    // C functions as these declarations.
    fs::write(
        &source,
        r#"
use core::ptr
extern {
    fn malloc(_ size: usize) -> *u8
    fn memmove(_ dest: *u8, _ src: *u8, _ len: usize) -> *u8
    fn free(_ ptr: *u8)
    fn write(_ fd: i32, _ buf: *u8, _ len: usize) -> isize
}
pub fn main() -> i32 {
    let text = unsafe { malloc(3) }
    let copy = ptr::try_alloc_bytes(3).unwrap()
    unsafe {
        *text = 104
        *ptr::offset_bytes(text, 1) = 105
        *ptr::offset_bytes(text, 2) = 10
        ptr::copy_raw(dest: copy, source: text, len: 3)
        let _ = memmove(text, copy, 3)
        let _ = write(2, copy, 3)
        free(text)
        ptr::dealloc(copy)
    }
    core::panic()
}
"#,
    )
    .unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("shared")).output().unwrap();
        assert_eq!(
            result.status.signal(),
            Some(libc::SIGABRT),
            "O{level}: {result:?}"
        );
        assert_eq!(result.stderr, b"hi\npanicked\n", "O{level}: {result:?}");
    }
}

#[test]
fn native_extern_declarations_must_match_runtime_imports() {
    let temp = tempdir().unwrap();
    for (name, declaration, call, error) in [
        (
            "write",
            "fn write(_ fd: i64, _ buf: *u8, _ len: usize) -> isize",
            "write(2, core::ptr::alloc_bytes(1), 0)",
            "native code calls the C library's `write`, so an `extern` declaration of it must be `fn write(_ fd: i32, _ buf: *u8, _ len: usize) -> isize`",
        ),
        (
            "__fe_revert",
            "fn __fe_revert(_ data: *u8, _ len: usize)",
            "__fe_revert(core::ptr::alloc_bytes(1), 0)",
            "extern symbol `__fe_revert` is reserved: symbols starting with `__fe_` are the compiler's",
        ),
    ] {
        let source = temp.path().join(format!("{name}.fe"));
        fs::write(
            &source,
            format!(
                "extern {{\n    {declaration}\n}}\npub fn main() -> i32 {{\n    let _ = unsafe {{ {call} }}\n    core::panic()\n}}\n"
            ),
        )
        .unwrap();
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["build", "--backend", "native", "--out-dir"])
            .arg(temp.path().join(format!("out-{name}")))
            .arg(&source)
            .output()
            .unwrap();
        let output = String::from_utf8_lossy(&result.stderr);
        assert!(!result.status.success(), "{result:?}");
        assert!(output.contains(error), "{output}");
    }
}

#[test]
fn native_memory_copy_checks_native_ranges_and_ignores_empty_addresses() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("copy_range.fe");
    for (name, operation, success) in [
        (
            "empty",
            "let invalid = ptr::offset(data, 1 << 62)\nptr::copy_raw(dest: invalid, source: invalid, len: 0)",
            true,
        ),
        (
            "wrapping_range",
            "ptr::copy_raw(dest: ptr::offset(data, 1 << 63), source: data, len: 1 << 63)",
            false,
        ),
    ] {
        fs::write(&source, format!("use core::ptr\npub fn main() -> i32 {{\nlet data = ptr::alloc_bytes(8)\nunsafe {{ *data = 42 }}\nunsafe {{ {operation} }}\nunsafe {{ core::assert(*data == 42) }}\n0\n}}\n")).unwrap();
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
                // The range check reverts before the copy; an invalid libc
                // access would instead be SIGSEGV/SIGBUS.
                assert_eq!(
                    result.status.signal(),
                    Some(libc::SIGABRT),
                    "{name}/O{level}: {result:?}"
                );
                assert_eq!(
                    result.stderr, b"panicked: reverted without data\n",
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
    var slot = Slot { value: Option::Some(40) }
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
fn native_buffer_slices_copy_ranges_between_buffers() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("byte_buffer_copy.fe");
    fs::write(
        &source,
        r#"
use core::MemBuffer
pub fn main() -> i32 {
    var from = MemBuffer::empty()
    var into = MemBuffer::empty()
    from.slice(offset: 0, len: 0).copy_from(into.span())
    core::assert(from.try_resize(100) && into.try_resize(70))
    for i in 0..100 {
        from[i] = (i + 1).downcast_truncate()
    }
    into.slice(offset: 3, len: 60).copy_from(from.slice(offset: 40, len: 60))
    into.slice(offset: 70, len: 0).copy_from(from.slice(offset: 100, len: 0))
    for i in 0..70 {
        let expected: u8 = if i >= 3 && i < 63 { (i + 38).downcast_truncate() } else { 0 }
        core::assert(into[i] == expected)
    }
    for i in 0..100 {
        core::assert(from[i] == (i + 1).downcast_truncate())
    }
    let copy = MemBuffer::try_from_span(from.slice(offset: 10, len: 5)).unwrap()
    core::assert(copy.len() == 5 && copy.capacity() == 5 && copy[0] == 11 && copy[4] == 15)
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "1"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("byte_buffer_copy")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}

#[test]
fn native_buffers_preserve_contents_and_reuse_zeroed_storage() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("byte_buffer.fe");
    fs::write(
        &source,
        r#"
use core::MemBuffer
fn consume(_ buffer: own MemBuffer) {}
pub fn main() -> i32 {
    var empty = MemBuffer::empty()
    core::assert(empty.len() == 0 && empty.capacity() == 0)
    empty.span().copy_within(source: 0, dest: 0, len: 0)
    for _ in 0..8 {
        var buffer = MemBuffer::empty()
        core::assert(buffer.try_resize(33))
        for i in 0..33 {
            core::assert(buffer[i] == 0)
            buffer[i] = (i + 1).downcast_truncate()
        }
        core::assert(buffer.try_resize(3000))
        for i in 0..3000 {
            let expected: u8 = if i < 33 { (i + 1).downcast_truncate() } else { 0 }
            core::assert(buffer[i] == expected)
        }
        buffer.span().copy_within(source: 0, dest: 5, len: 33)
        for i in 0..33 {
            core::assert(buffer[i + 5] == (i + 1).downcast_truncate())
        }
        buffer.span().copy_within(source: 5, dest: 0, len: 33)
        buffer.span().copy_within(source: 0, dest: 0, len: 33)
        for i in 0..33 {
            core::assert(buffer[i] == (i + 1).downcast_truncate())
        }
        let capacity = buffer.capacity()
        core::assert(!buffer.try_resize(usize::max()))
        core::assert(buffer.len() == 3000 && buffer.capacity() == capacity)
        core::assert(buffer[32] == 33)
        core::assert(buffer.try_resize(8))
        core::assert(buffer.try_resize(40))
        for i in 0..40 {
            let expected: u8 = if i < 8 { (i + 1).downcast_truncate() } else { 0 }
            core::assert(buffer[i] == expected)
        }
        buffer.clear()
        core::assert(buffer.len() == 0 && buffer.capacity() == capacity)
        core::assert(buffer.try_resize(3000))
        for i in 0..3000 {
            core::assert(buffer[i] == 0)
        }
        consume(buffer)
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
fn native_buffer_allocation_failure_is_atomic_and_storage_is_released() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("allocation_failure.fe");
    fs::write(
        &source,
        r#"
use core::MemBuffer
pub fn main() -> i32 {
    var empty = MemBuffer::empty()
    core::assert(!empty.try_resize(3000))
    core::assert(empty.len() == 0 && empty.capacity() == 0)
    var buffer = MemBuffer::empty()
    core::assert(buffer.try_resize(513))
    core::assert(buffer.capacity() == 1024)
    for i in 0..513 {
        core::assert(buffer[i] == 0)
        buffer[i] = (i % 251).downcast_truncate()
    }
    core::assert(buffer.try_resize(1500))
    core::assert(buffer.capacity() == 2048)
    core::assert(!buffer.try_resize(3000))
    core::assert(buffer.len() == 1500 && buffer.capacity() == 2048)
    for i in 0..1500 {
        let expected: u8 = if i < 513 { (i % 251).downcast_truncate() } else { 0 }
        core::assert(buffer[i] == expected)
    }
    buffer.clear()
    0
}
"#,
    )
    .unwrap();
    let allocator =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native/byte_buffer_allocator.c");
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        let object = build_native_object(&source, &out, level);
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
use core::MemBuffer
struct Frame { data: MemBuffer, value: u256 }
fn mixed(_ buffer: mut MemBuffer, _ value: mut u256) {
    value += 1
    core::assert(buffer.try_resize(33))
    let scalar: u256 = value
    buffer[32] = scalar.downcast_unchecked()
    buffer.span().copy_within(source: 32, dest: 0, len: 1)
    value += buffer[0] as u256
}
fn forward(_ buffer: mut MemBuffer, _ value: mut u256) {
    mixed(mut buffer, mut value)
}
fn run(_ frame: mut Frame) { forward(mut frame.data, mut frame.value) }
pub fn main() -> i32 {
    var frame = Frame { data: MemBuffer::empty(), value: 20 }
    run(mut frame)
    core::assert(frame.value == 42 && frame.data[32] == 21)
    run(mut frame)
    core::assert(frame.value == 86 && frame.data[0] == 43)
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
use core::MemBuffer
struct Journal { bytes: MemBuffer }
struct Frame { data: Journal, value: u256 }
impl Frame {
    fn new() -> Self { Self { data: Journal { bytes: MemBuffer::empty() }, value: 0 } }
    fn run(mut self, state: mut Frame) {
        self.data.bytes.clear()
        self.value = 1
        core::assert(self.data.bytes.try_resize(32))
        core::assert(state.data.bytes.try_resize(32))
        self.data.bytes[0] = 2
        state.data.bytes.span().fill(3)
        state.value += 1
    }
}
fn execute(state: mut Frame) -> Frame {
    var vm = Frame::new()
    vm.run(mut state)
    vm
}
fn finish(state: mut Frame) {
    var vm = Frame::new()
    vm.run(mut state)
}
pub fn main() -> i32 {
    var state = Frame::new()
    let vm = execute(state: mut state)
    core::assert(vm.value == 1 && state.value == 1)
    core::assert(vm.data.bytes[0] == 2 && state.data.bytes[31] == 3)
    finish(state: mut state)
    core::assert(state.value == 2 && state.data.bytes[31] == 3)
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
use core::MemBuffer
fn write_at_depth(_ buffer: mut MemBuffer, depth: usize, value: u8) {
    if depth == 0 {
        buffer[31] = value
    } else {
        write_at_depth(mut buffer, depth: depth - 1, value)
    }
}
fn write_alternating(_ first: mut MemBuffer, _ second: mut MemBuffer, depth: usize, value: u8) {
    if depth == 0 {
        first[31] = value
    } else {
        write_alternating(mut second, mut first, depth: depth - 1, value)
    }
}
pub fn main() -> i32 {
    var buffer = MemBuffer::alloc(32)
    write_at_depth(mut buffer, depth: 0, value: 19)
    core::assert(buffer[31] == 19)
    write_at_depth(mut buffer, depth: 32, value: 42)
    core::assert(buffer[31] == 42)
    var other = MemBuffer::alloc(32)
    other[31] = 7
    write_alternating(mut buffer, mut other, depth: 32, value: 19)
    core::assert(buffer[31] == 19 && other[31] == 7)
    write_alternating(mut buffer, mut other, depth: 33, value: 42)
    core::assert(buffer[31] == 19 && other[31] == 42)
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
use core::MemBuffer
fn grow(_ buffer: mut MemBuffer, depth: usize, length: usize, value: u8) {
    if depth == 0 {
        for size in 1..length + 1 {
            core::assert(buffer.try_resize(size))
            buffer[size - 1] = value
        }
    } else { grow(mut buffer, depth: depth - 1, length, value) }
}
fn clear(_ buffer: mut MemBuffer, depth: usize) {
    if depth == 0 { buffer.clear() }
    else { clear(mut buffer, depth: depth - 1) }
}
pub fn main() -> i32 {
    var buffer = MemBuffer::empty()
    grow(mut buffer, depth: 32, length: 32, value: 19)
    core::assert(buffer.len() == 32 && buffer[31] == 19)
    clear(mut buffer, depth: 32)
    core::assert(buffer.len() == 0)
    grow(mut buffer, depth: 16, length: 513, value: 42)
    core::assert(buffer.len() == 513)
    for index in 0..513 { core::assert(buffer[index] == 42) }
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

fn build_native_object(source: &Path, out: &Path, level: &str) -> PathBuf {
    let stem = source.file_stem().unwrap().to_str().unwrap();
    let object_name = format!("{stem}.o");
    let report = out.with_extension("tar.gz");
    build(
        source,
        out,
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
        .filter(|name| name.rsplit('/').next() == Some(object_name.as_str()))
        .collect();
    assert_eq!(objects.len(), 1, "{listing}");
    let extracted = Command::new("tar")
        .arg("-xOf")
        .arg(&report)
        .arg(objects[0])
        .output()
        .unwrap();
    assert!(extracted.status.success(), "{extracted:?}");
    let object = out.join(object_name);
    fs::write(&object, extracted.stdout).unwrap();
    object
}

#[test]
fn native_owners_free_each_allocation_once_on_every_path() {
    let temp = tempdir().unwrap();
    let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native");
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        let object = build_native_object(&fixtures.join("owned_frames.fe"), &out, level);
        for (stack_failure, buffer_failure, mode) in [
            (0, 0, "0"),
            (1, 0, "1"),
            (0, 1, "2"),
            (0, 2, "2"),
            (0, 3, "2"),
            (0, 4, "2"),
        ] {
            let executable = out.join(format!("stack-{stack_failure}-{buffer_failure}"));
            let linked = Command::new("cc")
                .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-fno-builtin"])
                .arg(format!("-DFAIL_STACK={stack_failure}"))
                .arg(format!("-DFAIL_BUFFER={buffer_failure}"))
                .arg(&object)
                .arg(fixtures.join("owner_allocator.c"))
                .arg("-o")
                .arg(&executable)
                .output()
                .unwrap();
            assert!(linked.status.success(), "{linked:?}");
            let result = Command::new(&executable).arg(mode).output().unwrap();
            assert!(
                result.status.success(),
                "O{level} {stack_failure}/{buffer_failure}: {result:?}"
            );
        }
    }
}

#[test]
fn native_vec_elements_reserve_their_owner_while_used() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("stack_access.fe");
    for operation in [
        "let value = stack.pop()\ncore::assert(top == value.unwrap())",
        "core::mem::drop(stack)\ncore::assert(top == 1)",
    ] {
        fs::write(
            &source,
            format!(
                r#"
use core::MemVec
pub fn main() -> i32 {{
    var stack: MemVec<u256> = MemVec::new()
    stack.push(1)
    let top = mut stack[0]
    {operation}
    0
}}
"#
            ),
        )
        .unwrap();
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .arg("check")
            .arg(&source)
            .output()
            .unwrap();
        assert!(!result.status.success(), "{operation}: {result:?}");
        assert!(
            String::from_utf8_lossy(&result.stderr).contains("access conflict"),
            "{operation}: {result:?}"
        );
    }
}

#[test]
fn native_runner_shares_helpers_across_modules_and_keeps_test_identities() {
    let temp = tempdir().unwrap();
    let root = temp.path().join("shared_tests");
    fs::create_dir_all(root.join("src")).unwrap();
    let root = root.as_path();
    fs::write(
        root.join("fe.toml"),
        "[ingot]\nname = \"shared_tests\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(
        root.join("src/lib.fe"),
        "pub fn shared_answer() -> u256 { 42 }\n",
    )
    .unwrap();
    fs::write(
        root.join("src/alpha.fe"),
        r#"
use ingot::shared_answer
extern {
    fn abs(_ value: i32) -> i32
}
#[test]
fn same_name() { core::assert(shared_answer() == 42) }
#[test]
fn keep_alpha() {
    core::assert(shared_answer() == 42)
    core::assert(unsafe { abs(-42) } == 42)
}
"#,
    )
    .unwrap();
    fs::write(
        root.join("src/beta.fe"),
        r#"
use ingot::shared_answer
extern {
    fn abs(_ value: i32) -> i32
}
#[test]
fn same_name() { core::assert(shared_answer() == 0) }
#[test]
fn keep_beta() {
    core::assert(shared_answer() == 42)
    core::assert(unsafe { abs(-7) } == 7)
}
"#,
    )
    .unwrap();
    for level in ["0", "1"] {
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["test", "--backend", "native", "--jobs", "2", "-O", level])
            .arg(root)
            .output()
            .unwrap();
        assert!(!result.status.success(), "{result:?}");
        assert!(
            String::from_utf8_lossy(&result.stdout).contains("3 passed; 1 failed"),
            "{result:?}"
        );

        let report = root.join(format!("report-{level}.tar.gz"));
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args([
                "test",
                "--backend",
                "native",
                "--grouped",
                "--filter",
                "keep_",
                "--emit",
                "ir,rmir",
                "--report",
                "--report-out",
            ])
            .arg(&report)
            .args(["-O", level])
            .arg(root)
            .output()
            .unwrap();
        assert!(result.status.success(), "{result:?}");
        assert!(
            String::from_utf8_lossy(&result.stdout).contains("2 passed; 0 failed"),
            "{result:?}"
        );
        let rmir = fs::read_to_string(root.join("out/shared_tests.native.test.rmir")).unwrap();
        assert_eq!(
            rmir.lines()
                .filter(|line| line.contains("inline=")
                    && line.contains("owner=")
                    && line.contains("shared_answer"))
                .count(),
            1,
            "{rmir}"
        );
        let ir = fs::read_to_string(root.join("out/shared_tests.native.test.sona")).unwrap();
        assert_eq!(
            ir.lines()
                .filter(|line| line.starts_with("declare external %abs("))
                .count(),
            1,
            "{ir}"
        );
        assert!(
            !ir.contains("same_name"),
            "filtered tests must not enter the shared graph"
        );
        let listing = Command::new("tar")
            .arg("-tzf")
            .arg(report)
            .output()
            .unwrap();
        assert!(listing.status.success(), "{listing:?}");
        let listing = String::from_utf8_lossy(&listing.stdout);
        assert_eq!(
            listing
                .lines()
                .filter(|line| line.ends_with("/tests.o"))
                .count(),
            1,
            "{listing}"
        );
        assert_eq!(
            listing
                .lines()
                .filter(|line| line.ends_with("/status.txt"))
                .count(),
            2,
            "{listing}"
        );
    }

    let report = root.join("report-no-matches.tar.gz");
    let result = Command::new(env!("CARGO_BIN_EXE_fe"))
        .args([
            "test",
            "--backend",
            "native",
            "--filter",
            "missing_test",
            "--report",
            "--report-out",
        ])
        .arg(&report)
        .arg(root)
        .output()
        .unwrap();
    assert!(result.status.success(), "{result:?}");
    let listing = Command::new("tar")
        .arg("-tzf")
        .arg(report)
        .output()
        .unwrap();
    assert!(listing.status.success(), "{listing:?}");
    assert!(
        !String::from_utf8_lossy(&listing.stdout)
            .lines()
            .any(|line| line.ends_with("/tests.o")),
        "no selected tests must not create a native object"
    );
}
