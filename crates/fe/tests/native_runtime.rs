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
        fs::symlink,
        process::{CommandExt, ExitStatusExt},
    },
    path::{Path, PathBuf},
    process::{Command, Output, Stdio},
    thread,
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};

use num_bigint::BigUint;
use tempfile::tempdir;
use tiny_keccak::{Hasher, Keccak};

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
fn native_allocations_the_host_cannot_make_run_out_of_memory() {
    // The C allocator cannot make `usize::max()` bytes.
    let temp = tempdir().unwrap();
    let test_source = temp.path().join("host_oom_test.fe");
    fs::write(
        &test_source,
        "use core::ptr\n#[test(should_revert, panic = 0x41)]\nfn too_large() {\n    let _ = ptr::alloc_bytes(usize::max())\n}\n",
    )
    .unwrap();
    let source = temp.path().join("host_oom.fe");
    fs::write(
        &source,
        "use core::ptr\npub fn main() -> i32 {\n    let data = ptr::alloc_bytes(usize::max())\n    unsafe { *data = 1 }\n    0\n}\n",
    )
    .unwrap();
    test_utils::skip_crash_reports();
    for level in ["0", "2"] {
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["test", "--backend", "native", "-O", level])
            .arg(&test_source)
            .output()
            .unwrap();
        assert!(
            String::from_utf8_lossy(&result.stdout).contains("1 passed; 0 failed"),
            "O{level}: {result:?}"
        );
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("host_oom")).output().unwrap();
        assert_eq!(
            result.status.signal(),
            Some(libc::SIGABRT),
            "O{level}: {result:?}"
        );
        assert_eq!(
            result.stderr, b"panicked: out of memory\n",
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
        "panicked: index out of bounds\n",
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
fn native_keccak_matches_a_reference_for_runtime_data() {
    let keccak = |bytes: &[u8]| {
        let mut hasher = Keccak::v256();
        hasher.update(bytes);
        let mut digest = [0; 32];
        hasher.finalize(&mut digest);
        format!("{} {}\n", bytes.len(), hex::encode(digest))
    };
    let bytes: Vec<u8> = (0..420).map(|index| (index * 7 + 3) as u8).collect();
    let word = |word: u8| array::from_fn::<u8, 32, _>(|i| if i == 31 { word } else { 0 });
    let mut expected: String = (0..420).map(|len| keccak(&bytes[..len])).collect();
    for len in [0, 1, 31, 32, 33, 135, 136, 137, 272] {
        expected += &keccak(&bytes[..len]);
    }
    expected += &keccak(&[]);
    expected += &keccak(&word(1));
    expected += &keccak(&word(1).repeat(10));

    let temp = tempdir().unwrap();
    let source =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native/keccak_digests.fe");
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("keccak_digests")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
        assert_eq!(
            String::from_utf8_lossy(&result.stdout),
            expected,
            "O{level}"
        );
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
    // A box's value is a Sonatina object in place, so the frame moves in one
    // object load and store despite the padding after `flag`.
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
                    let (frame, _) = line
                        .strip_prefix("type ")?
                        .split_once(" = {i1, [i256; 64], i64, [i8; 3], @layout_")?;
                    Some(frame)
                })
                .unwrap_or_else(|| panic!("no frame type\n{ir}"));
            // Value names are per function, and the object operand is the
            // whole token after `op`: `v5` must not match `v57`.
            let through_view = |op: &str| {
                ir.split("\nfunc ").any(|function| {
                    let views: Vec<_> = function
                        .lines()
                        .filter_map(|line| {
                            let (view, rest) =
                                line.trim().split_once(&format!(".objref<{frame}> = "))?;
                            rest.starts_with("obj.from_ptr ").then_some(view)
                        })
                        .collect();
                    function.lines().any(|line| {
                        line.split_once(&format!("{op} "))
                            .and_then(|(_, operands)| operands.split([' ', ';']).next())
                            .is_some_and(|object| views.contains(&object))
                    })
                })
            };
            assert!(
                through_view("obj.load") && through_view("obj.store"),
                "{frame}\n{ir}"
            );
        }
    }
}

#[test]
fn native_boxed_and_local_values_share_methods() {
    // A value behind a pointer is an object in place, like a local, so a
    // method called on both compiles once and keeps `noalias`.
    let temp = tempdir().unwrap();
    let source = temp.path().join("shared.fe");
    fs::write(
        &source,
        r#"
use core::{Box, Result}

struct Counter {
    tag: u8,
    total: u64,
    last: u256,
}

impl Counter {
    fn bump(mut self, by: u64) {
        self.total += by
        self.last = self.total as u256
    }
}

pub fn main() -> i32 {
    var local = Counter { tag: 1, total: 0, last: 0 }
    local.bump(by: 3)
    var boxed = match Box::try_new(Counter { tag: 2, total: 0, last: 0 }) {
        Result::Ok(boxed) => boxed,
        Result::Err(_) => return 1,
    }
    boxed.get().bump(by: 4)
    let inner = boxed.into_inner()
    if local.total == 3 && inner.total == 4 && inner.last == 4 { 0 } else { 2 }
}
"#,
    )
    .unwrap();
    for level in ["0", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("shared")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
        if level == "0" {
            let ir = fs::read_to_string(out.join("shared.native.sona")).unwrap();
            let bumps: Vec<_> = ir
                .lines()
                .filter(|line| line.starts_with("func") && line.contains("bump"))
                .collect();
            assert!(
                matches!(bumps[..], [bump] if bump.contains("(noalias nocapture v0.objref<")),
                "{bumps:?}"
            );
        }
    }
}

#[test]
fn native_object_views_and_raw_accesses_see_each_other() {
    // An object view and raw pointers address the same memory: a write
    // through either is seen through the other, in the same function and
    // across calls.
    let temp = tempdir().unwrap();
    let source = temp.path().join("views.fe");
    fs::write(
        &source,
        r#"
use core::ptr::{self, cast, offset_bytes}

struct Pair { a: u64, b: u64 }

impl Pair {
    fn bump(mut self) {
        self.a += 1
        self.b += 2
    }
}

#[inline(never)]
fn bump_at(_ pair: *Pair) {
    unsafe { (*pair).bump() }
}

#[inline(never)]
fn poke(_ pair: *Pair, b: u64) {
    unsafe { *offset_bytes(cast<Pair, u64>(pair), 8) = b }
}

pub fn main() -> i32 {
    let pair = ptr::alloc<Pair>()
    let first = cast<Pair, u64>(pair)
    let second = offset_bytes(first, 8)
    unsafe { *pair = Pair { a: 1, b: 2 } }
    if unsafe { *first } != 1 || unsafe { *second } != 2 { return 1 }
    unsafe { *first = 10 }
    if unsafe { (*pair).a } != 10 { return 2 }
    bump_at(pair)
    if unsafe { *first } != 11 || unsafe { *second } != 4 { return 3 }
    let before = unsafe { (*pair).b }
    poke(pair, b: 40)
    if before != 4 || unsafe { (*pair).b } != 40 { return 4 }
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("views")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}

#[test]
fn native_programs_parse_arguments_and_input_lines() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("sum.fe");
    fs::write(
        &source,
        r#"
use core::text
use std::env::Args
use std::io::{self, Stdin, Stdout}

/// Sums the second comma-separated field of each `key` line, plus the first argument.
pub fn main() -> i32 uses (args: Args, input: mut Stdin, out: mut Stdout) {
    var total = text::parse_uint(args.get(1)).unwrap()
    var stdin = MemBuffer::new(0)
    io::read_to_end(mut stdin)
    for line in stdin.span().lines() {
        if line.trim().starts_with("key,") {
            for (i, field) in line.split(b',').enumerate() {
                if i == 1 {
                    total += text::parse_uint(field.trim()).unwrap()
                }
            }
        }
    }
    println!(total)
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "2"] {
        let out = temp.path().join(format!("O{level}"));
        build(&source, &out, level, &[]);
        let mut child = Command::new(out.join("sum"))
            .arg("100")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();
        child
            .stdin
            .take()
            .unwrap()
            .write_all(b"key, 1\r\nother,1000\n  key,20, x\nkey,300")
            .unwrap();
        let result = child.wait_with_output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
        assert_eq!(String::from_utf8_lossy(&result.stdout), "421\n", "O{level}");
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
        "#[unsafe(export)]\npub fn answer() -> i32 { 42 }\npub fn unused<T>(x: own T) -> T { x }\n#[test]\nfn math_test() { core::assert(answer() == 42) }\n",
    )
    .unwrap();
    fs::write(
        root.join("app/src/lib.fe"),
        "use std::io::Stdout\npub fn main() -> i32 uses (out: mut Stdout) { helper::answer() }\n#[test]\nfn root_test() uses (out: mut Stdout) { core::assert(main() == 42) }\n",
    )
    .unwrap();
    fs::write(
        root.join("app/src/helper.fe"),
        "use std::io::Stdout\npub fn answer() -> i32 uses (out: mut Stdout) {\n print!(\"A\")\n math::answer()\n}\n#[test]\nfn nested_test() uses (out: mut Stdout) { core::assert(answer() == 42) }\n",
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
        // A library build selects the members that export functions.
        let library = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args([
                "build",
                "--backend",
                "native",
                "--emit",
                "staticlib",
                "-O",
                level,
            ])
            .arg("--out-dir")
            .arg(&out)
            .arg(root)
            .output()
            .unwrap();
        assert!(library.status.success(), "{library:?}");
        assert!(out.join("libmath.a").is_file());
        assert!(!out.join("libapp.a").exists());
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
        ("aggregate_equality.fe", 14),
        ("aggregate_object_copies.fe", 3),
        ("const_aggregate_values.fe", 5),
        ("hash_map.fe", 16),
        ("text_search.fe", 20),
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
fn text_fixtures_run_at_every_level() {
    for (name, expected_count) in [
        ("string_literal_eq.fe", 1),
        ("span_string_eq.fe", 4),
        ("string_text_ops.fe", 5),
        ("byte_literals.fe", 1),
        ("write_macros.fe", 2),
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
use core::MemBuffer
use std::io::{Stdin, Stdout, read_to_end}
pub fn main() -> i32 uses (out: mut Stdout, input: mut Stdin) {
    println!("ready")
    var data = MemBuffer::empty()
    read_to_end(mut data)
    core::assert(data.len() == 1 && data[0] == 65)
    var reply = MemBuffer::empty()
    reply.push(data[0] + 1)
    print!(reply.span())
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
fn native_standard_streams_print_and_read() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("streams.fe");
    fs::write(
        &source,
        r#"
use core::MemBuffer
use std::io::{Stderr, Stdio, StdinReader, Stdout}

fn report(_ count: usize) uses (out: mut Stdout) {
    println!("lines: ", count)
}

// `Stdio` provides each stream, here two at once.
fn both() uses (out: mut Stdout, err: mut Stderr) {
    print!("no newline")
    println!()
    eprint!("to ")
    eprintln!("stderr")
}

pub fn main() -> i32 uses (io: mut Stdio) {
    println!(0 as u8, " ", 255 as u8, " ", 65535 as u16, " ", 4294967295 as u32)
    println!(18446744073709551615 as u64, " ", 340282366920938463463374607431768211455 as u128)
    println!(115792089237316195423570985008687907853269984665640564039457584007913129639935 as u256)
    let small: i8 = -128
    let half: i16 = -32768
    let word: i32 = -2147483648
    println!(small, " ", 127 as i8, " ", half, " ", word)
    let long: i64 = -9223372036854775808
    let wide: i128 = -170141183460469231731687303715884105728
    println!(long, " ", wide)
    let widest: i256 = -57896044618658097711785492504343953926634992332820282019728792003956564819968
    println!(widest)
    let minus: isize = -1
    println!(0 as usize, " ", minus, " ", true, " ", false)
    let padded: String<8> = "ab"
    println!(padded, "|", "a literal longer than thirty-one bytes")
    both()

    var reader = StdinReader::new()
    let first = reader.read_byte().unwrap()
    var line = MemBuffer::empty()
    var count: usize = 0
    while reader.read_line(mut line) {
        count += 1
        println!("[", line.span(), "]")
        line.clear()
    }
    report(count)
    var reply = MemBuffer::empty()
    reply.push(first)
    let first = format!("first byte: ", reply.span())
    println!(first, ", ", first.len(), " bytes")
    0
}
"#,
    )
    .unwrap();
    // Lines longer than the reader's 4 KiB reads span several of them.
    let long = "x".repeat(10_000);
    let input = format!("@first\n\nlast {long}");
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let mut child = Command::new(out.join("streams"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();
        child
            .stdin
            .take()
            .unwrap()
            .write_all(input.as_bytes())
            .unwrap();
        let result = child.wait_with_output().unwrap();
        assert!(result.status.success(), "{result:?}");
        assert_eq!(
            String::from_utf8(result.stdout).unwrap(),
            format!(
                "0 255 65535 4294967295\n\
                 18446744073709551615 340282366920938463463374607431768211455\n\
                 115792089237316195423570985008687907853269984665640564039457584007913129639935\n\
                 -128 127 -32768 -2147483648\n\
                 -9223372036854775808 -170141183460469231731687303715884105728\n\
                 -57896044618658097711785492504343953926634992332820282019728792003956564819968\n\
                 0 -1 true false\n\
                 ab|a literal longer than thirty-one bytes\n\
                 no newline\n\
                 [first]\n[]\n[last {long}]\nlines: 3\nfirst byte: @, 13 bytes\n"
            )
        );
        assert_eq!(result.stderr, b"to stderr\n");
    }

    // `read_to_end` keeps every byte, including zeros.
    let source = temp.path().join("echo.fe");
    fs::write(
        &source,
        r#"
use core::MemBuffer
use std::io::{Stdin, Stdout, read_to_end}
pub fn main() -> i32 uses (out: mut Stdout, input: mut Stdin) {
    var data = MemBuffer::empty()
    read_to_end(mut data)
    print!(data.span())
    0
}
"#,
    )
    .unwrap();
    let input = (0..20_000u32).map(|i| (i % 251) as u8).collect::<Vec<_>>();
    let out = temp.path().join("echo-out");
    build(&source, &out, "1", &[]);
    let mut child = Command::new(out.join("echo"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    child.stdin.take().unwrap().write_all(&input).unwrap();
    let result = child.wait_with_output().unwrap();
    assert!(result.status.success(), "{result:?}");
    assert_eq!(result.stdout, input);
}

#[test]
fn native_environment_and_exit() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("environment.fe");
    fs::write(
        &source,
        r#"
use core::Option
use std::env::Env
use std::io::Stdout
use std::process::Exit
pub fn main() -> i32 uses (out: mut Stdout, env: Env, proc: Exit) {
    match env.lookup("FE_NATIVE_VALUE") {
        Option::Some(value) => print!("[", value, "]"),
        Option::None => print!("unset"),
    }
    // Any `Display` value names a variable.
    let name = format!("FE_NATIVE", "_VALUE")
    core::assert(env.lookup(name.span()).is_some() == env.lookup("FE_NATIVE_VALUE").is_some())
    proc.exit(7)
}
"#,
    )
    .unwrap();
    for level in ["0", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let executable = out.join("environment");
        for (value, expected) in [
            (Some("a=b c"), &b"[a=b c]"[..]),
            (Some(""), b"[]"),
            (None, b"unset"),
        ] {
            let mut command = Command::new(&executable);
            match value {
                Some(value) => command.env("FE_NATIVE_VALUE", value),
                None => command.env_remove("FE_NATIVE_VALUE"),
            };
            let result = command.output().unwrap();
            assert_eq!(result.status.code(), Some(7), "{result:?}");
            assert_eq!(result.stdout, expected);
        }
    }

    // A test is not an executable's entry, so it has no arguments.
    let source = temp.path().join("tests.fe");
    fs::write(
        &source,
        "use std::env::Args\n#[test]\nfn no_arguments() uses (args: Args) { core::assert(args.count() == 0) }\n",
    )
    .unwrap();
    let result = Command::new(env!("CARGO_BIN_EXE_fe"))
        .args(["test", "--backend", "native"])
        .arg(&source)
        .output()
        .unwrap();
    assert!(result.status.success(), "{result:?}");
}

#[test]
fn native_entries_take_only_host_effects() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("evm_effect.fe");
    fs::write(
        &source,
        "use std::evm::{Ctx, Evm}\n#[test]\nfn reads() uses (evm: mut Evm) {}\n#[test]\nfn chain() uses (ctx: Ctx) {}\n",
    )
    .unwrap();
    let result = Command::new(env!("CARGO_BIN_EXE_fe"))
        .args(["test", "--backend", "native"])
        .arg(&source)
        .output()
        .unwrap();
    let output = String::from_utf8_lossy(&result.stdout);
    assert!(!result.status.success());
    assert!(
        output.contains("the root effect `Host` does not provide `Evm`")
            && output.contains("the root effect `Host` does not provide `Ctx`"),
        "{output}"
    );
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
use std::io::Stdout
#[test]
fn spin() { while true {} }
#[test]
fn noisy() uses (out: mut Stdout) {
    var i: u32 = 0
    while i < 2048 {
        print!("A")
        i += 1
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
fn native_sha256_compress_hashes_blocks() {
    // "abc" is one padded block. The 56-letter message from FIPS 180-2 takes
    // two: the second is its padding alone.
    let temp = tempdir().unwrap();
    let source = temp.path().join("sha.fe");
    fs::write(
        &source,
        r#"
use std::native::sha256_compress
const IV: [u32; 8] = [
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
]
fn same(_ a: [u32; 8], _ b: [u32; 8]) -> bool {
    for i in 0..8 {
        if a[i] != b[i] { return false }
    }
    true
}
pub fn main() -> i32 {
    var abc: [u32; 16] = [0; 16]
    abc[0] = 0x61626380
    abc[15] = 24
    let one = sha256_compress(IV, abc)
    let first: [u32; 16] = [
        0x61626364, 0x62636465, 0x63646566, 0x64656667, 0x65666768, 0x66676869, 0x6768696a, 0x68696a6b,
        0x696a6b6c, 0x6a6b6c6d, 0x6b6c6d6e, 0x6c6d6e6f, 0x6d6e6f70, 0x6e6f7071, 0x80000000, 0,
    ]
    var second: [u32; 16] = [0; 16]
    second[15] = 448
    let two = sha256_compress(sha256_compress(IV, first), second)
    if !same(one, [0xba7816bf, 0x8f01cfea, 0x414140de, 0x5dae2223, 0xb00361a3, 0x96177a9c, 0xb410ff61, 0xf20015ad]) {
        return 1
    }
    if !same(two, [0x248d6a61, 0xd20638b8, 0xe5c02693, 0x0c3e6039, 0xa33ce459, 0x64ff2167, 0xf6ecedd4, 0x19db06c1]) {
        return 2
    }
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("sha")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}

#[test]
fn native_process_clock_preserves_borrows_and_host_io() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("clock.fe");
    fs::write(
        &source,
        r#"
use core::MemBuffer
use std::io::{Stdin, StdinReader, Stdout}
use std::native::cpu_clock_ticks
pub fn main() -> i32 uses (out: mut Stdout, input: mut Stdin) {
    var reader = StdinReader::new()
    var value: u8 = 0
    let borrowed = mut value
    let start = cpu_clock_ticks()
    borrowed = reader.read_byte().unwrap()
    let end = cpu_clock_ticks()
    core::assert(start >= 0 && end >= start)
    var reply = MemBuffer::empty()
    reply.push(value)
    print!(reply.span())
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
fn native_arguments_preserve_bytes_bounds_and_the_entry_name() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("arguments.fe");
    fs::write(
        &source,
        r#"
use core::MemBuffer
use core::ops::Eq
use std::env::Args
use std::io::Stdout
// The entry's internal name is free for a Fe function.
fn __fe_native_main() -> i32 { 42 }
pub fn main() -> i32 uses (out: mut Stdout, args: Args) {
    core::assert(__fe_native_main() == 42)
    if args.count() == 1 { let _ = args.get(args.count()) }
    if args.count() == 2 {
        let arg = args.get(1)
        let _ = arg.span()[arg.len()]
    }
    var text = MemBuffer::empty()
    var i: usize = 1
    while i < args.count() {
        let arg = args.get(i)
        text.extend_from(arg.span())
        // An argument compares with a literal's text.
        if arg == "Fe"
            || arg.span() == "λ"
            || !arg.eq("Fee") && arg == "an argument of thirty-two bytes!"
        {
            text.push(33)
        }
        text.push(124)
        i += 1
    }
    print!(text)
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
            .args(["", "Fe", "λ", "Fee", "an argument of thirty-two bytes!"])
            .arg(OsString::from_vec(vec![0x80, 0xff]))
            .output()
            .unwrap();
        assert!(result.status.success(), "{result:?}");
        assert_eq!(
            result.stdout,
            b"|Fe!|\xce\xbb!|Fee|an argument of thirty-two bytes!!|\x80\xff|"
        );
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
use core::MemBuffer
use std::io::{Stdin, StdinReader, Stdout}
use std::evm::crypto::{addmod, mulmod}
fn read_word(_ reader: mut StdinReader) -> u256 uses (input: mut Stdin) {
    var value: u256 = 0
    var i: u256 = 0
    while i < 32 {
        let bits: u256 = reader.read_byte().unwrap() as u256
        value = value | (bits << (i * 8))
        i += 1
    }
    value
}
fn write_word(_ text: mut MemBuffer, _ value: u256) {
    var i: u256 = 0
    while i < 32 {
        text.push(((value >> (i * 8)) & 255).downcast_unchecked())
        i += 1
    }
}
pub fn main() -> i32 uses (out: mut Stdout, input: mut Stdin) {
    var reader = StdinReader::new()
    var text = MemBuffer::empty()
    while true {
        match reader.read_byte() {
            Option::Some(marker) => {
                core::assert(marker == 64)
                let lhs = read_word(mut reader)
                let rhs = read_word(mut reader)
                let modulus = read_word(mut reader)
                write_word(mut text, addmod(lhs, rhs, modulus))
                write_word(mut text, mulmod(lhs, rhs, modulus))
            }
            Option::None => break
        }
    }
    print!(text.span())
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
use core::MemBuffer
use core::num::{leading_zeros, trailing_zeros}
use std::io::{Stdin, StdinReader, Stdout}
fn read_word(_ reader: mut StdinReader) -> u256 uses (input: mut Stdin) {
    var value: u256 = 0
    var i: u256 = 0
    while i < 32 {
        let bits: u256 = reader.read_byte().unwrap() as u256
        value = value | (bits << (i * 8))
        i += 1
    }
    value
}
fn write_word(_ text: mut MemBuffer, _ value: u256) {
    var i: u256 = 0
    while i < 32 {
        text.push(((value >> (i * 8)) & 255).downcast_unchecked())
        i += 1
    }
}
pub fn main() -> i32 uses (out: mut Stdout, input: mut Stdin) {
    var reader = StdinReader::new()
    var text = MemBuffer::empty()
    while true {
        match reader.read_byte() {
            Option::Some(marker) => {
                core::assert(marker == 64)
                let value = read_word(mut reader)
                write_word(mut text, leading_zeros(value))
                write_word(mut text, trailing_zeros(value))
            }
            Option::None => break
        }
    }
    print!(text.span())
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
fn native_memory_copy_preserves_overlaps() {
    let temp = tempdir().unwrap();
    let source =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fe_test/raw_memory_copy.fe");
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("raw_memory_copy")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
        // The backend lowers the copy, so Fe declares no `memmove` and
        // `local::memmove` keeps its name.
        let ir = fs::read_to_string(out.join("raw_memory_copy.native.sona")).unwrap();
        assert!(ir.contains("memcopy "), "{ir}");
        assert!(!ir.contains("declare external %memmove"), "{ir}");
        if level == "0" {
            assert!(ir.contains("func private %memmove("), "{ir}");
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

#[test]
fn native_exports_are_callable_from_c() {
    let temp = tempdir().unwrap();
    let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native");
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        let object = build_native_object(&fixtures.join("exports.fe"), &out, level);
        let executable = out.join("call_exports");
        let linked = Command::new("cc")
            .args(["-std=c11", "-Wall", "-Wextra", "-Werror"])
            .arg(&object)
            .arg(fixtures.join("call_exports.c"))
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
fn native_static_libraries_link_into_c_programs() {
    // The library's panic aborts its host.
    test_utils::skip_crash_reports();
    let temp = tempdir().unwrap();
    let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native");
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        let built = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["build", "--backend", "native", "--emit", "object,staticlib"])
            .args(["-O", level, "--out-dir"])
            .arg(&out)
            .arg(fixtures.join("library.fe"))
            .output()
            .unwrap();
        assert!(built.status.success(), "O{level}: {built:?}");
        let mut library = OsString::from("-L");
        library.push(&out);
        for (form, inputs) in [
            ("archive", vec![library, OsString::from("-llibrary")]),
            ("object", vec![out.join("library.o").into_os_string()]),
        ] {
            let executable = out.join(format!("host-{form}"));
            let linked = Command::new("cc")
                .args(["-std=c11", "-Wall", "-Wextra", "-Werror"])
                .arg(fixtures.join("library_host.c"))
                .args(&inputs)
                .arg("-o")
                .arg(&executable)
                .output()
                .unwrap();
            assert!(linked.status.success(), "O{level} {form}: {linked:?}");
            let result = Command::new(&executable).output().unwrap();
            assert!(result.status.success(), "O{level} {form}: {result:?}");
            assert_eq!(result.stdout, b"42 zzz\n", "O{level} {form}: {result:?}");
            // A panic aborts the host process, after flushing its output.
            let panicked = Command::new(&executable).arg("x").output().unwrap();
            assert_eq!(
                panicked.status.signal(),
                Some(libc::SIGABRT),
                "O{level} {form}: {panicked:?}"
            );
            assert_eq!(panicked.stdout, b"42 zzz\n", "O{level} {form}");
            assert_eq!(
                panicked.stderr, b"panicked: arithmetic overflow\n",
                "O{level} {form}"
            );
        }
    }
}

#[test]
fn native_executables_link_host_objects_and_libraries() {
    let temp = tempdir().unwrap();
    let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native");
    // An object, and a library the C compiler finds on `LIBRARY_PATH`.
    let host = temp.path().join("host");
    fs::create_dir_all(&host).unwrap();
    for name in ["host_apply", "host_offset"] {
        let compiled = Command::new("cc")
            .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-c"])
            .arg(fixtures.join(format!("{name}.c")))
            .arg("-o")
            .arg(host.join(format!("{name}.o")))
            .output()
            .unwrap();
        assert!(compiled.status.success(), "{compiled:?}");
    }
    let archived = Command::new("ar")
        .arg("rcs")
        .arg(host.join("libhost_offset.a"))
        .arg(host.join("host_offset.o"))
        .output()
        .unwrap();
    assert!(archived.status.success(), "{archived:?}");
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        let built = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["build", "--backend", "native", "-O", level, "--out-dir"])
            .arg(&out)
            .arg("--link")
            .arg(host.join("host_apply.o"))
            .args(["--link-lib", "host_offset"])
            .arg(fixtures.join("host_calls.fe"))
            .env("LIBRARY_PATH", &host)
            .output()
            .unwrap();
        assert!(built.status.success(), "O{level}: {built:?}");
        let result = Command::new(out.join("host_calls")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}

#[test]
fn native_partial_words_stay_in_their_allocations() {
    let temp = tempdir().unwrap();
    let fixtures = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native");
    let allocator = temp.path().join("guarded_allocator.o");
    let compiled = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-c"])
        .arg(fixtures.join("guarded_allocator.c"))
        .arg("-o")
        .arg(&allocator)
        .output()
        .unwrap();
    assert!(compiled.status.success(), "{compiled:?}");
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(
            &fixtures.join("partial_words.fe"),
            &out,
            level,
            &["--link", allocator.to_str().unwrap()],
        );
        let result = Command::new(out.join("partial_words")).output().unwrap();
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
fn native_abi_buffers_and_scratch_free_what_they_allocate() {
    let temp = tempdir().unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        run_balanced("abi_buffers.fe", &out, level, &[]);
    }
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
            "allocations=1 releases=1 resizes=0\n",
            "O{level}"
        );
    }
}

#[test]
fn native_buffers_resize_past_cached_blocks_and_keep_refused_growth() {
    let temp = tempdir().unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        let result = run_balanced("resized_buffers.fe", &out, level, &["-DREFUSE_FROM=65536"]);
        assert_eq!(
            String::from_utf8_lossy(&result.stderr),
            "allocations=10 releases=10 resizes=10\n",
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
fn native_in_place_enum_drops_read_only_the_tag() {
    let temp = tempdir().unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        let result = run_balanced("in_place_enum_drop.fe", &out, level, &[]);
        assert!(result.status.success(), "O{level}: {result:?}");
        // Inlining merges `overwrite` into its caller at higher levels.
        if level == "0" {
            let ir = fs::read_to_string(out.join("in_place_enum_drop.native.sona")).unwrap();
            let overwrite = ir
                .split_once("%overwrite(")
                .and_then(|(_, body)| body.split_once("\n}\n"))
                .expect("`overwrite` is lowered")
                .0;
            assert!(overwrite.contains("enum.get_tag"), "{overwrite}");
            assert!(!overwrite.contains("enum.is_variant"), "{overwrite}");
        }
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
fn native_extern_declarations_of_a_symbol_must_agree() {
    let temp = tempdir().unwrap();
    // A program's declarations must agree with each other and with core's and
    // std's. `u64` and `i64` results are the same C type, so those two
    // declarations share one import.
    for (name, source, errors) in [
        (
            "agree",
            "mod a {\n    extern { pub fn clock() -> i64 }\n}\nmod b {\n    extern { pub fn clock() -> u64 }\n}\npub fn main() -> i32 {\n    let ticks = unsafe { b::clock() }\n    if unsafe { a::clock() } < 0 || ticks == 0 { 1 } else { 0 }\n}\n",
            &[][..],
        ),
        (
            "modules",
            "mod a {\n    extern { pub fn clock() -> i64 }\n}\nmod b {\n    extern { pub fn clock() -> i32 }\n}\npub fn main() -> i32 {\n    let ticks = unsafe { b::clock() }\n    if unsafe { a::clock() } < 0 || ticks < 0 { 1 } else { 0 }\n}\n",
            &[
                "conflicting `extern` declarations of `clock`: ",
                "`fn clock() -> i64` in `modules::a`",
                "`fn clock() -> i32` in `modules::b`",
            ][..],
        ),
        (
            "write",
            "extern { fn write(_ fd: i64, _ buf: *u8, _ len: usize) -> isize }\npub fn main() -> i32 {\n    let _ = unsafe { write(2, core::ptr::alloc_bytes(1), 0) }\n    0\n}\n",
            &[
                "conflicting `extern` declarations of `write`: ",
                "`fn write(_ fd: i32, _ buf: *u8, _ len: usize) -> isize` in `core::rt`",
                "`fn write(_ fd: i64, _ buf: *u8, _ len: usize) -> isize` in `write`",
            ][..],
        ),
        (
            "std",
            "extern { fn clock() -> i32 }\npub fn main() -> i32 {\n    let ticks = std::native::cpu_clock_ticks()\n    if ticks < 0 || unsafe { clock() } < 0 { 1 } else { 0 }\n}\n",
            &[
                "conflicting `extern` declarations of `clock`: ",
                "`fn clock() -> i64` in `std::native`",
                "`fn clock() -> i32` in `std`",
            ][..],
        ),
    ] {
        let path = temp.path().join(format!("{name}.fe"));
        fs::write(&path, source).unwrap();
        let out = temp.path().join(format!("out-{name}"));
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["build", "--backend", "native", "--out-dir"])
            .arg(&out)
            .arg(&path)
            .output()
            .unwrap();
        let output = String::from_utf8_lossy(&result.stderr);
        if errors.is_empty() {
            assert!(result.status.success(), "{name}: {output}");
            let run = Command::new(out.join(name)).output().unwrap();
            assert!(run.status.success(), "{name}: {run:?}");
        } else {
            assert!(!result.status.success(), "{name}: {result:?}");
            for error in errors {
                assert!(output.contains(error), "{name}: {output}");
            }
        }
    }
}

#[test]
fn native_functions_named_like_c_functions_leave_the_backends_calls_alone() {
    // The array copy and the zero fill exceed the inline limit, so they call
    // `memcpy` and `memset`, and the copy of dynamic length calls `memmove`:
    // the C library's, not these functions.
    let temp = tempdir().unwrap();
    let source = temp.path().join("c_names.fe");
    fs::write(
        &source,
        r#"
use core::ptr
use std::env::Args

#[inline(never)]
fn memcpy(_ x: u64) -> u64 { x + 1 }
#[inline(never)]
fn memset(_ x: u64) -> u64 { x + 2 }
#[inline(never)]
fn memmove(_ x: u64) -> u64 { x + 3 }

fn sum(_ a: [u64; 32]) -> u64 {
    var total: u64 = 0
    for x in a {
        total = total + x
    }
    total
}

pub fn main() -> i32 uses (args: Args) {
    var a: [u64; 32] = [3; 32]
    a[5] = memcpy(41) + memset(0) + memmove(0)
    let b = a
    a[0] = 100
    let len = 200 * args.count()
    let source = ptr::alloc_bytes(len)
    let dest = ptr::alloc_bytes(len)
    unsafe {
        ptr::zero_bytes(source, len)
        *ptr::offset_bytes(source, len - 1) = 7
        ptr::copy_raw(dest, source, len)
        if sum(b) != 31 * 3 + 47 || b[0] != 3 || *dest != 0 || *ptr::offset_bytes(dest, len - 1) != 7 {
            return 1
        }
    }
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("c_names")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}

#[test]
fn native_extern_declarations_must_not_name_compiler_symbols() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("reserved.fe");
    fs::write(
        &source,
        "extern {\n    fn __fe_revert(_ data: *u8, _ len: usize)\n}\npub fn main() -> i32 {\n    unsafe { __fe_revert(core::ptr::alloc_bytes(1), 0) }\n    core::panic()\n}\n",
    )
    .unwrap();
    let result = Command::new(env!("CARGO_BIN_EXE_fe"))
        .args(["build", "--backend", "native", "--out-dir"])
        .arg(temp.path().join("out"))
        .arg(&source)
        .output()
        .unwrap();
    let output = String::from_utf8_lossy(&result.stderr);
    assert!(!result.status.success(), "{result:?}");
    assert!(
        output.contains(
            "extern symbol `__fe_revert` is reserved: symbols starting with `__fe_` are the compiler's"
        ),
        "{output}"
    );
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
        var filled = MemBuffer::new(64)
        for i in 0..64 {
            filled[i] = 0xff
        }
        consume(filled)
        let zeroed = MemBuffer::try_new(64).unwrap()
        core::assert(zeroed.len() == 64 && zeroed.capacity() == 64)
        for i in 0..64 {
            core::assert(zeroed[i] == 0)
        }
        consume(zeroed)
    }
    core::assert(MemBuffer::try_new(usize::max()).is_none())
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
    core::assert(buffer.capacity() == 513)
    for i in 0..513 {
        core::assert(buffer[i] == 0)
        buffer[i] = (i % 251).downcast_truncate()
    }
    core::assert(buffer.try_resize(1500))
    core::assert(buffer.capacity() == 1500)
    core::assert(!buffer.try_resize(3000))
    core::assert(buffer.len() == 1500 && buffer.capacity() == 1500)
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
fn native_buffers_grow_past_the_mmap_threshold() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("large_growth.fe");
    // glibc maps blocks from 128 KiB, so the later doublings resize mapped
    // blocks, which it moves with `mremap`.
    fs::write(
        &source,
        r#"
use core::{MemBuffer, MemVec}
pub fn main() -> i32 {
    let words: usize = 1 << 18
    var bytes = MemBuffer::empty()
    for i in 0..words {
        bytes.extend_from_word(i as u256, len: 32)
    }
    var values: MemVec<u256> = MemVec::new()
    for i in 0..words * 4 {
        values.push(i as u256)
    }
    core::assert(values.ptr() as usize % 16 == 0)
    for i in 0..words {
        core::assert(bytes.read_word(i * 32) == i as u256)
    }
    for i in 0..words * 4 {
        core::assert(values[i] == i as u256)
    }
    // The host cannot make the largest allocation, which leaves both as
    // they were.
    let capacity = bytes.capacity()
    core::assert(!bytes.try_reserve(usize::max() - bytes.len()))
    core::assert(bytes.len() == words * 32 && bytes.capacity() == capacity)
    core::assert(bytes.read_word((words - 1) * 32) == (words - 1) as u256)
    core::assert(!values.try_reserve(usize::max() / 32))
    core::assert(values.len() == words * 4 && values[words * 4 - 1] == (words * 4 - 1) as u256)
    0
}
"#,
    )
    .unwrap();
    let test_source = temp.path().join("refused_growth.fe");
    fs::write(
        &test_source,
        r#"
use core::MemBuffer
#[test(should_revert, panic = 0x41)]
fn reserve_beyond_the_host() {
    var buffer = MemBuffer::new(2000)
    buffer.reserve(usize::max() - 2000)
}
"#,
    )
    .unwrap();
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("large_growth")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
        let result = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(["test", "--backend", "native", "-O", level])
            .arg(&test_source)
            .output()
            .unwrap();
        assert!(
            String::from_utf8_lossy(&result.stdout).contains("1 passed; 0 failed"),
            "O{level}: {result:?}"
        );
    }
}

#[test]
fn native_vectors_of_huge_elements_grow_within_the_allocation_limit() {
    // Three elements fit in `MAX_ALLOCATION` bytes but not four, the initial
    // capacity, so growth asks for three, which the host refuses. O0 cannot
    // translate a value this large, so only O1 and O2 run.
    let temp = tempdir().unwrap();
    let source = temp.path().join("huge_elements.fe");
    fs::write(
        &source,
        r#"
use core::MemVec
#[test]
fn reserve_huge_elements() {
    var values: MemVec<[u8; 4611686018427387905]> = MemVec::new()
    core::assert(!values.try_reserve(1))
    core::assert(values.capacity() == 0)
}
"#,
    )
    .unwrap();
    for level in ["1", "2"] {
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
    var buffer = MemBuffer::new(32)
    write_at_depth(mut buffer, depth: 0, value: 19)
    core::assert(buffer[31] == 19)
    write_at_depth(mut buffer, depth: 32, value: 42)
    core::assert(buffer[31] == 42)
    var other = MemBuffer::new(32)
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

#[test]
fn native_fs_reads_writes_and_lists_files() {
    let temp = tempdir().unwrap();
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native/fs.fe");
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let dir = temp.path().join(format!("files-{level}"));
        fs::create_dir(&dir).unwrap();
        symlink("a/b/c/file", dir.join("link")).unwrap();
        let result = Command::new(out.join("fs")).arg(&dir).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
        // Directory entries come in no particular order.
        let stdout = String::from_utf8(result.stdout).unwrap();
        let mut lines = stdout.lines().collect::<Vec<_>>();
        for entries in lines.chunk_by_mut(|a, b| a.starts_with("  ") && b.starts_with("  ")) {
            entries.sort();
        }
        let canonical = fs::canonicalize(&dir).unwrap();
        let expected = format!(
            r#"create_dir_all a/b/c: ok
create_dir_all a/b/c/: ok
write a/b/c/file: ok
append a/b/c/file: ok
append a/new: ok
write a/empty: ok
read a/b/c/file: 11 bytes "hello world"
read a/new: 3 bytes "new"
read a/empty: 0 bytes ""
read link: 11 bytes "hello world"
metadata a: dir
metadata a/b/c/file: file of 11 bytes
metadata link: file of 11 bytes
read_dir .: 2 entries
  a dir
  link symlink
read_dir a: 3 entries
  b dir
  empty file
  new file
canonicalize a/b/../b/./c/file: {}/a/b/c/file
read missing: NotFound (No such file or directory)
read a: IsADirectory (Is a directory)
write a: IsADirectory (Is a directory)
create_dir_all a/empty: AlreadyExists (File exists)
create_dir_all a/empty/x: NotADirectory (Not a directory)
metadata missing: NotFound (No such file or directory)
read_dir a/empty: NotADirectory (Not a directory)
read a\0: InvalidInput (Invalid argument)"#,
            canonical.display()
        );
        assert_eq!(lines.join("\n"), expected, "O{level}");
        assert_eq!(fs::read(dir.join("a/b/c/file")).unwrap(), b"hello world");
    }
}

#[test]
fn native_process_runs_programs_and_waits_for_them() {
    let temp = tempdir().unwrap();
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native/process.fe");
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let dir = temp.path().join(format!("scratch-{level}"));
        fs::create_dir(&dir).unwrap();
        let result = Command::new(out.join("process"))
            .arg(&dir)
            .output()
            .unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
        assert_eq!(
            String::from_utf8(result.stdout).unwrap(),
            r#"echo: exit status 0 (success) [hello 42
] []
exit 3: exit status 3 [out
] [err
]
merged: exit status 0 (success) [a
b
c
] []
killed: signal 9 [] []
null input: exit status 0 (success) [] []
null output: exit status 0 (success) [] []
env: exit status 0 (success) [a b unset
] []
dir: exit status 0, the directory: true
file: exit status 0 (success) [] []
file holds: to file

file in dir: exit status 0 (success) [] []
file in dir holds: in dir

cat: exit status 0, 1048576 bytes back, same: true
both streams: exit status 0, 300000 and 200000 bytes
input not taken: exit status 0 (success) [] []
missing program: NotFound (No such file or directory)
missing dir: NotFound (No such file or directory)
stdout to stdout: InvalidInput (Invalid argument)
nul argument: InvalidInput (Invalid argument)
key with =: InvalidInput (Invalid argument)
empty key: InvalidInput (Invalid argument)
timeout: timed out [] []
timeout with pipes: timed out [] []
  ended within 2 s: true
exited before the wait: exit status 0 (success) [hello
] []
killed by the wait: timed out [partial
] []
endless writer: timed out
  ended within 2 s: true
busy: exit status 0, used CPU: true, used memory: true
wait_any: fast exit status 0 [fast
]
wait_any: middle exit status 0 []
wait_any: slow exit status 0 [slow
]
"#,
            "O{level}"
        );
        assert_eq!(fs::read(dir.join("log")).unwrap(), b"to file\n");
    }
}

// A pipe made while a standard stream is closed takes its number. The child
// must still see the stream closed: reading its stdin must not reach its own
// stdout pipe, which would never end. Inheriting a closed stream is no error.
#[test]
fn native_process_pipes_avoid_closed_standard_streams() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("closed.fe");
    fs::write(
        &source,
        r#"
use std::process::{Command, Redirect, Spawn}
use std::time::Duration

pub fn main() -> i32 uses (proc: mut Spawn) {
    let command = Command::new("sh")
        .arg("-c")
        .arg("read x; echo done")
        .stdout(Redirect::Capture)
        .timeout(Duration::from_secs(5))
    match proc.output(command) {
        Result::Ok(output) => {
            var expected = format!("done")
            expected.push(10)
            if output.status.success() && output.stdout.span() == expected.span() { 0 } else { 1 }
        }
        Result::Err(_) => 2,
    }
}
"#,
    )
    .unwrap();
    let out = temp.path().join("out");
    build(&source, &out, "0", &[]);
    for closed in ["<&-", ">&-", "2>&-", "<&- >&-", "<&- >&- 2>&-"] {
        let result = Command::new("sh")
            .arg("-c")
            .arg(format!("exec \"$0\" {closed}"))
            .arg(out.join("closed"))
            .output()
            .unwrap();
        assert!(result.status.success(), "{closed}: {result:?}");
    }
}

#[test]
fn native_clock_reads_wall_and_monotonic_time() {
    let temp = tempdir().unwrap();
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/native/time.fe");
    let unix_secs = || {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs()
    };
    for level in ["0", "1", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let before = unix_secs();
        let result = Command::new(out.join("time")).output().unwrap();
        let after = unix_secs();
        assert!(result.status.success(), "O{level}: {result:?}");
        let stdout = String::from_utf8(result.stdout).unwrap();
        let (now, rest) = stdout
            .strip_prefix("now ")
            .unwrap()
            .split_once('\n')
            .unwrap();
        let now = now.parse::<u64>().unwrap();
        assert!(
            (before..=after).contains(&now),
            "O{level}: {now} outside {before}..={after}"
        );
        assert_eq!(
            rest,
            "spun 20 ms: true\nsince a later reading: 0\n2 s, 2499 ms, 2499750 us, 2499750000 ns\ntrue true\n",
            "O{level}"
        );
    }
}

#[test]
fn native_exports_replace_the_symbols_native_code_imports() {
    // Each export leaves a mark, which shows that native code called it: the
    // allocator of `core::rt`, the backend's copy, and a Fe `extern`.
    let temp = tempdir().unwrap();
    let source = temp.path().join("replaced.fe");
    fs::write(
        &source,
        r#"
use core::ptr
use std::env::Args

extern {
    fn calloc(_ count: usize, _ size: usize) -> *u8
}

mod host {
    extern {
        pub fn shared(_ x: i64) -> i64
    }
}

#[unsafe(export)]
pub fn malloc(_ size: usize) -> *u8 {
    let bytes = unsafe { calloc(1, size) }
    unsafe { *bytes = 77 }
    bytes
}

#[unsafe(export)]
pub fn memmove(_ dest: *u8, _ src: *u8, _ len: usize) -> *u8 {
    var i: usize = 0
    while i < len {
        unsafe { *ptr::offset_bytes(dest, i) = *ptr::offset_bytes(src, i) }
        i += 1
    }
    unsafe { *dest = 99 }
    dest
}

#[unsafe(export)]
pub fn shared(_ x: i64) -> i64 {
    x + 1
}

pub fn main() -> i32 uses (args: Args) {
    let len = 200 * args.count()
    let source = ptr::alloc_bytes(len)
    if unsafe { *source } != 77 {
        return 1
    }
    let dest = ptr::alloc_bytes(len)
    unsafe {
        ptr::zero_bytes(source, len)
        *ptr::offset_bytes(source, len - 1) = 7
        ptr::copy_raw(dest, source, len)
        if *dest != 99 || *ptr::offset_bytes(dest, len - 1) != 7 {
            return 2
        }
        if host::shared(41) != 42 {
            return 3
        }
    }
    0
}
"#,
    )
    .unwrap();
    for level in ["0", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("replaced")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}

#[test]
fn native_selector_store_stays_in_its_allocation() {
    // Canary bytes past every allocation catch a selector word stored past
    // the encoding of a message with no fields.
    let temp = tempdir().unwrap();
    let source = temp.path().join("selector.fe");
    fs::write(
        &source,
        r#"
use std::abi::Sol
use core::error::ErrorVariant
use core::ptr
use std::evm::encode_error

extern {
    fn calloc(_ count: usize, _ size: usize) -> *u8
}

#[error]
pub struct Empty {}

#[unsafe(export)]
pub fn malloc(_ size: usize) -> *u8 {
    let bytes = unsafe { calloc(1, size + 32) }
    for i in 0..32 {
        unsafe { *ptr::offset_bytes(bytes, size + i) = 0xa5 }
    }
    bytes
}

pub fn main() -> i32 {
    let encoded = encode_error(Empty {})
    if encoded.len() != 4 || encoded.read_be<u32>(0) != <Empty as ErrorVariant<Sol>>::SELECTOR {
        return 1
    }
    for i in 0..32 {
        if unsafe { *ptr::offset_bytes(encoded.ptr(), encoded.capacity() + i) } != 0xa5 {
            return 2
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
        let result = Command::new(out.join("selector")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}

#[test]
fn native_unwrap_panics_on_errors_without_an_encoding() {
    // A native panic needs no revert data, so `unwrap` takes any error type.
    // Arguments choose the outcome, which keeps it from folding.
    let temp = tempdir().unwrap();
    let source = temp.path().join("unwrap.fe");
    fs::write(
        &source,
        r#"
use std::env::Args
use std::fs::Fs

enum PlainError { Oops }

fn fallible(_ ok: bool) -> Result<PlainError, i32> {
    if ok { Result::Ok(0) } else { Result::Err(PlainError::Oops) }
}

pub fn main() -> i32 uses (args: Args, fs: mut Fs) {
    if args.count() == 3 {
        let _ = fs.read("/no/such/file").unwrap()
    }
    fallible(args.count() == 1).unwrap()
}
"#,
    )
    .unwrap();
    for level in ["0", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let exe = out.join("unwrap");
        let ok = Command::new(&exe).output().unwrap();
        assert!(ok.status.success(), "O{level}: {ok:?}");
        for args in [&["plain"][..], &["io", "error"]] {
            let result = Command::new(&exe).args(args).output().unwrap();
            assert_eq!(
                result.status.signal(),
                Some(libc::SIGABRT),
                "O{level} {args:?}: {result:?}"
            );
            assert_eq!(
                result.stderr, b"panicked\n",
                "O{level} {args:?}: {result:?}"
            );
        }
    }
}

#[test]
fn native_main_without_a_result_exits_with_zero() {
    let temp = tempdir().unwrap();
    let source = temp.path().join("greet.fe");
    fs::write(
        &source,
        r#"
use std::env::Args
use std::io::Stdout

pub fn main() uses (out: mut Stdout, args: Args) {
    println!("args: ", args.count())
}
"#,
    )
    .unwrap();
    for level in ["0", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("greet")).arg("x").output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
        assert_eq!(result.stdout, b"args: 2\n", "O{level}: {result:?}");
    }
}

#[test]
fn native_functions_named_like_the_compilers_symbols_keep_working() {
    // Functions share an object namespace with the globals behind `std::env`.
    let temp = tempdir().unwrap();
    let source = temp.path().join("names.fe");
    fs::write(
        &source,
        r#"
use std::env::Args

extern {
    fn __fe_native_environ() -> **u8
}

fn __fe_native_argc() -> i32 { 10 }
fn __fe_native_argv() -> i32 { 20 }
fn __fe_argc() -> i32 { 1 }
fn __fe_argv() -> i32 { 2 }
fn environ() -> i32 { 3 }

pub fn main() -> i32 uses (args: Args) {
    let sum = __fe_native_argc() + __fe_native_argv() + __fe_argc() + __fe_argv() + environ()
    let count: i32 = args.count().downcast_unchecked()
    let has_environ = unsafe { __fe_native_environ() } as usize != 0
    if sum == 36 && count == 1 && args.get(0).len() > 0 && has_environ { 0 } else { 1 }
}
"#,
    )
    .unwrap();
    for level in ["0", "2"] {
        let out = temp.path().join(format!("out-{level}"));
        build(&source, &out, level, &[]);
        let result = Command::new(out.join("names")).output().unwrap();
        assert!(result.status.success(), "O{level}: {result:?}");
    }
}
