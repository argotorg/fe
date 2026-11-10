# Fe on WebAssembly

`fe build --backend wasm` compiles a standalone Fe file or an ingot to a WebAssembly
module. Every public function in the root of the file becomes an export that a host (a
JavaScript program in Node.js or a browser, or any other WebAssembly runtime)
can call. The module may import small host operations (logging, a clock, abort,
the standard streams, read-only files), takes text from the host, and returns a
number or a tagged reply. `fe build` also writes a manifest that describes the module
and its hash, and `fe test --backend wasm` runs Fe tests as WebAssembly modules in
Node.js.

This directory holds the pieces a host needs and the scripts that check them:

| File | Purpose |
| --- | --- |
| `host.mjs` | A host for Node.js with no dependencies: supplies the imports, places text, decodes replies, checks a module against its manifest. |
| `host.test.mjs` | Tests of the host (`node --test`). |
| `verify.mjs` | Checks a module against its manifest from the command line. |
| `lines.fe`, `lines.mjs` | A small program and a script that runs it. |
| `tests.fe` | Example tests for `fe test --backend wasm`. |
| `reproducible.sh` | Builds files twice in different surroundings and compares the bytes. |
| `check.sh` | Runs every check below. |

## Requirements

- A Fe compiler built with the `wasm` feature, as the released binaries are. The native backend (`cranelift`) is
  needed only to compare `fe test` results with the host-native backend.

  ```sh
  cargo build --release -p fe --features wasm,cranelift
  ```

  In the commands below, `fe` stands for `target/release/fe`.
- Node.js 20 or later, to run modules, the tests and the checks. Version 22 was
  used. The host itself needs only `WebAssembly`, `BigInt`, `TextEncoder`,
  `TextDecoder` and `crypto.subtle`, but it has not been run in a browser.
- `wasm-tools`, to validate modules independently of the engine that runs them
  (optional; `check.sh` needs it). Version 1.247.0 was used.

The compiler emits the WebAssembly features multi-value, sign extension, for
programs that copy memory, bulk memory, and for recursive functions that end in a call,
tail calls. Every current engine supports them.

## Build and run a program

`lines.fe` counts the lines of a file that the host provides:

```sh
fe build --backend wasm --out-dir out examples/wasm/lines.fe
node examples/wasm/lines.mjs out/lines.wasm examples/wasm/README.md
```

The first command writes `out/lines.wasm` and `out/lines.manifest.json`; the
second prints the number of lines of the file named on its command line.
`--emit wasm,ir` also writes the optimized Sonatina IR as `out/lines.wasm.sona`
(`--emit ir` writes only the IR). The optimization level is chosen with
`-O 0`, `-O 1` (the default), `-O 2` or `-O s`.

Validate the module and check it against its manifest:

```sh
wasm-tools validate out/lines.wasm
node examples/wasm/verify.mjs out/lines.wasm
```

`fe build` also takes an ingot directory, and makes one module named after the ingot
from the root module `src/lib.fe` and what it calls in the ingot's other modules and
its dependencies. For a workspace it makes one module for each member whose root
module has an export, or for the member that `--ingot` names. `--contract` is not
supported. `target = "wasm"` in `fe.toml` selects this backend for `fe build`,
`fe check` and `fe test`.

### What a program may export

Every non-generic `pub fn` in the root of the file, or of an ingot's `src/lib.fe`, is
exported under its own name, and the module always exports its linear memory as
`memory`. `#[unsafe(export)]` is an error on this backend. Parameters and results are
limited to:

- `bool` and the integers `u8` to `u64` and `i8` to `i64`;
- parameters of type `std::wasm::HostText`, text that the host placed in the
  module's memory (two `i32` values, the address and the length in bytes, valid
  for the call only);
- a result of type `std::wasm::Reply`, one `i64` that holds a tag in the low 32
  bits and a payload in the high 32 bits (tag 0 is success with a value; any
  other tag is an error number of the contract and the payload is 0).

`usize` and `isize` have 32 bits in a module and cross as an `i32`. Any other
type, including `u128`, `u256`, structs and pointers, is an error that names the
function and the parameter. An export may declare the capabilities of
`std::wasm::Host` in `uses`, as `lines.fe` does; a provider of anything else is chosen
inside the function with `with`. The capabilities include the streams of `std::io`,
so an export that declares `uses (out: mut Stdout)` prints with `print!` and
`println!` (see [Printing](#printing)).
Inside functions, structs, tuples, enums, arrays, constants, `usize` (32 bits, the
width of an address in a module) and 128-bit and 256-bit integers work. Overflow, division by
zero, a failed `assert!` and an index outside an array trap (the WebAssembly
instruction `unreachable`).

### What a program may import

An `extern` function that an export reaches becomes an import of the module
`env`, under the function's name. Calling it needs an `unsafe` block, as for every
foreign function outside `core` and `std`. Its parameters and result follow the
same rule as for exports, without `HostText` and `Reply`. `bool` and integers of up to 32
bits arrive as an `i32` that holds the value zero-extended (an `i8` of -1 arrives
as 255); 64-bit integers are `i64`. The module `std::wasm` declares the standard
imports as effects. The root effect, `std::wasm::Host`, calls the imports: an export
or a test takes its capabilities in `uses`, and nothing else can create one. `mock()`
is a provider written in Fe that needs no host:

| Import | Parameters | Result | Effect |
| --- | --- | --- | --- |
| `sys_wasm_log_i32` | `i32` | none | `Log` |
| `sys_wasm_log_u64` | `i64` | none | `Log` |
| `sys_wasm_now_ms` | none | `i64` (milliseconds since the Unix epoch) | `Clock` |
| `sys_wasm_abort` | `i32` (a code) | never returns | `Abort` |
| `sys_wasm_stream_write` | stream `i32`, address `i32`, length `i32` | count `i32` | `std::io::Stdout`, `Stderr` |
| `sys_wasm_stream_read` | stream `i32`, address `i32`, length `i32` | count `i32` | `std::io::Stdin` |
| `sys_wasm_files_open` | name address `i32`, name length `i32` | reply `i64` | `Files` |
| `sys_wasm_files_size` | handle `i32` | reply `i64` | `Files` |
| `sys_wasm_files_read_byte` | handle `i32`, index `i32` | reply `i64` | `Files` |
| `sys_wasm_files_close` | handle `i32` | none | `Files` |

An import that nothing reachable calls is not in the module.

### Printing

`std::wasm::Host` provides the streams of `std::io`: `Stdout`, `Stderr`, `Stdin`, and
`Stdio` for all three. An export or a test that declares one in `uses` prints with
`print!`, `println!`, `eprint!` and `eprintln!` and reads with `std::io::StdinReader`,
and the same source builds for the native target, whose root effect provides the same
streams:

```fe
use std::io::Stdout

pub fn greet(times: u32) uses (out: mut Stdout) {
    println!("hello, ", times, " times")
}
```

Each print is one call of `sys_wasm_stream_write`, or more if the host takes the text
in pieces. `host.mjs` keeps what a module wrote as text in `host.stdout` and
`host.stderr`, and passes each write to `onWrite` if the option is given. The input is
the option `stdin`, a string or a `Uint8Array`, and is empty without it:

```js
const guest = await instantiate(bytes, {
  stdin: "one\ntwo\n",
  onWrite: ({ stream, bytes }) => process[stream].write(bytes),
});
guest.call("greet", 3);
console.log(guest.host.stdout); // "hello, 3 times\n"
```

The other capabilities of the native root effect (`Args`, `Env`, `Fs`, `Exit`, `Spawn`
and the clocks of `std::time`) have no provider on this target.

## The host contract in brief

- **Text in.** A module with a `HostText` parameter exports `alloc(size) -> address`,
  `arena_mark() -> address` and `arena_release(mark)`. The host takes a mark, places
  each text with `alloc` and writes its bytes there, calls the export, reads what
  it needs, and releases the mark. The three names are reserved. `Guest.call` in
  `host.mjs` does this for every argument that is a string or a `Uint8Array`.
- **Replies.** `Reply` is the `i64` described above. The error numbers of the files
  contract are `NotFound` 1, `OutOfRange` 2 and `BadName` 3; a number outside the
  set is a fault, and the host refuses it.
- **Streams.** A stream has the number of its POSIX descriptor: 0 is standard input, 1
  standard output and 2 standard error. `sys_wasm_stream_write` offers the host the
  `length` bytes at `address`; the host takes all of them or only the first ones and
  returns how many, and the module offers the rest again. `sys_wasm_stream_read` asks
  the host to place at most `length` bytes of input at `address`; the host returns how
  many it placed, and 0 at the end of the input. A count that is negative as a signed
  `i32` says that the stream failed, and so does a count of 0 for a write of one byte or
  more; `print!` and its relatives and the readers of `std::io` then trap, as they
  panic on the native target. The range is valid for the call only. `host.mjs` takes or
  places at most 65,536 bytes in a call and never reports a failure.
- **Handles.** A handle is one opaque word that the host chooses. In safe Fe a
  `Handle` cannot be made or copied, and `close` takes it by value, so using it
  after `close` is a compile error. Dropping a handle without closing it is
  accepted by the compiler; the host counts open handles (`host.openHandles`,
  `host.assertNoOpenHandles()`).
- **What the host checks.** It ends a call with a `ContractError` that names the
  problem when a module passes a handle that was never issued or is closed, text
  outside the memory or longer than 65,536 bytes, an unknown error number, a range of
  a stream outside the memory, or a stream number that the operation does not apply
  to (a write to 0, a read of 1 or 2, any other number).
- **Traps and errors.** A trap in the module, or an error thrown by an import,
  reaches the caller of the export unchanged. `sys_wasm_abort` throws a
  `GuestAbort` in `host.mjs`.
- **The stack after an error.** A module that has imports exports the global
  `stack_top`, a mutable `i32` that holds where the next call places its first frame. A
  call to an import moves it and puts it back on return, so an import that throws leaves
  it above the frames of the calls it ended. A host reads the global before it calls an
  export and writes that value back if the call ends with an error; `Guest.call` does.
  The name is reserved.
- **Contract version.** The version of this contract is 2. It appears in every
  manifest as `contract_version`.

The contract is defined in `ingots/std/src/wasm.fe`, `ingots/std/src/wasm/boundary.fe`,
`ingots/std/src/wasm/files.fe` and `ingots/std/src/wasm/streams.fe`.

## The build manifest

Whenever `fe build` writes a module it writes `<name>.manifest.json` next to it. For
`lines.fe` (abbreviated):

```json
{
  "manifest_version": 1,
  "module": {
    "file": "lines.wasm",
    "sha256": "…64 hexadecimal digits…",
    "size": 7820,
    "initial_memory_pages": 18
  },
  "contract_version": 2,
  "optimization": "1",
  "compiler": "fe 26.4.0 (…)",
  "imports": [
    { "module": "env", "name": "sys_wasm_files_open", "kind": "function",
      "type": { "params": ["i32", "i32"], "results": ["i64"] } }
  ],
  "exports": [
    { "name": "lines", "kind": "function",
      "params": [{ "name": "name", "type": "HostText", "wasm": ["i32", "i32"] }],
      "result": { "type": "Reply", "wasm": ["i64"] } },
    { "name": "memory", "kind": "memory" }
  ]
}
```

- `module.sha256` is the SHA-256 of the `.wasm` file, as `sha256sum` prints it.
- `imports` lists what the host must supply, in the order of the module's import
  section, with WebAssembly types.
- `exports` is in name order. A function has the Fe parameter names and types, the
  WebAssembly types they become (`wasm`), and a `result` (`null` for none). The
  three allocator functions have `"role": "allocator"`, and the global `stack_top` of a
  module with imports has `"kind": "global"` and `"role": "stack"`.
- `contract_version` is the version of the host contract; `manifest_version` is the
  version of this format.
- `compiler` is the output of `fe --version`.

The manifest is built from the bytes of the module, so it cannot disagree with them;
the compiler refuses to write a manifest that does not match the module. It holds
nothing that depends on the machine, the time or the path of the sources.

### How a host uses the manifest

Before running a module a host should read the manifest and check that:

1. `manifest_version` and `contract_version` are versions it implements;
2. the SHA-256 of the module it is about to run equals `module.sha256`;
3. it provides every entry of `imports` (module, name, and a function of that type);
4. it calls only the entries of `exports`, with arguments of the listed types.

`host.mjs` does all four:

```js
import fs from "node:fs";
import { instantiate, parseManifest } from "./examples/wasm/host.mjs";

const manifest = parseManifest(fs.readFileSync("out/lines.manifest.json", "utf8"));
const guest = await instantiate(fs.readFileSync("out/lines.wasm"), {
  manifest,
  files: { "notes.txt": "one\ntwo\n" },
});
console.log(guest.callReply("lines", "notes.txt")); // { ok: true, value: 2 }
guest.host.assertNoOpenHandles();
```

`instantiate` throws a `ContractError` before the module runs if a check fails, with
a message that names the cause, for example `the module imports env.custom_import,
which the host does not provide`, `the module has SHA-256 … but the manifest
describes …`, or `argument `name` of `lines` must be text`. Without a manifest it
still refuses a module whose imports it does not provide. Extra imports are passed
as `instantiate(bytes, { imports: { env: { custom_import(x) { … } } } })`.

## Tests: `fe test --backend wasm`

```sh
fe test --backend wasm examples/wasm/tests.fe
fe test --backend native examples/wasm/tests.fe
```

The `#[test]` functions of a file or an ingot are built into one module, and each runs
in its own Node.js process with `host.mjs`. A test passes if it returns. The same file gives the same
results on the native backend (the second command, on x86-64 Linux or AArch64
macOS).

- A failed assertion, an overflow, a division by zero, an index outside an array and
  a revert with data reach `fe test` as revert data, as from a native test. The test
  fails with `Test reverted: 0x…` and what the data says, such as `(arithmetic
  overflow)`, unless it is a `#[test(should_revert)]` test that expected it. A panic
  without data, `core::panic()`, fails the test with `trap: unreachable`. A module
  built with `fe build` traps in every case: only a test passes the data to its host.
- A call of `abort` fails the test with `the test aborted with code N`. A call that
  breaks the host contract fails it with the host's message.
- The test host answers `Log`, `Clock`, `Abort` and the streams; it has no files, so
  `open` returns `NotFound`, and its standard input is empty. Providers written in Fe
  (`mock()`) work as usual.
- A failed test shows the numbers it logged and what it wrote to standard output and
  standard error, after `logged:`. A test that passes shows nothing, as on the native
  backend.
- A test that uses something this backend cannot build fails with `cannot build
  <name> for WebAssembly: unsupported: …` and the reason (the `balance`
  attribute is not supported). No test is skipped silently, and a green run
  means that every selected test ran.
- A missing or too old `node` ends the run with an error that names the
  requirement.
- A test that runs for more than 60 seconds fails, and so does one that writes more
  than 1 MiB to its standard output and standard error together.
  `--native-timeout-secs <seconds>` and `--native-output-limit-kib <kib>` set these
  limits, as they do for native tests.
- A test that calls an `extern` function of its own gets a module of its own, since the
  test host does not supply the import and a module that imports it does not start.
- `--filter`, `--jobs`, `-O`, `--emit ir` (one `.sona` file and one module per test) and ingot
  paths work as for the other backends. `--show-logs`, the tracing options,
  `--debug-dir`, `--emit rmir`, `--report` and `--report-dir` are errors.

## Reproducibility

Two builds of one program with one compiler give identical bytes for the module, its
IR and its manifest, at every optimization level. The result does not depend on the
working directory, the absolute path of the source or its modification time, the
output directory, the environment variables, the time zone, the locale, the time of
day, the order of parallel builds or address space randomization.

```sh
examples/wasm/reproducible.sh fe examples/wasm/lines.fe crates/fe/tests/fixtures/wasm/*.fe
```

builds each file at the levels 0, 1, 2 and s in two different surroundings and prints
a line for every difference; it exits with status 1 if there is one. The test
`crates/fe/tests/wasm_reproducible.rs` runs the same comparison under `cargo test`.
Different optimization levels can give different bytes, and a different compiler
version gives a different manifest (the `compiler` field).

## Checking everything

```sh
examples/wasm/check.sh target/release/fe
```

builds the test programs and examples at two levels, validates every module with
`wasm-tools`, verifies each against its manifest, checks that a module with an
unsupplied import is refused by name, runs the reproducibility check, the host
tests and `fe test --backend wasm` (and `--backend native` where available). The
workflow `.github/workflows/wasm.yml` runs this script and the WebAssembly tests of
the compiler:

```sh
cargo test -p fe --all-features \
  --test wasm_backend --test wasm_memory --test wasm_imports --test wasm_files \
  --test wasm_manifest --test wasm_ingot --test wasm_reproducible \
  --test wasm_test_command
```

## Known limits

- **Types.** Floats do not exist in Fe yet. 128-bit and 256-bit integers work inside
  functions (as 64-bit limbs) but cannot cross the boundary. Exports cannot return
  text or aggregates.
- **Programs.** Every root `pub fn` of an ingot is an export, so an ingot that is a
  library for other Fe code is a module only if those functions fit the boundary. Contracts
  and EVM features are not available: an EVM-only instruction is an error that names it.
  `fe check` and the language server do not know the export rule.
- **Imports.** The module is always `env`; no attribute names another module. Imports
  take scalars only, so the compiler does not check that an address and a length
  passed to an import describe a text; the host does.
- **Handles are affine, not linear.** The compiler rejects use after `close` but accepts
  a handle that is dropped; the leak shows only in the host's counter.
- **Unsafe Fe can forge handles.** A program can declare the four `extern` functions of
  the files contract itself and call them in an `unsafe` block with made-up numbers. The host refuses
  every forged or closed handle, so the protection is the host's check, not the type.
- **Streams.** The stream traits are sealed: only the root effects provide them, so
  there is no provider written in Fe that captures what a function prints. Output is
  not buffered in the module. `host.mjs` keeps everything a module wrote for the life
  of the instance.
- **Errors thrown inside an import.** When an import throws, or a callback traps and the
  host passes the error on, the module does not run again before the host call ends, so
  the frames of the calls that were active stay reserved until the host writes
  `stack_top` back, as `host.mjs` does. A host that does not loses that much of the stack
  for the life of the instance. A trap that happens inside the module, with no host call
  before it, reserves nothing.
- **Memory.** Calls use a stack of 1 MiB in the module's memory and a call that does
  not fit traps. `fe build --wasm-stack-size <bytes>` sets the size, which is rounded up
  so that the stack ends on a 64 KiB page boundary. Dynamic allocation comes from a heap
  of size classes with free lists: a block that is freed is reused, a fallible
  allocation returns `None` when the request cannot be met, any other allocation
  traps, and the memory grows with `memory.grow` and is never given back.
  `arena_release` frees what a call allocated. A module with text
  parameters starts with 18 pages (about 1.2 MB). The host limits a text to 65,536
  bytes, and takes or places at most as many bytes of a stream in one call, a limit of
  `host.mjs` and not of the compiler. A file has at most
  4,294,967,295 bytes, because a reply carries its size in 32 bits.
- **Traps.** The engine names the functions in the stack of a trap, from the module's
  name section, but no source location, and the message of a failed assertion is lost.
  A function that ends in a call of a recursive cycle makes a tail call, so it is not in
  that stack.
- **Tests.** Each test runs in a separate process, so tests share nothing. The test host
  has no files and an empty standard input, and the output of a test that passes is not
  shown.
- **Platforms.** CI runs the compiler's WebAssembly tests on Linux and Windows, and
  on macOS on `master`; `check.sh` runs on Linux. Browsers were not run.
