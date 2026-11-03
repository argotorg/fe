# Fe SP1 guests

Build Fe with `cargo build --locked --release -p fe --features sp1`. SP1 is
optional; the compiler workspace does not depend on the prover SDK.

The target is **RV64IM**, `riscv64im-succinct-zkvm-elf`, using SP1 **6.8.0**.
A guest is a standalone `.fe` file with `pub fn main() -> i32`. The runtime
uses the low eight bits of the return value as the exit code; zero means
success. Checked arithmetic traps on overflow; normal Fe arithmetic
attributes also apply to guests.

Host heap allocation, including `core::ptr::alloc_bytes`, is unsupported by the
pinned SP1 backend and is rejected during machine-code generation.
`fe test` supports EVM and host-native execution; run SP1 guests with the SDK
integration harness described below.

## Toolchain and build

ELF linking requires the pinned Succinct toolchain
`succinct-1.96.0-64bit-v2` on x86-64 Linux or AArch64 macOS. Install it explicitly
using Sonatina's checksum-verifying installer; Fe does not install toolchains or
modify rustup aliases:

```sh
git clone https://github.com/fe-lang/sonatina.git /tmp/fe-sp1-sonatina
git -C /tmp/fe-sp1-sonatina checkout b21df8e613821e4a85d4c2f04f6cf4f577b51f48
bash /tmp/fe-sp1-sonatina/crates/sp1/install-toolchain.sh /tmp/fe-sp1-toolchain
export SONATINA_SP1_TOOLCHAIN=/tmp/fe-sp1-toolchain

target/release/fe build --backend sp1 --emit elf,ir examples/sp1_increment.fe
```

This writes `examples/out/sp1_increment.elf` and `examples/out/sp1_increment.sp1.sona`.
The default emit for SP1 is `elf`; `--emit ir` needs no Succinct toolchain.
Linking uses Sonatina's embedded, locked SP1 runtime, which supplies startup,
public-value digest finalization, and halt handling. Fe builds it once and
reuses that build from `sp1-runtime` in Fe's cache directory (`$FE_CACHE_DIR`,
else the user cache directory, e.g. `~/.cache/fe`). The first build can require
network access to populate Cargo's dependency cache.

## Input and public values

[`examples/sp1_increment.fe`](../../examples/sp1_increment.fe) reads a `u32`
and a `u64`, commits the checked increment of the former, and echoes the latter.
The `std::sp1` module provides `ProverInput` and `PublicValues` effects, backed by
`sp1()` guest providers.

Each `read_u32()` or `read_u64()` consumes one separate input hint containing
exactly four or eight little-endian bytes. Host code must use raw hints:

```rust
let mut stdin = SP1Stdin::new();
stdin.write_slice(&41u32.to_le_bytes());
stdin.write_slice(&u64::MAX.to_le_bytes());
```

`commit_u32()` and `commit_u64()` append raw little-endian bytes to public values.
For this input, the result is the four bytes of `42u32` followed by the eight
bytes of `u64::MAX`. These are not SDK-serialized values. Missing or incorrectly
sized hints cause an unsuccessful guest exit. Check the executor report's
`exit_code` as well as execution errors, and require successful proof verification.

## Integration tests

The separate workspace pins `sp1-sdk = 6.8.0`. Building the SDK needs the
Protocol Buffers compiler, `protoc`, on `PATH` or named by `PROTOC`. With the
toolchain installed:

```sh
export FE_SP1_BINARY="$PWD/target/release/fe"
cargo nextest r --locked --release --no-fail-fast --manifest-path tests/sp1/Cargo.toml
cargo nextest r --locked --release --no-fail-fast --manifest-path tests/sp1/Cargo.toml --run-ignored all
```

The second command also generates and verifies a CPU core proof and checks that
overflow and missing input cannot verify as successful execution. The regular
tests execute O0/O2 guests, including scalar I/O and integer arithmetic through
256 bits.

The main CI builds and tests the SP1 compiler path on every pull request. The
`SP1` workflow executes the guests on Linux after each push to master. Dispatch
it manually with `prove` to also prove and verify, e.g. when changing the SP1
SDK, the Succinct toolchain, or the Sonatina pin.
