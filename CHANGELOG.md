# Changelog

[//]: # (towncrier release notes start)
## 26.1.0 (2026-04-30)

### Features

- Update the core ABI helper traits and APIs. Custom ABI implementations now use `AbiSize::HEAD_SIZE`, `payload_size`, `Decode::decode_payload`, and `AbiSpan::payload_end`; encoders are created with a fixed output size, and root versus field encoding is split into explicit helpers such as `encode_alloc` and `encode_single_root_alloc`. ([#1404-abi](https://github.com/argotorg/fe/issues/1404-abi))
- Added `assert_msg(cond, message)`, which reverts with a Solidity-compatible `Error(string)` payload (selector `0x08c379a0`) when `cond` is false. ([#1348](https://github.com/argotorg/fe/issues/1348))
- Replace the git resolver backend from git2 (libgit2) to gitoxide (pure Rust). This removes the OpenSSL/libgit2 C dependency and adds sparse checkout support, allowing the resolver to fetch only the required subdirectory of a remote repository instead of the entire tree. ([#1383](https://github.com/argotorg/fe/issues/1383))
- Expose missing `Ctx` trait APIs: `origin`, `coinbase`, `prevrandao`, `gaslimit`, `chainid`, `basefee`, `selfbalance`, and `blockhash`. ([#1388](https://github.com/argotorg/fe/issues/1388))
- Add `#[error]` attribute for Solidity-compatible custom error types. Structs annotated with `#[error]` get auto-generated `ErrorVariant<Sol>`, `AbiSize`, and `Encode<Sol>` implementations with a compile-time computed 4-byte selector. A new `revert_error()` function emits selector-prefixed ABI-encoded revert data. Includes a predefined `Panic` error type matching Solidity's `Panic(uint256)`. ([#1395](https://github.com/argotorg/fe/issues/1395))
- Improve `fe doc` and generated documentation bundles: message declarations and message variants now render with dedicated item kinds and complete signatures, `fe doc --builtins --stdlib-path <path>` can document a stdlib loaded from disk, docs JSON includes pre-rendered Markdown HTML, and the web viewer can load gzipped docs JSON bundles. ([#1401](https://github.com/argotorg/fe/issues/1401))
- Replace backend lowering with the staged SMIR/NSMIR/MIR pipeline. This expands compile-time function evaluation, including const calls that produce aggregate values and symbolic array repeat expressions such as `[value; N]` where `N` is a const generic. ([#1404](https://github.com/argotorg/fe/issues/1404))
- Add `static_assert(bool_expr)` for compile-time assertions, with diagnostics that show evaluated comparison operands and operators when an assertion fails. ([#1412](https://github.com/argotorg/fe/issues/1412))
- Expand `const fn` evaluation to support mutable locals, assignments, aggregate field and index writes, `while` and `while let` loops with `break` and `continue`, match/destructuring patterns, and const operator trait implementations. This allows more ordinary helper code, including array and proof builders, to run during CTFE. ([#1413](https://github.com/argotorg/fe/issues/1413))
- The standard prelude now includes `sol`, `Bytes`, `Decode`, and `AbiDecoder`, so common Solidity selector and ABI decoding code no longer needs explicit imports. ([#1417](https://github.com/argotorg/fe/issues/1417))
- Checked arithmetic overflow now reverts with a Solidity-compatible `Panic(uint256)` payload (code `0x11`) instead of empty revert data. This makes overflow failures identifiable by off-chain tooling such as Foundry, Hardhat, and block explorers.
- Extend `#[test(should_revert)]` with `panic` and `selector` arguments for verifying revert payloads. `#[test(should_revert, panic = 0x11)]` checks that the test reverts with a Solidity-compatible `Panic(uint256)` and the expected code. `#[test(should_revert, selector = 0x4e487b71)]` checks only the 4-byte error selector.
- Make primitive numeric and boolean intrinsics, `IntDowncast` methods, and EVM `addmod`/`mulmod` const-evaluable. This enables modular arithmetic and field-arithmetic-heavy code to be used in `const fn` and `static_assert`.
- `Result::unwrap()` now emits selector-prefixed revert data for `#[error]` types. Previously, `unwrap()` on a `Result<E, T>` where `E` is an `#[error]` type would ABI-encode the error without the 4-byte selector. Now the monomorphizer routes these to `revert_error()` instead of `revert()`, producing Solidity-compatible error payloads.
- `assert(false)` now reverts with a Solidity-compatible `Panic(uint256)` payload (code `0x01`) instead of empty revert data. This makes assertion failures identifiable by off-chain tooling.

### Bugfixes

- Fix several array and aggregate codegen bugs. Readonly array locals, call results, constructor arguments, and view parameters now preserve their code-backed or borrowed representation until materialization is required. Code-backed arrays copied to storage are staged through memory before word loads and use storage slot offsets instead of byte offsets. Runtime array literals now populate each element before loading the aggregate value. ([#1404-array-codegen](https://github.com/argotorg/fe/issues/1404-array-codegen))
- Fix never type (`!`) handling in trait checking and lowering. The compiler now avoids probing trait implementations for bare `!`, producing the intended diagnostic for invalid uses, and treats extern functions declared `-> !` as intrinsically non-returning even when they appear in functions with generic or associated return types. ([#1410-never-type](https://github.com/argotorg/fe/issues/1410-never-type))
- Suppress downstream type mismatch diagnostics when the underlying type is already invalid. This reduces cascading error noise and makes compiler output easier to read. ([#1386](https://github.com/argotorg/fe/issues/1386))
- Fix chained method call type inference (e.g. `result.map(fn1).map(fn2)`). The compiler now correctly unifies types through canonicalized receivers, resolving incorrect type mismatch errors on valid method chains. ([#1389](https://github.com/argotorg/fe/issues/1389))
- Overhaul LSP stability and observability: worker-thread panics now surface as visible errors instead of being silently swallowed, a dual-layer logging system writes detailed diagnostics to workspace-local `.fe-lsp/` log files with automatic rotation and retention, and a dispatch-deadlock in concurrent request handling has been fixed via an upgraded async-lsp dependency. ([#1392](https://github.com/argotorg/fe/issues/1392))
- Remove redundant `EvmResultExt` trait and `unwrap_or_revert()` method. Since `Result::unwrap()` already ABI-encodes errors on revert via `panic_with_value`, `unwrap_or_revert()` was a leftover that duplicated this behavior. ([#1395](https://github.com/argotorg/fe/issues/1395))
- Fix several MIR correctness issues: `fe build --contract` now filters Sonatina IR output correctly, ingot builds work when contracts are re-exported from the root module, generic calls can forward concrete EVM effects, and escaping storage borrows are rejected. ([#1404](https://github.com/argotorg/fe/issues/1404))
- Report invalid dependency paths in `fe.toml` as configuration diagnostics instead of panicking. ([#1408](https://github.com/argotorg/fe/issues/1408))
- Fix `StorageBytes.encode_return` so multi-word `bytes` values are ABI-encoded from allocated return memory instead of clobbering scratch memory. ([#1409](https://github.com/argotorg/fe/issues/1409))
- Fix CTFE evaluation of unchecked and wrapping numeric intrinsics to use fixed-width word semantics for wrapping negation, bitwise not, shifts, and division/remainder edge cases.
- Fix generic operator overload resolution so ambiguous trait method candidates are preserved and can be disambiguated by argument constraints. Generic wrappers such as field element types can now support mixed operator impls like `Fr<M> + u256`.

### Performance improvements

- Improve CTFE performance and scale by reducing repeated const interning, using copy-on-write aggregate stores, caching semantic bodies and instances, and raising the default CTFE step limit to 1,000,000. Larger constant computations such as Poseidon test vectors can now complete during normal checks.

### Internal Changes - for Fe Contributors

- Bump sonatina to eb50941. ([#1393](https://github.com/argotorg/fe/issues/1393))
