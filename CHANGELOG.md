# Changelog

[//]: # (towncrier release notes start)
## 26.3 (2026-09-16)

### Features

- Support `==` and `!=` for fixed-size arrays whose elements implement `Eq`. ([#528](https://github.com/argotorg/fe/issues/528))
- Support `==` and `!=` for tuples of up to six elements whose types implement `Eq`, including the empty tuple. ([#748](https://github.com/argotorg/fe/issues/748))
- Added PC-accurate debug information. `fe dev trace emit` compiles a
  standalone Fe file or a whole ingot and writes a validated stream of typed
  compiler facts (JSONL) that links EVM bytecode instructions through the
  backend and MIR to source spans where attribution can be proven, and
  `fe dev debug emit --format ethdebug`
  projects that stream into a Fe-specific experimental instruction/source artifact
  with optional attribution details containing origin/confidence information.
  The CLI prints a per-program attribution summary without requiring that extra
  file. Each emitted instruction is classified as source-mapped, ambiguous,
  synthetic, or unmapped, and only a unique exact mapping in the recorded graph
  carries source context. This does not establish exhaustive contributing origins
  or transformation history. Compatibility with stock ethdebug consumers is not
  established; neither the artifact nor the optional attribution details is a
  stable public API. ([#1336](https://github.com/argotorg/fe/issues/1336))
- Added first-class typed memory pointers with `*T`, including allocation helpers, dereference reads and writes, pointer field access, and mutable pointer-backed array indexing. New bounded memory-region APIs—`MemSlice<T>`, `MemSpan`, `MemBuffer`, and `FixedMemBuffer<N>`—distinguish read-only views, owned allocations, and writable capacity across ABI and EVM operations. Pointer-bearing values cannot escape into persistent or transient storage. A bare `*` at the start of a line is interpreted as dereference; multiline multiplication must keep the operator on the preceding line. ([#1469](https://github.com/argotorg/fe/issues/1469))
- Added `fe build --from-metadata <PATH|->` to rebuild a contract from a `<Contract>.metadata.json` recompilation input (as produced by `fe build --emit metadata`); `-` reads the JSON from stdin. The recorded project is materialized into a temporary directory and built with the settings captured in the metadata: the contract from `settings.compilationTarget` (overridable with `--contract`) and the recorded optimizer level (an explicit `-O` wins, with a warning that the rebuilt bytecode no longer matches the verified artifact). A compiler version mismatch warns on stderr without aborting. Artifacts are written to `--out-dir`, defaulting to `./out`. This lets external toolchains such as Foundry or source verifiers recompile Fe contracts from a single self-contained JSON document. ([#1505](https://github.com/argotorg/fe/issues/1505))
- Added `core::num::isqrt` for `u256`, returning the integer square root rounded down. The implementation uses a fixed number of Newton iterations without input-dependent loops. ([#1509](https://github.com/argotorg/fe/issues/1509))
- Added `StorageBytes::to_memory` to copy stored bytes into memory without ending the contract call, allowing further processing such as hashing or contract deployment. Added `StorageBytes::word_at` for indexed access to individual payload words. ([#1510](https://github.com/argotorg/fe/issues/1510))
- Added `std::evm::encode_msg_calldata` to encode a message selector and arguments into memory for low-level calls. Added `Address::static` for typed `STATICCALL` operations that forward the available gas and propagate the callee's revert data on failure. ([#1511](https://github.com/argotorg/fe/issues/1511))
- Added `std::evm::packed` for tightly packed byte encoding, with `encode_packed` and `keccak_packed` tuple helpers and a growable `Packed` builder for payloads assembled at runtime. Supported values include integers, addresses, booleans, strings, and byte sequences. Added `std::evm::crypto::eip712_digest` to compute a digest from a domain separator and struct hash. ([#1512](https://github.com/argotorg/fe/issues/1512))
- Added `Call::call_with_default` and `Address::call_with_default` to return a caller-supplied default when a successful external call returns no data; non-empty returndata is still decoded strictly and reverts are propagated. Added `std::evm::erc20::{safe_transfer, safe_transfer_from, safe_approve}` for tokens that return either `true` or no data. These ERC-20 helpers reject `false` returns and targets without code; the general `call_with_default` helper also returns the default for successful calls to addresses without code. ([#1514](https://github.com/argotorg/fe/issues/1514))
- Added source-oriented contract layout information to language-server hovers. Hovering a contract name shows its complete layout grouped under Storage, Transient Storage, and Immutable (Code), while hovering a contract field shows only the entries belonging to that field.

  The hover labels inline fields and explicit or inferred layout parameters with their source paths and types, and shows index formulas and dimensions for static arrays. It no longer presents empty allocation cursors as occupied slots or exposes internal "root" terminology, and explains when a field's layout is invalid or unavailable. ([#1515](https://github.com/argotorg/fe/issues/1515))
- Added typed `DynArray<T>.get(index)` access and `std::abi::MemVec<T>`, a mutable memory array whose length is chosen at creation and remains fixed. `MemVec` supports zero-initialized construction, independent copies of existing `DynArray` values, indexed reads and writes, and conversion to an independent ABI-encodable `DynArray` for contract calls, return values, and event payloads. Supported elements occupy one static ABI word, including integers, `Address`, `bool`, and fixed-byte types. Reads use Solidity ABI decoding rules, and out-of-bounds reads or writes revert with `Panic(0x32)`. ([#1524](https://github.com/argotorg/fe/issues/1524))

### Bugfixes

- Report unsupported macro calls as compiler errors instead of panicking during semantic lowering. ([#1132](https://github.com/argotorg/fe/issues/1132))
- Reject associated type definitions in trait implementations when the type is not declared in the implemented trait. ([#1193](https://github.com/argotorg/fe/issues/1193))
- Fixed the bundled Tree-sitter grammar to parse chained `||` conditions consistently with the compiler parser. ([#1469](https://github.com/argotorg/fe/issues/1469))
- Fixed library modules being lowered into duplicate ingot main objects when building projects with dependencies. ([#1478](https://github.com/argotorg/fe/issues/1478))
- Improved the parser diagnostic for Rust-style turbofish syntax. Writing `Name::<...>` (for example `StorageMap::<u256, u256, 0>::new()`) now reports a clear "remove the double colons" error pointing at the `::`, instead of a confusing "expected newline or `}`" message. Use the bare `Name<...>` form instead. ([#1498](https://github.com/argotorg/fe/issues/1498))
- Support functions and recv arms with more than 16 arguments in the Sonatina backend. ([#1500](https://github.com/argotorg/fe/issues/1500))
- Fixed method selection incorrectly reporting missing trait imports when an inapplicable blanket implementation shadowed a valid concrete implementation. Diagnostics no longer expose unsatisfied bounds involving traits that are not visible at the call site. ([#1501](https://github.com/argotorg/fe/issues/1501))
- Fixed a compiler error when a mutable `own` aggregate parameter (such as `mut _ s: own [u256; 3]`) is both projected (indexed or field-accessed) and reassigned inside the function. Such functions previously failed to compile with an internal `SlotCarrierMismatch` error; they now compile correctly and keep the parameter's by-value calling convention. ([#1503](https://github.com/argotorg/fe/issues/1503))
- Fixed a compiler panic when selecting a trait method from an `impl` whose `where` clause constrains an associated-type projection. Satisfied projection bounds now type-check normally, while unsatisfied bounds produce the expected trait-bound diagnostic. ([#1505](https://github.com/argotorg/fe/issues/1505))
- Fixed a compiler crash when generating bytecode for tuple or struct constants containing string literals of 16 bytes or fewer. ([#1512](https://github.com/argotorg/fe/issues/1512))
- Fixed a critical contract-layout aliasing bug. Repeating a type with an inferred layout parameter, such as `StorageMap<K, V, const SALT: u256 = _>`, inside a struct, tuple, nested generic argument, type alias, enum, or static array now gives each structural occurrence an independent value. Previously, distinct maps could receive the same salt, causing writes through one field or array element to overwrite data belonging to another.

  Explicit layout values are now reserved before inferred values are assigned, so mixing explicit and inferred parameters cannot collide regardless of declaration order. Static arrays of layout-parameterized types receive checked per-element assignments, including nested arrays and enum payloads, while intentionally shared explicit parameters and enum overlays continue to share.

  Layout assignments are preserved when values flow through aggregates, pattern matching, indexing, control flow, function and effect calls, and returns. Invalid, unresolved, overflowing, cyclic, or unsupported contract layouts now fail with diagnostics instead of producing partial layouts, silently aliasing data, or crashing the compiler. ([#1515](https://github.com/argotorg/fe/issues/1515))
- Fixed compiler crashes when emitting events with eight or more total fields. Events support up to 16 non-indexed data fields, with a type diagnostic when that limit is exceeded. Unsatisfied trait bounds involving string literals are now reported instead of allowing invalid constants to reach code generation. ([#1516](https://github.com/argotorg/fe/issues/1516))
- Message declarations now report argument-count and ABI-type mismatches between `sol("...")` selector signatures and their fields during type checking, including builds that do not emit ABI JSON. Diagnostics identify the offending declaration or field and suggest the corresponding standard-library type when available. ([#1520](https://github.com/argotorg/fe/issues/1520))
- Fix Solidity signatures for dynamic array event fields, including imported aliases and fixed-size array elements. Report unsupported indexed dynamic fields and generated event constant evaluation failures as diagnostics. ([#1522](https://github.com/argotorg/fe/issues/1522))
- Fixed a compiler error when a mutable `own` enum parameter, such as `mut _ value: own Option<u256>`, is matched with `if let` or `while let` and reassigned. Such functions previously failed to compile with an internal `SlotCarrierMismatch` error. ([#1527](https://github.com/argotorg/fe/issues/1527))
- Improved pattern coverage checking to detect previously missed unreachable arms, including wildcards after complete boolean matches and redundant arms after irrefutable patterns. Matches over empty enums can now be exhaustive with no arms, and missing-pattern diagnostics report a representative uncovered case. ([#1528](https://github.com/argotorg/fe/issues/1528))
- Fixed trait resolution across ingots, handling of incomplete solver answers, and method selection by integrating the `tablesolve` trait solver. Improved type-growth limits during trait resolution. ([#1529](https://github.com/argotorg/fe/issues/1529))
- Fixed spurious "type must be known here" errors when a call's return type is determined only by a trait bound on the callee's generic parameters (for example `Option::map`). Such bounds were previously solved only after the entire function body had been checked, so the result of the call could not be used later in the same function without a redundant type annotation. These bounds are now solved at the call site before the return type is checked against its context, so the annotation is no longer required. The same call-site solving applies when selecting among deferred trait methods: an expected return type can disambiguate candidates after their bounds resolve, and a conflicting return type reports a type mismatch instead of leaving the call unresolved or spuriously ambiguous. ([#1531](https://github.com/argotorg/fe/issues/1531))
- Fixed clean builds depending on Tree-sitter parser generation order by ensuring parser generation precedes the web asset build. Installing the parser's npm dependencies no longer attempts to build an unused Node addon. ([#1532](https://github.com/argotorg/fe/issues/1532))
- Aligned runtime ABI argument-size validation with Solidity, fixing differences in whether malformed inputs are accepted or reverted before decoding. ([#1533](https://github.com/argotorg/fe/issues/1533))
- Fix compiler crashes when indexing empty arrays nested inside arrays, tuples, structs, or enum variants. These accesses now perform the expected bounds-check revert. ([#1547](https://github.com/argotorg/fe/issues/1547))
- Fix a compiler crash when a `recv` block names a file module instead of a message module. The compiler now reports an error and consistently describes the expected message module. ([#1552](https://github.com/argotorg/fe/issues/1552))

### Performance improvements

- Functions are now deduplicated in MIR after monomorphization. This reduces compile time for large projects. ([#1465](https://github.com/argotorg/fe/issues/1465))

### Deprecations and Removals

- Removed the old integer-address and wrapper-based memory surface, including `MemPtr<T>`, `core::abi::MemoryInput`, `std::evm::MemoryBytes`, `std::evm::mem::alloc`, and the cursor-based `AbiEncoder` API. Migrate typed memory addresses to `*T`, read-only memory regions to `MemSpan`, and owned or writable allocations to `MemBuffer`. The unused `core::convert::Into<T>` trait was also removed; use explicit conversion APIs instead. ([#1469](https://github.com/argotorg/fe/issues/1469))


## 26.2.0 (2026-06-23)

### Features

- Contract fields are now immutable by default unless declared with `mut`. Immutable fields must be initialized before every successful `init` exit and are embedded into contract code; declare fields as `mut` to keep using storage-backed mutable contract state. During `init`, immutable field reads before assignment observe the zero/default value, reassignment is allowed, and the last write is the value embedded into the deployed contract. Binding a field without `mut` as a `mut` effect (`uses (mut field)`) is an error everywhere except `init`'s code-backed fields; this applies to explicit handle fields (e.g. `StorPtr<T>`) as well.

  The compiler verifies initialization with a definite-assignment analysis: a field counts as initialized when it is assigned whole-value (including compound assignment such as `+=`) on every path that reaches the end of `init`, whether directly or through helper functions that receive the field via a `mut` effect or a `mut T` argument. Branches merge conservatively and loop bodies are assumed to possibly run zero times (conditions that are literal `true`/`false` are folded), so assignments guarded by runtime-dependent loop conditions are not credited. Member-wise initialization of an aggregate field (`p.a = ...; p.b = ...`) does not count; assign the whole value instead. ([#1334](https://github.com/argotorg/fe/issues/1334))
- `fe doc` overhaul for message-based contracts: `msg` variants now render
  as children of their parent msg page (matching enum variants), contract
  pages gain dedicated init and message-handler sections, and `#[test]`
  functions are hidden from generated docs by default (restore them with
  `--include-tests`). The doc viewer also exposes CSS variables for fonts,
  weights, and layout so downstream sites can retheme it without CSS
  specificity battles. ([#1419](https://github.com/argotorg/fe/issues/1419))
- Added `Option::ok_or` and `Option::ok_or_else` to `core`, mirroring Rust's API, plus a `Panic::from(code)` shorthand constructor. Combined with `Result::unwrap`, this lets users attach a typed error value (e.g. `Panic::from(POP_EMPTY)`) to an Option-unwrap so reverts carry meaningful Solidity-compatible selectors instead of always landing on `Panic(0x01)`. ([#1430](https://github.com/argotorg/fe/issues/1430))
- `StorageMap<K, V>` now accepts every primitive integer type (`u8`–`u128`, `usize`, `i8`–`i256`, `isize`) and `bool` as a key, in addition to the previously supported `u256`, `Address`, and tuples. The new `StorageKey` impls left-pad each key to 32 bytes via `WordRepr::to_word()`, so slot derivation matches Solidity's `mapping(intN/uintN/bool => V)` convention exactly. Useful for storage layouts that want to pack indices into a single slot (e.g. `mapping(uint128 => V)` queues). ([#1431](https://github.com/argotorg/fe/issues/1431))
- Improved optimization options for the default Sonatina backend:

  - `-O1`/`--optimize 1` is now distinct from `-O2` and is the default optimization level. It keeps Sonatina's optimization pipeline enabled while using lower exact-search caps for stack shuffling; usually getting close to `-O2` gas and bytecode while substantially reducing compile time.
  - `-O0`/`--optimize 0` now and disables the stack shuffling exact solver entirely in favor of a greedy heuristic, making unoptimized builds much faster.

  ([#1435](https://github.com/argotorg/fe/issues/1435))
- Added core `String<N>` utilities for fixed-width byte round-trips, effective byte length, concatenation, and equality in const and runtime code. ([#1437](https://github.com/argotorg/fe/issues/1437))
- Lossless `bool` to integer casts with `as` are now allowed. ([#1445](https://github.com/argotorg/fe/issues/1445))
- Exposed selected raw EVM operations through `std::evm::ops`, including byte/sign-extension, balance/code inspection, gas price, and blob fee/hash reads. ([#1453](https://github.com/argotorg/fe/issues/1453))
- Added marker-only `#[must_use]` support for functions, structs, and enums, and marked `core::result::Result` as must-use so ignored fallible results are diagnosed unless explicitly discarded with `let _ = ...`. ([#1468](https://github.com/argotorg/fe/issues/1468))
- Precompiles:
    - `std::evm::crypto` precompile wrappers now `Result<PrecompileError, T>`
    instead of reverting on precompile call failure.
    - Added typed wrappers for the remaining EVM crypto precompiles, including
    RIPEMD-160, identity, Blake2F, KZG point evaluation, BLS12-381 operations, and
    P-256 verification.
    - `ecrecover` now rejects non-canonical secp256k1 signatures before returning
    a recovered address.
    - added `ecrecover_raw` which does not perform canonical signature checks. ([#1468](https://github.com/argotorg/fe/issues/1468))
- Added `assert!` builtin, e.g. `assert!(ok)`. Takes an optional second string
  literal argument: `assert!(ok, "something's not ok")`. Assertion failure results
  in a revert, with Solidity-compatible `Error(string)` payload. `assert!` can
  also be used in `const fn`s at compile time; assertion failure results in a
  compile-time failure. ([#1471](https://github.com/argotorg/fe/issues/1471))
- Added the `Bounded` trait in `core::num`, exposing inclusive numeric type bounds as `T::min()` and `T::max()` `const fn` methods on all primitive integer types. ([#1474](https://github.com/argotorg/fe/issues/1474))
- Added the `Abs` and `UnsignedAbs` traits in `core::num`, providing magnitude operations as `const fn` on all signed integer types. `Abs` bundles `abs`, `checked_abs`, `wrapping_abs`, `saturating_abs`, and `overflowing_abs`, each taking a different stance on the asymmetric `T::MIN` edge case. `UnsignedAbs::unsigned_abs()` returns the magnitude in the unsigned type of the same width — handling `T::MIN` without overflow. ([#1474](https://github.com/argotorg/fe/issues/1474))
- Inherent `impl` blocks now support associated `const` items. Consts may reference the impl's generic parameters and other consts of the same impl, are evaluated at compile time per instantiation, and can be used in both value and type positions (e.g. array sizes):

  ```fe
  pub struct Packed<const BITS: u256> {}

  impl<const BITS: u256> Packed<BITS> {
      const LANES: u256 = 256 / BITS
      const LANE_MASK: u256 = (1 << BITS) - 1

      pub fn lanes(self) -> u256 {
          Self::LANES
      }
  }
  ```

  This is the idiomatic place for derived constants on a generic container; previously the math had to be repeated as local bindings in every method. Inherent consts take precedence over trait consts of the same name, with the trait const still reachable via a qualified path like `<Foo as Trait>::X`.

  Further details:

  - Consts are private to the defining module by default; mark them `pub const` to export them.
  - Consts of the enclosing impl are in scope unqualified inside the impl, like trait consts in trait default methods.
  - Inherent consts can be used as const generic arguments (`Holder<Buf::SIZE>`).
  - A const on a conditional impl (`impl<T> Wrap<T> where T: Marker`) is only available when the receiver satisfies the bound.
  - Two inherent impls of the same type that define the same const are a definition-site conflict, exactly like conflicting inherent methods (regardless of their `where` clauses). Collisions with an enum variant name or with an inherent associated function of the same name are also rejected.
  - A const initializer that is not const-evaluable (e.g. a non-`const fn` call) is rejected at its definition, like a top-level `const`.

  ([#1479](https://github.com/argotorg/fe/issues/1479))
- Added `fe build --emit metadata`, which writes a Solidity-standard contract `metadata.json` per contract (sources with `keccak256`, compiler version, resolved build settings, transitive dependency sources, and ABI) so verifiers like Sourcify can reproduce and check the bytecode. ([#1487](https://github.com/argotorg/fe/issues/1487))

### Bugfixes

- Fixed `fe test` output so failure details are printed immediately after the corresponding failed test status when tests run in parallel. ([#1420](https://github.com/argotorg/fe/issues/1420))
- Fixed a compiler panic when code_region_len or code_region_offset was called on an unresolved generic contract runtime parameter. ([#1429](https://github.com/argotorg/fe/issues/1429))
- Fixed a compiler panic when generic associated constants were used in const expressions before their concrete types were known. ([#1429](https://github.com/argotorg/fe/issues/1429))
- Fixed Sonatina ABI encoding and decoding for negative signed integers narrower than 256 bits. ([#1434](https://github.com/argotorg/fe/issues/1434))
- Sonatina:
  - Fixed EVM encoded-pointer provenance across calls and memory operations, with more precise escape summaries for i256 pointer carriers, aggregates, malloc-derived pointers, and local/argument/nonlocal storage. This also improves memory-planning performance by avoiding unnecessarily conservative escape assumptions.
  - Fixed GVN value-phi materialization so cached/generated value phis are rejected unless they cover the currently reachable predecessors.
  - Improved stackify spill slot reuse logic. This fixes a bug where a scratch slot could be reused for values that overlap during phi/control-flow transitions.

  ([#1435](https://github.com/argotorg/fe/issues/1435))
- Fixed ABfI encoding for `evm.call` message payloads with multiple dynamic fields, and for dynamic custom error payloads. ([#1439](https://github.com/argotorg/fe/issues/1439))
- Fixed expression-context `&&` and `||` lowering so right-hand side expressions are only evaluated when short-circuiting requires them. ([#1447](https://github.com/argotorg/fe/issues/1447))
- Fixed compiler panic when lowering zero-sized effect types. ([#1449](https://github.com/argotorg/fe/issues/1449))
- Allowed blanket implementations of external traits when the implementor is a type parameter directly bounded by a local trait. ([#1450](https://github.com/argotorg/fe/issues/1450))
- Fixed calls through projected effect providers, such as `with (store.map)`, so nested calls preserve the projected provider's concrete target type. ([#1459](https://github.com/argotorg/fe/issues/1459))
- Sonatina:
    - Fix unchecked signed EVM division and remainder for narrow signed integer types.
    - Fix EVM codegen for spilled phi values so control-flow joins do not let one edge's object storage clobber another edge's phi source. ([#1470](https://github.com/argotorg/fe/issues/1470))
- Fixed the parser rejecting multiple items on the same line inside `impl`, `trait`, and `extern` blocks. The tree-sitter grammar already allowed `impl Foo for Bar { fn a() {}  fn b() {} }`, but the main parser required a newline between items. ([#1475](https://github.com/argotorg/fe/issues/1475))
- Fixed compiler panics when tuple type aliases were used as event or custom error fields. ([#1481](https://github.com/argotorg/fe/issues/1481))
- Fixed JSON ABI generation for events so fields marked with `#[indexed]` are emitted with `"indexed": true`. ([#1485](https://github.com/argotorg/fe/issues/1485))
- Fixed contract field layout for nested types containing inferred const slot parameters. Contract fields whose storage slot or address space cannot be determined are now reported as errors instead of being laid out incorrectly. ([#1492](https://github.com/argotorg/fe/issues/1492))
- Trait associated `const` implementations are now validated against their declarations. A missing associated const, type mismatch (including param-typed const defaults), or otherwise ill-formed const definitions are reported as errors instead of being silently accepted or producing a panic in a later stage. ([#1492](https://github.com/argotorg/fe/issues/1492))
- Fixed a compiler panic when an `if let`/`while let` condition's pattern was not followed by `=`. The parser now reports a "expected `=`" error and recovers instead of crashing. ([#1497](https://github.com/argotorg/fe/issues/1497))

### Performance improvements

- Fe:
    - Lower named aggregate constants as Sonatina const refs.
    - Only emit Sonatina data regions for constants that are explicitly used as code regions.
    - Avoid duplicated const data in generated bytecode for large const arrays.
    - Improve runtime lowering for const-backed local borrows.
    - Avoid emitting real globals/data entries for zero-sized aggregate constants.
    - Lower view aggregate arguments in a representation-flexible way so the compiler can avoid unnecessary aggregate allocation and copying.
  Sonatina:
    - Compact and specialize constref loads.
    - Deduplicate const data.
    - Coalesce adjacent const loads.
    - Prune dead const sections and functions.
    - Reduce compile-time blowups for large const arrays. ([#1433](https://github.com/argotorg/fe/issues/1433))
- Sonatina:
  - Improved stack shuffling performance via cache improvements and short-circuiting trivial operand-prep cases.
  - Improved memory placement performance.

  ([#1435](https://github.com/argotorg/fe/issues/1435))
- Improved Solidity ABI encoding and decoding performance for dynamic payloads by avoiding redundant memory copies and adding faster paths for canonical dynamic arrays. ([#1440](https://github.com/argotorg/fe/issues/1440))
- Calldata ABI decode optimizations:
    - Decode contract `recv` arguments directly from calldata instead of first copying the payload into memory.
    - Inline hot ABI decoding helpers and storage map key paths to reduce generated wrapper overhead. ([#1448](https://github.com/argotorg/fe/issues/1448))
- Sonatina:
    - EVM lowering now goes via a "machine EVM" instruction set, with additional
      optimizations and better stack and memory management.
    - Improve inlining for small specialized helpers, especially helpers with known callsite arguments or scalarizable object arguments.
    - Sink pure computations into the branch or join block where their results are used, reducing unnecessary work and stack pressure. ([#1470](https://github.com/argotorg/fe/issues/1470))
- Fe:
    - Avoid object-backed runtime storage for read-only owned aggregates, tuple destructuring, and read-only aggregate helper calls.
    - Construct runtime aggregate values directly.
  Sonatina:
    - Account for scalarizable aggregate inserts, extracts, and returns when deciding whether to inline small helpers.
    - Copy aggregate insert-value webs directly to memory during aggregate legalization instead of materializing source aggregate slots.
    - Skip zero-sized aggregate leaf memory operations during aggregate legalization. ([#1483](https://github.com/argotorg/fe/issues/1483))
- Sonatina:
    - Keep explicit `inline(never)` annotations on generated const-data helper variants, preserving source-level code-size controls.
    - Emit large EVM constants in shorter forms when a small runtime operation can replace a much larger literal.
    - Remove duplicate private helper bodies after EVM lowering when they compile to identical bytecode.
    - Remove repeated constant-only helper parameters when every private call passes the same value.
    - Drop unnecessary overflow checks for offsets inside statically sized EVM memory allocations.
    - Deduplicate terminal EVM return/revert code.
    - Replace static const object initialization with direct scalar values where possible.
    - Reuse fixed scratch slots for private temporary EVM buffers whose lifetimes do not overlap.
    - Reuse known EVM free-pointer bounds across internal calls to avoid redundant allocator setup.
    - Build private return/revert payload buffers at fixed addresses instead of routing them through the heap allocator. ([#1488](https://github.com/argotorg/fe/issues/1488))
- Avoid lowering read-only scalars as memory-backed places. ([#1489](https://github.com/argotorg/fe/issues/1489))


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
