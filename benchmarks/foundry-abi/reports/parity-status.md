# ABI Parity Status

Last updated: 2026-03-30

This note records the current status of Solidity ABI parity work in the
`benchmarks/foundry-abi` harness after rebasing the parity branch onto the
current `std-release` branch.

## Current Verified State

Verified in this worktree during the latest post-Sonatina rerun:

- `cargo test --release -p sonatina-codegen gvn -- --nocapture`
- `cargo test --release -p fe-contract-harness dynamic_string_ -- --nocapture`
- `cargo test --release -p fe-contract-harness dynamic_tuple_ -- --nocapture`
- `cargo test --release -p fe-contract-harness fixed_array_contract_ -- --nocapture`
- `python3 benchmarks/foundry-abi/scripts/generate_matrix.py`
- `cargo run --release -q -p fe -- build --backend sonatina --contract AbiRoundtripFe --out-dir benchmarks/foundry-abi/fe-out benchmarks/foundry-abi/fe/AbiRoundtrip.fe`
- `forge test --root benchmarks/foundry-abi --offline`
- `python3 benchmarks/foundry-abi/scripts/run_gas_report.py`
- `cargo run --release -q -p fe -- build --backend sonatina --contract DynArraySuite --out-dir benchmarks/foundry-abi/fe-out benchmarks/foundry-abi/fe/DynArraySuite.fe`
- `forge test --root benchmarks/foundry-abi --offline --match-path test/DynArraySuiteEquivalence.t.sol --gas-report`
- `cargo run --release -q -p fe -- build --backend sonatina --contract BytesSuite --out-dir benchmarks/foundry-abi/fe-out benchmarks/foundry-abi/fe/BytesSuite.fe`
- `forge test --root benchmarks/foundry-abi --offline --match-path test/BytesSuiteEquivalence.t.sol --gas-report`
- `cargo run --release -q -p fe -- build --backend sonatina --contract DeepDynamicSuite --out-dir benchmarks/foundry-abi/fe-out benchmarks/foundry-abi/fe/DeepDynamicSuite.fe`
- `forge test --root benchmarks/foundry-abi --offline --match-path test/DeepDynamicSuiteEquivalence.t.sol --gas-report`
- `cargo run --release -q -p fe -- build --backend sonatina --contract FixedArraySuite --out-dir benchmarks/foundry-abi/fe-out benchmarks/foundry-abi/fe/FixedArraySuite.fe`
- `forge test --root benchmarks/foundry-abi --offline --match-path test/FixedArraySuiteEquivalence.t.sol --gas-report`
- `cargo run --release -q -p fe -- build --backend sonatina --contract FixedArrayCeilingSuite --out-dir benchmarks/foundry-abi/fe-out benchmarks/foundry-abi/fe/FixedArrayCeilingSuite.fe`
- `forge test --root benchmarks/foundry-abi --offline --match-path test/FixedArrayCeilingSuiteEquivalence.t.sol --gas-report`

Focused Foundry status is green on the rebased branch:

- `AbiRoundtripEquivalence.t.sol`: 7 tests passed
- `FixedArraySuiteEquivalence.t.sol`: 26 tests passed
- `FixedArrayCeilingSuiteEquivalence.t.sol`: 8 tests passed
- `BytesSuiteEquivalence.t.sol`: 4 tests passed
- `DynArraySuiteEquivalence.t.sol`: 8 tests passed
- `DeepDynamicSuiteEquivalence.t.sol`: 18 tests passed

Full generated-harness status from the latest pass:

- `340` suites
- `626` tests passed
- `0` failed
- `0` skipped

Formal `hevm` status from the latest curated pass:

- report: `benchmarks/foundry-abi/reports/hevm-equivalence-status.md`
- `70` proven equivalence checks
- `66` static scalar signatures proved on `AbiRoundtripFe` vs `AbiRoundtripSol`
- `4` focused static array / nested-array signatures proved on `FixedArraySuite`
- `hevm` compares revert payloads when both sides revert, in addition to
  matching success/failure and storage
- current `hevm` limits still block full formal coverage for dynamic ABI shapes
  like `bytes`, `string`, `T[]`, and tuple signatures

Dynamic string / tuple contract/message ABI coverage is also green:

- long-payload `DynString` contract decode / return roundtrips through
  `fe-contract-harness`
- long-payload string literals now roundtrip directly in `DynString`-typed
  contexts, including unannotated locals that later flow into `DynString`,
  through `fe-contract-harness`
- the friendlier `Text` alias now roundtrips correctly in both top-level and
  composite ABI shapes, including `(Text, u64)` and fixed arrays of `Text`,
  through `fe-contract-harness` plus the focused Foundry suites
- `Text.view()` and `Text.as_bytes()` are both exercised through
  `fe-contract-harness`
- long-payload `DynString` event ABI encoding roundtrips through
  `fe-contract-harness`
- `(DynString, u64)` return, message-call, and constructor-arg roundtrips
  through `fe-contract-harness`

Fixed-array contract/message ABI boundary coverage is also green:

- `bool[64]` roundtrips through `fe-contract-harness`
- `bool[65]` roundtrips through `fe-contract-harness`
- `string[65]` and `bytes[65]` roundtrip through `fe-contract-harness`

Historical focused stress snapshots retained from the earlier rebased pass:

- `forge test --root benchmarks/foundry-abi --offline --threads 0 --fuzz-runs 20000 --match-path test/BytesSuiteEquivalence.t.sol`
- `forge test --root benchmarks/foundry-abi --offline --threads 0 --fuzz-runs 10000 --match-path test/DynArraySuiteEquivalence.t.sol`
- `forge test --root benchmarks/foundry-abi --offline --threads 0 --fuzz-runs 256 --match-path test/DeepDynamicSuiteEquivalence.t.sol`
- `forge test --root benchmarks/foundry-abi --offline --threads 0 --fuzz-runs 5000 --match-path test/FixedArraySuiteEquivalence.t.sol`
- `forge test --root benchmarks/foundry-abi --offline --threads 0 --fuzz-runs 2000 --match-path test/FixedArrayCeilingSuiteEquivalence.t.sol`

Those remained green before the latest full-matrix recheck.

## Gas Snapshots

Rebuilt full-harness gas snapshot from the latest pass:

- report: `benchmarks/foundry-abi/reports/gas-summary.md`
- diagnosis note: `benchmarks/foundry-abi/reports/gas-diagnosis.md`
- Bench functions compared: `111`
- Mean Fe minus Solidity delta: `+1588.82` gas
- Median Fe minus Solidity delta: `+1534.00` gas
- Best delta: `benchEchoBoolAddressPairArray` = `-1078`
- Worst delta: `benchEchoStringU64PairArray2` = `+5203`

Focused dynamic-array gas snapshot:

- report: `benchmarks/foundry-abi/reports/dyn-array-suite-gas.md`
- mean wrapper delta: `+3952.00`
- median wrapper delta: `+3827.50`
- best delta: `benchEchoBoolAddressPairArray` = `+1532`
- worst delta: `benchEchoStringU64PairArray` = `+6621`

Focused bytes gas snapshot:

- report: `benchmarks/foundry-abi/reports/bytes-suite-gas.md`
- wrapper delta: `benchEchoBytes` = `+1718`

Focused deep-dynamic gas snapshot:

- report: `benchmarks/foundry-abi/reports/deep-dynamic-suite-gas.md`
- mean wrapper delta: `+7634.11`
- median wrapper delta: `+5814`
- best delta: `benchEchoBytesU64Pair` = `+2339`
- worst delta: `benchEchoNestedBytesU64PairArray` = `+15022`

Focused fixed-array suite gas snapshot:

- report: `benchmarks/foundry-abi/reports/fixed-array-suite-gas.md`
- mean wrapper delta: `+6558.85`
- median wrapper delta: `+3593`
- best delta: `benchEchoBoolAddressPairArray8` = `-4817`
- worst delta: `benchEchoStringArray17` = `+24071`

Focused fixed-array ceiling suite gas snapshot:

- report: `benchmarks/foundry-abi/reports/fixed-array-ceiling-suite-gas.md`
- mean wrapper delta: `+13204.00`
- median wrapper delta: `+13137.50`
- best delta: `benchEchoUintArray32` = `+2874`
- worst delta: `benchEchoStringArray17` = `+23667`

## What Is Implemented

The parity work currently includes:

- generated roundtrip coverage in `fe/AbiRoundtrip.fe`,
  `src/AbiRoundtripSol.sol`, and `test/generated/`
- first-class owned ABI strings via `std::abi::DynString`, used by the
  generated and focused parity suites for Solidity `string` roundtrips with
  payloads beyond the old single-word limit
- focused variable-length-array coverage in `fe/DynArraySuite.fe` and
  `test/DynArraySuiteEquivalence.t.sol`
- focused first-class `bytes` coverage in `fe/BytesSuite.fe` and
  `test/BytesSuiteEquivalence.t.sol`
- focused deeper dynamic coverage in `fe/DeepDynamicSuite.fe` and
  `test/DeepDynamicSuiteEquivalence.t.sol`
- focused fixed-array coverage in `fe/FixedArraySuite.fe` and
  `test/FixedArraySuiteEquivalence.t.sol`
- focused fixed-array ceiling coverage in `fe/FixedArrayCeilingSuite.fe` and
  `test/FixedArrayCeilingSuiteEquivalence.t.sol`
- const-generic fixed-array ABI support in `ingots/core/src/abi.fe`, with
  contract/message ABI roundtrips validated through `bool[65]`, `string[65]`,
  and `bytes[65]`

The deep-dynamic suite currently covers:

- `uint24[]`
- `bytes[]`
- `bytes[][]`
- `uint256[][]`
- `string[][]`
- `(string,uint64)[][]`
- `(bytes,uint64)`
- `(bytes,uint64)[]`
- `(bytes,uint64)[][]`

The fixed-array ceiling suite currently covers:

- `bool[17]`
- `uint256[32]`
- `string[17]`
- `bytes[17]`

## Safe Changes Kept

The following rebased parity changes are considered stable enough to keep in
the worktree:

- `crates/hir/src/analysis/ty/trait_lower.rs`
  Adds cycle recovery to `collect_trait_impls`, fixing a real Salsa cycle that
  appeared after the rebase and broke Fe builds broadly.
- `ingots/std/src/abi/sol/ints.fe`
  Adds `AbiSpan` support for the custom-width Solidity integer wrappers so they
  can participate in `DynArray<T>` decode/encode flows.
- `ingots/core/src/abi.fe`
  Restores the const-generic fixed-array ABI path for `AbiSize`, `AbiSpan`,
  `Decode`, and `Encode`, and adds the first-class owned `DynString` ABI type.
- `ingots/std/src/abi.fe`
- `ingots/std/src/abi/sol/types.fe`
- `crates/fe/src/abi.rs`
- `crates/hir/src/core/lower/hir_builder.rs`
  Wire `DynString` through the standard library surface and compiler ABI
  lowering so it is treated as Solidity `string`.
- `Makefile`
  Adds focused fixed-array and fixed-array-ceiling targets:
  `foundry-abi-build-fe-fixed`, `foundry-abi-test-fixed`,
  `foundry-abi-gas-fixed`, `foundry-abi-stress-fixed`,
  `foundry-abi-build-fe-ceiling`, `foundry-abi-test-ceiling`,
  `foundry-abi-gas-ceiling`, and `foundry-abi-stress-ceiling`.
- `crates/contract-harness/src/lib.rs`
  Adds long-payload `DynString` and `(DynString, u64)` contract ABI coverage,
  plus fixed-array contract ABI boundary coverage for `bool[64]`, `bool[65]`,
  `string[65]`, and `bytes[65]`.
- Adjacent `sona-std-release` fixes validated with this worktree
  Repair `cfg_cleanup` noreturn-tail phi cleanup, keep aggregates wider than
  16 scalar leaves out of scalarization for now, and re-enable `GVN` in the
  default optimized pipeline.
- `benchmarks/foundry-abi/fe/FixedArraySuite.fe`
- `benchmarks/foundry-abi/src/FixedArraySuiteSol.sol`
- `benchmarks/foundry-abi/test/FixedArraySuiteEquivalence.t.sol`
- `benchmarks/foundry-abi/fe/FixedArrayCeilingSuite.fe`
- `benchmarks/foundry-abi/src/FixedArrayCeilingSuiteSol.sol`
- `benchmarks/foundry-abi/test/FixedArrayCeilingSuiteEquivalence.t.sol`
- `benchmarks/foundry-abi/test/DynArraySuiteEquivalence.t.sol`
- `benchmarks/foundry-abi/test/BytesSuiteEquivalence.t.sol`
- `benchmarks/foundry-abi/test/DeepDynamicSuiteEquivalence.t.sol`

The focused equivalence suites assert both:

- raw return-byte equality for direct low-level calls
- typed roundtrip equality through Solidity wrapper callers during fuzzing

## Resolved In This Pass

- The old merged-suite fixed-array Sonatina panic is gone.
  `FixedArraySuite` and `FixedArrayCeilingSuite` both build and pass under the
  default optimized pipeline.
- Fixed arrays no longer stop at the old `[T; 64]` ceiling in the Fe ABI
  implementation. The const-generic path now roundtrips in the harness through
  `bool[65]`, `string[65]`, and `bytes[65]`.
- Fe now has a first-class owned ABI string path through `std::abi::DynString`.
  The generated matrix, focused suites, and contract harness all exercise
  string payloads beyond the old `32`-byte ceiling.
- The full generated harness is green again on the rebased branch:
  `340` suites, `626` passed, `0` failed, `0` skipped.
- The gas reports are refreshed after the latest Sonatina update. The
  full-harness mean delta is now `+1588.82` gas with real long-payload string
  coverage, and the worst remaining categories are still dynamic-element fixed
  arrays and nested deep-dynamic wrappers.

## Current Limits

This is still not an exhaustive proof of complete Solidity ABI parity.

Current limits are now mostly coverage and performance shape, not the old
fixed-array correctness breakage:

- coverage is representative rather than exhaustive across all fixed-array
  lengths and nested tuple/array combinations
- arbitrary-length ABI strings now roundtrip through `std::abi::DynString`,
  and string literals can now flow there either directly from `DynString`-
  typed contexts or from unannotated locals that are later constrained, but
  the default language string inference still resolves to fixed-capacity
  `String<N>`
- the ergonomic alias surface is only partially expanded today:
  `Text` / `Vec<T>` are in the prelude and validated for ABI-facing use, but
  richer dynamic-container helper APIs are still intentionally minimal until
  the generic method/lowering path is sturdier
- `T[]` validation is still split into focused suites because the single
  generated `AbiRoundtripFe` contract does not yet carry the full
  dynamic-array matrix cleanly
- gas is still materially above Solidity for dynamic-element fixed arrays and
  nested deep-dynamic wrappers even after the refreshed post-Sonatina rerun

## Operational Notes

- The generated matrix targets are not concurrency-safe:
  `make foundry-abi-test` and `make foundry-abi-gas` both rewrite
  `test/generated/`, so they should be run serially.
- `FixedArrayCeilingSuite` remains useful as a fast regression and gas-isolation
  suite even though the merged `FixedArraySuite` now passes.

## Recommended Next Step

The highest-value next step is to attack the largest remaining gas cliffs now
that the refreshed post-Sonatina reports are checked in:

1. investigate dynamic-element fixed arrays, especially `string[17]`
   (`+23667` in the ceiling suite / `+24071` in the merged suite),
   `bytes[17]` (`+23359` / `+23596`), and `(string,uint64)[5]` (`+9484`)
2. investigate nested deep-dynamic wrappers, especially
   `(bytes,uint64)[][]` (`+15022`), `(string,uint64)[][]` (`+14730`), and
   `bytes[][]` (`+10241`)
3. trim the remaining generated dynamic-array and string wrapper overhead,
   especially `(string,uint64)[]` (`+6621`), `string[]` (`+4553`), and
   `string` (`+2243`)

Then continue broadening coverage across:

- wider dynamic-element fixed arrays
- broader nested tuple/array combinations
- more generated `T[]` shapes in the monolithic matrix when compile scale
  permits

The current repaired parity envelope on this branch should be treated as:

- full generated matrix green
- focused bytes / dyn-array / deep-dynamic / fixed-array suites green
- long-payload Solidity `string` parity validated through `DynString`,
  including direct string literals and unannotated local literal bindings that
  are later constrained to `DynString`
- friendlier ABI-facing aliases validated through `Text` / `Vec<T>` without
  regressing composite tuple / array head-tail handling
- const-generic fixed arrays verified in harness through `[65]`
- remaining work is coverage and gas/performance improvement, not the old
  fixed-array correctness failure
