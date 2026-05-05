# Foundry ABI Roundtrip Harness

This subproject compares matching Solidity and Fe contracts that only decode
ABI input and re-encode the same value on return.

It exists for three jobs:

1. Equivalence: verify that Solidity and Fe produce the same raw return bytes
   for the same calldata.
2. Fuzzing: run broad randomized input coverage over the same Fe/Solidity ABI
   surface.
3. Benchmarking: run gas-reportable wrapper calls so Solidity and Fe paths can
   be compared function-by-function.

For the current rebased-branch parity status, including the focused fixed-array,
fixed-array-ceiling, and deep-dynamic suites plus the remaining compiler
limits, see
`benchmarks/foundry-abi/reports/parity-status.md`.
For the current gas-regression diagnosis, see
`benchmarks/foundry-abi/reports/gas-diagnosis.md`.
For the current formal `hevm` equivalence snapshot, see
`benchmarks/foundry-abi/reports/hevm-equivalence-status.md`.

## Generation Model

Most of the harness is generated from
`benchmarks/foundry-abi/scripts/generate_matrix.py`.

That script rewrites:

- `fe/AbiRoundtrip.fe`
- `src/AbiRoundtripSol.sol`
- `test/generated/`

Current generated size:

- 111 ABI cases
- 333 generated Foundry test files
- deterministic, fuzz, and benchmark coverage for every generated case

## Covered Shapes

Current generated coverage includes:

- native scalars: `bool`, `address`, `string`, `bytes`
- native integers: `uint8/16/32/64/128/256` and `int8/16/32/64/128/256`
- custom-width Solidity integers backed by `std::abi::sol` wrappers:
  `uint24/40/48/56/72/80/88/96/104/112/120/136/144/152/160/168/176/184/192/200/208/216/224/232/240/248`
  and the matching signed widths
- fixed arrays for:
  native scalars and integers
  representative custom widths:
  `uint24/40/96/160/248` and `int24/40/96/160/248`
- representative nested fixed arrays (`2x2`) for:
  `bool`, `address`, `uint256`, `int256`, `uint24`, `int40`
- tuple-shaped inputs/returns covering mixed static/dynamic layouts:
  `(string,uint64)`, `(bool,address)`, `(uint24,int40)`,
  `(bool,address,uint256)`, `(string,bool,uint64)`
- nested tuples (tuple-of-tuples) covering wrapper-required inner tuples:
  `((bool,address),uint256)`, `(bool,(address,uint256))`, `((string,uint64),bool)`,
  `((string,uint64),(bytes,bool))`
- static tuple arrays for:
  `(bool,address)[4]`, `(uint24,int40)[4]`, `(bool,address,uint256)[4]`
- fixed arrays with dynamic elements for:
  `string[2]`, `(string,uint64)[2]`
- variable-length arrays (`T[]`) for:
  `uint256[]`, `(bool,address)[]`, `string[]`, `(string,uint64)[]`
- focused deeper dynamic cases for:
  `uint24[]`, `bytes[]`, `bytes[][]`, `uint256[][]`, `string[][]`,
  `(string,uint64)[][]`, `(bytes,uint64)`, `(bytes,uint64)[]`,
  `(bytes,uint64)[][]`
- focused larger fixed-array cases for:
  `bool[5]`, `uint256[8]`, `uint256[16]`, `string[5]`, `bytes[5]`,
  `(bool,address)[8]`, `(string,uint64)[5]`, `(bytes,uint64)[5]`,
  `uint256[5][2]`
- focused fixed-array ceiling cases for:
  `bool[17]`, `uint256[32]`, `string[17]`, `bytes[17]`

Notes:

- Generated and focused string cases now use `std::abi::DynString`, and their
  deterministic / fuzz payloads intentionally cross the old single-word
  boundary so the harness measures real dynamic Solidity `string` behavior.
- String literals now coerce directly in `DynString`-typed contexts, and
  unannotated local bindings can still infer to `DynString` from later use
  sites, so contract/message returns and args can materialize real
  long-payload ABI strings without a manual bridge helper.
- The standard prelude now re-exports `std::abi::Text` as a friendlier alias
  for `DynString` and `std::abi::Vec<T>` as a friendlier alias for
  `DynArray<T>`. Composite ABI lowering now treats those aliases as dynamic
  too, so `(Text, u64)`, `[Text; N]` fixed-array element shapes, and
  the focused suite contracts keep the correct Solidity head/tail behavior.
- `Text.view()` and `Text.as_bytes()` are both exercised through
  `fe-contract-harness`, so the friendlier string surface now covers both
  plain roundtrips and direct payload inspection.
- The general language default still infers fixed-capacity `String<N>` when no
  `DynString` context is present, so default language-level string inference is
  still not fully migrated.
- The fixed-array ABI path in `ingots/core/src/abi.fe` is const-generic again.
  Contract/message ABI roundtrips are covered in `crates/contract-harness`
  through `bool[65]`, `string[65]`, and `bytes[65]`, and the merged
  `FixedArraySuite` / `FixedArrayCeilingSuite` both pass under the default
  optimized Sonatina pipeline.
- Generated fuzz suites live under `test/generated/fuzz/` and run under
  Foundry's fuzz engine. The default `forge test` path exercises them all.
- Custom-width integer wrappers now participate in array and tuple-array
  coverage because they implement the value-type `Copy` behavior needed by the
  generic Fe ABI array encoder.
- Dynamic tuple returns now use the same single-output Solidity struct/tuple ABI
  shape as Solidity itself. The harness compares the raw encoded return bytes
  directly instead of relying on flattened return-value workarounds.
- Variable-length arrays are currently verified through the focused
  `DynArraySuite` smoke suite and gas report. The generated all-cases
  `AbiRoundtripFe` contract currently hits a compile-scale wall once the `T[]`
  cases are included, so dynamic-array validation is split out instead of
  relying on the single monolithic generated contract.
- First-class dynamic `bytes` are verified through the focused `BytesSuite`
  smoke suite and gas report.
- Gas reporting is done through `SolBenchCaller` and `FeBenchCaller`.
- `make foundry-abi-gas` writes raw and summarized reports under
  `benchmarks/foundry-abi/reports/`.
- The generated matrix targets are not concurrency-safe: `make foundry-abi-test`
  and `make foundry-abi-gas` both rewrite `test/generated/`, so they should be
  run serially rather than in parallel.

## Verification Snapshot

Latest verified full generated-harness status in this worktree on
`2026-03-27`:

- `cargo test --release -p fe-contract-harness dynamic_string_ -- --nocapture`
- `cargo test --release -p fe-contract-harness dynamic_tuple_ -- --nocapture`
- `cargo test --release -p fe-contract-harness fixed_array_contract_ -- --nocapture`
- `python3 benchmarks/foundry-abi/scripts/generate_matrix.py`
- `cargo run --release -q -p fe -- build --backend sonatina --contract AbiRoundtripFe --out-dir benchmarks/foundry-abi/fe-out benchmarks/foundry-abi/fe/AbiRoundtrip.fe`
- `forge test --root benchmarks/foundry-abi --offline`
- `339` suites
- `615` tests passed
- `0` failed
- `0` skipped

Latest focused dynamic-array run:

- `make foundry-abi-test-dyn`
- 8 tests passed
- 0 failed
- includes deterministic and Foundry fuzz coverage for:
  `uint256[]`, `(bool,address)[]`, `string[]`, `(string,uint64)[]`

Latest focused bytes run:

- `make foundry-abi-test-bytes`
- 4 tests passed
- 0 failed
- includes deterministic coverage for short, multi-word, and `31/32/33`-byte
  boundary payloads plus Foundry fuzzing over `bytes`

Deep-dynamic focused suite:

- harness files:
  `benchmarks/foundry-abi/fe/DeepDynamicSuite.fe`
  `benchmarks/foundry-abi/src/DeepDynamicSuiteSol.sol`
  `benchmarks/foundry-abi/test/DeepDynamicSuiteEquivalence.t.sol`
- dedicated make targets:
  `make foundry-abi-build-fe-deep`
  `make foundry-abi-test-deep`
  `make foundry-abi-gas-deep`
- dedicated stress target:
  `make foundry-abi-stress-deep`
- currently covers 18 tests including `bytes[][]` and `(bytes,uint64)[][]`
- current detailed status is recorded in
  `benchmarks/foundry-abi/reports/parity-status.md`
- gas deltas are recorded in
  `benchmarks/foundry-abi/reports/deep-dynamic-suite-gas.md`

Fixed-array focused suite:

- harness files:
  `benchmarks/foundry-abi/fe/FixedArraySuite.fe`
  `benchmarks/foundry-abi/src/FixedArraySuiteSol.sol`
  `benchmarks/foundry-abi/test/FixedArraySuiteEquivalence.t.sol`
- dedicated make targets:
  `make foundry-abi-build-fe-fixed`
  `make foundry-abi-test-fixed`
  `make foundry-abi-gas-fixed`
- dedicated stress target:
  `make foundry-abi-stress-fixed`
- currently covers 26 tests across larger fixed-array shapes, including
  `bool[17]`, `uint256[32]`, nested `uint256[5][2]`, and dynamic-element cases
  like `bytes[5]`, `(bytes,uint64)[5]`, `string[17]`, and `bytes[17]`
- gas deltas are recorded in
  `benchmarks/foundry-abi/reports/fixed-array-suite-gas.md`

Fixed-array ceiling suite:

- harness files:
  `benchmarks/foundry-abi/fe/FixedArrayCeilingSuite.fe`
  `benchmarks/foundry-abi/src/FixedArrayCeilingSuiteSol.sol`
  `benchmarks/foundry-abi/test/FixedArrayCeilingSuiteEquivalence.t.sol`
- dedicated make targets:
  `make foundry-abi-build-fe-ceiling`
  `make foundry-abi-test-ceiling`
  `make foundry-abi-gas-ceiling`
- dedicated stress target:
  `make foundry-abi-stress-ceiling`
- currently covers 8 tests across the new fixed-array ceiling shapes:
  `bool[17]`, `uint256[32]`, `string[17]`, `bytes[17]`
- this suite remains useful even though the merged `FixedArraySuite` now builds
  and passes; it isolates the larger `[17]` / `[32]` cases for faster
  regressions and separate gas reporting
- gas deltas are recorded in
  `benchmarks/foundry-abi/reports/fixed-array-ceiling-suite-gas.md`

Latest focused stress snapshot on the rebased branch:

- `forge test --root benchmarks/foundry-abi --offline --threads 0 --fuzz-runs 20000 --match-path test/BytesSuiteEquivalence.t.sol`
- `forge test --root benchmarks/foundry-abi --offline --threads 0 --fuzz-runs 10000 --match-path test/DynArraySuiteEquivalence.t.sol`
- `forge test --root benchmarks/foundry-abi --offline --threads 0 --fuzz-runs 256 --match-path test/DeepDynamicSuiteEquivalence.t.sol`
- `forge test --root benchmarks/foundry-abi --offline --threads 0 --fuzz-runs 5000 --match-path test/FixedArraySuiteEquivalence.t.sol`
- `forge test --root benchmarks/foundry-abi --offline --threads 0 --fuzz-runs 2000 --match-path test/FixedArrayCeilingSuiteEquivalence.t.sol`

Those all passed. The detailed timings and current parity notes are tracked in
`benchmarks/foundry-abi/reports/parity-status.md`.

Latest gas summary:

- compared bench functions: `111`
- mean Fe minus Solidity delta: `+1594.33` gas
- median Fe minus Solidity delta: `+1419.00` gas
- worst category by mean delta:
  fixed arrays of dynamic tuples (`+6322.50` gas)
- best category by mean delta: scalar address (`-118.00` gas)

Representative gas findings:

- worst single regression: `benchEchoStringU64PairArray` `+6656` gas
- next worst: `benchEchoStringArray2` `+6063` gas
- outright improvements: `benchEchoAddress` `-118` gas and `benchEchoBool` `-10` gas
- closest remaining positive delta: `benchEchoBoolAddressPairArray4` `+63` gas

Focused dynamic-array gas snapshot:

- `make foundry-abi-gas-dyn`
- report: `benchmarks/foundry-abi/reports/dyn-array-suite-gas.md`
- wrapper deltas:
  `benchEchoUintArray` `+3102`
  `benchEchoBoolAddressPairArray` `+1532`
  `benchEchoStringArray` `+4553`
  `benchEchoStringU64PairArray` `+6621`

Focused bytes gas snapshot:

- `make foundry-abi-gas-bytes`
- report: `benchmarks/foundry-abi/reports/bytes-suite-gas.md`
- wrapper delta:
  `benchEchoBytes` `+1718`

Focused deep-dynamic gas snapshot:

- `make foundry-abi-gas-deep`
- report: `benchmarks/foundry-abi/reports/deep-dynamic-suite-gas.md`
- representative wrapper deltas:
  `benchEchoBytesU64Pair` `+2339`
  `benchEchoNestedUintArray` `+3906`
  `benchEchoNestedBytesArray` `+10241`
  `benchEchoNestedBytesU64PairArray` `+15022`

Focused fixed-array gas snapshot:

- `make foundry-abi-gas-fixed`
- report: `benchmarks/foundry-abi/reports/fixed-array-suite-gas.md`
- representative wrapper deltas:
  `benchEchoBoolAddressPairArray8` `-4817`
  `benchEchoUintArray32` `+3069`
  `benchEchoBytesArray17` `+23596`
  `benchEchoStringArray17` `+24071`

Focused fixed-array ceiling gas snapshot:

- `make foundry-abi-gas-ceiling`
- report: `benchmarks/foundry-abi/reports/fixed-array-ceiling-suite-gas.md`
- representative wrapper deltas:
  `benchEchoBoolArray17` `+2916`
  `benchEchoUintArray32` `+2874`
  `benchEchoBytesArray17` `+23359`
  `benchEchoStringArray17` `+23667`

## Commands

From repo root:

- `make foundry-abi-generate`
- `make foundry-abi-build-fe`
- `make foundry-abi-test`
- `make foundry-abi-gas`
- `make foundry-abi-build-fe-dyn`
- `make foundry-abi-test-dyn`
- `make foundry-abi-gas-dyn`
- `make foundry-abi-build-fe-bytes`
- `make foundry-abi-test-bytes`
- `make foundry-abi-gas-bytes`
- `make foundry-abi-build-fe-deep`
- `make foundry-abi-test-deep`
- `make foundry-abi-gas-deep`
- `make foundry-abi-build-fe-fixed`
- `make foundry-abi-test-fixed`
- `make foundry-abi-gas-fixed`
- `make foundry-abi-build-fe-ceiling`
- `make foundry-abi-test-ceiling`
- `make foundry-abi-gas-ceiling`
- `make foundry-abi-stress-bytes`
- `make foundry-abi-stress-dyn`
- `make foundry-abi-stress-deep`
- `make foundry-abi-stress-fixed`
- `make foundry-abi-stress-ceiling`
- `forge test --root benchmarks/foundry-abi --offline --match-path 'test/generated/fuzz/*'`
  to run only the generated fuzz suites
- `forge test --root benchmarks/foundry-abi --offline --match-path test/DynArraySuiteEquivalence.t.sol`
  to run only the focused variable-length-array suite
- `forge test --root benchmarks/foundry-abi --offline --match-path test/BytesSuiteEquivalence.t.sol`
  to run only the focused `bytes` suite
- `forge test --root benchmarks/foundry-abi --offline --match-path test/DeepDynamicSuiteEquivalence.t.sol`
  to run only the focused deep-dynamic suite
- `forge test --root benchmarks/foundry-abi --offline --match-path test/FixedArraySuiteEquivalence.t.sol`
  to run only the focused larger fixed-array suite

## Known Gaps

This is a large ABI expansion, not full Solidity ABI parity yet.

Not yet covered here:

- automatic migration of the default language-level string surface to dynamic
  owned strings; arbitrary-length ABI parity is available today through
  explicit `std::abi::DynString`

- exhaustive coverage across all fixed-array lengths and tuple/array shape
  combinations, even though the const-generic fixed-array ABI path now
  roundtrips beyond the old `[64]` ceiling
- broader variable-length-array coverage beyond the focused representative set:
  custom-width integers, deeper tuple nesting, nested dynamic arrays
- deeper nested tuple/array combinations beyond the generated representative set
- broader dynamic-element fixed-array coverage beyond the representative
  generated `string[2]` / `(string,uint64)[2]` cases and the focused
  `string[5]` / `bytes[5]` / `(string,uint64)[5]` / `(bytes,uint64)[5]` /
  `string[17]` / `bytes[17]` cases
- gas parity still lags most for dynamic-element fixed arrays and nested
  deep-dynamic wrappers, even after the refreshed post-Sonatina rerun
- current rebased-branch parity investigation notes, including the remaining
  coverage and gas hot spots, are tracked in
  `benchmarks/foundry-abi/reports/parity-status.md`
