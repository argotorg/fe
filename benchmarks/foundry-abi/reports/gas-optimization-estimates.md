# ABI Gas Optimization Estimates

Last updated: 2026-03-30

This note records the current rough estimate for how much ABI gas we could
still recover relative to Solidity, based on the latest reports in:

- `benchmarks/foundry-abi/reports/gas-summary.md`
- `benchmarks/foundry-abi/reports/gas-diagnosis.md`
- `benchmarks/foundry-abi/reports/dyn-array-suite-gas.md`
- `benchmarks/foundry-abi/reports/bytes-suite-gas.md`
- `benchmarks/foundry-abi/reports/deep-dynamic-suite-gas.md`
- `benchmarks/foundry-abi/reports/fixed-array-suite-gas.md`

These are estimates, not measured post-optimization results.

## Current Baseline

From `gas-summary.md`:

- benchmarked functions: `111`
- mean Fe minus Solidity delta: `+1588.82` gas
- median Fe minus Solidity delta: `+1534.00` gas
- worst single regression: `benchEchoStringU64PairArray2 = +5203`

## Where The Gap Currently Comes From

Using the category means in `gas-summary.md`, the current total excess gas is
about `176,359` across the `111` benchmark functions.

### Array-shaped ABI work

The following categories are all array-dominated:

- `fixed-array`
- `nested-fixed-array`
- `dynamic-fixed-array`
- `tuple-fixed-array`
- `tuple-fixed-array-dynamic`

Together they account for about `91,987` total excess gas, which is about
`829` gas of the current overall mean by themselves.

### Custom-width integer wrappers

The scalar custom-width categories:

- `custom-signed`
- `custom-unsigned`

Together they account for about `64,402` total excess gas, which is about
`580` gas of the current overall mean.

### Combined

Arrays plus custom-width scalars together account for about:

- `156,389` total excess gas
- about `1409` gas of the current mean
- about `89%` of the current overall gap

That leaves only about `180` gas of mean gap in everything else combined.

## What Different Optimization Outcomes Could Achieve

### 1. Conservative, review-friendly ABI cleanup

Focus:

- static word arrays
- custom-width scalar wrappers
- obvious scalar encode/decode cleanup

Expected gain:

- about `400-700` gas off the overall mean

Expected new mean:

- roughly `+900` to `+1200`

Why this seems realistic:

- static-element arrays are already much closer than dynamic-element arrays
- custom-width scalars are numerous and consistently positive deltas
- this path does not require changing ownership semantics

### 2. Strong practical parity pass

Focus:

- everything in the conservative pass
- one-pass dynamic array/span handling
- better fixed-array handling for dynamic elements
- lower wrapper overhead for nested static+dynamic composites

Expected gain:

- about `700-1100` gas off the overall mean

Expected new mean:

- roughly `+500` to `+900`

Why this seems realistic:

- the current reports show arrays dominate the gas problem
- fixed arrays with dynamic elements and nested dynamic arrays are still the
  biggest regressions in the focused suites

### 3. Benchmark-maximal ABI fast paths

Focus:

- everything in the strong parity pass
- view/forward fast paths for unchanged dynamic values
- especially `bytes`, `string`, `T[]`, and dynamic tuples in echo-like paths

Expected gain:

- another `250-500` gas on top of the strong parity pass

Expected new mean:

- roughly `+250` to `+650`

Why this seems realistic:

- the current parity harness mostly measures decode-to-owned followed by
  encode-from-owned
- many benchmark shapes are pure pass-through
- top-level `bytes` is only `+1718`, which feels attackable with a good
  zero-copy or borrowed fast path

### 4. Aggressive runtime redesign

Focus:

- keep dynamic ABI values borrowed or lazy by default
- materialize owned values only on mutation or storage escape
- make ownership the slower path rather than the default path

Expected gain:

- potentially enough to push the mean into roughly `+150` to `+400`

Why this seems plausible:

- `gas-diagnosis.md` strongly suggests the main cost is decode span walk,
  allocate, copy into owned memory, then copy back out
- removing that default ownership tax would attack the central cost model

Why this is riskier:

- this is a real runtime and semantics redesign, not just encoder cleanup
- correctness and ergonomics risk are much higher than the earlier options

## Per-Shape Upside

The overall mean hides how large the remaining opportunity still is on the
hardest shapes.

From the focused reports:

- top-level `bytes`: `+1718`
- `string[]`: `+4553`
- `(string,uint64)[]`: `+6621`
- `string[17]`: `+24071`
- `bytes[17]`: `+23596`
- `(string,uint64)[5]`: `+9484`
- nested `bytes` / `string` array cases: roughly `+10k` to `+15k`

So even if the overall mean only falls by hundreds of gas, the hardest ABI
families still have room for very large absolute wins.

## Practical Read

If the goal is engineering return on time spent:

1. attack array handling first
2. attack custom-width scalar paths second
3. only then decide how aggressive to be about borrowed/value-forward ABI paths

If the goal is the safest reviewable optimization pass:

1. static arrays
2. custom widths
3. defer ownership redesign

If the goal is the largest benchmark drop:

1. view/forward fast paths for unchanged dynamic values
2. reduce repeated span walks for nested dynamic arrays
3. trim per-element wrapper overhead for dynamic-element fixed arrays

## Bottom Line

The current data suggests:

- cutting the ABI gas gap roughly in half looks realistic without doing
  anything too radical
- cutting it by roughly two-thirds to three-quarters looks plausible if we are
  willing to change how dynamic ABI values flow through the runtime
- getting very close to Solidity on the current benchmark set looks more
  realistic than it did before the current parity work, because the remaining
  gap is now concentrated in a relatively clear set of dynamic ABI costs
