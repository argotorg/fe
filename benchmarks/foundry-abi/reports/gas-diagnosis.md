# ABI Gas Diagnosis

Last updated: 2026-03-30

This note records the current working diagnosis for why Fe still trails
Solidity on ABI gas in the Foundry parity harness.

It is based on the latest regenerated reports in:

- `benchmarks/foundry-abi/reports/gas-summary.md`
- `benchmarks/foundry-abi/reports/dyn-array-suite-gas.md`
- `benchmarks/foundry-abi/reports/deep-dynamic-suite-gas.md`
- `benchmarks/foundry-abi/reports/fixed-array-suite-gas.md`
- `benchmarks/foundry-abi/reports/fixed-array-ceiling-suite-gas.md`

## High-Level Conclusion

The remaining gas gap is mostly ABI marshalling overhead inside the Fe callee,
not extra work in the benchmark wrappers themselves.

The Solidity and Fe bench caller contracts are intentionally symmetric in:

- `benchmarks/foundry-abi/src/AbiRoundtripSol.sol`
- `benchmarks/foundry-abi/src/BytesSuiteSol.sol`
- `benchmarks/foundry-abi/src/DeepDynamicSuiteSol.sol`
- `benchmarks/foundry-abi/src/FixedArraySuiteSol.sol`
- `benchmarks/foundry-abi/src/FixedArrayCeilingSuiteSol.sol`

So the meaningful delta is in Fe decode / owned-value handling / re-encode.

## What The Current Reports Say

From `gas-summary.md`:

- worst category by mean delta is `tuple-dynamic`
- next worst categories are `fixed-array`, `tuple-static`,
  `tuple-fixed-array-dynamic`, and `dynamic-fixed-array`
- plain native scalars are close to parity
- `address` and `bool` are slightly better than Solidity in the current run

From the focused suites:

- deep-dynamic regressions are still dominated by nested `bytes` and nested
  tuple-array shapes
- fixed-array regressions are dominated by dynamic-element arrays such as
  `string[17]`, `bytes[17]`, and `(string,uint64)[5]`

That shape strongly suggests the cost is in dynamic ABI machinery rather than
generic call overhead.

## Main Gas Drivers

### 1. Owned dynamic decode copies payloads into memory

`Bytes` decode in `ingots/core/src/abi.fe` reads the dynamic tail, allocates a
new buffer, and copies the payload word-by-word into owned memory.

The same pattern exists for `DynArray<T>` decode.

Relevant code:

- `impl Decode<A> for Bytes`
- `impl Decode<A> for DynArray<T>`

Why it hurts:

- Solidity can often stay closer to calldata-oriented handling for simple echo
  paths
- Fe currently materializes owned dynamic values first, even when the contract
  body just returns the value unchanged
- nested dynamic values multiply this copy cost

### 2. Dynamic arrays do a recursive span walk before copying

`DynArray<T>` decode does not only copy the tail. It first computes the end of
the full payload with `dyn_array_payload_end`, which walks element heads and,
for dynamic elements, recursively asks for each field end.

Relevant code:

- `dyn_array_payload_end`
- `impl AbiSpan<A> for DynArray<T>`
- `abi_field_end`

Why it hurts:

- one pass determines dynamic extent
- another pass copies the payload into owned memory
- encode then copies the owned payload back out again
- nested dynamic arrays and arrays of tuples magnify all three passes

This matches the current deep-dynamic hot spots:

- `bytes[][]`
- `(bytes,uint64)[][]`
- `(string,uint64)[][]`

## 3. Dynamic-element fixed arrays pay per-element head/tail costs

Fixed arrays are cheap when their element type is static. They get expensive
when the element type is dynamic.

In `ingots/core/src/abi.fe`, `[T; N]` is treated as dynamic whenever `T` is
dynamic:

- `impl<T, const N: usize> AbiSize for [T; N]`

The array encode/decode path then loops over every element and routes each one
through `decode_field` / `encode_field`, which handle parent wrappers and tail
offsets.

Relevant code:

- `decode_field`
- `encode_field`
- `impl Decode<A> for [T; N]`
- `impl Encode<A> for [T; N]`
- `impl AbiSpan<A> for [T; N]`

Why it hurts:

- every `string` / `bytes` / dynamic tuple element contributes its own offset
  bookkeeping
- larger fixed arrays repeat that machinery many times
- arrays of dynamic tuples combine both tuple head/tail logic and array head/tail
  logic

This matches the current worst fixed-array regressions:

- `string[17]`
- `bytes[17]`
- `(string,uint64)[5]`

### 4. `string` is still a dynamic ABI type, and the benches now measure it through `DynString`

The parity harness no longer hides behind the old `String<32>` ceiling for
string coverage. Generated and focused string cases now use
`std::abi::DynString`, and the deterministic / fuzz payloads intentionally
cross the old single-word boundary.

Relevant code:

- `impl AbiSize for DynString`
- `impl Decode<A> for DynString`
- `impl Encode<A> for DynString`
- `impl SolCompat for DynString`

Why it hurts:

- every string still uses dynamic head/tail handling
- string arrays and string-containing tuples inherit the same dynamic costs
- the current string deltas now reflect real owned dynamic-string ABI work
  rather than an artificially capped `String<32>` subset

That is why:

- top-level `string` is still expensive
- `string[]`, `string[17]`, `(string,uint64)[]`, and `(string,uint64)[5]` are
  much worse
- the current reports are more representative of true Solidity `string`
  parity costs than the older capped-string snapshot

### 5. The current parity benches mostly measure owned paths, not zero-copy views

There are zero-copy-ish view helpers available:

- `decode_bytes_view_at` in `ingots/std/src/abi/sol.fe`
- `decode_string_view_at` in `ingots/std/src/abi/sol.fe`
- `encode_bytes_return_view` in `ingots/std/src/evm/storage_bytes.fe`

But the current parity contracts largely exercise owned values:

- `benchmarks/foundry-abi/fe/AbiRoundtrip.fe`
- `benchmarks/foundry-abi/fe/DynArraySuite.fe`
- `benchmarks/foundry-abi/fe/BytesSuite.fe`
- `benchmarks/foundry-abi/fe/DeepDynamicSuite.fe`
- `benchmarks/foundry-abi/fe/FixedArraySuite.fe`
- `benchmarks/foundry-abi/fe/FixedArrayCeilingSuite.fe`

Why it matters:

- the existing gas reports are mainly measuring decode-to-owned plus
  encode-from-owned
- they are not yet measuring an optimized view-based echo fast path

## What Does Not Look Like The Main Problem

These do not look like the primary gas issue in the current data:

- benchmark-wrapper asymmetry between Solidity and Fe
- plain scalar ABI encode/decode
- basic static tuple handling
- plain fixed arrays of static elements

Those shapes are already near parity, and a few are slightly better than
Solidity in the current run.

## Current Working Summary

If reduced to one sentence:

Fe is still paying too much for dynamic ABI ownership.

More concretely:

- decode walks dynamic structure to find bounds
- decode allocates and copies into owned memory
- encode copies the owned payload back out
- nested dynamic and dynamic-element fixed-array shapes amplify that overhead

## Best Follow-On Optimization Targets

The highest-value places to look next are:

1. avoid unnecessary owned materialization for pure echo-like dynamic paths
2. reduce repeated head/tail scanning for nested dynamic arrays
3. trim per-element wrapper overhead for fixed arrays of dynamic elements
4. use view-based fast paths where the contract only forwards or returns the
   ABI payload unchanged
