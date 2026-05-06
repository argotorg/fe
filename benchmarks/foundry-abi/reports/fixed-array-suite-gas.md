# Fixed Array Suite Gas Report

Generated: 2026-03-27

Suite summary:

- `26` tests passed
- `0` failed
- gas tables gathered from
  `forge test --root benchmarks/foundry-abi --offline --match-path test/FixedArraySuiteEquivalence.t.sol --gas-report`

Overall fixed-suite wrapper deltas (`FeBenchCaller` avg minus
`FixedArraySolBenchCaller` avg):

- mean delta: `+6558.85` gas
- median delta: `+3593` gas
- best delta: `benchEchoBoolAddressPairArray8` `-4817`
- worst delta: `benchEchoStringArray17` `+24071`

Per-function wrapper deltas:

| Function | Solidity Avg | Fe Avg | Delta |
| --- | ---: | ---: | ---: |
| `benchEchoBoolAddressPairArray8` | 61007 | 56190 | -4817 |
| `benchEchoBoolArray17` | 50162 | 53755 | 3593 |
| `benchEchoBoolArray5` | 33397 | 34109 | 712 |
| `benchEchoBytesArray17` | 97535 | 121131 | 23596 |
| `benchEchoBytesArray5` | 47772 | 54924 | 7152 |
| `benchEchoBytesU64PairArray5` | 60328 | 69600 | 9272 |
| `benchEchoNestedUintArray2x5` | 41176 | 41336 | 160 |
| `benchEchoStringArray17` | 98386 | 122457 | 24071 |
| `benchEchoStringArray5` | 48054 | 55388 | 7334 |
| `benchEchoStringU64PairArray5` | 60730 | 70214 | 9484 |
| `benchEchoUintArray16` | 42819 | 44067 | 1248 |
| `benchEchoUintArray32` | 59346 | 62415 | 3069 |
| `benchEchoUintArray8` | 34578 | 34969 | 391 |

Underlying Solidity target call averages from the same run:

| Function | Avg gas |
| --- | ---: |
| `echoBoolAddressPairArray8` | 37445 |
| `echoBoolArray17` | 29044 |
| `echoBoolArray5` | 24068 |
| `echoBytesArray17` | 60420 |
| `echoBytesArray5` | 33370 |
| `echoBytesU64PairArray5` | 38838 |
| `echoNestedUintArray2x5` | 28890 |
| `echoStringArray17` | 59790 |
| `echoStringArray5` | 33436 |
| `echoStringU64PairArray5` | 39249 |
| `echoUintArray16` | 31858 |
| `echoUintArray32` | 42450 |
| `echoUintArray8` | 26590 |

Notes:

- The merged fixed-array suite still includes the former ceiling cases
  (`bool[17]`, `uint256[32]`, `string[17]`, `bytes[17]`), so the suite-wide
  mean delta remains materially higher than the older `[5]`-only snapshot.
- The strongest fixed-array win is still `(bool,address)[8]`, where the Fe
  wrapper remains materially cheaper than the Solidity wrapper.
- The worst regressions are fixed arrays with dynamic elements, led by
  `string[17]`, `bytes[17]`, and `(string,uint64)[5]`.
- The string cases now measure explicit `DynString` roundtrips with payloads
  beyond the old single-word limit rather than the earlier capped-string path.
- The raw Forge gas tables that produced these deltas are in the command output
  from the focused fixed-array gas run; this markdown records the extracted
  wrapper averages across the full `257`-call gas-report sample.
