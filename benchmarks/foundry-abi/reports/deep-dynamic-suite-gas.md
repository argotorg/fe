# Deep Dynamic Suite Gas Report

Generated: 2026-03-27

Suite summary:

- `18` tests passed
- `0` failed
- gas tables gathered from
  `forge test --root benchmarks/foundry-abi --offline --match-path test/DeepDynamicSuiteEquivalence.t.sol --gas-report`

Overall deep-suite wrapper deltas (`DeepDynamicFeBenchCaller` avg minus
`DeepDynamicSolBenchCaller` avg):

- mean delta: `+7634.11` gas
- median delta: `+5814` gas
- best delta: `benchEchoBytesU64Pair` `+2339`
- worst delta: `benchEchoNestedBytesU64PairArray` `+15022`

Per-function wrapper deltas:

| Function | Solidity Avg | Fe Avg | Delta |
| --- | ---: | ---: | ---: |
| `benchEchoBytesArray` | 39796 | 43923 | 4127 |
| `benchEchoBytesU64Pair` | 30830 | 33169 | 2339 |
| `benchEchoBytesU64PairArray` | 46732 | 52546 | 5814 |
| `benchEchoNestedBytesArray` | 73495 | 83736 | 10241 |
| `benchEchoNestedBytesU64PairArray` | 93431 | 108453 | 15022 |
| `benchEchoNestedStringArray` | 73554 | 83416 | 9862 |
| `benchEchoNestedStringU64PairArray` | 94050 | 108780 | 14730 |
| `benchEchoNestedUintArray` | 48288 | 52194 | 3906 |
| `benchEchoUint24Array` | 31284 | 33950 | 2666 |

Underlying Solidity target call averages from the same run:

| Function | Avg gas |
| --- | ---: |
| `echoBytesArray` | 29668 |
| `echoBytesU64Pair` | 24780 |
| `echoBytesU64PairArray` | 32531 |
| `echoNestedBytesArray` | 46637 |
| `echoNestedBytesU64PairArray` | 55139 |
| `echoNestedStringArray` | 46026 |
| `echoNestedStringU64PairArray` | 55311 |
| `echoNestedUintArray` | 33768 |
| `echoUint24Array` | 24117 |

Notes:

- Nested dynamic regressions still dominate this suite; the heaviest wrappers
  are the nested `bytes` / tuple-array cases.
- `benchEchoUint24Array` is still based on a single bench-call observation in
  this gas report (`# Calls = 1`), so treat that line as directional rather
  than directly comparable to the `257`-call rows.
- The wrapper averages above otherwise come from the full gas-report sample and
  reflect the benchmark harness rather than a single deterministic invocation.
