# Fixed Array Ceiling Suite Gas Report

Generated: 2026-03-27

Suite summary:

- `8` tests passed
- `0` failed
- gas tables gathered from
  `forge test --root benchmarks/foundry-abi --offline --match-path test/FixedArrayCeilingSuiteEquivalence.t.sol --gas-report`

Overall ceiling-suite wrapper deltas (`FixedArrayCeilingFeBenchCaller` avg
minus `FixedArrayCeilingSolBenchCaller` avg):

- mean delta: `+13204.00` gas
- median delta: `+13137.50` gas
- best delta: `benchEchoUintArray32` `+2874`
- worst delta: `benchEchoStringArray17` `+23667`

Per-function wrapper deltas:

| Function | Solidity Avg | Fe Avg | Delta |
| --- | ---: | ---: | ---: |
| `benchEchoBoolArray17` | 50070 | 52986 | 2916 |
| `benchEchoBytesArray17` | 97603 | 120962 | 23359 |
| `benchEchoStringArray17` | 98072 | 121739 | 23667 |
| `benchEchoUintArray32` | 59306 | 62180 | 2874 |

Underlying Solidity target call averages from the same run:

| Function | Avg gas |
| --- | ---: |
| `echoBoolArray17` | 29020 |
| `echoBytesArray17` | 60620 |
| `echoStringArray17` | 59493 |
| `echoUintArray32` | 42318 |

Notes:

- This suite still exists because the `17` / `32` fixed-array cases compile and
  roundtrip cleanly; it remains useful as a faster regression and gas-isolation
  suite even though the merged `FixedArraySuite` also passes.
- The dynamic-element ceiling regressions are still materially larger than the
  smaller `[5]` fixed-array cases, especially for `string[17]`.
- The string ceiling case now measures explicit `DynString` roundtrips with
  payloads beyond the old single-word string limit.
- The fixed-array ABI path is const-generic again, with contract/message ABI
  roundtrips additionally verified through `bool[65]`, `string[65]`, and
  `bytes[65]` outside this focused gas suite.
- The wrapper averages above come from the full gas-report sample (`257`
  calls per bench function).
