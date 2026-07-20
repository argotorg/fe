# Dynamic Array Suite Gas Snapshot

Generated: 2026-03-27

Command:

- `forge test --root benchmarks/foundry-abi --offline --match-path test/DynArraySuiteEquivalence.t.sol --gas-report`

Test result:

- `8` tests passed
- `0` failed

Wrapper-call gas (`SolBenchCaller` vs `FeBenchCaller`):

| Function | Solidity | Fe | Delta |
| --- | ---: | ---: | ---: |
| `benchEchoUintArray` | 31382 | 34484 | 3102 |
| `benchEchoBoolAddressPairArray` | 43433 | 44965 | 1532 |
| `benchEchoStringArray` | 43961 | 48514 | 4553 |
| `benchEchoStringU64PairArray` | 53264 | 59885 | 6621 |

Underlying Solidity target call averages from the same run:

| Function | Avg gas |
| --- | ---: |
| `echoUintArray` | 24793 |
| `echoBoolAddressPairArray` | 29677 |
| `echoStringArray` | 31426 |
| `echoStringU64PairArray` | 35590 |

Notes:

- The wrapper averages above come from the full gas-report sample (`257` calls
  per bench function), not a single deterministic invocation.
- The largest delta in this focused suite is still `(string,uint64)[]`,
  followed by `string[]`.
- This focused suite verifies real variable-length ABI arrays (`T[]`) for the
  currently-supported representative element shapes: `uint256`, `(bool,address)`,
  `string`, and `(string,uint64)`.
- The string cases now run through `std::abi::DynString` with payloads that
  intentionally exceed the old single-word string ceiling.
