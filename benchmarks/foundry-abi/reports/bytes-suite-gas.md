# Bytes Suite Gas Snapshot

Generated: 2026-03-27

Command:

- `forge test --root benchmarks/foundry-abi --offline --match-path test/BytesSuiteEquivalence.t.sol --gas-report`

Test result:

- `4` tests passed
- `0` failed

Wrapper-call gas (`BytesSolBenchCaller` vs `BytesFeBenchCaller`):

| Function | Solidity Avg | Fe Avg | Delta |
| --- | ---: | ---: | ---: |
| `benchEchoBytes` | 27792 | 29510 | 1718 |

Underlying Solidity target call average from the same run:

| Function | Avg gas |
| --- | ---: |
| `echoBytes` | 23426 |

Notes:

- The wrapper average above comes from the full gas-report sample (`261`
  calls), which includes both deterministic coverage and the fuzz path.
- The focused `bytes` suite covers both sub-word and multi-word payloads plus
  Foundry fuzzing over arbitrary byte strings up to 96 bytes.
