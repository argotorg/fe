# ABI Gas Summary

Generated: 2026-05-06T16:33:26.606007+00:00

Suite summary: 340 suites, 626 passed, 0 failed, 0 skipped

Raw report: `reports/gas-report.txt`

CSV: `reports/gas-deltas.csv`

## Overall

- Bench functions compared: 111
- Mean Fe minus Solidity delta: -579.71 gas
- Median Fe minus Solidity delta: -33.00 gas
- Best delta: `benchEchoStringU64PairArray` = -6865 gas (-12.95%)
- Worst delta: `benchEchoInt56` = 34 gas (0.13%)

## By Category

| Category | Functions | Mean Delta | Median Delta | Best | Worst |
| --- | ---: | ---: | ---: | ---: | ---: |
| `scalar-bool` | 1 | 4.00 | 4.00 | 4 | 4 |
| `custom-signed` | 26 | -0.19 | 8.50 | -39 | 34 |
| `custom-unsigned` | 26 | -7.00 | 0.00 | -45 | 27 |
| `native-signed` | 6 | -21.17 | -11.50 | -94 | 12 |
| `native-unsigned` | 6 | -49.50 | 2.50 | -296 | 26 |
| `scalar-address` | 1 | -56.00 | -56.00 | -56 | -56 |
| `dynamic-string` | 1 | -90.00 | -90.00 | -90 | -90 |
| `fixed-array` | 25 | -628.84 | -594.00 | -881 | -528 |
| `tuple-static` | 3 | -859.67 | -793.00 | -993 | -793 |
| `tuple-dynamic` | 2 | -1105.00 | -1105.00 | -1305 | -905 |
| `nested-fixed-array` | 6 | -1548.33 | -1507.50 | -1695 | -1444 |
| `dynamic-fixed-array` | 2 | -3128.00 | -3128.00 | -4208 | -2048 |
| `tuple-fixed-array` | 4 | -4461.75 | -4346.00 | -5121 | -4034 |
| `tuple-fixed-array-dynamic` | 2 | -4846.00 | -4846.00 | -6865 | -2827 |

## Notable Patterns

- Highest mean regression category: `scalar-bool` at 4.00 gas across 1 functions.
- Lowest mean regression category: `tuple-fixed-array-dynamic` at -4846.00 gas across 2 functions.

## Largest Regressions

| Function | Solidity Avg | Fe Avg | Delta | Delta % |
| --- | ---: | ---: | ---: | ---: |
| `benchEchoInt56` | 26174 | 26208 | 34 | 0.13% |
| `benchEchoInt208` | 26131 | 26163 | 32 | 0.12% |
| `benchEchoInt240` | 26134 | 26162 | 28 | 0.11% |
| `benchEchoUint56` | 25812 | 25839 | 27 | 0.10% |
| `benchEchoUint128` | 25748 | 25774 | 26 | 0.10% |
| `benchEchoUint168` | 25812 | 25837 | 25 | 0.10% |
| `benchEchoUint80` | 25770 | 25793 | 23 | 0.09% |
| `benchEchoInt120` | 26196 | 26209 | 13 | 0.05% |
| `benchEchoInt88` | 26174 | 26187 | 13 | 0.05% |
| `benchEchoInt128` | 26155 | 26167 | 12 | 0.05% |
| `benchEchoInt168` | 26174 | 26185 | 11 | 0.04% |
| `benchEchoInt216` | 26198 | 26209 | 11 | 0.04% |
| `benchEchoInt224` | 26176 | 26187 | 11 | 0.04% |
| `benchEchoInt232` | 26198 | 26209 | 11 | 0.04% |
| `benchEchoInt96` | 26175 | 26186 | 11 | 0.04% |

## Largest Improvements

| Function | Solidity Avg | Fe Avg | Delta | Delta % |
| --- | ---: | ---: | ---: | ---: |
| `benchEchoStringU64PairArray` | 52992 | 46127 | -6865 | -12.95% |
| `benchEchoBoolAddressU256TripleArray4` | 47891 | 42770 | -5121 | -10.69% |
| `benchEchoUint24Int40PairArray4` | 43434 | 39064 | -4370 | -10.06% |
| `benchEchoBoolAddressPairArray4` | 43300 | 38978 | -4322 | -9.98% |
| `benchEchoStringArray` | 43795 | 39587 | -4208 | -9.61% |
| `benchEchoBoolAddressPairArray` | 43346 | 39312 | -4034 | -9.31% |
| `benchEchoStringU64PairArray2` | 40469 | 37642 | -2827 | -6.99% |
| `benchEchoStringArray2` | 35984 | 33936 | -2048 | -5.69% |
| `benchEchoInt256Matrix2x2` | 37379 | 35684 | -1695 | -4.53% |
| `benchEchoUintMatrix2x2` | 34953 | 33298 | -1655 | -4.73% |
| `benchEchoAddressMatrix2x2` | 37848 | 36340 | -1508 | -3.98% |
| `benchEchoUint24Matrix2x2` | 36803 | 35296 | -1507 | -4.09% |
| `benchEchoBoolMatrix2x2` | 36819 | 35338 | -1481 | -4.02% |
| `benchEchoInt40Matrix2x2` | 37841 | 36397 | -1444 | -3.82% |
| `benchEchoStringBoolU64Triple` | 31781 | 30476 | -1305 | -4.11% |
