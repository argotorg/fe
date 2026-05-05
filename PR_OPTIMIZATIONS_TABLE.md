# ABI Optimization PR Table

| Metric | Before (parity snapshot) | After (this series) | Delta |
| --- | ---: | ---: | ---: |
| Mean gas delta (Fe avg - Solidity avg) | +1588.82 | -579.71 | -2168.53 |
| Median gas delta (Fe avg - Solidity avg) | +1534.00 | -33.00 | -1567.00 |
| Best single-case delta | -1078 (`benchEchoBoolAddressPairArray`) | -6865 (-12.95%) (`benchEchoStringU64PairArray`) | n/a |
| Worst single-case delta | +5203 (`benchEchoStringU64PairArray2`) | +34 (+0.13%) (`benchEchoInt56`) | n/a |

| Commit | Area | Change |
| --- | --- | --- |
| [`9e0167d0f`](https://github.com/argotorg/fe/commit/9e0167d0f2ef086d3faec726cee984e4167f35e5) | ABI dynamic payloads | Bulk-copy dynamic ABI payloads and tighten dynamic-span calculations. |
| [`ee68df1e0`](https://github.com/argotorg/fe/commit/ee68df1e08fc313520b9356642f3be7c0394561f) | Solidity ABI custom integers | Cheaper custom-width canonical checks; `signextend`-based signed checks; tighten runtime lowering. |
| [`804fb842f`](https://github.com/argotorg/fe/commit/804fb842f04b0a10fcb6e62f3dd3d0c657e31e12) | ABI copy lowering | Lower core `mcopy` as an intrinsic across MIR/Yul/Sonatina. |
| [`59ba5430a`](https://github.com/argotorg/fe/commit/59ba5430ac8eece96bd4b4e7da167427b233ff0f) | ABI encode | Fast-path `DIRECT_ENCODE` `encode_to_ptr`. |
| [`96c078014`](https://github.com/argotorg/fe/commit/96c07801457ff83dcfc24f839117bf582530917c) | ABI passthrough validation | Add calldata passthrough validation fast paths across lowering/validation. |
| [`205555dd3`](https://github.com/argotorg/fe/commit/205555dd354677e457162777508c127789df0494) | Tuple/fixed-array validation | Optimize tuple + fixed-array `AbiValidate` and inline hot checks. |
| [`aa158ec69`](https://github.com/argotorg/fe/commit/aa158ec69dcc72d4b7d5d943b642fab15bc6f690) | Runtime selector dispatch | Replace linear selector dispatch with binary search. |
| [`4120cbccf`](https://github.com/argotorg/fe/commit/4120cbccf6e260f4438a49f8bdce0a7773dea03b) | ABI passthrough runtime | Avoid malloc in ABI passthrough arms by constructing calldata views directly. |
| [`9f8000eda`](https://github.com/argotorg/fe/commit/9f8000eda025cacc8d89b6ad44d753343896f374) | Calldata validation arithmetic | Avoid checked arithmetic in calldata view validation where bounds are established. |
| [`4a5777d33`](https://github.com/argotorg/fe/commit/4a5777d33909dc8fab8db298c9c9de6581379263) | Scalar/static ABI decode | Optimize scalar word decode + static ABI decode/validation; final source checkpoint used by the reports. |
