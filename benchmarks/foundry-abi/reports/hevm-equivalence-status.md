# hevm Equivalence Status

Date: 2026-05-06

Environment:
- `hevm 0.57.0`
- `z3 4.16.0`
- `fe-opt 4a5777d33`
- local install from the adjacent `hevm` checkout at `../hevm/local/bin`

Semantics:
- hevm checks matching success/failure, returndata, and storage.
- if both sides revert, hevm compares the revert payloads, not just the fact of reversion.
- logs and gas are not part of the equivalence relation.

Helpers:
- `benchmarks/foundry-abi/scripts/run_hevm_equivalence.sh`
- `benchmarks/foundry-abi/scripts/run_hevm_curated_matrix.sh`

Summary:
- proven equivalent: 70
- unsupported by current hevm parser/encoder: 12
- partial or timed out: 1
- suspicious malformed-calldata mismatches: 1
- other failures: 0

## Proven Equivalent

- `AbiRoundtrip scalars`: `echoBool(bool)`
- `AbiRoundtrip scalars`: `echoAddress(address)`
- `AbiRoundtrip scalars`: `echoUint8(uint8)`
- `AbiRoundtrip scalars`: `echoUint16(uint16)`
- `AbiRoundtrip scalars`: `echoUint32(uint32)`
- `AbiRoundtrip scalars`: `echoUint64(uint64)`
- `AbiRoundtrip scalars`: `echoUint128(uint128)`
- `AbiRoundtrip scalars`: `echoUint(uint256)`
- `AbiRoundtrip scalars`: `echoInt8(int8)`
- `AbiRoundtrip scalars`: `echoInt16(int16)`
- `AbiRoundtrip scalars`: `echoInt32(int32)`
- `AbiRoundtrip scalars`: `echoInt64(int64)`
- `AbiRoundtrip scalars`: `echoInt128(int128)`
- `AbiRoundtrip scalars`: `echoInt256(int256)`
- `AbiRoundtrip scalars`: `echoUint24(uint24)`
- `AbiRoundtrip scalars`: `echoUint40(uint40)`
- `AbiRoundtrip scalars`: `echoUint48(uint48)`
- `AbiRoundtrip scalars`: `echoUint56(uint56)`
- `AbiRoundtrip scalars`: `echoUint72(uint72)`
- `AbiRoundtrip scalars`: `echoUint80(uint80)`
- `AbiRoundtrip scalars`: `echoUint88(uint88)`
- `AbiRoundtrip scalars`: `echoUint96(uint96)`
- `AbiRoundtrip scalars`: `echoUint104(uint104)`
- `AbiRoundtrip scalars`: `echoUint112(uint112)`
- `AbiRoundtrip scalars`: `echoUint120(uint120)`
- `AbiRoundtrip scalars`: `echoUint136(uint136)`
- `AbiRoundtrip scalars`: `echoUint144(uint144)`
- `AbiRoundtrip scalars`: `echoUint152(uint152)`
- `AbiRoundtrip scalars`: `echoUint160(uint160)`
- `AbiRoundtrip scalars`: `echoUint168(uint168)`
- `AbiRoundtrip scalars`: `echoUint176(uint176)`
- `AbiRoundtrip scalars`: `echoUint184(uint184)`
- `AbiRoundtrip scalars`: `echoUint192(uint192)`
- `AbiRoundtrip scalars`: `echoUint200(uint200)`
- `AbiRoundtrip scalars`: `echoUint208(uint208)`
- `AbiRoundtrip scalars`: `echoUint216(uint216)`
- `AbiRoundtrip scalars`: `echoUint224(uint224)`
- `AbiRoundtrip scalars`: `echoUint232(uint232)`
- `AbiRoundtrip scalars`: `echoUint240(uint240)`
- `AbiRoundtrip scalars`: `echoUint248(uint248)`
- `AbiRoundtrip scalars`: `echoInt24(int24)`
- `AbiRoundtrip scalars`: `echoInt40(int40)`
- `AbiRoundtrip scalars`: `echoInt48(int48)`
- `AbiRoundtrip scalars`: `echoInt56(int56)`
- `AbiRoundtrip scalars`: `echoInt72(int72)`
- `AbiRoundtrip scalars`: `echoInt80(int80)`
- `AbiRoundtrip scalars`: `echoInt88(int88)`
- `AbiRoundtrip scalars`: `echoInt96(int96)`
- `AbiRoundtrip scalars`: `echoInt104(int104)`
- `AbiRoundtrip scalars`: `echoInt112(int112)`
- `AbiRoundtrip scalars`: `echoInt120(int120)`
- `AbiRoundtrip scalars`: `echoInt136(int136)`
- `AbiRoundtrip scalars`: `echoInt144(int144)`
- `AbiRoundtrip scalars`: `echoInt152(int152)`
- `AbiRoundtrip scalars`: `echoInt160(int160)`
- `AbiRoundtrip scalars`: `echoInt168(int168)`
- `AbiRoundtrip scalars`: `echoInt176(int176)`
- `AbiRoundtrip scalars`: `echoInt184(int184)`
- `AbiRoundtrip scalars`: `echoInt192(int192)`
- `AbiRoundtrip scalars`: `echoInt200(int200)`
- `AbiRoundtrip scalars`: `echoInt208(int208)`
- `AbiRoundtrip scalars`: `echoInt216(int216)`
- `AbiRoundtrip scalars`: `echoInt224(int224)`
- `AbiRoundtrip scalars`: `echoInt232(int232)`
- `AbiRoundtrip scalars`: `echoInt240(int240)`
- `AbiRoundtrip scalars`: `echoInt248(int248)`
- `FixedArraySuite static`: `echoUintArray8(uint256[8] calldata)`
- `FixedArraySuite static`: `echoUintArray16(uint256[16] calldata)`
- `FixedArraySuite static`: `echoUintArray32(uint256[32] calldata)`
- `FixedArraySuite static`: `echoNestedUintArray2x5(uint256[5][2] calldata)`

## Unsupported in hevm 0.57.0

- `AbiRoundtrip dynamic`: `echoString(string memory)`
- `AbiRoundtrip dynamic`: `echoUintArray(uint256[] memory)`
- `BytesSuite dynamic`: `echoBytes(bytes memory)`
- `FixedArraySuite dynamic fixed-array`: `echoStringArray5(string[5] calldata)`
- `AbiRoundtrip tuples`: `echoBoolAddressPair((bool,address) memory)`
- `AbiRoundtrip tuples`: `echoUint24Int40Pair((uint24,int40) memory)`
- `AbiRoundtrip tuples`: `echoBoolAddressU256Triple((bool,address,uint256) memory)`
- `AbiRoundtrip tuple arrays`: `echoBoolAddressPairArray4((bool,address)[4] calldata)`
- `AbiRoundtrip tuple arrays`: `echoUint24Int40PairArray4((uint24,int40)[4] calldata)`
- `AbiRoundtrip tuple arrays`: `echoBoolAddressU256TripleArray4((bool,address,uint256)[4] calldata)`
- `NestedTupleSuite tuples`: `echoNestedStatic(((bool,address),uint256) memory)`
- `NestedTupleSuite tuples`: `echoNestedStaticFlipped((bool,(address,uint256)) memory)`

## Partial / Timeout

- `FixedArraySuite bool timeout`: `echoBoolArray17(bool[17] calldata)`

## Suspected hevm Front-End Bugs

- `FixedArraySuite bool bug`: `echoBoolArray5(bool[5] calldata)`

## Other Failures


