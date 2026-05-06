// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {StringU64Pair} from "../../../src/AbiRoundtripSol.sol";

contract AbiStringU64PairArrayBenchTest is AbiRoundtripBase {
    function testBenchEchoStringU64PairArray() public {
        StringU64Pair[] memory value = new StringU64Pair[](2);
        value[0] = StringU64Pair({text: "bench-one-with-extra-payload", count: uint64(11)});
        value[1] = StringU64Pair({text: "bench-two-with-extra-payload", count: uint64(22)});
        for (uint256 i = 0; i < value.length; i++) {
            assumeShortString(value[i].text);
        }
        StringU64Pair[] memory solValue = solBench.benchEchoStringU64PairArray(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        StringU64Pair[] memory feValue = feBench.benchEchoStringU64PairArray(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
