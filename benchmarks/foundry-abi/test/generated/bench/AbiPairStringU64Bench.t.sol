// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {StringU64Pair} from "../../../src/AbiRoundtripSol.sol";

contract AbiPairStringU64BenchTest is AbiRoundtripBase {
    function testBenchEchoPair() public {
        StringU64Pair memory value = StringU64Pair({text: "bench pair", count: uint64(99)});
        assumeShortString(value.text);
        StringU64Pair memory solValue = solBench.benchEchoPair(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        StringU64Pair memory feValue = feBench.benchEchoPair(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
