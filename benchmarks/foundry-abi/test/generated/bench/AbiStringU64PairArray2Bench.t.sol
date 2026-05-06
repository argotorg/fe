// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {StringU64Pair} from "../../../src/AbiRoundtripSol.sol";

contract AbiStringU64PairArray2BenchTest is AbiRoundtripBase {
    function testBenchEchoStringU64PairArray2() public {
        StringU64Pair[2] memory value = [StringU64Pair({text: "bench-one-with-extra-payload", count: uint64(11)}), StringU64Pair({text: "bench-two-with-extra-payload", count: uint64(22)})];
        assumeShortString(value[0].text);
        assumeShortString(value[1].text);
        StringU64Pair[2] memory solValue = solBench.benchEchoStringU64PairArray2(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        StringU64Pair[2] memory feValue = feBench.benchEchoStringU64PairArray2(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
