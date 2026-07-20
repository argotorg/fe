// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiStringArray2BenchTest is AbiRoundtripBase {
    function testBenchEchoStringArray2() public {
        string[2] memory value = ["bench alpha with extra payload bytes", "bench beta with extra payload bytes"];
        assumeShortString(value[0]);
        assumeShortString(value[1]);
        string[2] memory solValue = solBench.benchEchoStringArray2(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        string[2] memory feValue = feBench.benchEchoStringArray2(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
