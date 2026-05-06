// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiBoolArray4BenchTest is AbiRoundtripBase {
    function testBenchEchoBoolArray4() public {
        bool[4] memory value = [true, false, true, true];
        bool[4] memory solValue = solBench.benchEchoBoolArray4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        bool[4] memory feValue = feBench.benchEchoBoolArray4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
