// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiBoolMatrix2x2BenchTest is AbiRoundtripBase {
    function testBenchEchoBoolMatrix2x2() public {
        bool[2][2] memory value = [[true, false], [true, true]];
        bool[2][2] memory solValue = solBench.benchEchoBoolMatrix2x2(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        bool[2][2] memory feValue = feBench.benchEchoBoolMatrix2x2(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
