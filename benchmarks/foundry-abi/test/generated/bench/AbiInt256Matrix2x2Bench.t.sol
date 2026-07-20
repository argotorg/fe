// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt256Matrix2x2BenchTest is AbiRoundtripBase {
    function testBenchEchoInt256Matrix2x2() public {
        int256[2][2] memory value = [[int256(-7), int256(0)], [int256(-1), type(int256).min]];
        int256[2][2] memory solValue = solBench.benchEchoInt256Matrix2x2(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        int256[2][2] memory feValue = feBench.benchEchoInt256Matrix2x2(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
