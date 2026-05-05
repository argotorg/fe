// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt40Matrix2x2BenchTest is AbiRoundtripBase {
    function testBenchEchoInt40Matrix2x2() public {
        int40[2][2] memory value = [[int40(-7), int40(0)], [int40(-1), type(int40).min]];
        int40[2][2] memory solValue = solBench.benchEchoInt40Matrix2x2(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        int40[2][2] memory feValue = feBench.benchEchoInt40Matrix2x2(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
