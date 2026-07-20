// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt160Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoInt160Array4() public {
        int160[4] memory value = [int160(-7), int160(0), int160(-1), type(int160).min];
        int160[4] memory solValue = solBench.benchEchoInt160Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        int160[4] memory feValue = feBench.benchEchoInt160Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
