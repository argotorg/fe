// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt248Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoInt248Array4() public {
        int248[4] memory value = [int248(-7), int248(0), int248(-1), type(int248).min];
        int248[4] memory solValue = solBench.benchEchoInt248Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        int248[4] memory feValue = feBench.benchEchoInt248Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
