// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt24Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoInt24Array4() public {
        int24[4] memory value = [int24(-7), int24(0), int24(-1), type(int24).min];
        int24[4] memory solValue = solBench.benchEchoInt24Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        int24[4] memory feValue = feBench.benchEchoInt24Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
