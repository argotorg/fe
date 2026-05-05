// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt16Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoInt16Array4() public {
        int16[4] memory value = [int16(-7), int16(0), int16(-1), type(int16).min];
        int16[4] memory solValue = solBench.benchEchoInt16Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        int16[4] memory feValue = feBench.benchEchoInt16Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
