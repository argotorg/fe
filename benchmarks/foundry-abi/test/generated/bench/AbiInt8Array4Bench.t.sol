// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt8Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoInt8Array4() public {
        int8[4] memory value = [int8(-7), int8(0), int8(-1), type(int8).min];
        int8[4] memory solValue = solBench.benchEchoInt8Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        int8[4] memory feValue = feBench.benchEchoInt8Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
