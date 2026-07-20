// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt32Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoInt32Array4() public {
        int32[4] memory value = [int32(-7), int32(0), int32(-1), type(int32).min];
        int32[4] memory solValue = solBench.benchEchoInt32Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        int32[4] memory feValue = feBench.benchEchoInt32Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
