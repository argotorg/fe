// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt64Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoInt64Array4() public {
        int64[4] memory value = [int64(-7), int64(0), int64(-1), type(int64).min];
        int64[4] memory solValue = solBench.benchEchoInt64Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        int64[4] memory feValue = feBench.benchEchoInt64Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
