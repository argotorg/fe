// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt32BenchTest is AbiRoundtripBase {
    function testBenchEchoInt32() public {
        int32 value = int32(-7);
        require(solBench.benchEchoInt32(value) == value, "sol value");
        require(feBench.benchEchoInt32(value) == value, "fe value");
    }
}
