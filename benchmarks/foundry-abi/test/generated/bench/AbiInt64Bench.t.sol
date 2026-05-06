// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt64BenchTest is AbiRoundtripBase {
    function testBenchEchoInt64() public {
        int64 value = int64(-7);
        require(solBench.benchEchoInt64(value) == value, "sol value");
        require(feBench.benchEchoInt64(value) == value, "fe value");
    }
}
