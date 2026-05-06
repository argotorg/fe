// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt168BenchTest is AbiRoundtripBase {
    function testBenchEchoInt168() public {
        int168 value = int168(-7);
        require(solBench.benchEchoInt168(value) == value, "sol value");
        require(feBench.benchEchoInt168(value) == value, "fe value");
    }
}
