// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt56BenchTest is AbiRoundtripBase {
    function testBenchEchoInt56() public {
        int56 value = int56(-7);
        require(solBench.benchEchoInt56(value) == value, "sol value");
        require(feBench.benchEchoInt56(value) == value, "fe value");
    }
}
