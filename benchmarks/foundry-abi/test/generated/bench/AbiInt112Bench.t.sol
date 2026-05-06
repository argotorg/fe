// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt112BenchTest is AbiRoundtripBase {
    function testBenchEchoInt112() public {
        int112 value = int112(-7);
        require(solBench.benchEchoInt112(value) == value, "sol value");
        require(feBench.benchEchoInt112(value) == value, "fe value");
    }
}
