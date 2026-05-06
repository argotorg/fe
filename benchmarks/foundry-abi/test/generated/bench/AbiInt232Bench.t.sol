// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt232BenchTest is AbiRoundtripBase {
    function testBenchEchoInt232() public {
        int232 value = int232(-7);
        require(solBench.benchEchoInt232(value) == value, "sol value");
        require(feBench.benchEchoInt232(value) == value, "fe value");
    }
}
