// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt104BenchTest is AbiRoundtripBase {
    function testBenchEchoInt104() public {
        int104 value = int104(-7);
        require(solBench.benchEchoInt104(value) == value, "sol value");
        require(feBench.benchEchoInt104(value) == value, "fe value");
    }
}
