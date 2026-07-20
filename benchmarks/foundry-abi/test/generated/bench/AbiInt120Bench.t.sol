// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt120BenchTest is AbiRoundtripBase {
    function testBenchEchoInt120() public {
        int120 value = int120(-7);
        require(solBench.benchEchoInt120(value) == value, "sol value");
        require(feBench.benchEchoInt120(value) == value, "fe value");
    }
}
