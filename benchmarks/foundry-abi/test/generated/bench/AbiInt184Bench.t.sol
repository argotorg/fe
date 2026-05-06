// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt184BenchTest is AbiRoundtripBase {
    function testBenchEchoInt184() public {
        int184 value = int184(-7);
        require(solBench.benchEchoInt184(value) == value, "sol value");
        require(feBench.benchEchoInt184(value) == value, "fe value");
    }
}
