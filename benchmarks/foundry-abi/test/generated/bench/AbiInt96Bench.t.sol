// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt96BenchTest is AbiRoundtripBase {
    function testBenchEchoInt96() public {
        int96 value = int96(-7);
        require(solBench.benchEchoInt96(value) == value, "sol value");
        require(feBench.benchEchoInt96(value) == value, "fe value");
    }
}
