// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt136BenchTest is AbiRoundtripBase {
    function testBenchEchoInt136() public {
        int136 value = int136(-7);
        require(solBench.benchEchoInt136(value) == value, "sol value");
        require(feBench.benchEchoInt136(value) == value, "fe value");
    }
}
