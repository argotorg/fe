// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt24BenchTest is AbiRoundtripBase {
    function testBenchEchoInt24() public {
        int24 value = int24(-7);
        require(solBench.benchEchoInt24(value) == value, "sol value");
        require(feBench.benchEchoInt24(value) == value, "fe value");
    }
}
