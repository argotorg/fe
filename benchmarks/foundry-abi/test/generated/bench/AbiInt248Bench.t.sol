// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt248BenchTest is AbiRoundtripBase {
    function testBenchEchoInt248() public {
        int248 value = int248(-7);
        require(solBench.benchEchoInt248(value) == value, "sol value");
        require(feBench.benchEchoInt248(value) == value, "fe value");
    }
}
