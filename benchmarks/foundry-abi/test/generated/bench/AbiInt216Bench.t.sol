// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt216BenchTest is AbiRoundtripBase {
    function testBenchEchoInt216() public {
        int216 value = int216(-7);
        require(solBench.benchEchoInt216(value) == value, "sol value");
        require(feBench.benchEchoInt216(value) == value, "fe value");
    }
}
