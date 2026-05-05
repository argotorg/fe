// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt200BenchTest is AbiRoundtripBase {
    function testBenchEchoInt200() public {
        int200 value = int200(-7);
        require(solBench.benchEchoInt200(value) == value, "sol value");
        require(feBench.benchEchoInt200(value) == value, "fe value");
    }
}
