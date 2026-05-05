// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt40BenchTest is AbiRoundtripBase {
    function testBenchEchoInt40() public {
        int40 value = int40(-7);
        require(solBench.benchEchoInt40(value) == value, "sol value");
        require(feBench.benchEchoInt40(value) == value, "fe value");
    }
}
