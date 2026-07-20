// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt192BenchTest is AbiRoundtripBase {
    function testBenchEchoInt192() public {
        int192 value = int192(-7);
        require(solBench.benchEchoInt192(value) == value, "sol value");
        require(feBench.benchEchoInt192(value) == value, "fe value");
    }
}
