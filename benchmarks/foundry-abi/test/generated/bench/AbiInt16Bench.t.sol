// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt16BenchTest is AbiRoundtripBase {
    function testBenchEchoInt16() public {
        int16 value = int16(-7);
        require(solBench.benchEchoInt16(value) == value, "sol value");
        require(feBench.benchEchoInt16(value) == value, "fe value");
    }
}
