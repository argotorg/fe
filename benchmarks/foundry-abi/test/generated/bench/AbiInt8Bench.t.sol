// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt8BenchTest is AbiRoundtripBase {
    function testBenchEchoInt8() public {
        int8 value = int8(-7);
        require(solBench.benchEchoInt8(value) == value, "sol value");
        require(feBench.benchEchoInt8(value) == value, "fe value");
    }
}
