// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint192BenchTest is AbiRoundtripBase {
    function testBenchEchoUint192() public {
        uint192 value = uint192(123);
        require(solBench.benchEchoUint192(value) == value, "sol value");
        require(feBench.benchEchoUint192(value) == value, "fe value");
    }
}
