// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint56BenchTest is AbiRoundtripBase {
    function testBenchEchoUint56() public {
        uint56 value = uint56(123);
        require(solBench.benchEchoUint56(value) == value, "sol value");
        require(feBench.benchEchoUint56(value) == value, "fe value");
    }
}
