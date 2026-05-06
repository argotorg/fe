// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint104BenchTest is AbiRoundtripBase {
    function testBenchEchoUint104() public {
        uint104 value = uint104(123);
        require(solBench.benchEchoUint104(value) == value, "sol value");
        require(feBench.benchEchoUint104(value) == value, "fe value");
    }
}
