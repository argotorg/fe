// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint216BenchTest is AbiRoundtripBase {
    function testBenchEchoUint216() public {
        uint216 value = uint216(123);
        require(solBench.benchEchoUint216(value) == value, "sol value");
        require(feBench.benchEchoUint216(value) == value, "fe value");
    }
}
