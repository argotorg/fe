// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint112BenchTest is AbiRoundtripBase {
    function testBenchEchoUint112() public {
        uint112 value = uint112(123);
        require(solBench.benchEchoUint112(value) == value, "sol value");
        require(feBench.benchEchoUint112(value) == value, "fe value");
    }
}
