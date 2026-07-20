// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint136BenchTest is AbiRoundtripBase {
    function testBenchEchoUint136() public {
        uint136 value = uint136(123);
        require(solBench.benchEchoUint136(value) == value, "sol value");
        require(feBench.benchEchoUint136(value) == value, "fe value");
    }
}
