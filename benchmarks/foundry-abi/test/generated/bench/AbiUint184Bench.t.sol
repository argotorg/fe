// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint184BenchTest is AbiRoundtripBase {
    function testBenchEchoUint184() public {
        uint184 value = uint184(123);
        require(solBench.benchEchoUint184(value) == value, "sol value");
        require(feBench.benchEchoUint184(value) == value, "fe value");
    }
}
