// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint224BenchTest is AbiRoundtripBase {
    function testBenchEchoUint224() public {
        uint224 value = uint224(123);
        require(solBench.benchEchoUint224(value) == value, "sol value");
        require(feBench.benchEchoUint224(value) == value, "fe value");
    }
}
