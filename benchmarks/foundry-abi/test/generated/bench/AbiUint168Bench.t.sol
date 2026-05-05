// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint168BenchTest is AbiRoundtripBase {
    function testBenchEchoUint168() public {
        uint168 value = uint168(123);
        require(solBench.benchEchoUint168(value) == value, "sol value");
        require(feBench.benchEchoUint168(value) == value, "fe value");
    }
}
