// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint200BenchTest is AbiRoundtripBase {
    function testBenchEchoUint200() public {
        uint200 value = uint200(123);
        require(solBench.benchEchoUint200(value) == value, "sol value");
        require(feBench.benchEchoUint200(value) == value, "fe value");
    }
}
