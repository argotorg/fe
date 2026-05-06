// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint120BenchTest is AbiRoundtripBase {
    function testBenchEchoUint120() public {
        uint120 value = uint120(123);
        require(solBench.benchEchoUint120(value) == value, "sol value");
        require(feBench.benchEchoUint120(value) == value, "fe value");
    }
}
