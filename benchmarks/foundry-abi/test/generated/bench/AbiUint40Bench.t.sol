// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint40BenchTest is AbiRoundtripBase {
    function testBenchEchoUint40() public {
        uint40 value = uint40(123);
        require(solBench.benchEchoUint40(value) == value, "sol value");
        require(feBench.benchEchoUint40(value) == value, "fe value");
    }
}
