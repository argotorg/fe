// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint24BenchTest is AbiRoundtripBase {
    function testBenchEchoUint24() public {
        uint24 value = uint24(123);
        require(solBench.benchEchoUint24(value) == value, "sol value");
        require(feBench.benchEchoUint24(value) == value, "fe value");
    }
}
