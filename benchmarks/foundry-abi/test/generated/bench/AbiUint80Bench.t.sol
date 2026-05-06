// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint80BenchTest is AbiRoundtripBase {
    function testBenchEchoUint80() public {
        uint80 value = uint80(123);
        require(solBench.benchEchoUint80(value) == value, "sol value");
        require(feBench.benchEchoUint80(value) == value, "fe value");
    }
}
