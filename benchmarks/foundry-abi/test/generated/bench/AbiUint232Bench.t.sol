// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint232BenchTest is AbiRoundtripBase {
    function testBenchEchoUint232() public {
        uint232 value = uint232(123);
        require(solBench.benchEchoUint232(value) == value, "sol value");
        require(feBench.benchEchoUint232(value) == value, "fe value");
    }
}
