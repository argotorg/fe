// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint248BenchTest is AbiRoundtripBase {
    function testBenchEchoUint248() public {
        uint248 value = uint248(123);
        require(solBench.benchEchoUint248(value) == value, "sol value");
        require(feBench.benchEchoUint248(value) == value, "fe value");
    }
}
