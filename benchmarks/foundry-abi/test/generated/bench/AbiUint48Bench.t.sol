// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint48BenchTest is AbiRoundtripBase {
    function testBenchEchoUint48() public {
        uint48 value = uint48(123);
        require(solBench.benchEchoUint48(value) == value, "sol value");
        require(feBench.benchEchoUint48(value) == value, "fe value");
    }
}
