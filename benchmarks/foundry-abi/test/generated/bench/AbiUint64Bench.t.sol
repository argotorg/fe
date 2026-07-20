// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint64BenchTest is AbiRoundtripBase {
    function testBenchEchoUint64() public {
        uint64 value = uint64(123);
        require(solBench.benchEchoUint64(value) == value, "sol value");
        require(feBench.benchEchoUint64(value) == value, "fe value");
    }
}
