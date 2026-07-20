// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint32BenchTest is AbiRoundtripBase {
    function testBenchEchoUint32() public {
        uint32 value = uint32(123);
        require(solBench.benchEchoUint32(value) == value, "sol value");
        require(feBench.benchEchoUint32(value) == value, "fe value");
    }
}
