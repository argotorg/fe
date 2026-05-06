// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint16BenchTest is AbiRoundtripBase {
    function testBenchEchoUint16() public {
        uint16 value = uint16(123);
        require(solBench.benchEchoUint16(value) == value, "sol value");
        require(feBench.benchEchoUint16(value) == value, "fe value");
    }
}
