// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint8BenchTest is AbiRoundtripBase {
    function testBenchEchoUint8() public {
        uint8 value = uint8(123);
        require(solBench.benchEchoUint8(value) == value, "sol value");
        require(feBench.benchEchoUint8(value) == value, "fe value");
    }
}
