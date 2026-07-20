// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint128BenchTest is AbiRoundtripBase {
    function testBenchEchoUint128() public {
        uint128 value = uint128(123);
        require(solBench.benchEchoUint128(value) == value, "sol value");
        require(feBench.benchEchoUint128(value) == value, "fe value");
    }
}
