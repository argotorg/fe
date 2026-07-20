// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint96BenchTest is AbiRoundtripBase {
    function testBenchEchoUint96() public {
        uint96 value = uint96(123);
        require(solBench.benchEchoUint96(value) == value, "sol value");
        require(feBench.benchEchoUint96(value) == value, "fe value");
    }
}
