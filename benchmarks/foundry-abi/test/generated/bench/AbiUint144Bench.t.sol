// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint144BenchTest is AbiRoundtripBase {
    function testBenchEchoUint144() public {
        uint144 value = uint144(123);
        require(solBench.benchEchoUint144(value) == value, "sol value");
        require(feBench.benchEchoUint144(value) == value, "fe value");
    }
}
