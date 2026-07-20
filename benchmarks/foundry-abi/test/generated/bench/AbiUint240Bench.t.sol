// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint240BenchTest is AbiRoundtripBase {
    function testBenchEchoUint240() public {
        uint240 value = uint240(123);
        require(solBench.benchEchoUint240(value) == value, "sol value");
        require(feBench.benchEchoUint240(value) == value, "fe value");
    }
}
