// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint72BenchTest is AbiRoundtripBase {
    function testBenchEchoUint72() public {
        uint72 value = uint72(123);
        require(solBench.benchEchoUint72(value) == value, "sol value");
        require(feBench.benchEchoUint72(value) == value, "fe value");
    }
}
