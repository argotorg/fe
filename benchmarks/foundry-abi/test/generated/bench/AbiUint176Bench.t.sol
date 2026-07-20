// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint176BenchTest is AbiRoundtripBase {
    function testBenchEchoUint176() public {
        uint176 value = uint176(123);
        require(solBench.benchEchoUint176(value) == value, "sol value");
        require(feBench.benchEchoUint176(value) == value, "fe value");
    }
}
