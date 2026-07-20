// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint152BenchTest is AbiRoundtripBase {
    function testBenchEchoUint152() public {
        uint152 value = uint152(123);
        require(solBench.benchEchoUint152(value) == value, "sol value");
        require(feBench.benchEchoUint152(value) == value, "fe value");
    }
}
