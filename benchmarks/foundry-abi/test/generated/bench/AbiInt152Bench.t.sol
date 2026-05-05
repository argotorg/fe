// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt152BenchTest is AbiRoundtripBase {
    function testBenchEchoInt152() public {
        int152 value = int152(-7);
        require(solBench.benchEchoInt152(value) == value, "sol value");
        require(feBench.benchEchoInt152(value) == value, "fe value");
    }
}
