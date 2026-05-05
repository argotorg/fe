// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt80BenchTest is AbiRoundtripBase {
    function testBenchEchoInt80() public {
        int80 value = int80(-7);
        require(solBench.benchEchoInt80(value) == value, "sol value");
        require(feBench.benchEchoInt80(value) == value, "fe value");
    }
}
