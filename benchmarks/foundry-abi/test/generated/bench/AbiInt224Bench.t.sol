// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt224BenchTest is AbiRoundtripBase {
    function testBenchEchoInt224() public {
        int224 value = int224(-7);
        require(solBench.benchEchoInt224(value) == value, "sol value");
        require(feBench.benchEchoInt224(value) == value, "fe value");
    }
}
