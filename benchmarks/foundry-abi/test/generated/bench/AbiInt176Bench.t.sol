// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt176BenchTest is AbiRoundtripBase {
    function testBenchEchoInt176() public {
        int176 value = int176(-7);
        require(solBench.benchEchoInt176(value) == value, "sol value");
        require(feBench.benchEchoInt176(value) == value, "fe value");
    }
}
