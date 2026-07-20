// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt144BenchTest is AbiRoundtripBase {
    function testBenchEchoInt144() public {
        int144 value = int144(-7);
        require(solBench.benchEchoInt144(value) == value, "sol value");
        require(feBench.benchEchoInt144(value) == value, "fe value");
    }
}
