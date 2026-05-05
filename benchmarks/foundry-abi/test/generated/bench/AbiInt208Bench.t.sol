// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt208BenchTest is AbiRoundtripBase {
    function testBenchEchoInt208() public {
        int208 value = int208(-7);
        require(solBench.benchEchoInt208(value) == value, "sol value");
        require(feBench.benchEchoInt208(value) == value, "fe value");
    }
}
