// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt256BenchTest is AbiRoundtripBase {
    function testBenchEchoInt256() public {
        int256 value = int256(-7);
        require(solBench.benchEchoInt256(value) == value, "sol value");
        require(feBench.benchEchoInt256(value) == value, "fe value");
    }
}
