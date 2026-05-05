// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt160BenchTest is AbiRoundtripBase {
    function testBenchEchoInt160() public {
        int160 value = int160(-7);
        require(solBench.benchEchoInt160(value) == value, "sol value");
        require(feBench.benchEchoInt160(value) == value, "fe value");
    }
}
