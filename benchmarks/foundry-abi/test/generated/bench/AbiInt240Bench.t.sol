// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt240BenchTest is AbiRoundtripBase {
    function testBenchEchoInt240() public {
        int240 value = int240(-7);
        require(solBench.benchEchoInt240(value) == value, "sol value");
        require(feBench.benchEchoInt240(value) == value, "fe value");
    }
}
