// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt48BenchTest is AbiRoundtripBase {
    function testBenchEchoInt48() public {
        int48 value = int48(-7);
        require(solBench.benchEchoInt48(value) == value, "sol value");
        require(feBench.benchEchoInt48(value) == value, "fe value");
    }
}
