// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt72BenchTest is AbiRoundtripBase {
    function testBenchEchoInt72() public {
        int72 value = int72(-7);
        require(solBench.benchEchoInt72(value) == value, "sol value");
        require(feBench.benchEchoInt72(value) == value, "fe value");
    }
}
