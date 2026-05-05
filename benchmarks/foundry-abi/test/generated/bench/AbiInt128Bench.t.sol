// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt128BenchTest is AbiRoundtripBase {
    function testBenchEchoInt128() public {
        int128 value = int128(-7);
        require(solBench.benchEchoInt128(value) == value, "sol value");
        require(feBench.benchEchoInt128(value) == value, "fe value");
    }
}
