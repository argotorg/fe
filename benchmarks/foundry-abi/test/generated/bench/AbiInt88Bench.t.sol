// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt88BenchTest is AbiRoundtripBase {
    function testBenchEchoInt88() public {
        int88 value = int88(-7);
        require(solBench.benchEchoInt88(value) == value, "sol value");
        require(feBench.benchEchoInt88(value) == value, "fe value");
    }
}
