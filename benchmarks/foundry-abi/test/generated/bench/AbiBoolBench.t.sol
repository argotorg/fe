// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiBoolBenchTest is AbiRoundtripBase {
    function testBenchEchoBool() public {
        bool value = true;
        require(solBench.benchEchoBool(value) == value, "sol value");
        require(feBench.benchEchoBool(value) == value, "fe value");
    }
}
