// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint208BenchTest is AbiRoundtripBase {
    function testBenchEchoUint208() public {
        uint208 value = uint208(123);
        require(solBench.benchEchoUint208(value) == value, "sol value");
        require(feBench.benchEchoUint208(value) == value, "fe value");
    }
}
