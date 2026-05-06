// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint88BenchTest is AbiRoundtripBase {
    function testBenchEchoUint88() public {
        uint88 value = uint88(123);
        require(solBench.benchEchoUint88(value) == value, "sol value");
        require(feBench.benchEchoUint88(value) == value, "fe value");
    }
}
