// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint160BenchTest is AbiRoundtripBase {
    function testBenchEchoUint160() public {
        uint160 value = uint160(123);
        require(solBench.benchEchoUint160(value) == value, "sol value");
        require(feBench.benchEchoUint160(value) == value, "fe value");
    }
}
