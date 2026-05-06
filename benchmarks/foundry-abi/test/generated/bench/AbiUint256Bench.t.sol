// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint256BenchTest is AbiRoundtripBase {
    function testBenchEchoUint() public {
        uint256 value = uint256(77);
        require(solBench.benchEchoUint(value) == value, "sol value");
        require(feBench.benchEchoUint(value) == value, "fe value");
    }
}
