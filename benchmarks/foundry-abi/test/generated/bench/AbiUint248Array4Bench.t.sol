// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint248Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoUint248Array4() public {
        uint248[4] memory value = [uint248(123), uint248(0), uint248(1), type(uint248).max];
        uint248[4] memory solValue = solBench.benchEchoUint248Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        uint248[4] memory feValue = feBench.benchEchoUint248Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
