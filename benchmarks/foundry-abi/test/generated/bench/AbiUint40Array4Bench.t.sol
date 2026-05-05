// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint40Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoUint40Array4() public {
        uint40[4] memory value = [uint40(123), uint40(0), uint40(1), type(uint40).max];
        uint40[4] memory solValue = solBench.benchEchoUint40Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        uint40[4] memory feValue = feBench.benchEchoUint40Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
