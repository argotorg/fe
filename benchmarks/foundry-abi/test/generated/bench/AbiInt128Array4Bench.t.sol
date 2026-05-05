// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt128Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoInt128Array4() public {
        int128[4] memory value = [int128(-7), int128(0), int128(-1), type(int128).min];
        int128[4] memory solValue = solBench.benchEchoInt128Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        int128[4] memory feValue = feBench.benchEchoInt128Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
