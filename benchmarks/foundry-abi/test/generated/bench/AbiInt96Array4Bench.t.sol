// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiInt96Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoInt96Array4() public {
        int96[4] memory value = [int96(-7), int96(0), int96(-1), type(int96).min];
        int96[4] memory solValue = solBench.benchEchoInt96Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        int96[4] memory feValue = feBench.benchEchoInt96Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
