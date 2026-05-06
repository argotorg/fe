// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint128Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoUint128Array4() public {
        uint128[4] memory value = [uint128(123), uint128(0), uint128(1), type(uint128).max];
        uint128[4] memory solValue = solBench.benchEchoUint128Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        uint128[4] memory feValue = feBench.benchEchoUint128Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
