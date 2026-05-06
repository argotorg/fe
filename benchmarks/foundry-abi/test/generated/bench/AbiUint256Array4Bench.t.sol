// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint256Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoUintArray4() public {
        uint256[4] memory value = [uint256(77), uint256(0), uint256(1), type(uint256).max];
        uint256[4] memory solValue = solBench.benchEchoUintArray4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        uint256[4] memory feValue = feBench.benchEchoUintArray4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
