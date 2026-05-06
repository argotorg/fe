// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint256Matrix2x2BenchTest is AbiRoundtripBase {
    function testBenchEchoUintMatrix2x2() public {
        uint256[2][2] memory value = [[uint256(77), uint256(0)], [uint256(1), type(uint256).max]];
        uint256[2][2] memory solValue = solBench.benchEchoUintMatrix2x2(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        uint256[2][2] memory feValue = feBench.benchEchoUintMatrix2x2(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
