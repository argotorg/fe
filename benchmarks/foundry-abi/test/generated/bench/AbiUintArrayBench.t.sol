// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUintArrayBenchTest is AbiRoundtripBase {
    function testBenchEchoUintArray() public {
        uint256[] memory value = new uint256[](3);
        value[0] = uint256(77);
        value[1] = uint256(1);
        value[2] = uint256(0);
        uint256[] memory solValue = solBench.benchEchoUintArray(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        uint256[] memory feValue = feBench.benchEchoUintArray(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
