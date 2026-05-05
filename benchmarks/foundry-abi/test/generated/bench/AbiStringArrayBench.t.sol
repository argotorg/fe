// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiStringArrayBenchTest is AbiRoundtripBase {
    function testBenchEchoStringArray() public {
        string[] memory value = new string[](2);
        value[0] = "bench alpha with extra payload bytes";
        value[1] = "bench beta with extra payload bytes";
        for (uint256 i = 0; i < value.length; i++) {
            assumeShortString(value[i]);
        }
        string[] memory solValue = solBench.benchEchoStringArray(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        string[] memory feValue = feBench.benchEchoStringArray(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
