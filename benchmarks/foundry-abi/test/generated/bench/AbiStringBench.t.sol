// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiStringBenchTest is AbiRoundtripBase {
    function testBenchEchoString() public {
        string memory value = string("benchmark string payload that exceeds thirty-two bytes");
        assumeShortString(value);
        string memory solValue = solBench.benchEchoString(value);
        require(keccak256(bytes(solValue)) == keccak256(bytes(value)), "sol value");
        string memory feValue = feBench.benchEchoString(value);
        require(keccak256(bytes(feValue)) == keccak256(bytes(value)), "fe value");
    }
}
