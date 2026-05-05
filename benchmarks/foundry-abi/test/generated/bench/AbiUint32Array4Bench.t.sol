// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint32Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoUint32Array4() public {
        uint32[4] memory value = [uint32(123), uint32(0), uint32(1), type(uint32).max];
        uint32[4] memory solValue = solBench.benchEchoUint32Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        uint32[4] memory feValue = feBench.benchEchoUint32Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
