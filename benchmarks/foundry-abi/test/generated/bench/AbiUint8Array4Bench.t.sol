// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint8Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoUint8Array4() public {
        uint8[4] memory value = [uint8(123), uint8(0), uint8(1), type(uint8).max];
        uint8[4] memory solValue = solBench.benchEchoUint8Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        uint8[4] memory feValue = feBench.benchEchoUint8Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
