// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint16Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoUint16Array4() public {
        uint16[4] memory value = [uint16(123), uint16(0), uint16(1), type(uint16).max];
        uint16[4] memory solValue = solBench.benchEchoUint16Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        uint16[4] memory feValue = feBench.benchEchoUint16Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
