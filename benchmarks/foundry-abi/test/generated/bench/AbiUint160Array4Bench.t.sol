// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint160Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoUint160Array4() public {
        uint160[4] memory value = [uint160(123), uint160(0), uint160(1), type(uint160).max];
        uint160[4] memory solValue = solBench.benchEchoUint160Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        uint160[4] memory feValue = feBench.benchEchoUint160Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
