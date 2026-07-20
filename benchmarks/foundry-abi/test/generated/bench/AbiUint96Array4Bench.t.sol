// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint96Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoUint96Array4() public {
        uint96[4] memory value = [uint96(123), uint96(0), uint96(1), type(uint96).max];
        uint96[4] memory solValue = solBench.benchEchoUint96Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        uint96[4] memory feValue = feBench.benchEchoUint96Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
