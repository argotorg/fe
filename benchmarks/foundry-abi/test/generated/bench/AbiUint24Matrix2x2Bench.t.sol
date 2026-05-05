// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint24Matrix2x2BenchTest is AbiRoundtripBase {
    function testBenchEchoUint24Matrix2x2() public {
        uint24[2][2] memory value = [[uint24(123), uint24(0)], [uint24(1), type(uint24).max]];
        uint24[2][2] memory solValue = solBench.benchEchoUint24Matrix2x2(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        uint24[2][2] memory feValue = feBench.benchEchoUint24Matrix2x2(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
