// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiUint64Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoUint64Array4() public {
        uint64[4] memory value = [uint64(123), uint64(0), uint64(1), type(uint64).max];
        uint64[4] memory solValue = solBench.benchEchoUint64Array4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        uint64[4] memory feValue = feBench.benchEchoUint64Array4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
