// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiAddressArray4BenchTest is AbiRoundtripBase {
    function testBenchEchoAddressArray4() public {
        address[4] memory value = [address(0x2000000000000000000000000000000000000002), address(0), address(0x1000000000000000000000000000000000000001), address(0xFFfFfFffFFfffFFfFFfFFFFFffFFFffffFfFFFfF)];
        address[4] memory solValue = solBench.benchEchoAddressArray4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        address[4] memory feValue = feBench.benchEchoAddressArray4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
