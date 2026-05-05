// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiAddressMatrix2x2BenchTest is AbiRoundtripBase {
    function testBenchEchoAddressMatrix2x2() public {
        address[2][2] memory value = [[address(0x2000000000000000000000000000000000000002), address(0)], [address(0x1000000000000000000000000000000000000001), address(0xFFfFfFffFFfffFFfFFfFFFFFffFFFffffFfFFFfF)]];
        address[2][2] memory solValue = solBench.benchEchoAddressMatrix2x2(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        address[2][2] memory feValue = feBench.benchEchoAddressMatrix2x2(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
