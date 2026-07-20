// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {StringBoolU64Triple} from "../../../src/AbiRoundtripSol.sol";

contract AbiTripleStringBoolU64BenchTest is AbiRoundtripBase {
    function testBenchEchoStringBoolU64Triple() public {
        StringBoolU64Triple memory value = StringBoolU64Triple({text: "bench triple", flag: true, count: uint64(77)});
        assumeShortString(value.text);
        StringBoolU64Triple memory solValue = solBench.benchEchoStringBoolU64Triple(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        StringBoolU64Triple memory feValue = feBench.benchEchoStringBoolU64Triple(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
