// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {BoolAddressU256Triple} from "../../../src/AbiRoundtripSol.sol";

contract AbiTripleBoolAddressU256BenchTest is AbiRoundtripBase {
    function testBenchEchoBoolAddressU256Triple() public {
        BoolAddressU256Triple memory value = BoolAddressU256Triple({flag: true, addr: address(0x6000000000000000000000000000000000000006), count: uint256(123456789)});
        BoolAddressU256Triple memory solValue = solBench.benchEchoBoolAddressU256Triple(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        BoolAddressU256Triple memory feValue = feBench.benchEchoBoolAddressU256Triple(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
