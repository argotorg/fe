// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {BoolAddressU256Triple} from "../../../src/AbiRoundtripSol.sol";

contract AbiTripleBoolAddressU256Array4BenchTest is AbiRoundtripBase {
    function testBenchEchoBoolAddressU256TripleArray4() public {
        BoolAddressU256Triple[4] memory value = [BoolAddressU256Triple({flag: true, addr: address(0x6000000000000000000000000000000000000006), count: uint256(123456789)}), BoolAddressU256Triple({flag: false, addr: address(0), count: uint256(0)}), BoolAddressU256Triple({flag: true, addr: address(0x5000000000000000000000000000000000000005), count: type(uint256).max}), BoolAddressU256Triple({flag: true, addr: address(0x6000000000000000000000000000000000000006), count: uint256(123456789)})];
        BoolAddressU256Triple[4] memory solValue = solBench.benchEchoBoolAddressU256TripleArray4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        BoolAddressU256Triple[4] memory feValue = feBench.benchEchoBoolAddressU256TripleArray4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
