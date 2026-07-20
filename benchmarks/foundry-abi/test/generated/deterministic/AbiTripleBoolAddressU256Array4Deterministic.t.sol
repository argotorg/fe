// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, BoolAddressU256Triple} from "../../../src/AbiRoundtripSol.sol";

contract AbiTripleBoolAddressU256Array4DeterministicTest is AbiRoundtripBase {
    function testEchoBoolAddressU256TripleArray4Deterministic0() public {
        BoolAddressU256Triple[4] memory value = [BoolAddressU256Triple({flag: false, addr: address(0), count: uint256(0)}), BoolAddressU256Triple({flag: true, addr: address(0x5000000000000000000000000000000000000005), count: type(uint256).max}), BoolAddressU256Triple({flag: false, addr: address(0), count: uint256(0)}), BoolAddressU256Triple({flag: true, addr: address(0x5000000000000000000000000000000000000005), count: type(uint256).max})];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolAddressU256TripleArray4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoBoolAddressU256TripleArray4Deterministic1() public {
        BoolAddressU256Triple[4] memory value = [BoolAddressU256Triple({flag: true, addr: address(0x5000000000000000000000000000000000000005), count: type(uint256).max}), BoolAddressU256Triple({flag: false, addr: address(0), count: uint256(0)}), BoolAddressU256Triple({flag: true, addr: address(0x5000000000000000000000000000000000000005), count: type(uint256).max}), BoolAddressU256Triple({flag: false, addr: address(0), count: uint256(0)})];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolAddressU256TripleArray4.selector, value);
        assertEquivalent(callData);
    }
}
