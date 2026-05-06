// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, BoolAddressU256Triple} from "../../../src/AbiRoundtripSol.sol";

contract AbiTripleBoolAddressU256Array4FuzzTest is AbiRoundtripBase {
    function testEchoBoolAddressU256TripleArray4Fuzz(BoolAddressU256Triple[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolAddressU256TripleArray4.selector, value);
        assertEquivalent(callData);
    }
}
