// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, BoolAddressU256Triple} from "../../../src/AbiRoundtripSol.sol";

contract AbiTripleBoolAddressU256FuzzTest is AbiRoundtripBase {
    function testEchoBoolAddressU256TripleFuzz(bool flag, address addr, uint256 count) public {
        BoolAddressU256Triple memory value = BoolAddressU256Triple({flag: flag, addr: addr, count: count});
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolAddressU256Triple.selector, value);
        assertEquivalent(callData);
    }
}
