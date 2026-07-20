// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, BoolAddressPair} from "../../../src/AbiRoundtripSol.sol";

contract AbiBoolAddressPairArrayDeterministicTest is AbiRoundtripBase {
    function testEchoBoolAddressPairArrayDeterministic0() public {
        BoolAddressPair[] memory value = new BoolAddressPair[](0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolAddressPairArray.selector, value);
        assertEquivalent(callData);
    }

    function testEchoBoolAddressPairArrayDeterministic1() public {
        BoolAddressPair[] memory value = new BoolAddressPair[](2);
        value[0] = BoolAddressPair({flag: false, addr: address(0)});
        value[1] = BoolAddressPair({flag: true, addr: address(0x3000000000000000000000000000000000000003)});
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolAddressPairArray.selector, value);
        assertEquivalent(callData);
    }
}
