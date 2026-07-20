// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, BoolAddressPair} from "../../../src/AbiRoundtripSol.sol";

contract AbiPairBoolAddressArray4DeterministicTest is AbiRoundtripBase {
    function testEchoBoolAddressPairArray4Deterministic0() public {
        BoolAddressPair[4] memory value = [BoolAddressPair({flag: false, addr: address(0)}), BoolAddressPair({flag: true, addr: address(0x3000000000000000000000000000000000000003)}), BoolAddressPair({flag: false, addr: address(0)}), BoolAddressPair({flag: true, addr: address(0x3000000000000000000000000000000000000003)})];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolAddressPairArray4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoBoolAddressPairArray4Deterministic1() public {
        BoolAddressPair[4] memory value = [BoolAddressPair({flag: true, addr: address(0x3000000000000000000000000000000000000003)}), BoolAddressPair({flag: false, addr: address(0)}), BoolAddressPair({flag: true, addr: address(0x3000000000000000000000000000000000000003)}), BoolAddressPair({flag: false, addr: address(0)})];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolAddressPairArray4.selector, value);
        assertEquivalent(callData);
    }
}
