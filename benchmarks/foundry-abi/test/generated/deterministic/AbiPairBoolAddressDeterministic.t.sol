// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, BoolAddressPair} from "../../../src/AbiRoundtripSol.sol";

contract AbiPairBoolAddressDeterministicTest is AbiRoundtripBase {
    function testEchoBoolAddressPairDeterministic0() public {
        BoolAddressPair memory value = BoolAddressPair({flag: false, addr: address(0)});
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolAddressPair.selector, value);
        assertEquivalent(callData);
    }

    function testEchoBoolAddressPairDeterministic1() public {
        BoolAddressPair memory value = BoolAddressPair({flag: true, addr: address(0x3000000000000000000000000000000000000003)});
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolAddressPair.selector, value);
        assertEquivalent(callData);
    }
}
