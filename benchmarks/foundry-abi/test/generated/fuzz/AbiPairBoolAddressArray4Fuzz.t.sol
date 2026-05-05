// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, BoolAddressPair} from "../../../src/AbiRoundtripSol.sol";

contract AbiPairBoolAddressArray4FuzzTest is AbiRoundtripBase {
    function testEchoBoolAddressPairArray4Fuzz(BoolAddressPair[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolAddressPairArray4.selector, value);
        assertEquivalent(callData);
    }
}
