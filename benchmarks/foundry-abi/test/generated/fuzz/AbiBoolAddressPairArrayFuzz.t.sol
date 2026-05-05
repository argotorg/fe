// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, BoolAddressPair} from "../../../src/AbiRoundtripSol.sol";

contract AbiBoolAddressPairArrayFuzzTest is AbiRoundtripBase {
    function testEchoBoolAddressPairArrayFuzz(BoolAddressPair[] memory value) public {
        vm.assume(value.length <= 4);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolAddressPairArray.selector, value);
        assertEquivalent(callData);
    }
}
