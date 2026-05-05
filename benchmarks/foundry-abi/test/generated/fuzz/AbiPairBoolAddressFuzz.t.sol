// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, BoolAddressPair} from "../../../src/AbiRoundtripSol.sol";

contract AbiPairBoolAddressFuzzTest is AbiRoundtripBase {
    function testEchoBoolAddressPairFuzz(bool flag, address addr) public {
        BoolAddressPair memory value = BoolAddressPair({flag: flag, addr: addr});
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolAddressPair.selector, value);
        assertEquivalent(callData);
    }
}
