// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiBoolArray4FuzzTest is AbiRoundtripBase {
    function testEchoBoolArray4Fuzz(bool[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolArray4.selector, value);
        assertEquivalent(callData);
    }
}
