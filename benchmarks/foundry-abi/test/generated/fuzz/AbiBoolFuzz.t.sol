// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiBoolFuzzTest is AbiRoundtripBase {
    function testEchoBoolFuzz(bool value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBool.selector, value);
        assertEquivalent(callData);
    }
}
