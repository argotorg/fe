// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiBoolDeterministicTest is AbiRoundtripBase {
    function testEchoBoolDeterministic0() public {
        bool value = false;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBool.selector, value);
        assertEquivalent(callData);
    }

    function testEchoBoolDeterministic1() public {
        bool value = true;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBool.selector, value);
        assertEquivalent(callData);
    }
}
