// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiBoolArray4DeterministicTest is AbiRoundtripBase {
    function testEchoBoolArray4Deterministic0() public {
        bool[4] memory value = [false, true, false, true];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolArray4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoBoolArray4Deterministic1() public {
        bool[4] memory value = [true, false, true, false];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolArray4.selector, value);
        assertEquivalent(callData);
    }
}
