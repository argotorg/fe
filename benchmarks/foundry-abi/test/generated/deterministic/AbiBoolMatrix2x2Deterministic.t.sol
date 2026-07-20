// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiBoolMatrix2x2DeterministicTest is AbiRoundtripBase {
    function testEchoBoolMatrix2x2Deterministic0() public {
        bool[2][2] memory value = [[false, true], [false, true]];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolMatrix2x2.selector, value);
        assertEquivalent(callData);
    }

    function testEchoBoolMatrix2x2Deterministic1() public {
        bool[2][2] memory value = [[true, false], [true, false]];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolMatrix2x2.selector, value);
        assertEquivalent(callData);
    }
}
