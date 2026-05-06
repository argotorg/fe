// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt16Array4DeterministicTest is AbiRoundtripBase {
    function testEchoInt16Array4Deterministic0() public {
        int16[4] memory value = [int16(0), int16(-1), type(int16).min, type(int16).max];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt16Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt16Array4Deterministic1() public {
        int16[4] memory value = [int16(-1), type(int16).min, type(int16).max, int16(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt16Array4.selector, value);
        assertEquivalent(callData);
    }
}
