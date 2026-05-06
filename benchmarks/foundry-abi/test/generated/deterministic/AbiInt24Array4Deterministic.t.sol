// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt24Array4DeterministicTest is AbiRoundtripBase {
    function testEchoInt24Array4Deterministic0() public {
        int24[4] memory value = [int24(0), int24(-1), type(int24).min, type(int24).max];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt24Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt24Array4Deterministic1() public {
        int24[4] memory value = [int24(-1), type(int24).min, type(int24).max, int24(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt24Array4.selector, value);
        assertEquivalent(callData);
    }
}
