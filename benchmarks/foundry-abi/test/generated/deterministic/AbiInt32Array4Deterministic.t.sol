// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt32Array4DeterministicTest is AbiRoundtripBase {
    function testEchoInt32Array4Deterministic0() public {
        int32[4] memory value = [int32(0), int32(-1), type(int32).min, type(int32).max];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt32Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt32Array4Deterministic1() public {
        int32[4] memory value = [int32(-1), type(int32).min, type(int32).max, int32(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt32Array4.selector, value);
        assertEquivalent(callData);
    }
}
