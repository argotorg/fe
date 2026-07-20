// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt8Array4DeterministicTest is AbiRoundtripBase {
    function testEchoInt8Array4Deterministic0() public {
        int8[4] memory value = [int8(0), int8(-1), type(int8).min, type(int8).max];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt8Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt8Array4Deterministic1() public {
        int8[4] memory value = [int8(-1), type(int8).min, type(int8).max, int8(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt8Array4.selector, value);
        assertEquivalent(callData);
    }
}
