// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt224DeterministicTest is AbiRoundtripBase {
    function testEchoInt224Deterministic0() public {
        int224 value = int224(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt224.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt224Deterministic1() public {
        int224 value = int224(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt224.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt224Deterministic2() public {
        int224 value = type(int224).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt224.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt224Deterministic3() public {
        int224 value = type(int224).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt224.selector, value);
        assertEquivalent(callData);
    }
}
