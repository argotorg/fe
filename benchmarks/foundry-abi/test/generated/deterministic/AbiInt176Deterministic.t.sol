// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt176DeterministicTest is AbiRoundtripBase {
    function testEchoInt176Deterministic0() public {
        int176 value = int176(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt176.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt176Deterministic1() public {
        int176 value = int176(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt176.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt176Deterministic2() public {
        int176 value = type(int176).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt176.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt176Deterministic3() public {
        int176 value = type(int176).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt176.selector, value);
        assertEquivalent(callData);
    }
}
