// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt112DeterministicTest is AbiRoundtripBase {
    function testEchoInt112Deterministic0() public {
        int112 value = int112(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt112.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt112Deterministic1() public {
        int112 value = int112(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt112.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt112Deterministic2() public {
        int112 value = type(int112).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt112.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt112Deterministic3() public {
        int112 value = type(int112).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt112.selector, value);
        assertEquivalent(callData);
    }
}
