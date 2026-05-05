// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt104DeterministicTest is AbiRoundtripBase {
    function testEchoInt104Deterministic0() public {
        int104 value = int104(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt104.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt104Deterministic1() public {
        int104 value = int104(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt104.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt104Deterministic2() public {
        int104 value = type(int104).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt104.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt104Deterministic3() public {
        int104 value = type(int104).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt104.selector, value);
        assertEquivalent(callData);
    }
}
