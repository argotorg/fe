// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt48DeterministicTest is AbiRoundtripBase {
    function testEchoInt48Deterministic0() public {
        int48 value = int48(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt48.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt48Deterministic1() public {
        int48 value = int48(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt48.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt48Deterministic2() public {
        int48 value = type(int48).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt48.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt48Deterministic3() public {
        int48 value = type(int48).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt48.selector, value);
        assertEquivalent(callData);
    }
}
