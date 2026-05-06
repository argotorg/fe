// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt200DeterministicTest is AbiRoundtripBase {
    function testEchoInt200Deterministic0() public {
        int200 value = int200(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt200.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt200Deterministic1() public {
        int200 value = int200(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt200.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt200Deterministic2() public {
        int200 value = type(int200).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt200.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt200Deterministic3() public {
        int200 value = type(int200).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt200.selector, value);
        assertEquivalent(callData);
    }
}
