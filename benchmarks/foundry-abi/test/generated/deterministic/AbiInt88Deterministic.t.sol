// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt88DeterministicTest is AbiRoundtripBase {
    function testEchoInt88Deterministic0() public {
        int88 value = int88(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt88.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt88Deterministic1() public {
        int88 value = int88(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt88.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt88Deterministic2() public {
        int88 value = type(int88).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt88.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt88Deterministic3() public {
        int88 value = type(int88).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt88.selector, value);
        assertEquivalent(callData);
    }
}
