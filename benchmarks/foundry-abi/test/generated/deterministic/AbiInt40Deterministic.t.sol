// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt40DeterministicTest is AbiRoundtripBase {
    function testEchoInt40Deterministic0() public {
        int40 value = int40(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt40.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt40Deterministic1() public {
        int40 value = int40(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt40.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt40Deterministic2() public {
        int40 value = type(int40).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt40.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt40Deterministic3() public {
        int40 value = type(int40).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt40.selector, value);
        assertEquivalent(callData);
    }
}
