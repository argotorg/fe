// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt40Array4DeterministicTest is AbiRoundtripBase {
    function testEchoInt40Array4Deterministic0() public {
        int40[4] memory value = [int40(0), int40(-1), type(int40).min, type(int40).max];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt40Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt40Array4Deterministic1() public {
        int40[4] memory value = [int40(-1), type(int40).min, type(int40).max, int40(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt40Array4.selector, value);
        assertEquivalent(callData);
    }
}
