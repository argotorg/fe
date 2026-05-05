// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt160Array4DeterministicTest is AbiRoundtripBase {
    function testEchoInt160Array4Deterministic0() public {
        int160[4] memory value = [int160(0), int160(-1), type(int160).min, type(int160).max];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt160Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt160Array4Deterministic1() public {
        int160[4] memory value = [int160(-1), type(int160).min, type(int160).max, int160(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt160Array4.selector, value);
        assertEquivalent(callData);
    }
}
