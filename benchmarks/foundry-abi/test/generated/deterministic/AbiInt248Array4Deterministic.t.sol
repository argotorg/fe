// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt248Array4DeterministicTest is AbiRoundtripBase {
    function testEchoInt248Array4Deterministic0() public {
        int248[4] memory value = [int248(0), int248(-1), type(int248).min, type(int248).max];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt248Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt248Array4Deterministic1() public {
        int248[4] memory value = [int248(-1), type(int248).min, type(int248).max, int248(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt248Array4.selector, value);
        assertEquivalent(callData);
    }
}
