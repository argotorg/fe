// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt96Array4DeterministicTest is AbiRoundtripBase {
    function testEchoInt96Array4Deterministic0() public {
        int96[4] memory value = [int96(0), int96(-1), type(int96).min, type(int96).max];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt96Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt96Array4Deterministic1() public {
        int96[4] memory value = [int96(-1), type(int96).min, type(int96).max, int96(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt96Array4.selector, value);
        assertEquivalent(callData);
    }
}
