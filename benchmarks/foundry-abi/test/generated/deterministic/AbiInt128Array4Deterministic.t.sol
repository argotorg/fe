// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt128Array4DeterministicTest is AbiRoundtripBase {
    function testEchoInt128Array4Deterministic0() public {
        int128[4] memory value = [int128(0), int128(-1), type(int128).min, type(int128).max];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt128Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt128Array4Deterministic1() public {
        int128[4] memory value = [int128(-1), type(int128).min, type(int128).max, int128(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt128Array4.selector, value);
        assertEquivalent(callData);
    }
}
