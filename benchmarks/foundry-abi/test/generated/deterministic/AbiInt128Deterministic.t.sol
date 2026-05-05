// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt128DeterministicTest is AbiRoundtripBase {
    function testEchoInt128Deterministic0() public {
        int128 value = int128(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt128.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt128Deterministic1() public {
        int128 value = int128(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt128.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt128Deterministic2() public {
        int128 value = type(int128).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt128.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt128Deterministic3() public {
        int128 value = type(int128).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt128.selector, value);
        assertEquivalent(callData);
    }
}
