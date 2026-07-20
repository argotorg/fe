// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt72DeterministicTest is AbiRoundtripBase {
    function testEchoInt72Deterministic0() public {
        int72 value = int72(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt72.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt72Deterministic1() public {
        int72 value = int72(-1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt72.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt72Deterministic2() public {
        int72 value = type(int72).min;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt72.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt72Deterministic3() public {
        int72 value = type(int72).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt72.selector, value);
        assertEquivalent(callData);
    }
}
