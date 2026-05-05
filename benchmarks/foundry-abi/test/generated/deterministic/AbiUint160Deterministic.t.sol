// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint160DeterministicTest is AbiRoundtripBase {
    function testEchoUint160Deterministic0() public {
        uint160 value = uint160(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint160.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint160Deterministic1() public {
        uint160 value = uint160(1);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint160.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint160Deterministic2() public {
        uint160 value = type(uint160).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint160.selector, value);
        assertEquivalent(callData);
    }
}
