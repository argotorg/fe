// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiStringDeterministicTest is AbiRoundtripBase {
    function testEchoStringDeterministic0() public {
        string memory value = string("");
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoString.selector, value);
        assertEquivalent(callData);
    }

    function testEchoStringDeterministic1() public {
        string memory value = string("hello roundtrip");
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoString.selector, value);
        assertEquivalent(callData);
    }

    function testEchoStringDeterministic2() public {
        string memory value = string("0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoString.selector, value);
        assertEquivalent(callData);
    }
}
