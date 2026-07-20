// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiStringArrayDeterministicTest is AbiRoundtripBase {
    function testEchoStringArrayDeterministic0() public {
        string[] memory value = new string[](0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringArray.selector, value);
        assertEquivalent(callData);
    }

    function testEchoStringArrayDeterministic1() public {
        string[] memory value = new string[](2);
        value[0] = "";
        value[1] = "hello dynamic with extra payload bytes";
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringArray.selector, value);
        assertEquivalent(callData);
    }

    function testEchoStringArrayDeterministic2() public {
        string[] memory value = new string[](2);
        value[0] = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
        value[1] = "tail";
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringArray.selector, value);
        assertEquivalent(callData);
    }
}
