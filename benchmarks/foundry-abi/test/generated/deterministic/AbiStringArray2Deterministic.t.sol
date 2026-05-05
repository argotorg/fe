// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiStringArray2DeterministicTest is AbiRoundtripBase {
    function testEchoStringArray2Deterministic0() public {
        string[2] memory value = ["", "hello"];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringArray2.selector, value);
        assertEquivalent(callData);
    }

    function testEchoStringArray2Deterministic1() public {
        string[2] memory value = ["0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", "roundtrip"];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringArray2.selector, value);
        assertEquivalent(callData);
    }
}
