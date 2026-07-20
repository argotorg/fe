// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt64Array4DeterministicTest is AbiRoundtripBase {
    function testEchoInt64Array4Deterministic0() public {
        int64[4] memory value = [int64(0), int64(-1), type(int64).min, type(int64).max];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt64Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt64Array4Deterministic1() public {
        int64[4] memory value = [int64(-1), type(int64).min, type(int64).max, int64(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt64Array4.selector, value);
        assertEquivalent(callData);
    }
}
