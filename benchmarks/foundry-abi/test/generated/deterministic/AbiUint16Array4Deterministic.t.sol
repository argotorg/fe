// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint16Array4DeterministicTest is AbiRoundtripBase {
    function testEchoUint16Array4Deterministic0() public {
        uint16[4] memory value = [uint16(0), uint16(1), type(uint16).max, uint16(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint16Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint16Array4Deterministic1() public {
        uint16[4] memory value = [uint16(1), type(uint16).max, uint16(0), uint16(1)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint16Array4.selector, value);
        assertEquivalent(callData);
    }
}
