// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint24Array4DeterministicTest is AbiRoundtripBase {
    function testEchoUint24Array4Deterministic0() public {
        uint24[4] memory value = [uint24(0), uint24(1), type(uint24).max, uint24(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint24Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint24Array4Deterministic1() public {
        uint24[4] memory value = [uint24(1), type(uint24).max, uint24(0), uint24(1)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint24Array4.selector, value);
        assertEquivalent(callData);
    }
}
