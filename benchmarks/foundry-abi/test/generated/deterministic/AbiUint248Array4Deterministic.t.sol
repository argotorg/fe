// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint248Array4DeterministicTest is AbiRoundtripBase {
    function testEchoUint248Array4Deterministic0() public {
        uint248[4] memory value = [uint248(0), uint248(1), type(uint248).max, uint248(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint248Array4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint248Array4Deterministic1() public {
        uint248[4] memory value = [uint248(1), type(uint248).max, uint248(0), uint248(1)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint248Array4.selector, value);
        assertEquivalent(callData);
    }
}
