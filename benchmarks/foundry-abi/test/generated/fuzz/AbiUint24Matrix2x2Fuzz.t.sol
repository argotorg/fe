// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint24Matrix2x2FuzzTest is AbiRoundtripBase {
    function testEchoUint24Matrix2x2Fuzz(uint24[2][2] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint24Matrix2x2.selector, value);
        assertEquivalent(callData);
    }
}
