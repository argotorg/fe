// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt16FuzzTest is AbiRoundtripBase {
    function testEchoInt16Fuzz(int16 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt16.selector, value);
        assertEquivalent(callData);
    }
}
