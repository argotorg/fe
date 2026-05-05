// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt24FuzzTest is AbiRoundtripBase {
    function testEchoInt24Fuzz(int24 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt24.selector, value);
        assertEquivalent(callData);
    }
}
