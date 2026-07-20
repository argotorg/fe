// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt152FuzzTest is AbiRoundtripBase {
    function testEchoInt152Fuzz(int152 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt152.selector, value);
        assertEquivalent(callData);
    }
}
