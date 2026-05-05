// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt56FuzzTest is AbiRoundtripBase {
    function testEchoInt56Fuzz(int56 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt56.selector, value);
        assertEquivalent(callData);
    }
}
