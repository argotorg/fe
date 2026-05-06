// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt120FuzzTest is AbiRoundtripBase {
    function testEchoInt120Fuzz(int120 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt120.selector, value);
        assertEquivalent(callData);
    }
}
