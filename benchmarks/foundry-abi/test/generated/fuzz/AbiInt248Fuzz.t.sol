// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt248FuzzTest is AbiRoundtripBase {
    function testEchoInt248Fuzz(int248 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt248.selector, value);
        assertEquivalent(callData);
    }
}
