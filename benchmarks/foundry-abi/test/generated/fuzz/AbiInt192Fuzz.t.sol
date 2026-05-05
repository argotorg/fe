// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt192FuzzTest is AbiRoundtripBase {
    function testEchoInt192Fuzz(int192 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt192.selector, value);
        assertEquivalent(callData);
    }
}
