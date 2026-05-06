// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt104FuzzTest is AbiRoundtripBase {
    function testEchoInt104Fuzz(int104 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt104.selector, value);
        assertEquivalent(callData);
    }
}
