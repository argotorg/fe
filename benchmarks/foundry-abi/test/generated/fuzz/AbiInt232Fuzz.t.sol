// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt232FuzzTest is AbiRoundtripBase {
    function testEchoInt232Fuzz(int232 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt232.selector, value);
        assertEquivalent(callData);
    }
}
