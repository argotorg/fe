// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt176FuzzTest is AbiRoundtripBase {
    function testEchoInt176Fuzz(int176 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt176.selector, value);
        assertEquivalent(callData);
    }
}
