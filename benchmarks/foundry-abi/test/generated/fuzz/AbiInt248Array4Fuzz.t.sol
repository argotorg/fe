// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt248Array4FuzzTest is AbiRoundtripBase {
    function testEchoInt248Array4Fuzz(int248[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt248Array4.selector, value);
        assertEquivalent(callData);
    }
}
