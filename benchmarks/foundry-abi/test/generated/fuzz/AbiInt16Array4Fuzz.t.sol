// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt16Array4FuzzTest is AbiRoundtripBase {
    function testEchoInt16Array4Fuzz(int16[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt16Array4.selector, value);
        assertEquivalent(callData);
    }
}
