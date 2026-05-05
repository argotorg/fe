// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt32Array4FuzzTest is AbiRoundtripBase {
    function testEchoInt32Array4Fuzz(int32[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt32Array4.selector, value);
        assertEquivalent(callData);
    }
}
