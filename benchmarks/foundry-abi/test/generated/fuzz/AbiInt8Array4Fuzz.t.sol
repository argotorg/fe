// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt8Array4FuzzTest is AbiRoundtripBase {
    function testEchoInt8Array4Fuzz(int8[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt8Array4.selector, value);
        assertEquivalent(callData);
    }
}
