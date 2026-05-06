// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt24Array4FuzzTest is AbiRoundtripBase {
    function testEchoInt24Array4Fuzz(int24[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt24Array4.selector, value);
        assertEquivalent(callData);
    }
}
