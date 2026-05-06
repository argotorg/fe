// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt256FuzzTest is AbiRoundtripBase {
    function testEchoInt256Fuzz(int256 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt256.selector, value);
        assertEquivalent(callData);
    }
}
