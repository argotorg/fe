// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt224FuzzTest is AbiRoundtripBase {
    function testEchoInt224Fuzz(int224 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt224.selector, value);
        assertEquivalent(callData);
    }
}
