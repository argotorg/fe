// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt160Array4FuzzTest is AbiRoundtripBase {
    function testEchoInt160Array4Fuzz(int160[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt160Array4.selector, value);
        assertEquivalent(callData);
    }
}
