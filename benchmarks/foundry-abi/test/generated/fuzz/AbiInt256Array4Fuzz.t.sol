// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt256Array4FuzzTest is AbiRoundtripBase {
    function testEchoInt256Array4Fuzz(int256[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt256Array4.selector, value);
        assertEquivalent(callData);
    }
}
