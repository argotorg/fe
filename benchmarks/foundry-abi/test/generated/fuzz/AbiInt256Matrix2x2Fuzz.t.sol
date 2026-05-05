// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt256Matrix2x2FuzzTest is AbiRoundtripBase {
    function testEchoInt256Matrix2x2Fuzz(int256[2][2] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt256Matrix2x2.selector, value);
        assertEquivalent(callData);
    }
}
