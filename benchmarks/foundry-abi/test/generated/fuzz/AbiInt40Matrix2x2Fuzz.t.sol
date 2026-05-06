// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt40Matrix2x2FuzzTest is AbiRoundtripBase {
    function testEchoInt40Matrix2x2Fuzz(int40[2][2] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt40Matrix2x2.selector, value);
        assertEquivalent(callData);
    }
}
