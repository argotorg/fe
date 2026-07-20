// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint256Matrix2x2FuzzTest is AbiRoundtripBase {
    function testEchoUintMatrix2x2Fuzz(uint256[2][2] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUintMatrix2x2.selector, value);
        assertEquivalent(callData);
    }
}
