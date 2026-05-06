// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint256Array4FuzzTest is AbiRoundtripBase {
    function testEchoUintArray4Fuzz(uint256[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUintArray4.selector, value);
        assertEquivalent(callData);
    }
}
