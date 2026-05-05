// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint32Array4FuzzTest is AbiRoundtripBase {
    function testEchoUint32Array4Fuzz(uint32[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint32Array4.selector, value);
        assertEquivalent(callData);
    }
}
