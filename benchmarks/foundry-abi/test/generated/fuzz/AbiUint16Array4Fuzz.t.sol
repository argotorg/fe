// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint16Array4FuzzTest is AbiRoundtripBase {
    function testEchoUint16Array4Fuzz(uint16[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint16Array4.selector, value);
        assertEquivalent(callData);
    }
}
