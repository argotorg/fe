// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint8Array4FuzzTest is AbiRoundtripBase {
    function testEchoUint8Array4Fuzz(uint8[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint8Array4.selector, value);
        assertEquivalent(callData);
    }
}
