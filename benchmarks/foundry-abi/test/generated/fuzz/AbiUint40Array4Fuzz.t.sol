// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint40Array4FuzzTest is AbiRoundtripBase {
    function testEchoUint40Array4Fuzz(uint40[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint40Array4.selector, value);
        assertEquivalent(callData);
    }
}
