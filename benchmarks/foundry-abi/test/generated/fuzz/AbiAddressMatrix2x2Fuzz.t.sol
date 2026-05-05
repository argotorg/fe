// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiAddressMatrix2x2FuzzTest is AbiRoundtripBase {
    function testEchoAddressMatrix2x2Fuzz(address[2][2] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoAddressMatrix2x2.selector, value);
        assertEquivalent(callData);
    }
}
