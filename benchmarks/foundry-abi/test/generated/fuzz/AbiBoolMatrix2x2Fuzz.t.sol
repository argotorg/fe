// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiBoolMatrix2x2FuzzTest is AbiRoundtripBase {
    function testEchoBoolMatrix2x2Fuzz(bool[2][2] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoBoolMatrix2x2.selector, value);
        assertEquivalent(callData);
    }
}
