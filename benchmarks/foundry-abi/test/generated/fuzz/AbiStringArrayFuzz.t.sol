// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiStringArrayFuzzTest is AbiRoundtripBase {
    function testEchoStringArrayFuzz(string[] memory value) public {
        vm.assume(value.length <= 4);
        for (uint256 i = 0; i < value.length; i++) {
            assumeShortString(value[i]);
        }
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringArray.selector, value);
        assertEquivalent(callData);
    }
}
