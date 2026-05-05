// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, StringU64Pair} from "../../../src/AbiRoundtripSol.sol";

contract AbiStringU64PairArrayFuzzTest is AbiRoundtripBase {
    function testEchoStringU64PairArrayFuzz(StringU64Pair[] memory value) public {
        vm.assume(value.length <= 4);
        for (uint256 i = 0; i < value.length; i++) {
            assumeShortString(value[i].text);
        }
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringU64PairArray.selector, value);
        assertEquivalent(callData);
    }
}
