// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, Uint24Int40Pair} from "../../../src/AbiRoundtripSol.sol";

contract AbiPairUint24Int40Array4FuzzTest is AbiRoundtripBase {
    function testEchoUint24Int40PairArray4Fuzz(Uint24Int40Pair[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint24Int40PairArray4.selector, value);
        assertEquivalent(callData);
    }
}
