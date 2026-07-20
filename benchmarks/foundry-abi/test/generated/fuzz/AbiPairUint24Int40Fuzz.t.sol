// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, Uint24Int40Pair} from "../../../src/AbiRoundtripSol.sol";

contract AbiPairUint24Int40FuzzTest is AbiRoundtripBase {
    function testEchoUint24Int40PairFuzz(uint24 left, int40 right) public {
        Uint24Int40Pair memory value = Uint24Int40Pair({left: left, right: right});
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint24Int40Pair.selector, value);
        assertEquivalent(callData);
    }
}
