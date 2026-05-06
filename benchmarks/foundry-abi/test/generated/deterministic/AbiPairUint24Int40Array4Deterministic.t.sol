// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip, Uint24Int40Pair} from "../../../src/AbiRoundtripSol.sol";

contract AbiPairUint24Int40Array4DeterministicTest is AbiRoundtripBase {
    function testEchoUint24Int40PairArray4Deterministic0() public {
        Uint24Int40Pair[4] memory value = [Uint24Int40Pair({left: uint24(0), right: int40(0)}), Uint24Int40Pair({left: type(uint24).max, right: type(int40).min}), Uint24Int40Pair({left: uint24(0), right: int40(0)}), Uint24Int40Pair({left: type(uint24).max, right: type(int40).min})];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint24Int40PairArray4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint24Int40PairArray4Deterministic1() public {
        Uint24Int40Pair[4] memory value = [Uint24Int40Pair({left: type(uint24).max, right: type(int40).min}), Uint24Int40Pair({left: uint24(0), right: int40(0)}), Uint24Int40Pair({left: type(uint24).max, right: type(int40).min}), Uint24Int40Pair({left: uint24(0), right: int40(0)})];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint24Int40PairArray4.selector, value);
        assertEquivalent(callData);
    }
}
