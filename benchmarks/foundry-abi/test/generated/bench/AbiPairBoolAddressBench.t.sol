// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {BoolAddressPair} from "../../../src/AbiRoundtripSol.sol";

contract AbiPairBoolAddressBenchTest is AbiRoundtripBase {
    function testBenchEchoBoolAddressPair() public {
        BoolAddressPair memory value = BoolAddressPair({flag: true, addr: address(0x4000000000000000000000000000000000000004)});
        BoolAddressPair memory solValue = solBench.benchEchoBoolAddressPair(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        BoolAddressPair memory feValue = feBench.benchEchoBoolAddressPair(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
