// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {BoolAddressPair} from "../../../src/AbiRoundtripSol.sol";

contract AbiPairBoolAddressArray4BenchTest is AbiRoundtripBase {
    function testBenchEchoBoolAddressPairArray4() public {
        BoolAddressPair[4] memory value = [BoolAddressPair({flag: true, addr: address(0x4000000000000000000000000000000000000004)}), BoolAddressPair({flag: false, addr: address(0)}), BoolAddressPair({flag: true, addr: address(0x3000000000000000000000000000000000000003)}), BoolAddressPair({flag: true, addr: address(0x4000000000000000000000000000000000000004)})];
        BoolAddressPair[4] memory solValue = solBench.benchEchoBoolAddressPairArray4(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");
        BoolAddressPair[4] memory feValue = feBench.benchEchoBoolAddressPairArray4(value);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");
    }
}
