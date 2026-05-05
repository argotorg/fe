// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

struct StringU64Pair {
    string text;
    uint64 count;
}

struct BoolAddressPair {
    bool flag;
    address addr;
}

struct Uint24Int40Pair {
    uint24 left;
    int40 right;
}

struct BoolAddressU256Triple {
    bool flag;
    address addr;
    uint256 count;
}

struct StringBoolU64Triple {
    string text;
    bool flag;
    uint64 count;
}

interface IAbiRoundtrip {
    function echoBool(bool value) external returns (bool);
    function echoAddress(address value) external returns (address);
    function echoString(string calldata value) external returns (string memory);
    function echoUint8(uint8 value) external returns (uint8);
    function echoUint16(uint16 value) external returns (uint16);
    function echoUint32(uint32 value) external returns (uint32);
    function echoUint64(uint64 value) external returns (uint64);
    function echoUint128(uint128 value) external returns (uint128);
    function echoUint(uint256 value) external returns (uint256);
    function echoInt8(int8 value) external returns (int8);
    function echoInt16(int16 value) external returns (int16);
    function echoInt32(int32 value) external returns (int32);
    function echoInt64(int64 value) external returns (int64);
    function echoInt128(int128 value) external returns (int128);
    function echoInt256(int256 value) external returns (int256);
    function echoUint24(uint24 value) external returns (uint24);
    function echoUint40(uint40 value) external returns (uint40);
    function echoUint48(uint48 value) external returns (uint48);
    function echoUint56(uint56 value) external returns (uint56);
    function echoUint72(uint72 value) external returns (uint72);
    function echoUint80(uint80 value) external returns (uint80);
    function echoUint88(uint88 value) external returns (uint88);
    function echoUint96(uint96 value) external returns (uint96);
    function echoUint104(uint104 value) external returns (uint104);
    function echoUint112(uint112 value) external returns (uint112);
    function echoUint120(uint120 value) external returns (uint120);
    function echoUint136(uint136 value) external returns (uint136);
    function echoUint144(uint144 value) external returns (uint144);
    function echoUint152(uint152 value) external returns (uint152);
    function echoUint160(uint160 value) external returns (uint160);
    function echoUint168(uint168 value) external returns (uint168);
    function echoUint176(uint176 value) external returns (uint176);
    function echoUint184(uint184 value) external returns (uint184);
    function echoUint192(uint192 value) external returns (uint192);
    function echoUint200(uint200 value) external returns (uint200);
    function echoUint208(uint208 value) external returns (uint208);
    function echoUint216(uint216 value) external returns (uint216);
    function echoUint224(uint224 value) external returns (uint224);
    function echoUint232(uint232 value) external returns (uint232);
    function echoUint240(uint240 value) external returns (uint240);
    function echoUint248(uint248 value) external returns (uint248);
    function echoInt24(int24 value) external returns (int24);
    function echoInt40(int40 value) external returns (int40);
    function echoInt48(int48 value) external returns (int48);
    function echoInt56(int56 value) external returns (int56);
    function echoInt72(int72 value) external returns (int72);
    function echoInt80(int80 value) external returns (int80);
    function echoInt88(int88 value) external returns (int88);
    function echoInt96(int96 value) external returns (int96);
    function echoInt104(int104 value) external returns (int104);
    function echoInt112(int112 value) external returns (int112);
    function echoInt120(int120 value) external returns (int120);
    function echoInt136(int136 value) external returns (int136);
    function echoInt144(int144 value) external returns (int144);
    function echoInt152(int152 value) external returns (int152);
    function echoInt160(int160 value) external returns (int160);
    function echoInt168(int168 value) external returns (int168);
    function echoInt176(int176 value) external returns (int176);
    function echoInt184(int184 value) external returns (int184);
    function echoInt192(int192 value) external returns (int192);
    function echoInt200(int200 value) external returns (int200);
    function echoInt208(int208 value) external returns (int208);
    function echoInt216(int216 value) external returns (int216);
    function echoInt224(int224 value) external returns (int224);
    function echoInt232(int232 value) external returns (int232);
    function echoInt240(int240 value) external returns (int240);
    function echoInt248(int248 value) external returns (int248);
    function echoBoolArray4(bool[4] calldata value) external returns (bool[4] memory);
    function echoAddressArray4(address[4] calldata value) external returns (address[4] memory);
    function echoUint8Array4(uint8[4] calldata value) external returns (uint8[4] memory);
    function echoUint16Array4(uint16[4] calldata value) external returns (uint16[4] memory);
    function echoUint32Array4(uint32[4] calldata value) external returns (uint32[4] memory);
    function echoUint64Array4(uint64[4] calldata value) external returns (uint64[4] memory);
    function echoUint128Array4(uint128[4] calldata value) external returns (uint128[4] memory);
    function echoUintArray4(uint256[4] calldata value) external returns (uint256[4] memory);
    function echoInt8Array4(int8[4] calldata value) external returns (int8[4] memory);
    function echoInt16Array4(int16[4] calldata value) external returns (int16[4] memory);
    function echoInt32Array4(int32[4] calldata value) external returns (int32[4] memory);
    function echoInt64Array4(int64[4] calldata value) external returns (int64[4] memory);
    function echoInt128Array4(int128[4] calldata value) external returns (int128[4] memory);
    function echoInt256Array4(int256[4] calldata value) external returns (int256[4] memory);
    function echoUint24Array4(uint24[4] calldata value) external returns (uint24[4] memory);
    function echoUint40Array4(uint40[4] calldata value) external returns (uint40[4] memory);
    function echoUint96Array4(uint96[4] calldata value) external returns (uint96[4] memory);
    function echoUint160Array4(uint160[4] calldata value) external returns (uint160[4] memory);
    function echoUint248Array4(uint248[4] calldata value) external returns (uint248[4] memory);
    function echoInt24Array4(int24[4] calldata value) external returns (int24[4] memory);
    function echoInt40Array4(int40[4] calldata value) external returns (int40[4] memory);
    function echoInt96Array4(int96[4] calldata value) external returns (int96[4] memory);
    function echoInt160Array4(int160[4] calldata value) external returns (int160[4] memory);
    function echoInt248Array4(int248[4] calldata value) external returns (int248[4] memory);
    function echoBoolMatrix2x2(bool[2][2] calldata value) external returns (bool[2][2] memory);
    function echoAddressMatrix2x2(address[2][2] calldata value) external returns (address[2][2] memory);
    function echoUintMatrix2x2(uint256[2][2] calldata value) external returns (uint256[2][2] memory);
    function echoInt256Matrix2x2(int256[2][2] calldata value) external returns (int256[2][2] memory);
    function echoUint24Matrix2x2(uint24[2][2] calldata value) external returns (uint24[2][2] memory);
    function echoInt40Matrix2x2(int40[2][2] calldata value) external returns (int40[2][2] memory);
    function echoBoolAddressPairArray4(BoolAddressPair[4] calldata value) external returns (BoolAddressPair[4] memory);
    function echoUint24Int40PairArray4(Uint24Int40Pair[4] calldata value) external returns (Uint24Int40Pair[4] memory);
    function echoBoolAddressU256TripleArray4(BoolAddressU256Triple[4] calldata value) external returns (BoolAddressU256Triple[4] memory);
    function echoStringArray2(string[2] calldata value) external returns (string[2] memory);
    function echoStringU64PairArray2(StringU64Pair[2] calldata value) external returns (StringU64Pair[2] memory);
    function echoUintArray(uint256[] calldata value) external returns (uint256[] memory);
    function echoBoolAddressPairArray(BoolAddressPair[] calldata value) external returns (BoolAddressPair[] memory);
    function echoStringArray(string[] calldata value) external returns (string[] memory);
    function echoStringU64PairArray(StringU64Pair[] calldata value) external returns (StringU64Pair[] memory);
    function echoPair(StringU64Pair calldata value) external returns (StringU64Pair memory);
    function echoBoolAddressPair(BoolAddressPair calldata value) external returns (BoolAddressPair memory);
    function echoUint24Int40Pair(Uint24Int40Pair calldata value) external returns (Uint24Int40Pair memory);
    function echoBoolAddressU256Triple(BoolAddressU256Triple calldata value) external returns (BoolAddressU256Triple memory);
    function echoStringBoolU64Triple(StringBoolU64Triple calldata value) external returns (StringBoolU64Triple memory);
}

contract AbiRoundtripSol is IAbiRoundtrip {
    function echoBool(bool value) external pure returns (bool) {
        return value;
    }

    function echoAddress(address value) external pure returns (address) {
        return value;
    }

    function echoString(string calldata value) external pure returns (string memory) {
        return value;
    }

    function echoUint8(uint8 value) external pure returns (uint8) {
        return value;
    }

    function echoUint16(uint16 value) external pure returns (uint16) {
        return value;
    }

    function echoUint32(uint32 value) external pure returns (uint32) {
        return value;
    }

    function echoUint64(uint64 value) external pure returns (uint64) {
        return value;
    }

    function echoUint128(uint128 value) external pure returns (uint128) {
        return value;
    }

    function echoUint(uint256 value) external pure returns (uint256) {
        return value;
    }

    function echoInt8(int8 value) external pure returns (int8) {
        return value;
    }

    function echoInt16(int16 value) external pure returns (int16) {
        return value;
    }

    function echoInt32(int32 value) external pure returns (int32) {
        return value;
    }

    function echoInt64(int64 value) external pure returns (int64) {
        return value;
    }

    function echoInt128(int128 value) external pure returns (int128) {
        return value;
    }

    function echoInt256(int256 value) external pure returns (int256) {
        return value;
    }

    function echoUint24(uint24 value) external pure returns (uint24) {
        return value;
    }

    function echoUint40(uint40 value) external pure returns (uint40) {
        return value;
    }

    function echoUint48(uint48 value) external pure returns (uint48) {
        return value;
    }

    function echoUint56(uint56 value) external pure returns (uint56) {
        return value;
    }

    function echoUint72(uint72 value) external pure returns (uint72) {
        return value;
    }

    function echoUint80(uint80 value) external pure returns (uint80) {
        return value;
    }

    function echoUint88(uint88 value) external pure returns (uint88) {
        return value;
    }

    function echoUint96(uint96 value) external pure returns (uint96) {
        return value;
    }

    function echoUint104(uint104 value) external pure returns (uint104) {
        return value;
    }

    function echoUint112(uint112 value) external pure returns (uint112) {
        return value;
    }

    function echoUint120(uint120 value) external pure returns (uint120) {
        return value;
    }

    function echoUint136(uint136 value) external pure returns (uint136) {
        return value;
    }

    function echoUint144(uint144 value) external pure returns (uint144) {
        return value;
    }

    function echoUint152(uint152 value) external pure returns (uint152) {
        return value;
    }

    function echoUint160(uint160 value) external pure returns (uint160) {
        return value;
    }

    function echoUint168(uint168 value) external pure returns (uint168) {
        return value;
    }

    function echoUint176(uint176 value) external pure returns (uint176) {
        return value;
    }

    function echoUint184(uint184 value) external pure returns (uint184) {
        return value;
    }

    function echoUint192(uint192 value) external pure returns (uint192) {
        return value;
    }

    function echoUint200(uint200 value) external pure returns (uint200) {
        return value;
    }

    function echoUint208(uint208 value) external pure returns (uint208) {
        return value;
    }

    function echoUint216(uint216 value) external pure returns (uint216) {
        return value;
    }

    function echoUint224(uint224 value) external pure returns (uint224) {
        return value;
    }

    function echoUint232(uint232 value) external pure returns (uint232) {
        return value;
    }

    function echoUint240(uint240 value) external pure returns (uint240) {
        return value;
    }

    function echoUint248(uint248 value) external pure returns (uint248) {
        return value;
    }

    function echoInt24(int24 value) external pure returns (int24) {
        return value;
    }

    function echoInt40(int40 value) external pure returns (int40) {
        return value;
    }

    function echoInt48(int48 value) external pure returns (int48) {
        return value;
    }

    function echoInt56(int56 value) external pure returns (int56) {
        return value;
    }

    function echoInt72(int72 value) external pure returns (int72) {
        return value;
    }

    function echoInt80(int80 value) external pure returns (int80) {
        return value;
    }

    function echoInt88(int88 value) external pure returns (int88) {
        return value;
    }

    function echoInt96(int96 value) external pure returns (int96) {
        return value;
    }

    function echoInt104(int104 value) external pure returns (int104) {
        return value;
    }

    function echoInt112(int112 value) external pure returns (int112) {
        return value;
    }

    function echoInt120(int120 value) external pure returns (int120) {
        return value;
    }

    function echoInt136(int136 value) external pure returns (int136) {
        return value;
    }

    function echoInt144(int144 value) external pure returns (int144) {
        return value;
    }

    function echoInt152(int152 value) external pure returns (int152) {
        return value;
    }

    function echoInt160(int160 value) external pure returns (int160) {
        return value;
    }

    function echoInt168(int168 value) external pure returns (int168) {
        return value;
    }

    function echoInt176(int176 value) external pure returns (int176) {
        return value;
    }

    function echoInt184(int184 value) external pure returns (int184) {
        return value;
    }

    function echoInt192(int192 value) external pure returns (int192) {
        return value;
    }

    function echoInt200(int200 value) external pure returns (int200) {
        return value;
    }

    function echoInt208(int208 value) external pure returns (int208) {
        return value;
    }

    function echoInt216(int216 value) external pure returns (int216) {
        return value;
    }

    function echoInt224(int224 value) external pure returns (int224) {
        return value;
    }

    function echoInt232(int232 value) external pure returns (int232) {
        return value;
    }

    function echoInt240(int240 value) external pure returns (int240) {
        return value;
    }

    function echoInt248(int248 value) external pure returns (int248) {
        return value;
    }

    function echoBoolArray4(bool[4] calldata value) external pure returns (bool[4] memory) {
        return value;
    }

    function echoAddressArray4(address[4] calldata value) external pure returns (address[4] memory) {
        return value;
    }

    function echoUint8Array4(uint8[4] calldata value) external pure returns (uint8[4] memory) {
        return value;
    }

    function echoUint16Array4(uint16[4] calldata value) external pure returns (uint16[4] memory) {
        return value;
    }

    function echoUint32Array4(uint32[4] calldata value) external pure returns (uint32[4] memory) {
        return value;
    }

    function echoUint64Array4(uint64[4] calldata value) external pure returns (uint64[4] memory) {
        return value;
    }

    function echoUint128Array4(uint128[4] calldata value) external pure returns (uint128[4] memory) {
        return value;
    }

    function echoUintArray4(uint256[4] calldata value) external pure returns (uint256[4] memory) {
        return value;
    }

    function echoInt8Array4(int8[4] calldata value) external pure returns (int8[4] memory) {
        return value;
    }

    function echoInt16Array4(int16[4] calldata value) external pure returns (int16[4] memory) {
        return value;
    }

    function echoInt32Array4(int32[4] calldata value) external pure returns (int32[4] memory) {
        return value;
    }

    function echoInt64Array4(int64[4] calldata value) external pure returns (int64[4] memory) {
        return value;
    }

    function echoInt128Array4(int128[4] calldata value) external pure returns (int128[4] memory) {
        return value;
    }

    function echoInt256Array4(int256[4] calldata value) external pure returns (int256[4] memory) {
        return value;
    }

    function echoUint24Array4(uint24[4] calldata value) external pure returns (uint24[4] memory) {
        return value;
    }

    function echoUint40Array4(uint40[4] calldata value) external pure returns (uint40[4] memory) {
        return value;
    }

    function echoUint96Array4(uint96[4] calldata value) external pure returns (uint96[4] memory) {
        return value;
    }

    function echoUint160Array4(uint160[4] calldata value) external pure returns (uint160[4] memory) {
        return value;
    }

    function echoUint248Array4(uint248[4] calldata value) external pure returns (uint248[4] memory) {
        return value;
    }

    function echoInt24Array4(int24[4] calldata value) external pure returns (int24[4] memory) {
        return value;
    }

    function echoInt40Array4(int40[4] calldata value) external pure returns (int40[4] memory) {
        return value;
    }

    function echoInt96Array4(int96[4] calldata value) external pure returns (int96[4] memory) {
        return value;
    }

    function echoInt160Array4(int160[4] calldata value) external pure returns (int160[4] memory) {
        return value;
    }

    function echoInt248Array4(int248[4] calldata value) external pure returns (int248[4] memory) {
        return value;
    }

    function echoBoolMatrix2x2(bool[2][2] calldata value) external pure returns (bool[2][2] memory) {
        return value;
    }

    function echoAddressMatrix2x2(address[2][2] calldata value) external pure returns (address[2][2] memory) {
        return value;
    }

    function echoUintMatrix2x2(uint256[2][2] calldata value) external pure returns (uint256[2][2] memory) {
        return value;
    }

    function echoInt256Matrix2x2(int256[2][2] calldata value) external pure returns (int256[2][2] memory) {
        return value;
    }

    function echoUint24Matrix2x2(uint24[2][2] calldata value) external pure returns (uint24[2][2] memory) {
        return value;
    }

    function echoInt40Matrix2x2(int40[2][2] calldata value) external pure returns (int40[2][2] memory) {
        return value;
    }

    function echoBoolAddressPairArray4(BoolAddressPair[4] calldata value) external pure returns (BoolAddressPair[4] memory) {
        return value;
    }

    function echoUint24Int40PairArray4(Uint24Int40Pair[4] calldata value) external pure returns (Uint24Int40Pair[4] memory) {
        return value;
    }

    function echoBoolAddressU256TripleArray4(BoolAddressU256Triple[4] calldata value) external pure returns (BoolAddressU256Triple[4] memory) {
        return value;
    }

    function echoStringArray2(string[2] calldata value) external pure returns (string[2] memory) {
        return value;
    }

    function echoStringU64PairArray2(StringU64Pair[2] calldata value) external pure returns (StringU64Pair[2] memory) {
        return value;
    }

    function echoUintArray(uint256[] calldata value) external pure returns (uint256[] memory) {
        return value;
    }

    function echoBoolAddressPairArray(BoolAddressPair[] calldata value) external pure returns (BoolAddressPair[] memory) {
        return value;
    }

    function echoStringArray(string[] calldata value) external pure returns (string[] memory) {
        return value;
    }

    function echoStringU64PairArray(StringU64Pair[] calldata value) external pure returns (StringU64Pair[] memory) {
        return value;
    }

    function echoPair(StringU64Pair calldata value) external pure returns (StringU64Pair memory) {
        return value;
    }

    function echoBoolAddressPair(BoolAddressPair calldata value) external pure returns (BoolAddressPair memory) {
        return value;
    }

    function echoUint24Int40Pair(Uint24Int40Pair calldata value) external pure returns (Uint24Int40Pair memory) {
        return value;
    }

    function echoBoolAddressU256Triple(BoolAddressU256Triple calldata value) external pure returns (BoolAddressU256Triple memory) {
        return value;
    }

    function echoStringBoolU64Triple(StringBoolU64Triple calldata value) external pure returns (StringBoolU64Triple memory) {
        return value;
    }
}

contract SolBenchCaller {
    IAbiRoundtrip public immutable target;

    constructor(address target_) {
        target = IAbiRoundtrip(target_);
    }

    function benchEchoBool(bool value) external returns (bool) {
        return target.echoBool(value);
    }

    function benchEchoAddress(address value) external returns (address) {
        return target.echoAddress(value);
    }

    function benchEchoString(string calldata value) external returns (string memory) {
        return target.echoString(value);
    }

    function benchEchoUint8(uint8 value) external returns (uint8) {
        return target.echoUint8(value);
    }

    function benchEchoUint16(uint16 value) external returns (uint16) {
        return target.echoUint16(value);
    }

    function benchEchoUint32(uint32 value) external returns (uint32) {
        return target.echoUint32(value);
    }

    function benchEchoUint64(uint64 value) external returns (uint64) {
        return target.echoUint64(value);
    }

    function benchEchoUint128(uint128 value) external returns (uint128) {
        return target.echoUint128(value);
    }

    function benchEchoUint(uint256 value) external returns (uint256) {
        return target.echoUint(value);
    }

    function benchEchoInt8(int8 value) external returns (int8) {
        return target.echoInt8(value);
    }

    function benchEchoInt16(int16 value) external returns (int16) {
        return target.echoInt16(value);
    }

    function benchEchoInt32(int32 value) external returns (int32) {
        return target.echoInt32(value);
    }

    function benchEchoInt64(int64 value) external returns (int64) {
        return target.echoInt64(value);
    }

    function benchEchoInt128(int128 value) external returns (int128) {
        return target.echoInt128(value);
    }

    function benchEchoInt256(int256 value) external returns (int256) {
        return target.echoInt256(value);
    }

    function benchEchoUint24(uint24 value) external returns (uint24) {
        return target.echoUint24(value);
    }

    function benchEchoUint40(uint40 value) external returns (uint40) {
        return target.echoUint40(value);
    }

    function benchEchoUint48(uint48 value) external returns (uint48) {
        return target.echoUint48(value);
    }

    function benchEchoUint56(uint56 value) external returns (uint56) {
        return target.echoUint56(value);
    }

    function benchEchoUint72(uint72 value) external returns (uint72) {
        return target.echoUint72(value);
    }

    function benchEchoUint80(uint80 value) external returns (uint80) {
        return target.echoUint80(value);
    }

    function benchEchoUint88(uint88 value) external returns (uint88) {
        return target.echoUint88(value);
    }

    function benchEchoUint96(uint96 value) external returns (uint96) {
        return target.echoUint96(value);
    }

    function benchEchoUint104(uint104 value) external returns (uint104) {
        return target.echoUint104(value);
    }

    function benchEchoUint112(uint112 value) external returns (uint112) {
        return target.echoUint112(value);
    }

    function benchEchoUint120(uint120 value) external returns (uint120) {
        return target.echoUint120(value);
    }

    function benchEchoUint136(uint136 value) external returns (uint136) {
        return target.echoUint136(value);
    }

    function benchEchoUint144(uint144 value) external returns (uint144) {
        return target.echoUint144(value);
    }

    function benchEchoUint152(uint152 value) external returns (uint152) {
        return target.echoUint152(value);
    }

    function benchEchoUint160(uint160 value) external returns (uint160) {
        return target.echoUint160(value);
    }

    function benchEchoUint168(uint168 value) external returns (uint168) {
        return target.echoUint168(value);
    }

    function benchEchoUint176(uint176 value) external returns (uint176) {
        return target.echoUint176(value);
    }

    function benchEchoUint184(uint184 value) external returns (uint184) {
        return target.echoUint184(value);
    }

    function benchEchoUint192(uint192 value) external returns (uint192) {
        return target.echoUint192(value);
    }

    function benchEchoUint200(uint200 value) external returns (uint200) {
        return target.echoUint200(value);
    }

    function benchEchoUint208(uint208 value) external returns (uint208) {
        return target.echoUint208(value);
    }

    function benchEchoUint216(uint216 value) external returns (uint216) {
        return target.echoUint216(value);
    }

    function benchEchoUint224(uint224 value) external returns (uint224) {
        return target.echoUint224(value);
    }

    function benchEchoUint232(uint232 value) external returns (uint232) {
        return target.echoUint232(value);
    }

    function benchEchoUint240(uint240 value) external returns (uint240) {
        return target.echoUint240(value);
    }

    function benchEchoUint248(uint248 value) external returns (uint248) {
        return target.echoUint248(value);
    }

    function benchEchoInt24(int24 value) external returns (int24) {
        return target.echoInt24(value);
    }

    function benchEchoInt40(int40 value) external returns (int40) {
        return target.echoInt40(value);
    }

    function benchEchoInt48(int48 value) external returns (int48) {
        return target.echoInt48(value);
    }

    function benchEchoInt56(int56 value) external returns (int56) {
        return target.echoInt56(value);
    }

    function benchEchoInt72(int72 value) external returns (int72) {
        return target.echoInt72(value);
    }

    function benchEchoInt80(int80 value) external returns (int80) {
        return target.echoInt80(value);
    }

    function benchEchoInt88(int88 value) external returns (int88) {
        return target.echoInt88(value);
    }

    function benchEchoInt96(int96 value) external returns (int96) {
        return target.echoInt96(value);
    }

    function benchEchoInt104(int104 value) external returns (int104) {
        return target.echoInt104(value);
    }

    function benchEchoInt112(int112 value) external returns (int112) {
        return target.echoInt112(value);
    }

    function benchEchoInt120(int120 value) external returns (int120) {
        return target.echoInt120(value);
    }

    function benchEchoInt136(int136 value) external returns (int136) {
        return target.echoInt136(value);
    }

    function benchEchoInt144(int144 value) external returns (int144) {
        return target.echoInt144(value);
    }

    function benchEchoInt152(int152 value) external returns (int152) {
        return target.echoInt152(value);
    }

    function benchEchoInt160(int160 value) external returns (int160) {
        return target.echoInt160(value);
    }

    function benchEchoInt168(int168 value) external returns (int168) {
        return target.echoInt168(value);
    }

    function benchEchoInt176(int176 value) external returns (int176) {
        return target.echoInt176(value);
    }

    function benchEchoInt184(int184 value) external returns (int184) {
        return target.echoInt184(value);
    }

    function benchEchoInt192(int192 value) external returns (int192) {
        return target.echoInt192(value);
    }

    function benchEchoInt200(int200 value) external returns (int200) {
        return target.echoInt200(value);
    }

    function benchEchoInt208(int208 value) external returns (int208) {
        return target.echoInt208(value);
    }

    function benchEchoInt216(int216 value) external returns (int216) {
        return target.echoInt216(value);
    }

    function benchEchoInt224(int224 value) external returns (int224) {
        return target.echoInt224(value);
    }

    function benchEchoInt232(int232 value) external returns (int232) {
        return target.echoInt232(value);
    }

    function benchEchoInt240(int240 value) external returns (int240) {
        return target.echoInt240(value);
    }

    function benchEchoInt248(int248 value) external returns (int248) {
        return target.echoInt248(value);
    }

    function benchEchoBoolArray4(bool[4] calldata value) external returns (bool[4] memory) {
        return target.echoBoolArray4(value);
    }

    function benchEchoAddressArray4(address[4] calldata value) external returns (address[4] memory) {
        return target.echoAddressArray4(value);
    }

    function benchEchoUint8Array4(uint8[4] calldata value) external returns (uint8[4] memory) {
        return target.echoUint8Array4(value);
    }

    function benchEchoUint16Array4(uint16[4] calldata value) external returns (uint16[4] memory) {
        return target.echoUint16Array4(value);
    }

    function benchEchoUint32Array4(uint32[4] calldata value) external returns (uint32[4] memory) {
        return target.echoUint32Array4(value);
    }

    function benchEchoUint64Array4(uint64[4] calldata value) external returns (uint64[4] memory) {
        return target.echoUint64Array4(value);
    }

    function benchEchoUint128Array4(uint128[4] calldata value) external returns (uint128[4] memory) {
        return target.echoUint128Array4(value);
    }

    function benchEchoUintArray4(uint256[4] calldata value) external returns (uint256[4] memory) {
        return target.echoUintArray4(value);
    }

    function benchEchoInt8Array4(int8[4] calldata value) external returns (int8[4] memory) {
        return target.echoInt8Array4(value);
    }

    function benchEchoInt16Array4(int16[4] calldata value) external returns (int16[4] memory) {
        return target.echoInt16Array4(value);
    }

    function benchEchoInt32Array4(int32[4] calldata value) external returns (int32[4] memory) {
        return target.echoInt32Array4(value);
    }

    function benchEchoInt64Array4(int64[4] calldata value) external returns (int64[4] memory) {
        return target.echoInt64Array4(value);
    }

    function benchEchoInt128Array4(int128[4] calldata value) external returns (int128[4] memory) {
        return target.echoInt128Array4(value);
    }

    function benchEchoInt256Array4(int256[4] calldata value) external returns (int256[4] memory) {
        return target.echoInt256Array4(value);
    }

    function benchEchoUint24Array4(uint24[4] calldata value) external returns (uint24[4] memory) {
        return target.echoUint24Array4(value);
    }

    function benchEchoUint40Array4(uint40[4] calldata value) external returns (uint40[4] memory) {
        return target.echoUint40Array4(value);
    }

    function benchEchoUint96Array4(uint96[4] calldata value) external returns (uint96[4] memory) {
        return target.echoUint96Array4(value);
    }

    function benchEchoUint160Array4(uint160[4] calldata value) external returns (uint160[4] memory) {
        return target.echoUint160Array4(value);
    }

    function benchEchoUint248Array4(uint248[4] calldata value) external returns (uint248[4] memory) {
        return target.echoUint248Array4(value);
    }

    function benchEchoInt24Array4(int24[4] calldata value) external returns (int24[4] memory) {
        return target.echoInt24Array4(value);
    }

    function benchEchoInt40Array4(int40[4] calldata value) external returns (int40[4] memory) {
        return target.echoInt40Array4(value);
    }

    function benchEchoInt96Array4(int96[4] calldata value) external returns (int96[4] memory) {
        return target.echoInt96Array4(value);
    }

    function benchEchoInt160Array4(int160[4] calldata value) external returns (int160[4] memory) {
        return target.echoInt160Array4(value);
    }

    function benchEchoInt248Array4(int248[4] calldata value) external returns (int248[4] memory) {
        return target.echoInt248Array4(value);
    }

    function benchEchoBoolMatrix2x2(bool[2][2] calldata value) external returns (bool[2][2] memory) {
        return target.echoBoolMatrix2x2(value);
    }

    function benchEchoAddressMatrix2x2(address[2][2] calldata value) external returns (address[2][2] memory) {
        return target.echoAddressMatrix2x2(value);
    }

    function benchEchoUintMatrix2x2(uint256[2][2] calldata value) external returns (uint256[2][2] memory) {
        return target.echoUintMatrix2x2(value);
    }

    function benchEchoInt256Matrix2x2(int256[2][2] calldata value) external returns (int256[2][2] memory) {
        return target.echoInt256Matrix2x2(value);
    }

    function benchEchoUint24Matrix2x2(uint24[2][2] calldata value) external returns (uint24[2][2] memory) {
        return target.echoUint24Matrix2x2(value);
    }

    function benchEchoInt40Matrix2x2(int40[2][2] calldata value) external returns (int40[2][2] memory) {
        return target.echoInt40Matrix2x2(value);
    }

    function benchEchoBoolAddressPairArray4(BoolAddressPair[4] calldata value) external returns (BoolAddressPair[4] memory) {
        return target.echoBoolAddressPairArray4(value);
    }

    function benchEchoUint24Int40PairArray4(Uint24Int40Pair[4] calldata value) external returns (Uint24Int40Pair[4] memory) {
        return target.echoUint24Int40PairArray4(value);
    }

    function benchEchoBoolAddressU256TripleArray4(BoolAddressU256Triple[4] calldata value) external returns (BoolAddressU256Triple[4] memory) {
        return target.echoBoolAddressU256TripleArray4(value);
    }

    function benchEchoStringArray2(string[2] calldata value) external returns (string[2] memory) {
        return target.echoStringArray2(value);
    }

    function benchEchoStringU64PairArray2(StringU64Pair[2] calldata value) external returns (StringU64Pair[2] memory) {
        return target.echoStringU64PairArray2(value);
    }

    function benchEchoUintArray(uint256[] calldata value) external returns (uint256[] memory) {
        return target.echoUintArray(value);
    }

    function benchEchoBoolAddressPairArray(BoolAddressPair[] calldata value) external returns (BoolAddressPair[] memory) {
        return target.echoBoolAddressPairArray(value);
    }

    function benchEchoStringArray(string[] calldata value) external returns (string[] memory) {
        return target.echoStringArray(value);
    }

    function benchEchoStringU64PairArray(StringU64Pair[] calldata value) external returns (StringU64Pair[] memory) {
        return target.echoStringU64PairArray(value);
    }

    function benchEchoPair(StringU64Pair calldata value) external returns (StringU64Pair memory) {
        return target.echoPair(value);
    }

    function benchEchoBoolAddressPair(BoolAddressPair calldata value) external returns (BoolAddressPair memory) {
        return target.echoBoolAddressPair(value);
    }

    function benchEchoUint24Int40Pair(Uint24Int40Pair calldata value) external returns (Uint24Int40Pair memory) {
        return target.echoUint24Int40Pair(value);
    }

    function benchEchoBoolAddressU256Triple(BoolAddressU256Triple calldata value) external returns (BoolAddressU256Triple memory) {
        return target.echoBoolAddressU256Triple(value);
    }

    function benchEchoStringBoolU64Triple(StringBoolU64Triple calldata value) external returns (StringBoolU64Triple memory) {
        return target.echoStringBoolU64Triple(value);
    }
}

contract FeBenchCaller {
    IAbiRoundtrip public immutable target;

    constructor(address target_) {
        target = IAbiRoundtrip(target_);
    }

    function benchEchoBool(bool value) external returns (bool) {
        return target.echoBool(value);
    }

    function benchEchoAddress(address value) external returns (address) {
        return target.echoAddress(value);
    }

    function benchEchoString(string calldata value) external returns (string memory) {
        return target.echoString(value);
    }

    function benchEchoUint8(uint8 value) external returns (uint8) {
        return target.echoUint8(value);
    }

    function benchEchoUint16(uint16 value) external returns (uint16) {
        return target.echoUint16(value);
    }

    function benchEchoUint32(uint32 value) external returns (uint32) {
        return target.echoUint32(value);
    }

    function benchEchoUint64(uint64 value) external returns (uint64) {
        return target.echoUint64(value);
    }

    function benchEchoUint128(uint128 value) external returns (uint128) {
        return target.echoUint128(value);
    }

    function benchEchoUint(uint256 value) external returns (uint256) {
        return target.echoUint(value);
    }

    function benchEchoInt8(int8 value) external returns (int8) {
        return target.echoInt8(value);
    }

    function benchEchoInt16(int16 value) external returns (int16) {
        return target.echoInt16(value);
    }

    function benchEchoInt32(int32 value) external returns (int32) {
        return target.echoInt32(value);
    }

    function benchEchoInt64(int64 value) external returns (int64) {
        return target.echoInt64(value);
    }

    function benchEchoInt128(int128 value) external returns (int128) {
        return target.echoInt128(value);
    }

    function benchEchoInt256(int256 value) external returns (int256) {
        return target.echoInt256(value);
    }

    function benchEchoUint24(uint24 value) external returns (uint24) {
        return target.echoUint24(value);
    }

    function benchEchoUint40(uint40 value) external returns (uint40) {
        return target.echoUint40(value);
    }

    function benchEchoUint48(uint48 value) external returns (uint48) {
        return target.echoUint48(value);
    }

    function benchEchoUint56(uint56 value) external returns (uint56) {
        return target.echoUint56(value);
    }

    function benchEchoUint72(uint72 value) external returns (uint72) {
        return target.echoUint72(value);
    }

    function benchEchoUint80(uint80 value) external returns (uint80) {
        return target.echoUint80(value);
    }

    function benchEchoUint88(uint88 value) external returns (uint88) {
        return target.echoUint88(value);
    }

    function benchEchoUint96(uint96 value) external returns (uint96) {
        return target.echoUint96(value);
    }

    function benchEchoUint104(uint104 value) external returns (uint104) {
        return target.echoUint104(value);
    }

    function benchEchoUint112(uint112 value) external returns (uint112) {
        return target.echoUint112(value);
    }

    function benchEchoUint120(uint120 value) external returns (uint120) {
        return target.echoUint120(value);
    }

    function benchEchoUint136(uint136 value) external returns (uint136) {
        return target.echoUint136(value);
    }

    function benchEchoUint144(uint144 value) external returns (uint144) {
        return target.echoUint144(value);
    }

    function benchEchoUint152(uint152 value) external returns (uint152) {
        return target.echoUint152(value);
    }

    function benchEchoUint160(uint160 value) external returns (uint160) {
        return target.echoUint160(value);
    }

    function benchEchoUint168(uint168 value) external returns (uint168) {
        return target.echoUint168(value);
    }

    function benchEchoUint176(uint176 value) external returns (uint176) {
        return target.echoUint176(value);
    }

    function benchEchoUint184(uint184 value) external returns (uint184) {
        return target.echoUint184(value);
    }

    function benchEchoUint192(uint192 value) external returns (uint192) {
        return target.echoUint192(value);
    }

    function benchEchoUint200(uint200 value) external returns (uint200) {
        return target.echoUint200(value);
    }

    function benchEchoUint208(uint208 value) external returns (uint208) {
        return target.echoUint208(value);
    }

    function benchEchoUint216(uint216 value) external returns (uint216) {
        return target.echoUint216(value);
    }

    function benchEchoUint224(uint224 value) external returns (uint224) {
        return target.echoUint224(value);
    }

    function benchEchoUint232(uint232 value) external returns (uint232) {
        return target.echoUint232(value);
    }

    function benchEchoUint240(uint240 value) external returns (uint240) {
        return target.echoUint240(value);
    }

    function benchEchoUint248(uint248 value) external returns (uint248) {
        return target.echoUint248(value);
    }

    function benchEchoInt24(int24 value) external returns (int24) {
        return target.echoInt24(value);
    }

    function benchEchoInt40(int40 value) external returns (int40) {
        return target.echoInt40(value);
    }

    function benchEchoInt48(int48 value) external returns (int48) {
        return target.echoInt48(value);
    }

    function benchEchoInt56(int56 value) external returns (int56) {
        return target.echoInt56(value);
    }

    function benchEchoInt72(int72 value) external returns (int72) {
        return target.echoInt72(value);
    }

    function benchEchoInt80(int80 value) external returns (int80) {
        return target.echoInt80(value);
    }

    function benchEchoInt88(int88 value) external returns (int88) {
        return target.echoInt88(value);
    }

    function benchEchoInt96(int96 value) external returns (int96) {
        return target.echoInt96(value);
    }

    function benchEchoInt104(int104 value) external returns (int104) {
        return target.echoInt104(value);
    }

    function benchEchoInt112(int112 value) external returns (int112) {
        return target.echoInt112(value);
    }

    function benchEchoInt120(int120 value) external returns (int120) {
        return target.echoInt120(value);
    }

    function benchEchoInt136(int136 value) external returns (int136) {
        return target.echoInt136(value);
    }

    function benchEchoInt144(int144 value) external returns (int144) {
        return target.echoInt144(value);
    }

    function benchEchoInt152(int152 value) external returns (int152) {
        return target.echoInt152(value);
    }

    function benchEchoInt160(int160 value) external returns (int160) {
        return target.echoInt160(value);
    }

    function benchEchoInt168(int168 value) external returns (int168) {
        return target.echoInt168(value);
    }

    function benchEchoInt176(int176 value) external returns (int176) {
        return target.echoInt176(value);
    }

    function benchEchoInt184(int184 value) external returns (int184) {
        return target.echoInt184(value);
    }

    function benchEchoInt192(int192 value) external returns (int192) {
        return target.echoInt192(value);
    }

    function benchEchoInt200(int200 value) external returns (int200) {
        return target.echoInt200(value);
    }

    function benchEchoInt208(int208 value) external returns (int208) {
        return target.echoInt208(value);
    }

    function benchEchoInt216(int216 value) external returns (int216) {
        return target.echoInt216(value);
    }

    function benchEchoInt224(int224 value) external returns (int224) {
        return target.echoInt224(value);
    }

    function benchEchoInt232(int232 value) external returns (int232) {
        return target.echoInt232(value);
    }

    function benchEchoInt240(int240 value) external returns (int240) {
        return target.echoInt240(value);
    }

    function benchEchoInt248(int248 value) external returns (int248) {
        return target.echoInt248(value);
    }

    function benchEchoBoolArray4(bool[4] calldata value) external returns (bool[4] memory) {
        return target.echoBoolArray4(value);
    }

    function benchEchoAddressArray4(address[4] calldata value) external returns (address[4] memory) {
        return target.echoAddressArray4(value);
    }

    function benchEchoUint8Array4(uint8[4] calldata value) external returns (uint8[4] memory) {
        return target.echoUint8Array4(value);
    }

    function benchEchoUint16Array4(uint16[4] calldata value) external returns (uint16[4] memory) {
        return target.echoUint16Array4(value);
    }

    function benchEchoUint32Array4(uint32[4] calldata value) external returns (uint32[4] memory) {
        return target.echoUint32Array4(value);
    }

    function benchEchoUint64Array4(uint64[4] calldata value) external returns (uint64[4] memory) {
        return target.echoUint64Array4(value);
    }

    function benchEchoUint128Array4(uint128[4] calldata value) external returns (uint128[4] memory) {
        return target.echoUint128Array4(value);
    }

    function benchEchoUintArray4(uint256[4] calldata value) external returns (uint256[4] memory) {
        return target.echoUintArray4(value);
    }

    function benchEchoInt8Array4(int8[4] calldata value) external returns (int8[4] memory) {
        return target.echoInt8Array4(value);
    }

    function benchEchoInt16Array4(int16[4] calldata value) external returns (int16[4] memory) {
        return target.echoInt16Array4(value);
    }

    function benchEchoInt32Array4(int32[4] calldata value) external returns (int32[4] memory) {
        return target.echoInt32Array4(value);
    }

    function benchEchoInt64Array4(int64[4] calldata value) external returns (int64[4] memory) {
        return target.echoInt64Array4(value);
    }

    function benchEchoInt128Array4(int128[4] calldata value) external returns (int128[4] memory) {
        return target.echoInt128Array4(value);
    }

    function benchEchoInt256Array4(int256[4] calldata value) external returns (int256[4] memory) {
        return target.echoInt256Array4(value);
    }

    function benchEchoUint24Array4(uint24[4] calldata value) external returns (uint24[4] memory) {
        return target.echoUint24Array4(value);
    }

    function benchEchoUint40Array4(uint40[4] calldata value) external returns (uint40[4] memory) {
        return target.echoUint40Array4(value);
    }

    function benchEchoUint96Array4(uint96[4] calldata value) external returns (uint96[4] memory) {
        return target.echoUint96Array4(value);
    }

    function benchEchoUint160Array4(uint160[4] calldata value) external returns (uint160[4] memory) {
        return target.echoUint160Array4(value);
    }

    function benchEchoUint248Array4(uint248[4] calldata value) external returns (uint248[4] memory) {
        return target.echoUint248Array4(value);
    }

    function benchEchoInt24Array4(int24[4] calldata value) external returns (int24[4] memory) {
        return target.echoInt24Array4(value);
    }

    function benchEchoInt40Array4(int40[4] calldata value) external returns (int40[4] memory) {
        return target.echoInt40Array4(value);
    }

    function benchEchoInt96Array4(int96[4] calldata value) external returns (int96[4] memory) {
        return target.echoInt96Array4(value);
    }

    function benchEchoInt160Array4(int160[4] calldata value) external returns (int160[4] memory) {
        return target.echoInt160Array4(value);
    }

    function benchEchoInt248Array4(int248[4] calldata value) external returns (int248[4] memory) {
        return target.echoInt248Array4(value);
    }

    function benchEchoBoolMatrix2x2(bool[2][2] calldata value) external returns (bool[2][2] memory) {
        return target.echoBoolMatrix2x2(value);
    }

    function benchEchoAddressMatrix2x2(address[2][2] calldata value) external returns (address[2][2] memory) {
        return target.echoAddressMatrix2x2(value);
    }

    function benchEchoUintMatrix2x2(uint256[2][2] calldata value) external returns (uint256[2][2] memory) {
        return target.echoUintMatrix2x2(value);
    }

    function benchEchoInt256Matrix2x2(int256[2][2] calldata value) external returns (int256[2][2] memory) {
        return target.echoInt256Matrix2x2(value);
    }

    function benchEchoUint24Matrix2x2(uint24[2][2] calldata value) external returns (uint24[2][2] memory) {
        return target.echoUint24Matrix2x2(value);
    }

    function benchEchoInt40Matrix2x2(int40[2][2] calldata value) external returns (int40[2][2] memory) {
        return target.echoInt40Matrix2x2(value);
    }

    function benchEchoBoolAddressPairArray4(BoolAddressPair[4] calldata value) external returns (BoolAddressPair[4] memory) {
        return target.echoBoolAddressPairArray4(value);
    }

    function benchEchoUint24Int40PairArray4(Uint24Int40Pair[4] calldata value) external returns (Uint24Int40Pair[4] memory) {
        return target.echoUint24Int40PairArray4(value);
    }

    function benchEchoBoolAddressU256TripleArray4(BoolAddressU256Triple[4] calldata value) external returns (BoolAddressU256Triple[4] memory) {
        return target.echoBoolAddressU256TripleArray4(value);
    }

    function benchEchoStringArray2(string[2] calldata value) external returns (string[2] memory) {
        return target.echoStringArray2(value);
    }

    function benchEchoStringU64PairArray2(StringU64Pair[2] calldata value) external returns (StringU64Pair[2] memory) {
        return target.echoStringU64PairArray2(value);
    }

    function benchEchoUintArray(uint256[] calldata value) external returns (uint256[] memory) {
        return target.echoUintArray(value);
    }

    function benchEchoBoolAddressPairArray(BoolAddressPair[] calldata value) external returns (BoolAddressPair[] memory) {
        return target.echoBoolAddressPairArray(value);
    }

    function benchEchoStringArray(string[] calldata value) external returns (string[] memory) {
        return target.echoStringArray(value);
    }

    function benchEchoStringU64PairArray(StringU64Pair[] calldata value) external returns (StringU64Pair[] memory) {
        return target.echoStringU64PairArray(value);
    }

    function benchEchoPair(StringU64Pair calldata value) external returns (StringU64Pair memory) {
        return target.echoPair(value);
    }

    function benchEchoBoolAddressPair(BoolAddressPair calldata value) external returns (BoolAddressPair memory) {
        return target.echoBoolAddressPair(value);
    }

    function benchEchoUint24Int40Pair(Uint24Int40Pair calldata value) external returns (Uint24Int40Pair memory) {
        return target.echoUint24Int40Pair(value);
    }

    function benchEchoBoolAddressU256Triple(BoolAddressU256Triple calldata value) external returns (BoolAddressU256Triple memory) {
        return target.echoBoolAddressU256Triple(value);
    }

    function benchEchoStringBoolU64Triple(StringBoolU64Triple calldata value) external returns (StringBoolU64Triple memory) {
        return target.echoStringBoolU64Triple(value);
    }
}
