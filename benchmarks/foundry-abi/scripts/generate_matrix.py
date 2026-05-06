#!/usr/bin/env python3
from __future__ import annotations

import shutil
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
FE_PATH = ROOT / "fe" / "AbiRoundtrip.fe"
SOL_PATH = ROOT / "src" / "AbiRoundtripSol.sol"
GENERATED_ROOT = ROOT / "test" / "generated"
SUPPORT_DIR = GENERATED_ROOT / "support"
DETERMINISTIC_DIR = GENERATED_ROOT / "deterministic"
FUZZ_DIR = GENERATED_ROOT / "fuzz"
BENCH_DIR = GENERATED_ROOT / "bench"


@dataclass(frozen=True)
class TypeSpec:
    slug: str
    suffix: str
    sol_abi: str
    sol_param: str
    sol_return: str
    sol_local: str
    sol_struct_field: str
    fe_type: str
    compare_kind: str
    det_values: tuple[str, ...]
    bench_value: str
    assume_short_string: bool = False
    is_dynamic: bool = False


@dataclass(frozen=True)
class ScalarCase:
    slug: str
    fn_name: str
    bench_name: str
    variant_name: str
    ty: TypeSpec


@dataclass(frozen=True)
class TupleField:
    name: str
    ty: TypeSpec


@dataclass(frozen=True)
class TupleCase:
    slug: str
    fn_name: str
    bench_name: str
    variant_name: str
    struct_name: str
    value_name: str
    fields: tuple[TupleField, ...]
    det_values: tuple[dict[str, str], ...]
    bench_value: dict[str, str]


@dataclass(frozen=True)
class ArrayCase:
    slug: str
    fn_name: str
    bench_name: str
    variant_name: str
    sol_selector_sig: str
    sol_param: str
    sol_return: str
    sol_local: str
    fe_type: str
    det_values: tuple[str, ...]
    bench_value: str
    assume_suffixes: tuple[str, ...] = ()
    assume_loop_exprs: tuple[str, ...] = ()
    max_fuzz_len: int | None = None
    det_init_blocks: tuple[str, ...] = ()
    bench_init_block: str = ""
    import_structs: tuple[str, ...] = ()


def scalar(
    slug: str,
    suffix: str,
    sol_abi: str,
    fe_type: str,
    compare_kind: str,
    det_values: tuple[str, ...],
    bench_value: str,
    *,
    assume_short_string: bool = False,
    is_dynamic: bool = False,
) -> ScalarCase:
    if suffix == "Uint":
        fn_name = "echoUint"
        bench_name = "benchEchoUint"
        variant_name = "EchoUint"
    elif suffix == "String":
        fn_name = "echoString"
        bench_name = "benchEchoString"
        variant_name = "EchoString"
    else:
        fn_name = f"echo{suffix}"
        bench_name = f"benchEcho{suffix}"
        variant_name = f"Echo{suffix}"

    sol_param = "string calldata" if sol_abi == "string" else sol_abi
    sol_return = "string memory" if sol_abi == "string" else sol_abi
    sol_local = "string memory" if sol_abi == "string" else sol_abi
    sol_struct_field = "string" if sol_abi == "string" else sol_abi

    return ScalarCase(
        slug=slug,
        fn_name=fn_name,
        bench_name=bench_name,
        variant_name=variant_name,
        ty=TypeSpec(
            slug=slug,
            suffix=suffix,
            sol_abi=sol_abi,
            sol_param=sol_param,
            sol_return=sol_return,
            sol_local=sol_local,
            sol_struct_field=sol_struct_field,
            fe_type=fe_type,
            compare_kind=compare_kind,
            det_values=det_values,
            bench_value=bench_value,
            assume_short_string=assume_short_string,
            is_dynamic=is_dynamic,
        ),
    )


BOOL = TypeSpec(
    slug="bool",
    suffix="Bool",
    sol_abi="bool",
    sol_param="bool",
    sol_return="bool",
    sol_local="bool",
    sol_struct_field="bool",
    fe_type="bool",
    compare_kind="eq",
    det_values=("false", "true"),
    bench_value="true",
)

ADDRESS = TypeSpec(
    slug="address",
    suffix="Address",
    sol_abi="address",
    sol_param="address",
    sol_return="address",
    sol_local="address",
    sol_struct_field="address",
    fe_type="Address",
    compare_kind="eq",
    det_values=(
        "address(0)",
        "address(0x1000000000000000000000000000000000000001)",
        "address(0xFFfFfFffFFfffFFfFFfFFFFFffFFFffffFfFFFfF)",
    ),
    bench_value="address(0x2000000000000000000000000000000000000002)",
)

STRING = TypeSpec(
    slug="string",
    suffix="String",
    sol_abi="string",
    sol_param="string calldata",
    sol_return="string memory",
    sol_local="string memory",
    sol_struct_field="string",
    fe_type="DynString",
    compare_kind="string",
    det_values=(
        'string("")',
        'string("hello roundtrip")',
        'string("0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")',
    ),
    bench_value='string("benchmark string payload that exceeds thirty-two bytes")',
    assume_short_string=True,
    is_dynamic=True,
)

U64 = TypeSpec(
    slug="uint64",
    suffix="Uint64",
    sol_abi="uint64",
    sol_param="uint64",
    sol_return="uint64",
    sol_local="uint64",
    sol_struct_field="uint64",
    fe_type="u64",
    compare_kind="eq",
    det_values=("uint64(0)", "uint64(1)", "type(uint64).max"),
    bench_value="uint64(99)",
)

U256 = TypeSpec(
    slug="uint256",
    suffix="Uint",
    sol_abi="uint256",
    sol_param="uint256",
    sol_return="uint256",
    sol_local="uint256",
    sol_struct_field="uint256",
    fe_type="u256",
    compare_kind="eq",
    det_values=("uint256(0)", "uint256(1)", "type(uint256).max"),
    bench_value="uint256(77)",
)


def native_uint(width: int) -> ScalarCase:
    return scalar(
        slug=f"uint{width}",
        suffix=f"Uint{width}",
        sol_abi=f"uint{width}",
        fe_type=f"u{width}",
        compare_kind="eq",
        det_values=(f"uint{width}(0)", f"uint{width}(1)", f"type(uint{width}).max"),
        bench_value=f"uint{width}(123)",
    )


def native_int(width: int) -> ScalarCase:
    return scalar(
        slug=f"int{width}",
        suffix=f"Int{width}",
        sol_abi=f"int{width}",
        fe_type=f"i{width}",
        compare_kind="eq",
        det_values=(f"int{width}(0)", f"int{width}(-1)", f"type(int{width}).min", f"type(int{width}).max"),
        bench_value=f"int{width}(-7)",
    )


def custom_uint(width: int, fe_type: str) -> ScalarCase:
    return scalar(
        slug=f"uint{width}",
        suffix=f"Uint{width}",
        sol_abi=f"uint{width}",
        fe_type=fe_type,
        compare_kind="eq",
        det_values=(f"uint{width}(0)", f"uint{width}(1)", f"type(uint{width}).max"),
        bench_value=f"uint{width}(123)",
    )


def custom_int(width: int, fe_type: str) -> ScalarCase:
    return scalar(
        slug=f"int{width}",
        suffix=f"Int{width}",
        sol_abi=f"int{width}",
        fe_type=fe_type,
        compare_kind="eq",
        det_values=(f"int{width}(0)", f"int{width}(-1)", f"type(int{width}).min", f"type(int{width}).max"),
        bench_value=f"int{width}(-7)",
    )


SCALAR_CASES: list[ScalarCase] = [
    scalar("bool", "Bool", "bool", "bool", "eq", ("false", "true"), "true"),
    scalar("address", "Address", "address", "Address", "eq", ADDRESS.det_values, ADDRESS.bench_value),
    scalar(
        "string",
        "String",
        "string",
        "DynString",
        "string",
        STRING.det_values,
        STRING.bench_value,
        assume_short_string=True,
        is_dynamic=True,
    ),
    native_uint(8),
    native_uint(16),
    native_uint(32),
    native_uint(64),
    native_uint(128),
    scalar("uint256", "Uint", "uint256", "u256", "eq", U256.det_values, U256.bench_value),
    native_int(8),
    native_int(16),
    native_int(32),
    native_int(64),
    native_int(128),
    native_int(256),
]

for width in (24, 40, 48, 56, 72, 80, 88, 96, 104, 112, 120, 136, 144, 152, 160, 168, 176, 184, 192, 200, 208, 216, 224, 232, 240, 248):
    SCALAR_CASES.append(custom_uint(width, f"Uint{width}"))

for width in (24, 40, 48, 56, 72, 80, 88, 96, 104, 112, 120, 136, 144, 152, 160, 168, 176, 184, 192, 200, 208, 216, 224, 232, 240, 248):
    SCALAR_CASES.append(custom_int(width, f"Int{width}"))


TUPLE_CASES: list[TupleCase] = [
    TupleCase(
        slug="pair_string_u64",
        fn_name="echoPair",
        bench_name="benchEchoPair",
        variant_name="EchoPair",
        struct_name="StringU64Pair",
        value_name="pair",
        fields=(TupleField("text", STRING), TupleField("count", U64)),
        det_values=(
            {"text": '"pair payload"', "count": "uint64(42)"},
            {"text": '"0123456789abcdefghijklmnopqrstuv"', "count": "type(uint64).max"},
        ),
        bench_value={"text": '"bench pair"', "count": "uint64(99)"},
    ),
    TupleCase(
        slug="pair_bool_address",
        fn_name="echoBoolAddressPair",
        bench_name="benchEchoBoolAddressPair",
        variant_name="EchoBoolAddressPair",
        struct_name="BoolAddressPair",
        value_name="pair",
        fields=(TupleField("flag", BOOL), TupleField("addr", ADDRESS)),
        det_values=(
            {"flag": "false", "addr": "address(0)"},
            {"flag": "true", "addr": "address(0x3000000000000000000000000000000000000003)"},
        ),
        bench_value={"flag": "true", "addr": "address(0x4000000000000000000000000000000000000004)"},
    ),
    TupleCase(
        slug="pair_uint24_int40",
        fn_name="echoUint24Int40Pair",
        bench_name="benchEchoUint24Int40Pair",
        variant_name="EchoUint24Int40Pair",
        struct_name="Uint24Int40Pair",
        value_name="pair",
        fields=(
            TupleField("left", TypeSpec(
                slug="uint24",
                suffix="Uint24",
                sol_abi="uint24",
                sol_param="uint24",
                sol_return="uint24",
                sol_local="uint24",
                sol_struct_field="uint24",
                fe_type="Uint24",
                compare_kind="eq",
                det_values=("uint24(0)", "type(uint24).max"),
                bench_value="uint24(123)",
            )),
            TupleField("right", TypeSpec(
                slug="int40",
                suffix="Int40",
                sol_abi="int40",
                sol_param="int40",
                sol_return="int40",
                sol_local="int40",
                sol_struct_field="int40",
                fe_type="Int40",
                compare_kind="eq",
                det_values=("int40(0)", "type(int40).min", "type(int40).max"),
                bench_value="int40(-7)",
            )),
        ),
        det_values=(
            {"left": "uint24(0)", "right": "int40(0)"},
            {"left": "type(uint24).max", "right": "type(int40).min"},
        ),
        bench_value={"left": "uint24(123)", "right": "int40(-7)"},
    ),
    TupleCase(
        slug="triple_bool_address_u256",
        fn_name="echoBoolAddressU256Triple",
        bench_name="benchEchoBoolAddressU256Triple",
        variant_name="EchoBoolAddressU256Triple",
        struct_name="BoolAddressU256Triple",
        value_name="triple",
        fields=(TupleField("flag", BOOL), TupleField("addr", ADDRESS), TupleField("count", U256)),
        det_values=(
            {"flag": "false", "addr": "address(0)", "count": "uint256(0)"},
            {"flag": "true", "addr": "address(0x5000000000000000000000000000000000000005)", "count": "type(uint256).max"},
        ),
        bench_value={"flag": "true", "addr": "address(0x6000000000000000000000000000000000000006)", "count": "uint256(123456789)"},
    ),
    TupleCase(
        slug="triple_string_bool_u64",
        fn_name="echoStringBoolU64Triple",
        bench_name="benchEchoStringBoolU64Triple",
        variant_name="EchoStringBoolU64Triple",
        struct_name="StringBoolU64Triple",
        value_name="triple",
        fields=(TupleField("text", STRING), TupleField("flag", BOOL), TupleField("count", U64)),
        det_values=(
            {"text": '"hello"', "flag": "false", "count": "uint64(1)"},
            {"text": '"0123456789abcdefghijklmnopqrstuv"', "flag": "true", "count": "type(uint64).max"},
        ),
        bench_value={"text": '"bench triple"', "flag": "true", "count": "uint64(77)"},
    ),
]

def all_fe_custom_types() -> list[str]:
    out: set[str] = set()
    for case in SCALAR_CASES:
        if case.ty.fe_type.startswith(("Uint", "Int")):
            out.add(case.ty.fe_type)
    for case in TUPLE_CASES:
        for field in case.fields:
            if field.ty.fe_type.startswith(("Uint", "Int")):
                out.add(field.ty.fe_type)
    return sorted(out)


def fe_tuple_type(case: TupleCase) -> str:
    return "(" + ", ".join(field.ty.fe_type for field in case.fields) + ")"


def sol_tuple_signature(case: TupleCase) -> str:
    return "(" + ",".join(field.ty.sol_abi for field in case.fields) + ")"


def tuple_is_dynamic(case: TupleCase) -> bool:
    return any(field.ty.is_dynamic for field in case.fields)


def struct_literal(case: TupleCase, value: dict[str, str]) -> str:
    fields = ", ".join(f"{field.name}: {value[field.name]}" for field in case.fields)
    return f"{case.struct_name}({{{fields}}})"


def cycle_values(values: tuple[str, ...], count: int, *, offset: int = 0) -> list[str]:
    return [values[(offset + idx) % len(values)] for idx in range(count)]


def array_literal(values: list[str]) -> str:
    return "[" + ", ".join(values) + "]"


def matrix_literal(rows: list[list[str]]) -> str:
    return "[" + ", ".join(array_literal(row) for row in rows) + "]"


def indent_block(block: str, prefix: str = "        ") -> list[str]:
    return [prefix + line if line else "" for line in block.strip().splitlines()]


def scalar_array_case(case: ScalarCase, length: int) -> ArrayCase:
    suffix = f"{case.ty.suffix}Array{length}"
    bench_values = (case.ty.bench_value, *case.ty.det_values)
    return ArrayCase(
        slug=f"{case.slug}_array{length}",
        fn_name=f"echo{suffix}",
        bench_name=f"benchEcho{suffix}",
        variant_name=f"Echo{suffix}",
        sol_selector_sig=f"{case.ty.sol_abi}[{length}]",
        sol_param=f"{case.ty.sol_abi}[{length}] calldata",
        sol_return=f"{case.ty.sol_abi}[{length}] memory",
        sol_local=f"{case.ty.sol_abi}[{length}] memory",
        fe_type=f"[{case.ty.fe_type}; {length}]",
        det_values=(
            array_literal(cycle_values(case.ty.det_values, length, offset=0)),
            array_literal(cycle_values(case.ty.det_values, length, offset=1)),
        ),
        bench_value=array_literal(cycle_values(bench_values, length, offset=0)),
    )


def scalar_matrix_case(case: ScalarCase) -> ArrayCase:
    suffix = f"{case.ty.suffix}Matrix2x2"
    bench_values = (case.ty.bench_value, *case.ty.det_values)
    return ArrayCase(
        slug=f"{case.slug}_matrix2x2",
        fn_name=f"echo{suffix}",
        bench_name=f"benchEcho{suffix}",
        variant_name=f"Echo{suffix}",
        sol_selector_sig=f"{case.ty.sol_abi}[2][2]",
        sol_param=f"{case.ty.sol_abi}[2][2] calldata",
        sol_return=f"{case.ty.sol_abi}[2][2] memory",
        sol_local=f"{case.ty.sol_abi}[2][2] memory",
        fe_type=f"[[{case.ty.fe_type}; 2]; 2]",
        det_values=(
            matrix_literal(
                [
                    cycle_values(case.ty.det_values, 2, offset=0),
                    cycle_values(case.ty.det_values, 2, offset=2),
                ]
            ),
            matrix_literal(
                [
                    cycle_values(case.ty.det_values, 2, offset=1),
                    cycle_values(case.ty.det_values, 2, offset=3),
                ]
            ),
        ),
        bench_value=matrix_literal(
            [
                cycle_values(bench_values, 2, offset=0),
                cycle_values(bench_values, 2, offset=2),
            ]
        ),
    )


def tuple_array_case(case: TupleCase, length: int) -> ArrayCase:
    bench_values = (case.bench_value, *case.det_values)
    return ArrayCase(
        slug=f"{case.slug}_array{length}",
        fn_name=f"{case.fn_name}Array{length}",
        bench_name=f"{case.bench_name}Array{length}",
        variant_name=f"{case.variant_name}Array{length}",
        sol_selector_sig=f"{sol_tuple_signature(case)}[{length}]",
        sol_param=f"{case.struct_name}[{length}] calldata",
        sol_return=f"{case.struct_name}[{length}] memory",
        sol_local=f"{case.struct_name}[{length}] memory",
        fe_type=f"[{fe_tuple_type(case)}; {length}]",
        det_values=(
            array_literal(
                [struct_literal(case, value) for value in cycle_values(case.det_values, length, offset=0)]
            ),
            array_literal(
                [struct_literal(case, value) for value in cycle_values(case.det_values, length, offset=1)]
            ),
        ),
        bench_value=array_literal(
            [struct_literal(case, value) for value in cycle_values(bench_values, length, offset=0)]
        ),
        import_structs=(case.struct_name,),
    )


STATIC_SCALAR_CASES = [case for case in SCALAR_CASES if not case.ty.is_dynamic]
STATIC_TUPLE_CASES = [case for case in TUPLE_CASES if not tuple_is_dynamic(case)]
ARRAY_SCALAR_CASES = [
    case
    for case in STATIC_SCALAR_CASES
    if case.slug
    in {
        "bool",
        "address",
        "uint8",
        "uint16",
        "uint32",
        "uint64",
        "uint128",
        "uint256",
        "int8",
        "int16",
        "int32",
        "int64",
        "int128",
        "int256",
        "uint24",
        "uint40",
        "uint96",
        "uint160",
        "uint248",
        "int24",
        "int40",
        "int96",
        "int160",
        "int248",
    }
]
MATRIX_SCALAR_CASES = [
    case
    for case in STATIC_SCALAR_CASES
    if case.slug in {"bool", "address", "uint256", "int256", "uint24", "int40"}
]
DYNAMIC_ARRAY_CASES: list[ArrayCase] = [
    ArrayCase(
        slug="string_array2",
        fn_name="echoStringArray2",
        bench_name="benchEchoStringArray2",
        variant_name="EchoStringArray2",
        sol_selector_sig="string[2]",
        sol_param="string[2] calldata",
        sol_return="string[2] memory",
        sol_local="string[2] memory",
        fe_type="[DynString; 2]",
        det_values=(
            '["", "hello"]',
            '["0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", "roundtrip"]',
        ),
        bench_value='["bench alpha with extra payload bytes", "bench beta with extra payload bytes"]',
        assume_suffixes=("[0]", "[1]"),
    ),
    ArrayCase(
        slug="string_u64_pair_array2",
        fn_name="echoStringU64PairArray2",
        bench_name="benchEchoStringU64PairArray2",
        variant_name="EchoStringU64PairArray2",
        sol_selector_sig="(string,uint64)[2]",
        sol_param="StringU64Pair[2] calldata",
        sol_return="StringU64Pair[2] memory",
        sol_local="StringU64Pair[2] memory",
        fe_type="[(DynString, u64); 2]",
        det_values=(
            '[StringU64Pair({text: "pair-one", count: uint64(1)}), StringU64Pair({text: "pair-two", count: uint64(2)})]',
            '[StringU64Pair({text: "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", count: type(uint64).max}), StringU64Pair({text: "tail", count: uint64(9)})]',
        ),
        bench_value='[StringU64Pair({text: "bench-one-with-extra-payload", count: uint64(11)}), StringU64Pair({text: "bench-two-with-extra-payload", count: uint64(22)})]',
        assume_suffixes=("[0].text", "[1].text"),
        import_structs=("StringU64Pair",),
    ),
]

DYNAMIC_LENGTH_ARRAY_CASES: list[ArrayCase] = [
    ArrayCase(
        slug="uint_array",
        fn_name="echoUintArray",
        bench_name="benchEchoUintArray",
        variant_name="EchoUintArray",
        sol_selector_sig="uint256[]",
        sol_param="uint256[] calldata",
        sol_return="uint256[] memory",
        sol_local="uint256[] memory",
        fe_type="DynArray<u256>",
        det_values=(),
        bench_value="",
        max_fuzz_len=4,
        det_init_blocks=(
            """
uint256[] memory value = new uint256[](0);
""",
            """
uint256[] memory value = new uint256[](3);
value[0] = uint256(0);
value[1] = uint256(1);
value[2] = type(uint256).max;
""",
        ),
        bench_init_block="""
uint256[] memory value = new uint256[](3);
value[0] = uint256(77);
value[1] = uint256(1);
value[2] = uint256(0);
""",
    ),
    ArrayCase(
        slug="bool_address_pair_array",
        fn_name="echoBoolAddressPairArray",
        bench_name="benchEchoBoolAddressPairArray",
        variant_name="EchoBoolAddressPairArray",
        sol_selector_sig="(bool,address)[]",
        sol_param="BoolAddressPair[] calldata",
        sol_return="BoolAddressPair[] memory",
        sol_local="BoolAddressPair[] memory",
        fe_type="DynArray<(bool, Address)>",
        det_values=(),
        bench_value="",
        max_fuzz_len=4,
        det_init_blocks=(
            """
BoolAddressPair[] memory value = new BoolAddressPair[](0);
""",
            """
BoolAddressPair[] memory value = new BoolAddressPair[](2);
value[0] = BoolAddressPair({flag: false, addr: address(0)});
value[1] = BoolAddressPair({flag: true, addr: address(0x3000000000000000000000000000000000000003)});
""",
        ),
        bench_init_block="""
BoolAddressPair[] memory value = new BoolAddressPair[](2);
value[0] = BoolAddressPair({flag: true, addr: address(0x4000000000000000000000000000000000000004)});
value[1] = BoolAddressPair({flag: false, addr: address(0x5000000000000000000000000000000000000005)});
""",
        import_structs=("BoolAddressPair",),
    ),
    ArrayCase(
        slug="string_array",
        fn_name="echoStringArray",
        bench_name="benchEchoStringArray",
        variant_name="EchoStringArray",
        sol_selector_sig="string[]",
        sol_param="string[] calldata",
        sol_return="string[] memory",
        sol_local="string[] memory",
        fe_type="DynArray<DynString>",
        det_values=(),
        bench_value="",
        assume_loop_exprs=("value[i]",),
        max_fuzz_len=4,
        det_init_blocks=(
            """
string[] memory value = new string[](0);
""",
            """
string[] memory value = new string[](2);
value[0] = "";
value[1] = "hello dynamic with extra payload bytes";
""",
            """
string[] memory value = new string[](2);
value[0] = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
value[1] = "tail";
""",
        ),
        bench_init_block="""
string[] memory value = new string[](2);
value[0] = "bench alpha with extra payload bytes";
value[1] = "bench beta with extra payload bytes";
""",
    ),
    ArrayCase(
        slug="string_u64_pair_array",
        fn_name="echoStringU64PairArray",
        bench_name="benchEchoStringU64PairArray",
        variant_name="EchoStringU64PairArray",
        sol_selector_sig="(string,uint64)[]",
        sol_param="StringU64Pair[] calldata",
        sol_return="StringU64Pair[] memory",
        sol_local="StringU64Pair[] memory",
        fe_type="DynArray<(DynString, u64)>",
        det_values=(),
        bench_value="",
        assume_loop_exprs=("value[i].text",),
        max_fuzz_len=4,
        det_init_blocks=(
            """
StringU64Pair[] memory value = new StringU64Pair[](0);
""",
            """
StringU64Pair[] memory value = new StringU64Pair[](2);
value[0] = StringU64Pair({text: "pair-one", count: uint64(1)});
value[1] = StringU64Pair({text: "pair-two", count: uint64(2)});
""",
            """
StringU64Pair[] memory value = new StringU64Pair[](2);
value[0] = StringU64Pair({text: "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", count: type(uint64).max});
value[1] = StringU64Pair({text: "tail", count: uint64(9)});
""",
        ),
        bench_init_block="""
StringU64Pair[] memory value = new StringU64Pair[](2);
value[0] = StringU64Pair({text: "bench-one-with-extra-payload", count: uint64(11)});
value[1] = StringU64Pair({text: "bench-two-with-extra-payload", count: uint64(22)});
""",
        import_structs=("StringU64Pair",),
    ),
]


ARRAY_CASES: list[ArrayCase] = [
    *(scalar_array_case(case, 4) for case in ARRAY_SCALAR_CASES),
    *(scalar_matrix_case(case) for case in MATRIX_SCALAR_CASES),
    *(tuple_array_case(case, 4) for case in STATIC_TUPLE_CASES),
    *DYNAMIC_ARRAY_CASES,
    *DYNAMIC_LENGTH_ARRAY_CASES,
]


def tuple_return_signature(case: TupleCase) -> str:
    return f"{case.struct_name} memory"


def tuple_local_signature(case: TupleCase) -> str:
    return f"{case.struct_name} memory"


def tuple_return_expr(case: TupleCase) -> str:
    return "value"


def tuple_binding_decls(prefix: str, case: TupleCase) -> str:
    return ", ".join(
        f"{field.ty.sol_local} {prefix}{slug_to_pascal(field.name)}"
        for field in case.fields
    )


def render_fe() -> str:
    imports = all_fe_custom_types()
    lines: list[str] = ["use std::abi::{sol, DynArray, DynString}"]
    if imports:
        joined = ", ".join(imports)
        lines.append(f"use std::abi::sol::{{{joined}}}")
    lines.append("use std::evm::Address")
    lines.append("")
    lines.append("msg AbiRoundtripMsg {")
    for case in SCALAR_CASES:
        lines.append(f'    #[selector = sol("{case.fn_name}({case.ty.sol_abi})")]')
        lines.append(f"    {case.variant_name} {{ value: {case.ty.fe_type} }} -> {case.ty.fe_type},")
    for case in ARRAY_CASES:
        lines.append(f'    #[selector = sol("{case.fn_name}({case.sol_selector_sig})")]')
        lines.append(f"    {case.variant_name} {{ value: {case.fe_type} }} -> {case.fe_type},")
    for case in TUPLE_CASES:
        lines.append(f'    #[selector = sol("{case.fn_name}({sol_tuple_signature(case)})")]')
        lines.append(f"    {case.variant_name} {{ value: {fe_tuple_type(case)} }} -> {fe_tuple_type(case)},")
    lines.append("}")
    lines.append("")
    lines.append("pub contract AbiRoundtripFe {")
    lines.append("    recv AbiRoundtripMsg {")
    for case in SCALAR_CASES:
        lines.append(f"        {case.variant_name} {{ value }} -> {case.ty.fe_type} {{")
        lines.append("            value")
        lines.append("        }")
        lines.append("")
    for case in ARRAY_CASES:
        lines.append(f"        {case.variant_name} {{ value }} -> {case.fe_type} {{")
        lines.append("            value")
        lines.append("        }")
        lines.append("")
    for case in TUPLE_CASES:
        lines.append(f"        {case.variant_name} {{ value }} -> {fe_tuple_type(case)} {{")
        lines.append("            value")
        lines.append("        }")
        lines.append("")
    if lines[-1] == "":
        lines.pop()
    lines.append("    }")
    lines.append("}")
    lines.append("")
    return "\n".join(lines)


def render_sol() -> str:
    lines: list[str] = ["// SPDX-License-Identifier: UNLICENSED", "pragma solidity ^0.8.24;", ""]
    for case in TUPLE_CASES:
        lines.append(f"struct {case.struct_name} {{")
        for field in case.fields:
            lines.append(f"    {field.ty.sol_struct_field} {field.name};")
        lines.append("}")
        lines.append("")
    lines.append("interface IAbiRoundtrip {")
    for case in SCALAR_CASES:
        lines.append(
            f"    function {case.fn_name}({case.ty.sol_param} value) external returns ({case.ty.sol_return});"
        )
    for case in ARRAY_CASES:
        lines.append(
            f"    function {case.fn_name}({case.sol_param} value) external returns ({case.sol_return});"
        )
    for case in TUPLE_CASES:
        lines.append(
            f"    function {case.fn_name}({case.struct_name} calldata value) external returns ({tuple_return_signature(case)});"
        )
    lines.append("}")
    lines.append("")
    lines.append("contract AbiRoundtripSol is IAbiRoundtrip {")
    for case in SCALAR_CASES:
        lines.append(
            f"    function {case.fn_name}({case.ty.sol_param} value) external pure returns ({case.ty.sol_return}) {{"
        )
        lines.append("        return value;")
        lines.append("    }")
        lines.append("")
    for case in ARRAY_CASES:
        lines.append(
            f"    function {case.fn_name}({case.sol_param} value) external pure returns ({case.sol_return}) {{"
        )
        lines.append("        return value;")
        lines.append("    }")
        lines.append("")
    for case in TUPLE_CASES:
        lines.append(
            f"    function {case.fn_name}({case.struct_name} calldata value) external pure returns ({tuple_return_signature(case)}) {{"
        )
        lines.append(f"        return {tuple_return_expr(case)};")
        lines.append("    }")
        lines.append("")
    if lines[-1] == "":
        lines.pop()
    lines.append("}")
    lines.append("")
    for contract_name in ("SolBenchCaller", "FeBenchCaller"):
        lines.append(f"contract {contract_name} {{")
        lines.append("    IAbiRoundtrip public immutable target;")
        lines.append("")
        lines.append("    constructor(address target_) {")
        lines.append("        target = IAbiRoundtrip(target_);")
        lines.append("    }")
        lines.append("")
        for case in SCALAR_CASES:
            lines.append(
                f"    function {case.bench_name}({case.ty.sol_param} value) external returns ({case.ty.sol_return}) {{"
            )
            lines.append(f"        return target.{case.fn_name}(value);")
            lines.append("    }")
            lines.append("")
        for case in ARRAY_CASES:
            lines.append(
                f"    function {case.bench_name}({case.sol_param} value) external returns ({case.sol_return}) {{"
            )
            lines.append(f"        return target.{case.fn_name}(value);")
            lines.append("    }")
            lines.append("")
        for case in TUPLE_CASES:
            lines.append(
                f"    function {case.bench_name}({case.struct_name} calldata value) external returns ({tuple_return_signature(case)}) {{"
            )
            lines.append(f"        return target.{case.fn_name}(value);")
            lines.append("    }")
            lines.append("")
        if lines[-1] == "":
            lines.pop()
        lines.append("}")
        lines.append("")
    return "\n".join(lines)


def render_base() -> str:
    return """// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripSol, FeBenchCaller, SolBenchCaller} from "../../../src/AbiRoundtripSol.sol";

interface Vm {
    function readFile(string calldata path) external returns (string memory);
    function assume(bool condition) external;
}

abstract contract AbiRoundtripBase {
    Vm constant vm = Vm(address(uint160(uint256(keccak256("hevm cheat code")))));

    AbiRoundtripSol internal solTarget;
    address internal feTarget;
    SolBenchCaller internal solBench;
    FeBenchCaller internal feBench;

    function setUp() public virtual {
        solTarget = new AbiRoundtripSol();
        feTarget = deploy(fromHex(vm.readFile("fe-out/AbiRoundtripFe.bin")));
        require(feTarget != address(0), "fe create failed");

        solBench = new SolBenchCaller(address(solTarget));
        feBench = new FeBenchCaller(feTarget);
    }

    function assertEquivalent(bytes memory callData) internal {
        (bool okSol, bytes memory outSol) = address(solTarget).call(callData);
        (bool okFe, bytes memory outFe) = feTarget.call(callData);

        require(okSol == okFe, "success mismatch");
        require(okSol, "call failed");
        require(keccak256(outSol) == keccak256(outFe), "return bytes mismatch");
    }

    function assumeShortString(string memory text) internal {
        vm.assume(bytes(text).length <= 96);
    }

    function deploy(bytes memory initCode) internal returns (address deployed) {
        assembly {
            deployed := create(0, add(initCode, 0x20), mload(initCode))
        }
    }

    function fromHex(string memory s) internal pure returns (bytes memory) {
        bytes memory strBytes = bytes(s);
        uint256 start = 0;
        while (start < strBytes.length && isWhitespace(strBytes[start])) {
            start++;
        }

        if (
            start + 1 < strBytes.length &&
            strBytes[start] == bytes1("0") &&
            (strBytes[start + 1] == bytes1("x") || strBytes[start + 1] == bytes1("X"))
        ) {
            start += 2;
        }

        uint256 digits = 0;
        for (uint256 i = start; i < strBytes.length; i++) {
            if (isWhitespace(strBytes[i])) continue;
            digits++;
        }
        require(digits % 2 == 0, "odd hex length");

        bytes memory out = new bytes(digits / 2);
        uint256 outIndex = 0;
        uint8 high = 0;
        bool highNibble = true;
        for (uint256 i = start; i < strBytes.length; i++) {
            bytes1 ch = strBytes[i];
            if (isWhitespace(ch)) continue;
            uint8 val = fromHexChar(ch);
            if (highNibble) {
                high = val;
                highNibble = false;
            } else {
                out[outIndex] = bytes1((high << 4) | val);
                outIndex++;
                highNibble = true;
            }
        }
        return out;
    }

    function isWhitespace(bytes1 ch) private pure returns (bool) {
        return ch == 0x20 || ch == 0x0a || ch == 0x0d || ch == 0x09;
    }

    function fromHexChar(bytes1 c) private pure returns (uint8) {
        uint8 b = uint8(c);
        if (b >= 48 && b <= 57) return b - 48;
        if (b >= 65 && b <= 70) return b - 55;
        if (b >= 97 && b <= 102) return b - 87;
        revert("invalid hex");
    }
}
"""


def slug_to_pascal(slug: str) -> str:
    return "".join(part.capitalize() for part in slug.split("_"))


def tuple_imports(case: TupleCase) -> str:
    return f"IAbiRoundtrip, {case.struct_name}"


def array_imports(case: ArrayCase) -> str:
    imports = ["IAbiRoundtrip", *case.import_structs]
    return ", ".join(imports)


def render_array_deterministic(case: ArrayCase) -> str:
    class_name = f"Abi{slug_to_pascal(case.slug)}DeterministicTest"
    lines = [
        "// SPDX-License-Identifier: UNLICENSED",
        "pragma solidity ^0.8.24;",
        "",
        'import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";',
        f'import {{{array_imports(case)}}} from "../../../src/AbiRoundtripSol.sol";',
        "",
        f"contract {class_name} is AbiRoundtripBase {{",
    ]
    setups = case.det_init_blocks or case.det_values
    for idx, value in enumerate(setups):
        lines.append(f"    function test{case.variant_name}Deterministic{idx}() public {{")
        if case.det_init_blocks:
            lines.extend(indent_block(value))
        else:
            lines.append(f"        {case.sol_local} value = {value};")
        lines.append(
            f"        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.{case.fn_name}.selector, value);"
        )
        lines.append("        assertEquivalent(callData);")
        lines.append("    }")
        lines.append("")
    if lines[-1] == "":
        lines.pop()
    lines.append("}")
    lines.append("")
    return "\n".join(lines)


def render_array_fuzz(case: ArrayCase) -> str:
    class_name = f"Abi{slug_to_pascal(case.slug)}FuzzTest"
    lines = [
        "// SPDX-License-Identifier: UNLICENSED",
        "pragma solidity ^0.8.24;",
        "",
        'import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";',
        f'import {{{array_imports(case)}}} from "../../../src/AbiRoundtripSol.sol";',
        "",
        f"contract {class_name} is AbiRoundtripBase {{",
        f"    function test{case.variant_name}Fuzz({case.sol_local} value) public {{",
    ]
    if case.max_fuzz_len is not None:
        lines.append(f"        vm.assume(value.length <= {case.max_fuzz_len});")
    for suffix in case.assume_suffixes:
        lines.append(f"        assumeShortString(value{suffix});")
    if case.assume_loop_exprs:
        lines.append("        for (uint256 i = 0; i < value.length; i++) {")
        for expr in case.assume_loop_exprs:
            lines.append(f"            assumeShortString({expr});")
        lines.append("        }")
    lines.append(
        f"        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.{case.fn_name}.selector, value);"
    )
    lines.append("        assertEquivalent(callData);")
    lines.append("    }")
    lines.append("}")
    lines.append("")
    return "\n".join(lines)


def render_array_bench(case: ArrayCase) -> str:
    class_name = f"Abi{slug_to_pascal(case.slug)}BenchTest"
    lines = [
        "// SPDX-License-Identifier: UNLICENSED",
        "pragma solidity ^0.8.24;",
        "",
        'import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";',
    ]
    if case.import_structs:
        lines.append(
            f'import {{{", ".join(case.import_structs)}}} from "../../../src/AbiRoundtripSol.sol";'
        )
    lines.extend(
        [
            "",
            f"contract {class_name} is AbiRoundtripBase {{",
            f"    function test{case.bench_name[0].upper()}{case.bench_name[1:]}() public {{",
        ]
    )
    if case.bench_init_block:
        lines.extend(indent_block(case.bench_init_block))
    else:
        lines.append(f"        {case.sol_local} value = {case.bench_value};")
    for suffix in case.assume_suffixes:
        lines.append(f"        assumeShortString(value{suffix});")
    if case.assume_loop_exprs:
        lines.append("        for (uint256 i = 0; i < value.length; i++) {")
        for expr in case.assume_loop_exprs:
            lines.append(f"            assumeShortString({expr});")
        lines.append("        }")
    lines.append(f"        {case.sol_local} solValue = solBench.{case.bench_name}(value);")
    lines.append(
        '        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");'
    )
    lines.append(f"        {case.sol_local} feValue = feBench.{case.bench_name}(value);")
    lines.append(
        '        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");'
    )
    lines.append("    }")
    lines.append("}")
    lines.append("")
    return "\n".join(lines)


def render_scalar_deterministic(case: ScalarCase) -> str:
    class_name = f"Abi{slug_to_pascal(case.slug)}DeterministicTest"
    lines = [
        "// SPDX-License-Identifier: UNLICENSED",
        "pragma solidity ^0.8.24;",
        "",
        'import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";',
        'import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";',
        "",
        f"contract {class_name} is AbiRoundtripBase {{",
    ]
    for idx, value in enumerate(case.ty.det_values):
        lines.append(f"    function test{case.variant_name}Deterministic{idx}() public {{")
        lines.append(f"        {case.ty.sol_local} value = {value};")
        lines.append(
            f"        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.{case.fn_name}.selector, value);"
        )
        lines.append("        assertEquivalent(callData);")
        lines.append("    }")
        lines.append("")
    if lines[-1] == "":
        lines.pop()
    lines.append("}")
    lines.append("")
    return "\n".join(lines)


def render_scalar_fuzz(case: ScalarCase) -> str:
    class_name = f"Abi{slug_to_pascal(case.slug)}FuzzTest"
    lines = [
        "// SPDX-License-Identifier: UNLICENSED",
        "pragma solidity ^0.8.24;",
        "",
        'import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";',
        'import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";',
        "",
        f"contract {class_name} is AbiRoundtripBase {{",
        f"    function test{case.variant_name}Fuzz({case.ty.sol_local} value) public {{",
    ]
    if case.ty.assume_short_string:
        lines.append("        assumeShortString(value);")
    lines.append(
        f"        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.{case.fn_name}.selector, value);"
    )
    lines.append("        assertEquivalent(callData);")
    lines.append("    }")
    lines.append("}")
    lines.append("")
    return "\n".join(lines)


def render_scalar_bench(case: ScalarCase) -> str:
    class_name = f"Abi{slug_to_pascal(case.slug)}BenchTest"
    lines = [
        "// SPDX-License-Identifier: UNLICENSED",
        "pragma solidity ^0.8.24;",
        "",
        'import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";',
        "",
        f"contract {class_name} is AbiRoundtripBase {{",
        f"    function test{case.bench_name[0].upper()}{case.bench_name[1:]}() public {{",
        f"        {case.ty.sol_local} value = {case.ty.bench_value};",
    ]
    if case.ty.assume_short_string:
        lines.append("        assumeShortString(value);")
    if case.ty.compare_kind == "string":
        lines.append(f"        string memory solValue = solBench.{case.bench_name}(value);")
        lines.append(
            '        require(keccak256(bytes(solValue)) == keccak256(bytes(value)), "sol value");'
        )
        lines.append(f"        string memory feValue = feBench.{case.bench_name}(value);")
        lines.append(
            '        require(keccak256(bytes(feValue)) == keccak256(bytes(value)), "fe value");'
        )
    else:
        lines.append(
            f"        require(solBench.{case.bench_name}(value) == value, \"sol value\");"
        )
        lines.append(
            f"        require(feBench.{case.bench_name}(value) == value, \"fe value\");"
        )
    lines.append("    }")
    lines.append("}")
    lines.append("")
    return "\n".join(lines)

def tuple_assumptions(case: TupleCase, names: dict[str, str]) -> list[str]:
    out: list[str] = []
    for field in case.fields:
        if field.ty.assume_short_string:
            out.append(f"        assumeShortString({names[field.name]});")
    return out


def tuple_compare(prefix: str, actual_prefix: str, case: TupleCase) -> list[str]:
    lines: list[str] = []
    for idx, field in enumerate(case.fields):
        actual_name = f"{actual_prefix}{slug_to_pascal(field.name)}"
        expected = f"{prefix}.{field.name}"
        if field.ty.compare_kind == "string":
            lines.append(
                f'        require(keccak256(bytes({actual_name})) == keccak256(bytes({expected})), "{actual_prefix.lower()} {field.name}");'
            )
        else:
            lines.append(f'        require({actual_name} == {expected}, "{actual_prefix.lower()} {field.name}");')
    return lines


def render_tuple_deterministic(case: TupleCase) -> str:
    class_name = f"Abi{slug_to_pascal(case.slug)}DeterministicTest"
    lines = [
        "// SPDX-License-Identifier: UNLICENSED",
        "pragma solidity ^0.8.24;",
        "",
        'import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";',
        f'import {{{tuple_imports(case)}}} from "../../../src/AbiRoundtripSol.sol";',
        "",
        f"contract {class_name} is AbiRoundtripBase {{",
    ]
    for idx, value in enumerate(case.det_values):
        lines.append(f"    function test{case.variant_name}Deterministic{idx}() public {{")
        lines.append(f"        {case.struct_name} memory value = {struct_literal(case, value)};")
        lines.append(
            f"        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.{case.fn_name}.selector, value);"
        )
        lines.append("        assertEquivalent(callData);")
        lines.append("    }")
        lines.append("")
    if lines[-1] == "":
        lines.pop()
    lines.append("}")
    lines.append("")
    return "\n".join(lines)


def render_tuple_fuzz(case: TupleCase) -> str:
    class_name = f"Abi{slug_to_pascal(case.slug)}FuzzTest"
    params = ", ".join(f"{field.ty.sol_local} {field.name}" for field in case.fields)
    names = {field.name: field.name for field in case.fields}
    lines = [
        "// SPDX-License-Identifier: UNLICENSED",
        "pragma solidity ^0.8.24;",
        "",
        'import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";',
        f'import {{{tuple_imports(case)}}} from "../../../src/AbiRoundtripSol.sol";',
        "",
        f"contract {class_name} is AbiRoundtripBase {{",
        f"    function test{case.variant_name}Fuzz({params}) public {{",
    ]
    lines.extend(tuple_assumptions(case, names))
    lines.append(f"        {case.struct_name} memory value = {struct_literal(case, names)};")
    lines.append(
        f"        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.{case.fn_name}.selector, value);"
    )
    lines.append("        assertEquivalent(callData);")
    lines.append("    }")
    lines.append("}")
    lines.append("")
    return "\n".join(lines)


def render_tuple_bench(case: TupleCase) -> str:
    class_name = f"Abi{slug_to_pascal(case.slug)}BenchTest"
    lines = [
        "// SPDX-License-Identifier: UNLICENSED",
        "pragma solidity ^0.8.24;",
        "",
        'import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";',
        f'import {{{case.struct_name}}} from "../../../src/AbiRoundtripSol.sol";',
        "",
        f"contract {class_name} is AbiRoundtripBase {{",
        f"    function test{case.bench_name[0].upper()}{case.bench_name[1:]}() public {{",
        f"        {case.struct_name} memory value = {struct_literal(case, case.bench_value)};",
    ]
    for field in case.fields:
        if field.ty.assume_short_string:
            lines.append(f"        assumeShortString(value.{field.name});")
    lines.append(f"        {case.struct_name} memory solValue = solBench.{case.bench_name}(value);")
    lines.append(
        '        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(value)), "sol value");'
    )
    lines.append(f"        {case.struct_name} memory feValue = feBench.{case.bench_name}(value);")
    lines.append(
        '        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(value)), "fe value");'
    )
    lines.append("    }")
    lines.append("}")
    lines.append("")
    return "\n".join(lines)


def write(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")


def main() -> None:
    if GENERATED_ROOT.exists():
        shutil.rmtree(GENERATED_ROOT)

    SUPPORT_DIR.mkdir(parents=True, exist_ok=True)
    DETERMINISTIC_DIR.mkdir(parents=True, exist_ok=True)
    FUZZ_DIR.mkdir(parents=True, exist_ok=True)
    BENCH_DIR.mkdir(parents=True, exist_ok=True)

    write(FE_PATH, render_fe())
    write(SOL_PATH, render_sol())
    write(SUPPORT_DIR / "AbiRoundtripBase.sol", render_base())

    for case in SCALAR_CASES:
        pascal = slug_to_pascal(case.slug)
        write(DETERMINISTIC_DIR / f"Abi{pascal}Deterministic.t.sol", render_scalar_deterministic(case))
        write(FUZZ_DIR / f"Abi{pascal}Fuzz.t.sol", render_scalar_fuzz(case))
        write(BENCH_DIR / f"Abi{pascal}Bench.t.sol", render_scalar_bench(case))

    for case in ARRAY_CASES:
        pascal = slug_to_pascal(case.slug)
        write(DETERMINISTIC_DIR / f"Abi{pascal}Deterministic.t.sol", render_array_deterministic(case))
        write(FUZZ_DIR / f"Abi{pascal}Fuzz.t.sol", render_array_fuzz(case))
        write(BENCH_DIR / f"Abi{pascal}Bench.t.sol", render_array_bench(case))

    for case in TUPLE_CASES:
        pascal = slug_to_pascal(case.slug)
        write(DETERMINISTIC_DIR / f"Abi{pascal}Deterministic.t.sol", render_tuple_deterministic(case))
        write(FUZZ_DIR / f"Abi{pascal}Fuzz.t.sol", render_tuple_fuzz(case))
        write(BENCH_DIR / f"Abi{pascal}Bench.t.sol", render_tuple_bench(case))

    generated_count = len(list(GENERATED_ROOT.rglob("*.t.sol")))
    print(f"Generated {generated_count} test files across {len(SCALAR_CASES) + len(ARRAY_CASES) + len(TUPLE_CASES)} ABI cases.")


if __name__ == "__main__":
    main()
