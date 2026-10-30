#![cfg(feature = "wasm")]

//! `fe build --backend wasm` on scalar programs.
//!
//! Each program in `tests/fixtures/wasm` exports scalar functions. A Rust
//! reference gives the expected result of every call, or says that the call
//! traps: the reference computes the exact integer result and the call traps
//! when the result does not fit the declared type or the operation is
//! undefined (division by zero). Every program is built at the four
//! optimization levels (0, 1, 2 and s) and run with wasmtime. On x86-64 Linux
//! with the `cranelift` feature, the same functions are also built for the host
//! and the native executable is checked against the wasm results.

mod wasm_support;

use std::{
    fs,
    path::{Path, PathBuf},
};

use tempfile::tempdir;
use wasm_support::*;
use wasmtime::{Module, Trap, Val};

macro_rules! program_test {
    ($test:ident, $file:literal, $exports:expr) => {
        #[test]
        fn $test() {
            check_program($file, $exports);
        }
    };
}

program_test!(arith_i32, "arith_i32", {
    vec![
        Export::new("add", &[("a", I32), ("b", I32)], I32).edges(|a| Some(a[0] + a[1])),
        Export::new("sub", &[("a", I32), ("b", I32)], I32).edges(|a| Some(a[0] - a[1])),
        Export::new("mul", &[("a", I32), ("b", I32)], I32).edges(|a| Some(a[0] * a[1])),
        Export::new("neg", &[("a", I32)], I32).edges(|a| Some(-a[0])),
        Export::new("expr", &[("a", I32), ("b", I32), ("c", I32)], I32).edges(|a| {
            let sum = I32.chk(a[0] + a[1])?;
            let product = I32.chk(sum * a[2])?;
            I32.chk(product - a[0])
        }),
    ]
});

program_test!(arith_i64, "arith_i64", {
    vec![
        Export::new("add", &[("a", I64), ("b", I64)], I64).edges(|a| Some(a[0] + a[1])),
        Export::new("sub", &[("a", I64), ("b", I64)], I64).edges(|a| Some(a[0] - a[1])),
        Export::new("mul", &[("a", I64), ("b", I64)], I64).edges(|a| Some(a[0] * a[1])),
        Export::new(
            "dot",
            &[("a", I64), ("b", I64), ("c", I64), ("d", I64)],
            I64,
        )
        .product(
            &[
                vec![0, 3, -3, 1 << 31],
                vec![0, 5, -7, 1 << 31],
                vec![1, 11, i128::from(i64::MAX)],
                vec![2, -13, 4],
            ],
            |a| {
                let left = I64.chk(a[0] * a[1])?;
                let right = I64.chk(a[2] * a[3])?;
                I64.chk(left + right)
            },
        ),
    ]
});

program_test!(arith_unsigned, "arith_unsigned", {
    vec![
        Export::new("add32", &[("a", U32), ("b", U32)], U32).edges(|a| Some(a[0] + a[1])),
        Export::new("sub32", &[("a", U32), ("b", U32)], U32).edges(|a| Some(a[0] - a[1])),
        Export::new("mul32", &[("a", U32), ("b", U32)], U32).edges(|a| Some(a[0] * a[1])),
        Export::new("add64", &[("a", U64), ("b", U64)], U64).edges(|a| Some(a[0] + a[1])),
        Export::new("mul64", &[("a", U64), ("b", U64)], U64).edges(|a| a[0].checked_mul(a[1])),
        Export::new("sub64", &[("a", U64), ("b", U64)], U64).edges(|a| Some(a[0] - a[1])),
    ]
});

program_test!(division, "division", {
    // Division truncates toward zero and the remainder takes the sign of the
    // dividend. Dividing the minimum value by -1 overflows and traps; the
    // remainder of that pair is 0.
    vec![
        Export::new("sdiv", &[("a", I32), ("b", I32)], I32)
            .edges(|a| (a[1] != 0).then(|| a[0] / a[1])),
        Export::new("srem", &[("a", I32), ("b", I32)], I32)
            .edges(|a| (a[1] != 0).then(|| a[0] % a[1])),
        Export::new("udiv", &[("a", U32), ("b", U32)], U32)
            .edges(|a| (a[1] != 0).then(|| a[0] / a[1])),
        Export::new("urem", &[("a", U32), ("b", U32)], U32)
            .edges(|a| (a[1] != 0).then(|| a[0] % a[1])),
        Export::new("udiv64", &[("a", U64), ("b", U64)], U64)
            .edges(|a| (a[1] != 0).then(|| a[0] / a[1])),
        Export::new("sdiv64", &[("a", I64), ("b", I64)], I64)
            .edges(|a| (a[1] != 0).then(|| a[0] / a[1])),
        Export::new("srem64", &[("a", I64), ("b", I64)], I64)
            .edges(|a| (a[1] != 0).then(|| a[0] % a[1])),
    ]
});

program_test!(bitwise, "bitwise", {
    vec![
        Export::new("and32", &[("a", U32), ("b", U32)], U32).edges(|a| Some(a[0] & a[1])),
        Export::new("or32", &[("a", U32), ("b", U32)], U32).edges(|a| Some(a[0] | a[1])),
        Export::new("xor32", &[("a", U32), ("b", U32)], U32).edges(|a| Some(a[0] ^ a[1])),
        Export::new("not32", &[("a", U32)], U32).edges(|a| Some(U32.wrap(!a[0]))),
        Export::new("xor64", &[("a", U64), ("b", U64)], U64).edges(|a| Some(a[0] ^ a[1])),
        Export::new("not64", &[("a", U64)], U64).edges(|a| Some(U64.wrap(!a[0]))),
        Export::new("andnot_i32", &[("a", I32), ("b", I32)], I32).edges(|a| Some(a[0] & !a[1])),
    ]
});

program_test!(shifts, "shifts", {
    // A shift by at least the width gives zero, or the sign fill for `>>` on
    // a signed type.
    let counts: Vec<i128> = vec![0, 1, 5, 16, 31, 32, 33, 63, 64, 65, 100];
    let words = |ty: Ty| {
        small(
            ty,
            &[
                0,
                1,
                2,
                0x1234_5678,
                0x8000_0000,
                0xffff_ffff,
                -1,
                -2,
                i128::from(i32::MIN),
            ],
        )
    };
    vec![
        Export::new("shl32", &[("a", U32), ("n", U32)], U32).product(
            &[words(U32), counts.clone()],
            |a| {
                Some(if a[1] >= 32 {
                    0
                } else {
                    U32.wrap(a[0] << a[1])
                })
            },
        ),
        Export::new("shr32", &[("a", U32), ("n", U32)], U32)
            .product(&[words(U32), counts.clone()], |a| {
                Some(if a[1] >= 32 { 0 } else { a[0] >> a[1] })
            }),
        Export::new("sar32", &[("a", I32), ("n", I32)], I32).product(
            &[words(I32), counts.clone()],
            |a| {
                Some(if a[1] >= 32 {
                    a[0] >> 127
                } else {
                    a[0] >> a[1]
                })
            },
        ),
        Export::new("shl64", &[("a", U64), ("n", U64)], U64).product(
            &[
                vec![0, 1, 0x1234_5678_9abc_def0, (1 << 63), (1 << 64) - 1],
                counts.clone(),
            ],
            |a| {
                Some(if a[1] >= 64 {
                    0
                } else {
                    U64.wrap(a[0] << a[1])
                })
            },
        ),
        Export::new("shr64", &[("a", U64), ("n", U64)], U64).product(
            &[
                vec![0, 1, 0x1234_5678_9abc_def0, (1 << 63), (1 << 64) - 1],
                counts,
            ],
            |a| Some(if a[1] >= 64 { 0 } else { a[0] >> a[1] }),
        ),
        // `32 - n` underflows for n > 32, and `a >> 32` is zero.
        Export::new("rotl32", &[("a", U32), ("n", U32)], U32).product(
            &[words(U32), vec![0, 1, 8, 31, 32, 33]],
            |a| {
                let back = U32.chk(32 - a[1])?;
                let left = if a[1] >= 32 {
                    0
                } else {
                    U32.wrap(a[0] << a[1])
                };
                let right = if back >= 32 { 0 } else { a[0] >> back };
                Some(left | right)
            },
        ),
    ]
});

program_test!(compare, "compare", {
    vec![
        Export::new("lt_i32", &[("a", I32), ("b", I32)], Bool)
            .edges(|a| Some(i128::from(a[0] < a[1]))),
        Export::new("le_i32", &[("a", I32), ("b", I32)], Bool)
            .edges(|a| Some(i128::from(a[0] <= a[1]))),
        Export::new("lt_u32", &[("a", U32), ("b", U32)], Bool)
            .edges(|a| Some(i128::from(a[0] < a[1]))),
        Export::new("ge_u64", &[("a", U64), ("b", U64)], Bool)
            .edges(|a| Some(i128::from(a[0] >= a[1]))),
        Export::new("gt_i64", &[("a", I64), ("b", I64)], Bool)
            .edges(|a| Some(i128::from(a[0] > a[1]))),
        Export::new("eq_i32", &[("a", I32), ("b", I32)], Bool)
            .edges(|a| Some(i128::from(a[0] == a[1]))),
        Export::new("ne_u32", &[("a", U32), ("b", U32)], Bool)
            .edges(|a| Some(i128::from(a[0] != a[1]))),
    ]
});

program_test!(bool_logic, "bool_logic", {
    vec![
        Export::new("xor", &[("a", Bool), ("b", Bool)], Bool).edges(|a| Some(a[0] ^ a[1])),
        Export::new("majority", &[("a", Bool), ("b", Bool), ("c", Bool)], Bool)
            .edges(|a| Some(i128::from(a[0] + a[1] + a[2] >= 2))),
        Export::new("not", &[("a", Bool)], Bool).edges(|a| Some(1 - a[0])),
        Export::new("pick", &[("flag", Bool), ("a", I32), ("b", I32)], I32)
            .edges(|a| Some(if a[0] != 0 { a[1] } else { a[2] })),
        Export::new("is_positive_even", &[("x", I32)], Bool)
            .edges(|a| Some(i128::from(a[0] > 0 && a[0] % 2 == 0))),
    ]
});

program_test!(narrow_ints, "narrow_ints", {
    vec![
        Export::new("add_u8", &[("a", U8), ("b", U8)], U8).edges(|a| Some(a[0] + a[1])),
        Export::new("sub_u8", &[("a", U8), ("b", U8)], U8).edges(|a| Some(a[0] - a[1])),
        Export::new("mul_i8", &[("a", I8), ("b", I8)], I8).edges(|a| Some(a[0] * a[1])),
        Export::new("add_i16", &[("a", I16), ("b", I16)], I16).edges(|a| Some(a[0] + a[1])),
        Export::new("mul_u16", &[("a", U16), ("b", U16)], U16).edges(|a| Some(a[0] * a[1])),
        Export::new("neg_i8", &[("a", I8)], I8).edges(|a| Some(-a[0])),
        Export::new("lt_i8", &[("a", I8), ("b", I8)], Bool)
            .edges(|a| Some(i128::from(a[0] < a[1]))),
        Export::new("lt_u8", &[("a", U8), ("b", U8)], Bool)
            .edges(|a| Some(i128::from(a[0] < a[1]))),
        Export::new("shr_i16", &[("a", I16), ("n", I16)], I16).product(
            &[I16.edges(), vec![0, 1, 7, 15, 16, 20]],
            |a| {
                Some(if a[1] >= 16 {
                    a[0] >> 127
                } else {
                    a[0] >> a[1]
                })
            },
        ),
        Export::new("div_i8", &[("a", I8), ("b", I8)], I8)
            .edges(|a| (a[1] != 0).then(|| a[0] / a[1])),
    ]
});

program_test!(casts, "casts", {
    vec![
        Export::new("u8_to_u32", &[("a", U8)], U32).edges(|a| Some(a[0])),
        Export::new("i8_to_i32", &[("a", I8)], I32).edges(|a| Some(a[0])),
        Export::new("i8_to_i64", &[("a", I8)], I64).edges(|a| Some(a[0])),
        Export::new("u16_to_i64", &[("a", U16)], I64).edges(|a| Some(a[0])),
        Export::new("u32_to_u64", &[("a", U32)], U64).edges(|a| Some(a[0])),
        Export::new("i32_to_i64", &[("a", I32)], I64).edges(|a| Some(a[0])),
        Export::new("bool_to_u8", &[("a", Bool)], U8).edges(|a| Some(a[0])),
        Export::new("widen_sum", &[("a", U8), ("b", U16), ("c", U32)], U64)
            .edges(|a| Some(a[0] + a[1] + a[2])),
    ]
});

program_test!(factorial, "factorial", {
    vec![
        // 21! does not fit in a u64.
        Export::new("factorial", &[("n", U64)], U64).product(&[(0..=25).collect()], |a| {
            let mut result = 1i128;
            for i in 2..=a[0] {
                result = U64.chk(result * i)?;
            }
            Some(result)
        }),
        Export::new("sum_to", &[("n", U32)], U64)
            .product(&[vec![0, 1, 2, 10, 100, 1000, 100_000]], |a| {
                Some(a[0] * (a[0] + 1) / 2)
            }),
    ]
});

program_test!(fibonacci, "fibonacci", {
    let fib = |n: i128| {
        let (mut a, mut b) = (0i128, 1i128);
        for _ in 0..n {
            (a, b) = (b, a + b);
        }
        a
    };
    vec![
        Export::new("fib", &[("n", U32)], U32)
            .product(&[(0..=24).collect()], move |a| Some(fib(a[0]))),
        // `next = a + b` overflows once the sum passes u64::MAX, which is
        // when n reaches 93 (fib(94) does not fit).
        Export::new("fib_iter", &[("n", U32)], U64).product(
            &[vec![0, 1, 2, 3, 10, 50, 90, 91, 92, 93, 94, 100]],
            move |a| {
                let (mut x, mut y) = (0i128, 1i128);
                for _ in 0..a[0] {
                    let next = U64.chk(x + y)?;
                    (x, y) = (y, next);
                }
                Some(x)
            },
        ),
    ]
});

fn gcd(a: i128, b: i128) -> i128 {
    if b == 0 { a } else { gcd(b, a % b) }
}

program_test!(gcd_lcm, "gcd", {
    let inputs = vec![
        0,
        1,
        2,
        6,
        12,
        35,
        48,
        97,
        100,
        1 << 20,
        4_294_967_295,
        3_000_000_000,
    ];
    vec![
        Export::new("gcd", &[("a", U32), ("b", U32)], U32)
            .product(&[inputs.clone(), inputs.clone()], |a| Some(gcd(a[0], a[1]))),
        Export::new("lcm", &[("a", U32), ("b", U32)], U32).product(
            &[inputs.clone(), inputs],
            |a| {
                if a[0] == 0 || a[1] == 0 {
                    return Some(0);
                }
                U32.chk(a[0] / gcd(a[0], a[1]) * a[1])
            },
        ),
    ]
});

program_test!(collatz, "collatz", {
    vec![
        Export::new("collatz_steps", &[("start", U64)], U32).product(
            &[{
                let mut starts: Vec<i128> = (1..=40).collect();
                starts.extend([97, 871, 6171, 77_031, 837_799]);
                starts
            }],
            |a| {
                let (mut n, mut steps) = (a[0], 0i128);
                while n != 1 {
                    n = if n % 2 == 0 { n / 2 } else { 3 * n + 1 };
                    steps += 1;
                }
                Some(steps)
            },
        ),
    ]
});

fn is_prime(n: i128) -> bool {
    n >= 2 && (2..).take_while(|d| d * d <= n).all(|d| n % d != 0)
}

program_test!(primes, "primes", {
    vec![
        // `d * d` overflows a u32 for the largest inputs, as the loop runs
        // until `d * d > n`: it only does so for n above 4_294_836_225.
        Export::new("is_prime", &[("n", U32)], Bool).product(
            &[{
                let mut values: Vec<i128> = (0..60).collect();
                values.extend([7919, 104_729, 1_000_003, 2_147_483_647, 4_294_967_291]);
                values
            }],
            |a| {
                if a[0] > 4_294_836_225 && is_prime(a[0]) {
                    // The loop passes d = 65536, whose square overflows.
                    return None;
                }
                Some(i128::from(is_prime(a[0])))
            },
        ),
        Export::new("count_primes", &[("limit", U32)], U32)
            .product(&[vec![0, 1, 2, 3, 10, 100, 1000, 2000]], |a| {
                Some((2..=a[0]).filter(|n| is_prime(*n)).count() as i128)
            }),
    ]
});

program_test!(popcount, "popcount", {
    let words = vec![
        0,
        1,
        2,
        3,
        255,
        256,
        0xffff,
        0x8000_0000,
        0xdead_beef,
        0xffff_ffff,
    ];
    vec![
        Export::new("popcount", &[("x", U32)], U32).product(std::slice::from_ref(&words), |a| {
            Some(i128::from((a[0] as u32).count_ones()))
        }),
        Export::new("popcount64", &[("x", U64)], U64).product(
            &[vec![0, 1, 3, 255, 0xdead_beef, 1 << 63, (1 << 64) - 1]],
            |a| Some(i128::from((a[0] as u64).count_ones())),
        ),
        Export::new("leading_zeros", &[("x", U32)], U32).product(&[words], |a| {
            Some(i128::from((a[0] as u32).leading_zeros()))
        }),
    ]
});

fn isqrt(n: i128) -> i128 {
    let (mut lo, mut hi) = (0i128, 1i128 << 32);
    while lo + 1 < hi {
        let mid = lo + (hi - lo) / 2;
        if mid * mid <= n {
            lo = mid;
        } else {
            hi = mid;
        }
    }
    lo
}

program_test!(isqrt_search, "isqrt", {
    vec![Export::new("isqrt", &[("n", U64)], U64).product(
        &[vec![
            0,
            1,
            2,
            3,
            4,
            15,
            16,
            17,
            99,
            100,
            1 << 32,
            (1 << 32) + 1,
            1_000_000_007,
            (1 << 63),
            (1 << 64) - 1,
        ]],
        |a| Some(isqrt(a[0])),
    )]
});

program_test!(powmod, "powmod", {
    vec![
        Export::new(
            "pow_mod",
            &[("base", U64), ("exp", U64), ("modulus", U64)],
            U64,
        )
        .product(
            &[
                vec![0, 1, 2, 3, 7, 123_456_789],
                vec![0, 1, 2, 10, 64, 1_000_003],
                // A modulus above 2^32 lets `result * b` overflow a u64.
                vec![0, 1, 2, 13, 1_000_000_007, 4_294_967_291, 1 << 40],
            ],
            |a| {
                let (base, exp, modulus) = (a[0], a[1], a[2]);
                if modulus == 0 {
                    return None;
                }
                let mut result = 1i128;
                let mut b = base % modulus;
                let mut e = exp;
                while e > 0 {
                    if e & 1 == 1 {
                        result = U64.chk(result * b)? % modulus;
                    }
                    b = U64.chk(b * b)? % modulus;
                    e >>= 1;
                }
                Some(result)
            },
        ),
    ]
});

program_test!(loops, "loops", {
    vec![
        Export::new("triangle", &[("n", U32)], U32).product(&[vec![0, 1, 2, 5, 50, 200]], |a| {
            Some(a[0] * (a[0] + 1) / 2)
        }),
        Export::new("count_odd", &[("n", U32)], U32)
            .product(&[vec![0, 1, 2, 3, 10, 99, 100, 101, 1000]], |a| {
                Some(((a[0] + 1) / 2).min(50))
            }),
        Export::new("first_multiple", &[("of", U32), ("above", U32)], U32)
            .product(&[vec![0, 1, 2, 7, 100], vec![0, 6, 7, 99, 1000]], |a| {
                (a[0] != 0).then(|| (a[1] / a[0] + 1) * a[0])
            }),
    ]
});

program_test!(helpers, "helpers", {
    vec![
        // Two rounds of `2x + 1`.
        Export::new("run", &[("x", U32)], U32).edges(|a| {
            let first = U32.chk(U32.chk(a[0] * 2)? + 1)?;
            U32.chk(U32.chk(first * 2)? + 1)
        }),
        Export::new("max3", &[("a", I32), ("b", I32), ("c", I32)], I32)
            .edges(|a| Some(a[0].max(a[1]).max(a[2]))),
        Export::new("abs_diff", &[("a", I32), ("b", I32)], I32).edges(|a| {
            Some(if a[0] > a[1] {
                a[0] - a[1]
            } else {
                a[1] - a[0]
            })
        }),
    ]
});

program_test!(exports_call_exports, "exports_call_exports", {
    let values = I64.edges();
    vec![
        Export::new("square", &[("x", I64)], I64).edges(|a| Some(a[0] * a[0])),
        Export::new("sum_squares", &[("a", I64), ("b", I64)], I64)
            .product(&[values.clone(), values.clone()], |a| {
                I64.chk(I64.chk(a[0] * a[0])? + I64.chk(a[1] * a[1])?)
            }),
        Export::new("hypot_squared", &[("a", I64), ("b", I64)], I64)
            .product(&[values.clone(), values], |a| {
                I64.chk(I64.chk(a[0] * a[0])? + I64.chk(a[1] * a[1])?)
            }),
    ]
});

program_test!(consts, "consts", {
    vec![
        Export::new("offset", &[], U32).product(&[], |_| Some(128)),
        Export::new("tripled_plus", &[("x", U32)], U32).edges(|a| Some(21 + a[0])),
        Export::new("masked", &[("x", U64)], U64).edges(|a| Some(a[0] & 0xff00_ff00_ff00_ff00)),
        Export::new("scaled", &[("x", U32)], U32).edges(|a| Some(a[0] * 1000 + 128)),
    ]
});

program_test!(branching, "branching", {
    vec![
        Export::new("sign", &[("x", I32)], I32).edges(|a| Some(a[0].signum())),
        Export::new("clamp", &[("x", I32), ("lo", I32), ("hi", I32)], I32).product(
            &[vec![-5, 0, 5, 100], vec![-10, 0, 10], vec![-10, 0, 10, 50]],
            |a| {
                Some(if a[0] < a[1] {
                    a[1]
                } else if a[0] > a[2] {
                    a[2]
                } else {
                    a[0]
                })
            },
        ),
        Export::new("abs", &[("x", I32)], I32).edges(|a| Some(a[0].abs())),
        Export::new("classify", &[("x", U32)], U32).product(
            &[vec![0, 9, 10, 99, 100, 999, 1000, 4_294_967_295]],
            |a| {
                Some(match a[0] {
                    0..=9 => 1,
                    10..=99 => 2,
                    100..=999 => 3,
                    _ => 4,
                })
            },
        ),
    ]
});

program_test!(digits, "digits", {
    let numbers = vec![
        0,
        1,
        9,
        10,
        99,
        12_345,
        1_000_000,
        9_876_543_210,
        1_234_567_890_123_456_789,
    ];
    vec![
        Export::new("digit_sum", &[("n", U64)], U64).product(std::slice::from_ref(&numbers), |a| {
            Some(a[0].to_string().bytes().map(|b| i128::from(b - b'0')).sum())
        }),
        // Reversing a 19-digit number can pass u64::MAX.
        Export::new("reverse_digits", &[("n", U64)], U64).product(
            std::slice::from_ref(&numbers),
            |a| {
                let mut out = 0i128;
                let mut v = a[0];
                while v > 0 {
                    out = U64.chk(U64.chk(out * 10)? + v % 10)?;
                    v /= 10;
                }
                Some(out)
            },
        ),
        Export::new("digit_count", &[("n", U64)], U32)
            .product(&[numbers], |a| Some(a[0].to_string().len() as i128)),
    ]
});

program_test!(many_params, "many_params", {
    vec![
        Export::new(
            "weighted",
            &[
                ("a", I32),
                ("b", I32),
                ("c", I32),
                ("d", I32),
                ("e", I32),
                ("f", I32),
                ("g", I32),
            ],
            I32,
        )
        .calls(
            &[
                &[1, 2, 3, 4, 5, 6, 7],
                &[0, 0, 0, 0, 0, 0, 0],
                &[-1, -2, -3, -4, -5, -6, -7],
                &[1000, 1000, 1000, 1000, 1000, 1000, 1000],
                &[100_000_000, 100_000_000, 100_000_000, 0, 0, 0, 0],
                &[2_147_483_647, 0, 0, 0, 0, 0, 1],
                &[-2_147_483_648, 0, 0, 0, 0, 0, 0],
            ],
            |a| {
                let mut total = a[0];
                for (weight, value) in (2..=7).zip(&a[1..]) {
                    total = I32.chk(total + I32.chk(weight * value)?)?;
                }
                Some(total)
            },
        ),
        Export::new(
            "mixed_widths",
            &[("a", U8), ("b", I16), ("c", U32), ("d", I64), ("e", Bool)],
            I64,
        )
        .calls(
            &[
                &[1, 2, 3, 4, 1],
                &[255, -32768, 4_294_967_295, -1, 1],
                &[255, -32768, 4_294_967_295, -1, 0],
                &[0, 0, 0, i64::MAX as i128, 1],
                &[1, 0, 0, i64::MAX as i128, 1],
            ],
            |a| {
                if a[4] != 0 {
                    I64.chk(I64.chk(I64.chk(a[0] + a[1])? + a[2])? + a[3])
                } else {
                    Some(a[3])
                }
            },
        ),
    ]
});

program_test!(bit_tricks, "bit_tricks", {
    vec![
        Export::new("is_power_of_two", &[("x", U32)], Bool).product(
            &[vec![
                0,
                1,
                2,
                3,
                4,
                6,
                8,
                1023,
                1024,
                1 << 31,
                4_294_967_295,
            ]],
            |a| Some(i128::from(a[0] != 0 && a[0] & (a[0] - 1) == 0)),
        ),
        // Above 2^31 the loop shifts to zero and never ends, so the inputs stop there.
        Export::new("next_power_of_two", &[("x", U32)], U32).product(
            &[vec![
                0,
                1,
                2,
                3,
                5,
                17,
                1000,
                1 << 30,
                (1 << 30) + 1,
                1 << 31,
            ]],
            |a| Some((a[0] as u32).next_power_of_two().max(1).into()),
        ),
        Export::new("reverse_bits", &[("x", U32)], U32).product(
            &[vec![0, 1, 2, 0xf0, 0x1234_5678, 0x8000_0000, 0xffff_ffff]],
            |a| Some(i128::from((a[0] as u32).reverse_bits())),
        ),
        Export::new("parity", &[("x", U64)], Bool).product(
            &[vec![0, 1, 3, 7, 255, 0xdead_beef, 1 << 63, (1 << 64) - 1]],
            |a| Some(i128::from((a[0] as u64).count_ones() % 2)),
        ),
    ]
});

program_test!(mixed_signed, "mixed_signed", {
    vec![
        Export::new("mean_i16", &[("a", I16), ("b", I16)], I16)
            .edges(|a| Some(I16.chk(a[0] + a[1])? / 2)),
        Export::new("offset_u16", &[("a", U16), ("b", U16)], U16)
            .edges(|a| Some((a[0] - a[1]).abs())),
        Export::new("scale_i8", &[("a", I8)], I16).edges(|a| Some(a[0] * 3)),
        Export::new("lerp", &[("a", I64), ("b", I64), ("t", I64)], I64).product(
            &[
                vec![-1000, 0, 7, 1 << 40],
                vec![-5, 0, 1000, i128::from(i64::MAX)],
                vec![0, 25, 100, -50, 1 << 40],
            ],
            |a| {
                let diff = I64.chk(a[1] - a[0])?;
                let scaled = I64.chk(diff * a[2])?;
                I64.chk(a[0] + scaled / 100)
            },
        ),
    ]
});

program_test!(trap_overflow, "trap_overflow", {
    vec![
        Export::new("add_i32", &[("a", I32), ("b", I32)], I32).edges(|a| Some(a[0] + a[1])),
        Export::new("sub_u8", &[("a", U8), ("b", U8)], U8).edges(|a| Some(a[0] - a[1])),
        Export::new("mul_u64", &[("a", U64), ("b", U64)], U64).edges(|a| a[0].checked_mul(a[1])),
        Export::new("neg_i32", &[("a", I32)], I32).edges(|a| Some(-a[0])),
        Export::new("mul_i16", &[("a", I16), ("b", I16)], I16).edges(|a| Some(a[0] * a[1])),
        // 4_000_000_000 + n * 100_000_000 passes u32::MAX at n = 3.
        Export::new("add_u32_loop", &[("n", U32)], U32).product(&[vec![0, 1, 2, 3, 4]], |a| {
            Some(4_000_000_000 + a[0] * 100_000_000)
        }),
    ]
});

program_test!(trap_div_zero, "trap_div_zero", {
    vec![
        Export::new("div", &[("a", U32), ("b", U32)], U32)
            .edges(|a| (a[1] != 0).then(|| a[0] / a[1])),
        Export::new("rem", &[("a", I32), ("b", I32)], I32)
            .edges(|a| (a[1] != 0).then(|| a[0] % a[1])),
        Export::new("sdiv", &[("a", I32), ("b", I32)], I32)
            .edges(|a| (a[1] != 0).then(|| a[0] / a[1])),
        Export::new("div64", &[("a", U64), ("b", U64)], U64)
            .edges(|a| (a[1] != 0).then(|| a[0] / a[1])),
    ]
});

program_test!(trap_assert, "trap_assert", {
    vec![
        Export::new("positive", &[("x", I32)], I32).edges(|a| (a[0] > 0).then_some(a[0])),
        Export::new("small", &[("x", U8)], U8).edges(|a| (a[0] < 100).then_some(a[0])),
        Export::new("guarded_sum", &[("n", U32)], U32)
            .product(&[upto(14)], |a| (a[0] <= 10).then(|| (0..a[0]).sum())),
    ]
});

// ---------------------------------------------------------------------------
// The module itself.
// ---------------------------------------------------------------------------

fn write_source(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(format!("{name}.fe"));
    fs::write(&path, source).unwrap();
    path
}

#[test]
fn the_module_exports_the_public_functions_and_a_memory() {
    let temp = tempdir().unwrap();
    let source = write_source(
        temp.path(),
        "exports",
        "fn hidden() -> u32 { 1 }\n\
         pub fn visible(x: u32) -> u32 { x + hidden() }\n\
         pub fn another() -> bool { true }\n\
         pub fn generic<T>(x: own T) -> T { x }\n",
    );
    let bytes = build_wasm(&source, "1", &temp.path().join("out"));
    let module = Module::new(engine(), &bytes).unwrap();
    let mut names: Vec<_> = module
        .exports()
        .map(|export| export.name().to_string())
        .collect();
    names.sort();
    assert_eq!(names, ["another", "memory", "visible"]);
}

#[test]
fn building_twice_gives_identical_bytes_at_every_level() {
    let temp = tempdir().unwrap();
    let source = fixture_path("primes");
    for level in ["0", "2"] {
        let first = build_wasm(&source, level, &temp.path().join(format!("a{level}")));
        let second = build_wasm(&source, level, &temp.path().join(format!("b{level}")));
        assert_eq!(first, second, "O{level} output differs between builds");
    }
}

#[test]
fn narrow_parameters_ignore_bits_above_their_width() {
    let temp = tempdir().unwrap();
    let source = write_source(
        temp.path(),
        "narrow",
        "pub fn add(a: u8, b: u8) -> u8 { a + b }\n\
         pub fn lt(a: i8, b: i8) -> bool { a < b }\n",
    );
    let bytes = build_wasm(&source, "1", &temp.path().join("out"));
    let mut running = Running::new(&bytes);
    let add = running
        .instance
        .get_func(&mut running.store, "add")
        .unwrap();
    let mut result = [Val::I32(0)];
    // 0x1_05 + 0xff_00_00_03 read as u8 are 5 and 3.
    add.call(
        &mut running.store,
        &[Val::I32(0x105), Val::I32(0xff00_0003u32 as i32)],
        &mut result,
    )
    .unwrap();
    assert_eq!(result[0].unwrap_i32(), 8);
    let lt = running.instance.get_func(&mut running.store, "lt").unwrap();
    // 0xff is -1 as an i8, and 0x7f00_0001 holds +1.
    lt.call(
        &mut running.store,
        &[Val::I32(0xff), Val::I32(0x7f00_0001)],
        &mut result,
    )
    .unwrap();
    assert_eq!(result[0].unwrap_i32(), 1);
}

#[test]
fn a_trap_leaves_the_instance_usable() {
    let temp = tempdir().unwrap();
    let source = write_source(
        temp.path(),
        "after_trap",
        "pub fn half(x: u32, d: u32) -> u32 { x / d }\n",
    );
    let bytes = build_wasm(&source, "2", &temp.path().join("out"));
    let mut running = Running::new(&bytes);
    let export = Export::new("half", &[("x", U32), ("d", U32)], U32);
    assert!(running.call(&export, &[10, 0]).is_err());
    assert_eq!(running.call(&export, &[10, 2]), Ok(5));
}

#[test]
fn traps_are_the_unreachable_instruction_where_the_program_checks() {
    let temp = tempdir().unwrap();
    let source = fixture_path("trap_assert");
    let bytes = build_wasm(&source, "1", &temp.path().join("out"));
    let mut running = Running::new(&bytes);
    let export = Export::new("positive", &[("x", I32)], I32);
    assert_eq!(
        running.call(&export, &[-3]),
        Err(Trap::UnreachableCodeReached)
    );
}

// ---------------------------------------------------------------------------
// The command line.
// ---------------------------------------------------------------------------

#[test]
fn the_default_output_is_the_module() {
    let temp = tempdir().unwrap();
    let source = write_source(temp.path(), "plain", "pub fn one() -> u32 { 1 }\n");
    let out = temp.path().join("out");
    let output = fe(&[
        "build",
        "--backend",
        "wasm",
        "--out-dir",
        out.to_str().unwrap(),
        source.to_str().unwrap(),
    ]);
    assert!(output.status.success(), "{output:?}");
    let bytes = fs::read(out.join("plain.wasm")).unwrap();
    assert_eq!(&bytes[..4], b"\0asm");
    assert!(!out.join("plain.wasm.sona").exists());
}

#[test]
fn ir_can_be_emitted_with_or_without_the_module() {
    let temp = tempdir().unwrap();
    let source = write_source(temp.path(), "ir", "pub fn one() -> u32 { 1 }\n");
    for (emit, module) in [("ir", false), ("wasm,ir", true)] {
        let out = temp.path().join(format!("out-{module}"));
        let output = fe(&[
            "build",
            "--backend",
            "wasm",
            "--emit",
            emit,
            "--out-dir",
            out.to_str().unwrap(),
            source.to_str().unwrap(),
        ]);
        assert!(output.status.success(), "{output:?}");
        let ir = fs::read_to_string(out.join("ir.wasm.sona")).unwrap();
        assert!(ir.contains("wasm32-unknown-native"), "{ir}");
        assert!(ir.contains("func public %one"), "{ir}");
        assert_eq!(out.join("ir.wasm").exists(), module);
    }
}

fn expect_failure(args: &[&str], needles: &[&str]) -> String {
    let output = fe(args);
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    assert!(
        !output.status.success(),
        "the build should fail: {output:?}"
    );
    assert_eq!(output.status.code(), Some(1), "{stderr}");
    assert!(
        !stderr.contains("panicked"),
        "the compiler panicked:\n{stderr}"
    );
    for needle in needles {
        assert!(stderr.contains(needle), "missing `{needle}` in:\n{stderr}");
    }
    stderr
}

#[test]
fn other_backends_and_outputs_are_refused_with_a_reason() {
    let temp = tempdir().unwrap();
    let source = write_source(temp.path(), "refused", "pub fn one() -> u32 { 1 }\n");
    let path = source.to_str().unwrap();
    expect_failure(
        &["build", "--emit", "wasm", path],
        &["`--emit wasm` requires `--backend wasm`"],
    );
    for emit in [
        "bytecode",
        "runtime-bytecode",
        "abi",
        "metadata",
        "executable",
    ] {
        expect_failure(
            &["build", "--backend", "wasm", "--emit", emit, path],
            &["WebAssembly backend only supports `--emit wasm` and `--emit ir`"],
        );
    }
    expect_failure(
        &["build", "--backend", "wasm", "--contract", "C", path],
        &["WebAssembly output does not support `--ingot` or `--contract`"],
    );
}

#[test]
fn an_ingot_directory_is_refused() {
    let temp = tempdir().unwrap();
    fs::create_dir_all(temp.path().join("src")).unwrap();
    fs::write(
        temp.path().join("fe.toml"),
        "[ingot]\nname = \"wasm_ingot\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    fs::write(
        temp.path().join("src/lib.fe"),
        "pub fn one() -> u32 { 1 }\n",
    )
    .unwrap();
    expect_failure(
        &["build", "--backend", "wasm", temp.path().to_str().unwrap()],
        &["WebAssembly output only supports standalone `.fe` files"],
    );
}

/// Builds `source` for wasm, which must fail with every needle in the message,
/// without a panic and without writing a module.
fn rejected(name: &str, source: &str, needles: &[&str]) -> String {
    let temp = tempdir().unwrap();
    let path = write_source(temp.path(), name, source);
    let out = temp.path().join("out");
    let stderr = expect_failure(
        &[
            "build",
            "--backend",
            "wasm",
            "--out-dir",
            out.to_str().unwrap(),
            path.to_str().unwrap(),
        ],
        needles,
    );
    assert!(
        !out.exists(),
        "no output may be written for a rejected program"
    );
    stderr
}

#[test]
fn a_u256_parameter_is_a_named_error() {
    rejected(
        "wide_param",
        "pub fn total(x: u256, y: u32) -> u32 { y }\n",
        &[
            "cannot export `total` to WebAssembly",
            "parameter `x: u256`",
            "256-bit integer",
        ],
    );
}

#[test]
fn an_i256_parameter_is_a_named_error() {
    rejected(
        "signed_wide",
        "pub fn f(a: u32, b: i256) -> u32 { a }\n",
        &[
            "cannot export `f`",
            "parameter `b: i256`",
            "256-bit integer",
        ],
    );
}

#[test]
fn usize_is_a_32_bit_integer_of_the_module() {
    let temp = tempdir().unwrap();
    let source = write_source(
        temp.path(),
        "usize_param",
        "pub fn double(n: usize) -> usize { n + n }\n",
    );
    let bytes = build_wasm(&source, "1", &temp.path().join("out"));
    let module = Module::new(engine(), &bytes).unwrap();
    let export = module
        .exports()
        .find(|export| export.name() == "double")
        .unwrap();
    let signature = export.ty().unwrap_func().clone();
    assert_eq!(
        signature
            .params()
            .map(|ty| ty.to_string())
            .collect::<Vec<_>>(),
        ["i32"]
    );
    assert_eq!(
        signature
            .results()
            .map(|ty| ty.to_string())
            .collect::<Vec<_>>(),
        ["i32"]
    );
    let mut running = Running::new(&bytes);
    let double = running
        .instance
        .get_func(&mut running.store, "double")
        .unwrap();
    let mut result = [Val::I32(0)];
    double
        .call(&mut running.store, &[Val::I32(21)], &mut result)
        .unwrap();
    assert_eq!(result[0].unwrap_i32(), 42);
    // The sum of 2^31 and itself does not fit 32 bits.
    let trap = double
        .call(&mut running.store, &[Val::I32(i32::MIN)], &mut result)
        .unwrap_err();
    assert_eq!(
        trap.downcast_ref::<Trap>(),
        Some(&Trap::UnreachableCodeReached)
    );
}

#[test]
fn a_wide_result_is_a_named_error() {
    rejected(
        "wide_result",
        "pub fn big(x: u32) -> u128 { x as u128 }\n",
        &[
            "cannot export `big`",
            "result type `u128`",
            "128-bit integer",
        ],
    );
}

#[test]
fn non_scalar_exports_are_named_errors() {
    rejected(
        "struct_param",
        "struct Point { x: i32, y: i32 }\npub fn norm(p: Point) -> i32 { p.x + p.y }\n",
        &[
            "cannot export `norm`",
            "parameter `p: Point`",
            "not a scalar",
        ],
    );
    rejected(
        "tuple_result",
        "pub fn pair(x: i32) -> (i32, i32) { (x, x) }\n",
        &[
            "cannot export `pair`",
            "result type `(i32, i32)`",
            "not a scalar",
        ],
    );
    rejected(
        "array_param",
        "pub fn first(items: [u8; 4]) -> u8 { items[0] }\n",
        &[
            "cannot export `first`",
            "parameter `items: [u8; 4]`",
            "not a scalar",
        ],
    );
    rejected(
        "mut_param",
        "pub fn bump(x: mut i32) { x = x + 1 }\n",
        &["cannot export `bump`", "parameter `x: i32`", "not a scalar"],
    );
}

#[test]
fn an_evm_builtin_is_a_named_error() {
    let stderr = rejected(
        "evm_builtin",
        "use std::evm::ops\n\
         pub fn lowest_byte(x: u32) -> u32 {\n\
         \x20   let b: u256 = ops::byte(pos: 31, value: x as u256)\n\
         \x20   1\n\
         }\n",
        &["lowest_byte", "EVM-only instruction", "EvmByte"],
    );
    assert!(stderr.contains("wasm32-unknown-native"), "{stderr}");
}

#[test]
fn sixteen_byte_integers_inside_a_body_are_named_errors() {
    // 256-bit integers work (they are what `usize` is); 128-bit ones do not.
    rejected(
        "wide_128",
        "pub fn widen(a: u32) -> u32 {\n\
         \x20   let x: u128 = a as u128\n\
         \x20   let y: u128 = x * x\n\
         \x20   y.downcast_truncate()\n\
         }\n",
        &["i128"],
    );
}

#[test]
fn a_function_named_memory_is_refused() {
    rejected(
        "reserved_name",
        "pub fn memory() -> u32 { 1 }\n",
        &["cannot export `memory`", "reserved for the linear memory"],
    );
}

#[test]
fn a_function_that_needs_effects_is_refused() {
    rejected(
        "effects",
        "trait Counter { fn next(mut self) -> u32 }\n\
         pub fn tick() -> u32 uses (c: mut Counter) { c.next() }\n",
        &["cannot export `tick`", "effect"],
    );
}

#[test]
fn a_file_without_exports_is_refused() {
    rejected(
        "no_exports",
        "fn private_only() -> u32 { 1 }\npub fn generic<T>(x: own T) -> T { x }\n",
        &["needs at least one exported function"],
    );
}
