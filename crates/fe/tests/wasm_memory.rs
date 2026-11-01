#![cfg(feature = "wasm")]

//! `fe build --backend wasm` on programs that use structs, arrays, tuples,
//! enums and constants in their bodies.
//!
//! The exports of every program take and return scalars. A Rust reference
//! below gives the result of each call, or says that it traps. The reference
//! restates the program in `i128` arithmetic, and a call traps where the
//! program overflows, divides by zero or indexes outside an array. Every
//! program is built at the four optimization levels (0, 1, 2 and s) and run with
//! wasmtime, and the same functions are also built for the host and compared
//! (see `tests/wasm_support`).

mod wasm_support;

use wasm_support::*;

macro_rules! program_test {
    ($test:ident, $file:literal, $exports:expr) => {
        #[test]
        fn $test() {
            check_program($file, $exports);
        }
    };
}

fn i32c(value: i128) -> Option<i128> {
    I32.chk(value)
}

fn u32c(value: i128) -> Option<i128> {
    U32.chk(value)
}

fn i64c(value: i128) -> Option<i128> {
    I64.chk(value)
}

fn u64c(value: i128) -> Option<i128> {
    U64.chk(value)
}

/// Values around zero and the limits of a 32-bit signed integer.
fn some_i32() -> Vec<i128> {
    vec![-100_000, -7, -1, 0, 1, 3, 12, 50_000, i128::from(i32::MAX)]
}

/// The element at `index` of a fixed-size array, or a trap.
fn at<T: Copy>(array: &[T], index: i128) -> Option<T> {
    usize::try_from(index)
        .ok()
        .and_then(|i| array.get(i).copied())
}

// ---------------------------------------------------------------------------
// Structs by value.
// ---------------------------------------------------------------------------

program_test!(struct_basic, "struct_basic", {
    let coords = vec![-5, 0, 3, 40_000];
    vec![
        // (x1 - x0) * (y1 - y0), each step checked.
        Export::new(
            "area",
            &[("x0", I32), ("y0", I32), ("x1", I32), ("y1", I32)],
            I32,
        )
        .product(
            &[
                coords.clone(),
                coords.clone(),
                coords.clone(),
                coords.clone(),
            ],
            |a| {
                let width = i32c(a[2] - a[0])?;
                let height = i32c(a[3] - a[1])?;
                i32c(width * height)
            },
        )
        .calls(
            &[
                &[i128::from(i32::MIN), 0, i128::from(i32::MAX), 1],
                &[0, 0, 100_000, 100_000],
                &[1, 1, 2, 2],
            ],
            |a| {
                let width = i32c(a[2] - a[0])?;
                let height = i32c(a[3] - a[1])?;
                i32c(width * height)
            },
        ),
        // c = a + b; c.dot(a) + a.dot(b)
        Export::new(
            "dot_sum",
            &[("ax", I32), ("ay", I32), ("bx", I32), ("by", I32)],
            I32,
        )
        .product(
            &[
                some_i32(),
                vec![0, 2, -3],
                vec![1, -4, 9],
                vec![0, 5, 46_000],
            ],
            |a| {
                let cx = i32c(a[0] + a[2])?;
                let cy = i32c(a[1] + a[3])?;
                let c_dot_a = i32c(i32c(cx * a[0])? + i32c(cy * a[1])?)?;
                let a_dot_b = i32c(i32c(a[0] * a[2])? + i32c(a[1] * a[3])?)?;
                i32c(c_dot_a + a_dot_b)
            },
        ),
        // A copy keeps the old field after the original changes.
        Export::new("copies", &[("x", I32)], I32).product(&[some_i32()], |a| {
            let changed = i32c(a[0] + 100)?;
            i32c(i32c(a[0] * 1000)? + changed)
        }),
    ]
});

program_test!(struct_nested, "struct_nested", {
    let points = vec![-3, 0, 4, 70_000];
    vec![
        Export::new(
            "spans",
            &[("a", I32), ("b", I32), ("c", I32), ("d", I32)],
            I32,
        )
        .product(
            &[
                points.clone(),
                points.clone(),
                points.clone(),
                points.clone(),
            ],
            |a| {
                let dx = i32c(a[2] - a[0])?;
                let dy = i32c(a[3] - a[1])?;
                i32c(i32c(dx * 1000)? + dy)
            },
        ),
        // The segment is swapped through a copy of its old value.
        Export::new(
            "reversed",
            &[("a", I32), ("b", I32), ("c", I32), ("d", I32)],
            I32,
        )
        .product(
            &[points.clone(), points.clone(), points.clone(), points],
            |a| {
                let dx = i32c(a[0] - a[2])?;
                let dy = i32c(a[1] - a[3])?;
                i32c(i32c(dx * 1000)? + dy)
            },
        ),
        // id + weight * from.x + to.y, where to.y is `a`.
        Export::new("weighted", &[("a", I32), ("b", I32), ("w", I64)], I64).product(
            &[
                vec![-2, 0, 5],
                vec![1, 9],
                vec![0, 7, -3, i128::from(i64::MAX)],
            ],
            |a| i64c(i64c(3 + i64c(a[2] * a[0])?)? + a[0]),
        ),
        // The copy of `from` keeps its x after the original is changed.
        Export::new("untouched_copy", &[("a", I32)], I32).product(&[some_i32()], |a| {
            let moved = i32c(a[0] + 1000)?;
            i32c(i32c(a[0] * 10)? + moved)
        }),
    ]
});

program_test!(struct_mut_param, "struct_mut_param", {
    let values = vec![-4, -1, 0, 1, 9];
    vec![
        Export::new(
            "run",
            &[("a", I32), ("b", I32), ("c", I32), ("threshold", I32)],
            I32,
        )
        .product(
            &[
                values.clone(),
                values.clone(),
                values.clone(),
                vec![-1, 0, 5],
            ],
            |a| {
                let hits = a[..3].iter().filter(|&&v| v >= a[3]).count() as i128;
                let misses = 3 - hits;
                i32c(hits * 100 + misses * 10 + a[2])
            },
        ),
        Export::new("reset_then_count", &[("a", I32)], I32).product(&[values], |a| {
            let (hits, misses) = if a[0] >= 0 { (1, 0) } else { (0, 1) };
            i32c(hits * 100 + misses * 10 + a[0])
        }),
    ]
});

/// Complex integers as a struct of two `i64`.
fn complex_mul(a: (i128, i128), b: (i128, i128)) -> Option<(i128, i128)> {
    let re = i64c(i64c(a.0 * b.0)? - i64c(a.1 * b.1)?)?;
    let im = i64c(i64c(a.0 * b.1)? + i64c(a.1 * b.0)?)?;
    Some((re, im))
}

fn complex_pow(base: (i128, i128), n: i128) -> Option<(i128, i128)> {
    let mut result = (1, 0);
    for _ in 0..n {
        result = complex_mul(result, base)?;
    }
    Some(result)
}

program_test!(struct_complex, "struct_complex", {
    let parts = vec![-3, -1, 0, 1, 2];
    let powers = vec![0, 1, 2, 5, 13];
    vec![
        Export::new("power_re", &[("re", I64), ("im", I64), ("n", U32)], I64)
            .product(&[parts.clone(), parts.clone(), powers.clone()], |a| {
                Some(complex_pow((a[0], a[1]), a[2])?.0)
            }),
        Export::new("power_im", &[("re", I64), ("im", I64), ("n", U32)], I64)
            .product(&[parts.clone(), parts.clone(), powers], |a| {
                Some(complex_pow((a[0], a[1]), a[2])?.1)
            })
            // Large components overflow in the first multiplication.
            .calls(
                &[&[i128::from(i64::MAX), 1, 2], &[1 << 40, 1 << 40, 1]],
                |a| Some(complex_pow((a[0], a[1]), a[2])?.1),
            ),
        Export::new(
            "mandel_steps",
            &[("cre", I64), ("cim", I64), ("limit", U32)],
            U32,
        )
        .product(&[parts.clone(), parts, vec![0, 1, 7, 50]], |a| {
            let c = (a[0], a[1]);
            let mut z = (0, 0);
            let mut steps = 0;
            while steps < a[2] {
                if i64c(i64c(z.0 * z.0)? + i64c(z.1 * z.1)?)? > 4 {
                    return Some(steps);
                }
                let squared = complex_mul(z, z)?;
                z = (i64c(squared.0 + c.0)?, i64c(squared.1 + c.1)?);
                steps += 1;
            }
            Some(steps)
        }),
    ]
});

/// `Pair { a, b }` stepped as Fibonacci or Lucas: the sums computed after `n`
/// steps are the first `n + 1` terms after the seeds.
fn pair_after(n: i128, seed: (i128, i128)) -> Option<(i128, i128)> {
    let mut state = seed;
    for _ in 0..n {
        state = (state.1, u64c(state.0 + state.1)?);
    }
    Some(state)
}

program_test!(struct_state, "struct_state", {
    vec![
        Export::new("fib", &[("n", U32)], U64)
            .product(&[vec![0, 1, 2, 10, 50, 91, 92, 93, 94, 200]], |a| {
                Some(pair_after(a[0], (0, 1))?.0)
            }),
        Export::new("lucas_or_fib", &[("n", U32), ("lucas", Bool)], U64).product(
            &[vec![0, 1, 5, 40, 90, 91, 92], vec![0, 1]],
            |a| {
                let seed = if a[1] == 1 { (2, 1) } else { (0, 1) };
                Some(pair_after(a[0], seed)?.0)
            },
        ),
        Export::new("alternate", &[("n", U32)], U64).product(&[upto(12)], |a| {
            let step = |p: (i128, i128)| Some((p.1, u64c(p.0 + p.1)?));
            let (mut left, mut right) = ((1, 2), (10, 20));
            for _ in 0..a[0] {
                std::mem::swap(&mut left, &mut right);
                left = step(left)?;
            }
            u64c(u64c(left.0 * 1000)? + right.0)
        }),
    ]
});

program_test!(struct_vec3, "struct_vec3", {
    let k = vec![-3, 0, 2, 1 << 30];
    vec![
        // (u x v) . w for u = (a, 1, 0), v = (0, b, 1), w = (1, 0, c)
        Export::new("triple_product", &[("a", I64), ("b", I64), ("c", I64)], I64).product(
            &[k.clone(), k.clone(), k.clone()],
            |a| {
                let (ux, uy, uz) = (a[0], 1, 0);
                let (vx, vy, vz) = (0, a[1], 1);
                let (wx, wy, wz) = (1, 0, a[2]);
                let x = i64c(i64c(uy * vz)? - i64c(uz * vy)?)?;
                let y = i64c(i64c(uz * vx)? - i64c(ux * vz)?)?;
                let z = i64c(i64c(ux * vy)? - i64c(uy * vx)?)?;
                i64c(i64c(i64c(x * wx)? + i64c(y * wy)?)? + i64c(z * wz)?)
            },
        ),
        Export::new("lincomb", &[("a", I64), ("b", I64)], I64)
            .product(&[vec![-7, 0, 3, 90], vec![-1, 0, 5, 77]], |a| {
                i64c(i64c(i64c(a[0] + 1)? * 10000)? + i64c(i64c(a[1] + 1)? * 100)? + 1)
            }),
        Export::new(
            "cross_z",
            &[("ax", I64), ("ay", I64), ("bx", I64), ("by", I64)],
            I64,
        )
        .product(&[k.clone(), k.clone(), k.clone(), k], |a| {
            i64c(i64c(a[0] * a[3])? - i64c(a[1] * a[2])?)
        }),
    ]
});

// ---------------------------------------------------------------------------
// Arrays.
// ---------------------------------------------------------------------------

program_test!(array_basic, "array_basic", {
    vec![
        Export::new(
            "sum5",
            &[("a", U32), ("b", U32), ("c", U32), ("d", U32), ("e", U32)],
            U32,
        )
        .product(
            &[
                vec![0, 7],
                vec![1, 100],
                vec![2, 9],
                vec![3, 4_000_000_000],
                vec![4, 300_000_000],
            ],
            |a| a.iter().try_fold(0, |s, &v| u32c(s + v)),
        ),
        // Index 4 and above are outside the array.
        Export::new("pick", &[("i", U32)], U32)
            .product(&[vec![0, 1, 2, 3, 4, 5, 1000, 4_294_967_295]], |a| {
                at(&[10, 20, 30, 40], a[0])
            }),
        Export::new("squares", &[("n", U32)], U32).product(
            &[vec![0, 1, 100, 4_294_967_246, 4_294_967_247, 4_294_967_295]],
            |a| {
                let mut all = Vec::new();
                for i in 0..8i128 {
                    all.push(u32c(i * i + a[0])?);
                }
                u32c(all[3] + all[7])
            },
        ),
        Export::new("for_sum", &[("n", U32)], U32).product(
            &[vec![0, 1, 2, 1000, 204_522_252, 204_522_253, 4_294_967_295]],
            |a| (1..=6i128).try_fold(0, |s, x| u32c(s + u32c(x * a[0])?)),
        ),
    ]
});

program_test!(array_bounds, "array_bounds", {
    vec![
        Export::new("read", &[("i", U32)], U8).product(
            &[vec![0, 1, 3, 4, 5, 255, 256, 65_536, 4_294_967_295]],
            |a| at(&[11, 22, 33, 44], a[0]),
        ),
        Export::new("write_then_sum", &[("i", U32), ("v", U8)], U32).product(
            &[vec![0, 1, 5, 6, 7, 100, 4_294_967_295], vec![0, 9, 255]],
            |a| {
                let mut arr = [1i128, 2, 3, 4, 5, 6];
                *arr.get_mut(usize::try_from(a[0]).ok()?)? = a[1];
                arr.iter().try_fold(0, |s, &v| u32c(s * 3 + v))
            },
        ),
        // Reads the element just past the end.
        Export::new("read_at_end", &[], U32).product(&[], |_| None),
        // A write through an index that is in range leaves other arrays alone.
        Export::new("neighbors_untouched", &[("i", U32)], U32).product(
            &[vec![0, 1, 2, 3, 4, 100, 4_294_967_295]],
            |a| {
                let i = usize::try_from(a[0]).ok().filter(|&i| i < 3)?;
                let mut first = [100i128, 200, 300];
                let mut second = [1000i128, 2000, 3000];
                first[i] = 9;
                second[i] = 8;
                Some(first.iter().chain(&second).sum())
            },
        ),
    ]
});

program_test!(array_sort, "array_sort", {
    let seeds = vec![
        [5, 3, 9, 1, 7, 2],
        [1, 2, 3, 4, 5, 6],
        [6, 5, 4, 3, 2, 1],
        [0, 0, 0, 0, 0, 0],
        [-4, 8, -4, 8, i128::from(i32::MIN), i128::from(i32::MAX)],
    ];
    let mut kth = Export::new(
        "kth",
        &[
            ("a", I32),
            ("b", I32),
            ("c", I32),
            ("d", I32),
            ("e", I32),
            ("f", I32),
            ("k", U32),
        ],
        I32,
    );
    let mut sorted_flag = Export::new(
        "is_sorted_after",
        &[
            ("a", I32),
            ("b", I32),
            ("c", I32),
            ("d", I32),
            ("e", I32),
            ("f", I32),
        ],
        Bool,
    );
    for seed in &seeds {
        let lists: Vec<Vec<i128>> = seed.iter().map(|&v| vec![v]).collect();
        let mut with_k = lists.clone();
        with_k.push(vec![0, 2, 5, 6, 100]);
        let seed = *seed;
        kth = kth.product(&with_k, move |a| {
            let mut sorted = seed.to_vec();
            sorted.sort_unstable();
            at(&sorted, a[6])
        });
        sorted_flag = sorted_flag.product(&lists, |_| Some(1));
    }
    vec![
        kth,
        sorted_flag,
        // The array is [a, b, c, c, c, c]; its third element when sorted.
        Export::new("padded_third", &[("a", I32), ("b", I32), ("c", I32)], I32).product(
            &[vec![-5, 0, 7], vec![-1, 3, 8], vec![-9, 2, 6, 7]],
            |a| {
                let mut sorted = [a[0], a[1], a[2], a[2], a[2], a[2]];
                sorted.sort_unstable();
                Some(sorted[2])
            },
        ),
    ]
});

/// A 3x3 matrix as in `array_matrix.fe`, with checked `i32` arithmetic.
type Matrix = [[i128; 3]; 3];

fn seed_matrix(s: i128) -> Option<Matrix> {
    let mut m = [[0; 3]; 3];
    for (n, cell) in m.iter_mut().flatten().enumerate() {
        *cell = i32c(s + n as i128)?;
    }
    Some(m)
}

fn matrix_mul(a: &Matrix, b: &Matrix) -> Option<Matrix> {
    let mut r = [[0; 3]; 3];
    for i in 0..3 {
        for j in 0..3 {
            let mut acc = 0;
            for k in 0..3 {
                acc = i32c(acc + i32c(a[i][k] * b[k][j])?)?;
            }
            r[i][j] = acc;
        }
    }
    Some(r)
}

program_test!(array_matrix, "array_matrix", {
    let seeds = vec![
        -20,
        -1,
        0,
        1,
        3,
        100,
        1000,
        20_000,
        i128::from(i32::MAX) - 7,
        i128::from(i32::MAX),
    ];
    vec![
        Export::new("trace_of_square", &[("s", I32)], I32).product(
            std::slice::from_ref(&seeds),
            |a| {
                let m = seed_matrix(a[0])?;
                let sq = matrix_mul(&m, &m)?;
                i32c(i32c(sq[0][0] + sq[1][1])? + sq[2][2])
            },
        ),
        Export::new("identity_is_neutral", &[("s", I32)], Bool)
            .product(std::slice::from_ref(&seeds), |a| {
                seed_matrix(a[0]).map(|_| 1)
            }),
        Export::new("cell", &[("s", I32), ("row", U32), ("col", U32)], I32).product(
            &[
                vec![-3, 0, 2, 600],
                vec![0, 1, 2, 3, 4_000_000_000],
                vec![0, 1, 2, 3, 99],
            ],
            |a| {
                let m = matrix_mul(&seed_matrix(a[0])?, &seed_matrix(1)?)?;
                at(m.get(usize::try_from(a[1]).ok()?)?, a[2])
            },
        ),
        Export::new("transpose_sum", &[("s", I32)], I32).product(&[seeds], |a| {
            seed_matrix(a[0])?;
            // t[0][1] = m[1][0], t[2][0] = m[0][2], t[1][2] = m[2][1]
            i32c(i32c(i32c(a[0] + 3)? * 100)? + i32c(i32c(a[0] + 2)? * 10)? + i32c(a[0] + 7)?)
        }),
    ]
});

program_test!(array_copy, "array_copy", {
    let xs = vec![
        -50,
        -3,
        0,
        1,
        7,
        100_000,
        2_147_483_637,
        2_147_483_644,
        i128::from(i32::MAX),
    ];
    vec![
        // `bump_all` returns a changed copy and leaves its argument alone.
        Export::new("value_semantics", &[("x", I32)], I32).product(
            std::slice::from_ref(&xs),
            |a| {
                let original: Vec<i128> = (0..4).map(|i| i32c(a[0] + i)).collect::<Option<_>>()?;
                let bumped: Vec<i128> = original
                    .iter()
                    .map(|&v| i32c(v + 10))
                    .collect::<Option<_>>()?;
                i32c(i32c(i32c(original[0] * 1000)? + i32c(bumped[0] * 10)?)? + bumped[3])
                    .and_then(|s| i32c(s - original[3]))
            },
        ),
        // `add_in_place` changes the caller's array; the saved copy stays.
        Export::new("in_place", &[("x", I32)], I32).product(std::slice::from_ref(&xs), |a| {
            let before: Vec<i128> = (0..4).map(|i| i32c(a[0] + i)).collect::<Option<_>>()?;
            let after: Vec<i128> = before.iter().map(|&v| i32c(v + 5)).collect::<Option<_>>()?;
            let first = i32c(i32c(after[0] * 1000)? + i32c(before[0] * 100)?)?;
            i32c(i32c(first + after[3])? - before[3])
        }),
        Export::new("chosen", &[("flag", Bool), ("x", I32)], I32).product(
            &[vec![0, 1], vec![-8, 0, 5, 20_000_000, 21_474_837]],
            |a| {
                let (first, last) = if a[0] == 1 { (a[1], 3) } else { (4, a[1]) };
                i32c(i32c(first * 100)? + last)
            },
        ),
    ]
});

program_test!(array_of_structs, "array_of_structs", {
    // (weight, value) of each item for a given `n`.
    fn items(n: i128) -> Option<[(i128, i128); 4]> {
        Some([(3, n), (4, u32c(n * 2)?), (5, u32c(n + 7)?), (2, 40)])
    }
    let ns = vec![0, 1, 5, 10, 1000, 2_147_483_648, 4_294_967_288];
    vec![
        Export::new("total_value", &[("n", U32)], U32).product(std::slice::from_ref(&ns), |a| {
            items(a[0])?.iter().try_fold(0, |s, item| u32c(s + item.1))
        }),
        Export::new("best_ratio_index", &[("n", U32)], U32).product(
            std::slice::from_ref(&ns),
            |a| {
                let list = items(a[0])?;
                let mut best = 0;
                for i in 1..4 {
                    let left = u32c(list[i].1 * list[best].0)?;
                    let right = u32c(list[best].1 * list[i].0)?;
                    if left > right {
                        best = i;
                    }
                }
                Some(best as i128)
            },
        ),
        Export::new(
            "update_one",
            &[("n", U32), ("which", U32), ("delta", U32)],
            U32,
        )
        .product(
            &[
                vec![0, 5, 100],
                vec![0, 1, 2, 3, 4, 99],
                vec![0, 7, 4_000_000_000],
            ],
            |a| {
                let mut list = items(a[0])?;
                let slot = usize::try_from(a[1]).ok().filter(|&s| s < 4)?;
                list[slot].1 = u32c(list[slot].1 + a[2])?;
                let v: Vec<i128> = list.iter().map(|item| item.1).collect();
                u32c(u32c(u32c(v[0] * 1_000_000)? + u32c(v[1] * 10_000)?)? + u32c(v[2] * 100)?)
                    .and_then(|s| u32c(s + v[3]))
            },
        ),
        Export::new("knapsack", &[("cap", U32)], U32).product(&[upto(15)], |a| {
            let list = items(10)?;
            let mut best = 0;
            for mask in 0..16 {
                let (mut weight, mut value) = (0, 0);
                for (i, item) in list.iter().enumerate() {
                    if mask & (1 << i) != 0 {
                        weight += item.0;
                        value += item.1;
                    }
                }
                if weight <= a[0] && value > best {
                    best = value;
                }
            }
            Some(best)
        }),
    ]
});

program_test!(array_loops, "array_loops", {
    vec![
        Export::new("for_in_sum", &[("a", I32), ("b", I32), ("c", I32)], I32).product(
            &[
                vec![-3, 0, 9, i128::from(i32::MAX) - 8],
                vec![0, 1, 4],
                vec![-1, 2, i128::from(i32::MAX) - 5],
            ],
            |a| {
                [a[0], a[1], a[2], 4, 5]
                    .iter()
                    .try_fold(0, |s, &x| i32c(s + x))
            },
        ),
        Export::new("for_in_nested", &[("n", U32)], U32).product(
            &[vec![0, 1, 7, 100, 4_294_967_000, 4_294_967_295]],
            |a| {
                let mut s = 0;
                for x in [1, 2, 3] {
                    for y in [10, 20, 30] {
                        s = u32c(s + u32c(x * y + a[0])?)?;
                    }
                }
                Some(s)
            },
        ),
        Export::new(
            "prefix_max_and_sum",
            &[("a", I32), ("b", I32), ("c", I32), ("d", I32)],
            I32,
        )
        .product(
            &[vec![3, -2], vec![1, 8], vec![4, -9], vec![1, 5, 100]],
            |a| {
                let mut run = 0;
                let mut peak = a[0];
                let (mut sums, mut maxes) = ([0; 4], [0; 4]);
                for i in 0..4 {
                    run = i32c(run + a[i])?;
                    peak = peak.max(a[i]);
                    sums[i] = run;
                    maxes[i] = peak;
                }
                i32c(i32c(i32c(sums[3] * 1000)? + i32c(maxes[1] * 100)?)? + i32c(maxes[2] * 10)?)
                    .and_then(|s| i32c(s + sums[0]))
            },
        ),
        Export::new(
            "first_negative",
            &[("a", I32), ("b", I32), ("c", I32), ("d", I32)],
            I32,
        )
        .product(&[vec![1, -1], vec![0, -2], vec![5, -3], vec![2, -4]], |a| {
            Some(a.iter().position(|&v| v < 0).map_or(-1, |i| i as i128))
        }),
        Export::new("skip_and_stop", &[("limit", U32)], U32).product(&[upto(14)], |a| {
            let mut s = 0;
            for x in [5, 6, 7, 8, 9, 10, 11, 12] {
                if x % 2 == 0 {
                    continue;
                }
                if x > a[0] {
                    break;
                }
                s += x;
            }
            Some(s)
        }),
    ]
});

const SORTED: [i128; 8] = [-20, -3, 0, 4, 9, 15, 15, 42];

program_test!(array_search, "array_search", {
    vec![
        // Binary search for the lower bound; the index of the first match.
        Export::new("binary_search", &[("key", I32)], I32).product(
            &[vec![-100, -20, -3, 0, 1, 4, 9, 15, 16, 42, 43]],
            |a| {
                let lo = SORTED.partition_point(|&v| v < a[0]);
                Some(if lo < 8 && SORTED[lo] == a[0] {
                    lo as i128
                } else {
                    -1
                })
            },
        ),
        Export::new("count_equal", &[("key", I32)], U32)
            .product(&[vec![-100, 0, 15, 42, 7]], |a| {
                Some(SORTED.iter().filter(|&&v| v == a[0]).count() as i128)
            }),
        Export::new("middle_pair_sum", &[("shift", I32)], I32).product(
            &[vec![
                -9,
                0,
                4,
                i128::from(i32::MAX) - 6,
                i128::from(i32::MAX) - 5,
            ]],
            |a| {
                for i in 1..=6 {
                    i32c(a[0] + i)?;
                }
                i32c(i32c(a[0] + 3)? + i32c(a[0] + 4)?)
            },
        ),
    ]
});

// ---------------------------------------------------------------------------
// Constants.
// ---------------------------------------------------------------------------

program_test!(const_array, "const_array", {
    const PRIMES: [i128; 8] = [2, 3, 5, 7, 11, 13, 17, 19];
    const SQUARES: [i128; 6] = [0, 1, 4, 9, 16, 25];
    vec![
        Export::new("prime", &[("i", U32)], U32)
            .product(&[vec![0, 1, 2, 7, 8, 9, 4_294_967_295]], |a| {
                at(&PRIMES, a[0])
            }),
        Export::new("square", &[("i", U32)], U64)
            .product(&[vec![0, 1, 5, 6, 7]], |a| at(&SQUARES, a[0])),
        Export::new("sign_sum", &[("n", U32)], I32).product(&[upto(12)], |a| {
            let signs = [1, -1, 1, -1];
            Some((0..a[0]).map(|i| signs[(i % 4) as usize]).sum())
        }),
        Export::new("sum_primes", &[("n", U32)], U32).product(&[upto(11)], |a| {
            let mut s = 0;
            for i in 0..a[0] {
                s = u32c(s + at(&PRIMES, i)?)?;
            }
            Some(s)
        }),
        Export::new("largest_below", &[("x", U32)], U32).product(
            &[vec![0, 2, 3, 4, 12, 19, 20, 4_294_967_295]],
            |a| {
                Some(
                    PRIMES
                        .iter()
                        .copied()
                        .filter(|&p| p < a[0])
                        .max()
                        .unwrap_or(0),
                )
            },
        ),
    ]
});

program_test!(const_struct, "const_struct", {
    // (key, rgb, scale) of the constant table.
    const TABLE: [(i128, [i128; 3], i128); 3] = [
        (7, [255, 128, 0], -2),
        (9, [1, 2, 3], 10),
        (300, [10, 20, 30], 1000),
    ];
    const GRID: [[i128; 3]; 2] = [[1, 2, 3], [4, 5, 6]];
    vec![
        Export::new("find_scale", &[("key", U16)], I32)
            .product(&[vec![0, 7, 8, 9, 10, 300, 301, 65_535]], |a| {
                Some(TABLE.iter().find(|e| e.0 == a[0]).map_or(0, |e| e.2))
            }),
        Export::new("brightness", &[("i", U32)], U32).product(&[vec![0, 1, 2, 3, 100]], |a| {
            Some(at(&TABLE, a[0])?.1.iter().sum())
        }),
        Export::new("origin_distance", &[("r", U8), ("g", U8), ("b", U8)], U32)
            .product(&[vec![0, 1, 255], vec![0, 17], vec![0, 200]], |a| {
                Some(a[0] + a[1] + a[2])
            }),
        Export::new("grid", &[("row", U32), ("col", U32)], U8)
            .product(&[vec![0, 1, 2, 3], vec![0, 1, 2, 3, 4]], |a| {
                at(at(&GRID, a[0])?.as_slice(), a[1])
            }),
    ]
});

/// The CRC-32 table of the polynomial 0xedb88320.
fn crc_table() -> [u32; 256] {
    let mut table = [0u32; 256];
    for (n, entry) in table.iter_mut().enumerate() {
        let mut c = n as u32;
        for _ in 0..8 {
            c = if c & 1 != 0 {
                0xedb8_8320 ^ (c >> 1)
            } else {
                c >> 1
            };
        }
        *entry = c;
    }
    table
}

program_test!(const_crc, "const_crc", {
    let table = crc_table();
    vec![
        Export::new("table_entry", &[("i", U32)], U32)
            .product(&[vec![0, 1, 2, 17, 128, 254, 255, 256, 1000]], move |a| {
                at(&table.map(i128::from), a[0])
            }),
        // CRC-32 of a pseudo-random byte stream of `n` bytes.
        Export::new("crc_of_stream", &[("seed", U32), ("n", U32)], U32).product(
            &[vec![0, 1, 77, 4_294_967_295], vec![0, 1, 9, 100, 700]],
            move |a| {
                let table = crc_table();
                let mut crc = 0xffff_ffffu32;
                for i in 0..a[1] as u32 {
                    let mixed = (a[0] as u32)
                        .wrapping_add(i)
                        .wrapping_mul(2_654_435_761)
                        .wrapping_add(12345);
                    let byte = (mixed >> 24) as u8;
                    crc = table[((crc ^ u32::from(byte)) & 0xff) as usize] ^ (crc >> 8);
                }
                Some(i128::from(crc ^ 0xffff_ffff))
            },
        ),
    ]
});

program_test!(string_bytes, "string_bytes", {
    vec![
        // FNV-1a of "hello".
        Export::new("hello_hash", &[], U32).product(&[], |_| {
            let mut h = 2_166_136_261u32;
            for byte in b"hello" {
                h = (h ^ u32::from(*byte)).wrapping_mul(16_777_619);
            }
            Some(i128::from(h))
        }),
        Export::new("literal_bytes", &[("i", U32)], U8)
            .product(&[vec![0, 1, 2, 6, 7, 8, 4_294_967_295]], |a| {
                at(b"fe-wasm".map(i128::from).as_slice(), a[0])
            }),
        Export::new("count_byte", &[("target", U8)], U32).product(
            &[vec![0, 105, 109, 112, 115, 255]],
            |a| {
                Some(
                    b"mississippi"
                        .iter()
                        .filter(|&&b| i128::from(b) == a[0])
                        .count() as i128,
                )
            },
        ),
    ]
});

// ---------------------------------------------------------------------------
// Tuples, enums and other aggregates.
// ---------------------------------------------------------------------------

program_test!(tuple_basic, "tuple_basic", {
    vec![
        Export::new("quotient_and_remainder", &[("a", U32), ("b", U32)], U32).product(
            &[vec![0, 17, 4_294_967_295], vec![0, 1, 5, 4_294_967_295]],
            |a| {
                if a[1] == 0 {
                    return None;
                }
                u32c((a[0] / a[1]) * 1000 + a[0] % a[1])
            },
        ),
        Export::new("range_of", &[("a", I32), ("b", I32), ("c", I32)], I32).product(
            &[some_i32(), vec![-9, 4], vec![2, i128::from(i32::MIN)]],
            |a| i32c(a.iter().max()? - a.iter().min()?),
        ),
        Export::new("nested_fields", &[("a", I32), ("b", I32)], I32).product(
            &[vec![-3, 0, 2, 100_000, 30_000_000], vec![-7, 0, 5, 9000]],
            |a| i32c(i32c(i32c(a[0] * 100)? + i32c(a[1] * 10)?)? + i32c(a[0] * a[1])?),
        ),
        Export::new("array_of_pairs", &[("n", I32)], I32).product(&[some_i32()], |a| {
            let pairs = [(a[0], 1), (i32c(a[0] + 1)?, 2), (i32c(a[0] + 2)?, 3)];
            pairs.iter().try_fold(0, |s, p| i32c(s + i32c(p.0 * p.1)?))
        }),
    ]
});

program_test!(sieve, "sieve", {
    let is_prime = |n: i128| n >= 2 && (2..n).all(|d| n % d != 0);
    vec![
        // The array has 64 entries; the loop reads entry `n - 1` and below.
        Export::new("count_primes_below", &[("n", U32)], U32).product(
            &[vec![0, 1, 2, 3, 10, 50, 63, 64, 65, 100, 4_294_967_295]],
            move |a| {
                if a[0] > 64 {
                    return None;
                }
                Some((2..a[0]).filter(|&n| is_prime(n)).count() as i128)
            },
        ),
        Export::new("is_prime", &[("n", U32)], Bool).product(
            &[vec![0, 1, 2, 3, 4, 9, 61, 62, 63, 64, 65, 4_294_967_295]],
            move |a| {
                if a[0] >= 64 {
                    return None;
                }
                Some(i128::from(a[0] >= 2 && is_prime(a[0])))
            },
        ),
    ]
});

program_test!(fib_table, "fib_table", {
    let table = |n: usize| -> i128 {
        let mut t = vec![0i128, 1];
        for i in 2..=n.max(1) {
            t.push(t[i - 1] + t[i - 2]);
        }
        t[n]
    };
    vec![
        Export::new("fib", &[("n", U32)], U64)
            .product(&[vec![0, 1, 2, 10, 38, 39, 40, 41, 1000]], move |a| {
                (a[0] < 40).then(|| table(a[0] as usize))
            }),
        Export::new("sum_to", &[("n", U32)], U64)
            .product(&[vec![0, 1, 2, 10, 38, 39, 40, 41]], move |a| {
                (a[0] < 40).then(|| (0..=a[0] as usize).map(table).sum())
            }),
    ]
});

/// The ring buffer of `ring_buffer.fe`: four slots, a head and a length.
struct Ring {
    items: [i128; 4],
    head: usize,
    len: usize,
}

impl Ring {
    fn new() -> Self {
        Ring {
            items: [0; 4],
            head: 0,
            len: 0,
        }
    }

    fn push(&mut self, value: i128) -> bool {
        if self.len == 4 {
            return false;
        }
        let tail = (self.head + self.len) % 4;
        self.items[tail] = value;
        self.len += 1;
        true
    }

    /// `None` where the length underflows.
    fn pop(&mut self) -> Option<i128> {
        let value = self.items[self.head];
        self.head = (self.head + 1) % 4;
        self.len = self.len.checked_sub(1)?;
        Some(value)
    }
}

program_test!(ring_buffer, "ring_buffer", {
    let values = vec![0, 1, 5, 4_000, 30_000];
    vec![
        Export::new(
            "script",
            &[("a", U32), ("b", U32), ("c", U32), ("d", U32), ("e", U32)],
            U32,
        )
        .product(
            &[
                vec![1, 2, 9],
                vec![2, 5],
                vec![3, 6],
                vec![4, 1, 700],
                values.clone(),
            ],
            |a| {
                let mut r = Ring::new();
                r.push(a[0]);
                r.push(a[1]);
                let x = r.pop()?;
                r.push(a[2]);
                r.push(a[3]);
                let rejected = !r.push(a[4]);
                r.push(a[4]);
                let y = r.pop()?;
                let z = r.pop()?;
                u32c(u32c(u32c(x * 1_000_000)? + u32c(y * 10_000)?)? + u32c(z * 100)?)
                    .and_then(|s| u32c(s + i128::from(rejected)))
            },
        ),
        // Popping an empty buffer underflows its length.
        Export::new("underflow", &[], U32).product(&[], |_| None),
        Export::new("wraps", &[("n", U32)], U32).product(
            &[vec![0, 1, 3, 4, 5, 10, 100, 1000, 70_000]],
            |a| {
                let mut r = Ring::new();
                let mut total = 0;
                for i in 0..a[0] {
                    r.push(i + 1);
                    total = u32c(total + u32c(r.pop()? * (i + 1))?)?;
                }
                u32c(total + r.head as i128)
            },
        ),
    ]
});

program_test!(bitset, "bitset", {
    let bits = vec![0, 1, 63, 64, 70, 127, 128, 255, 256, 1000];
    vec![
        Export::new(
            "count_after",
            &[("a", U32), ("b", U32), ("c", U32), ("d", U32)],
            U32,
        )
        .product(
            &[
                bits.clone(),
                vec![5, 64, 255],
                vec![5, 192, 256],
                vec![0, 5, 64, 192, 300],
            ],
            |a| {
                let mut set = [false; 256];
                for &bit in &a[..3] {
                    *set.get_mut(usize::try_from(bit).ok()?)? = true;
                }
                *set.get_mut(usize::try_from(a[3]).ok()?)? = false;
                Some(set.iter().filter(|&&b| b).count() as i128)
            },
        ),
        Export::new("has", &[("a", U32), ("b", U32), ("q", U32)], Bool).product(
            &[vec![3, 255], vec![70, 3], bits],
            |a| {
                let mut set = [false; 256];
                for &bit in &a[..2] {
                    *set.get_mut(usize::try_from(bit).ok()?)? = true;
                }
                Some(i128::from(*set.get(usize::try_from(a[2]).ok()?)?))
            },
        ),
    ]
});

/// The stack machine of `stack_vm.fe`.
fn run_vm(input: i128, steps: i128) -> Option<i128> {
    const PROGRAM: [(u8, i128); 10] = [
        (1, 7),
        (2, 0),
        (3, 0),
        (1, 3),
        (4, 0),
        (5, 0),
        (3, 0),
        (1, 100),
        (6, 0),
        (0, 0),
    ];
    let mut stack = [0i128; 8];
    let mut sp = 0usize;
    let mut pc = 0usize;
    let mut executed = 0;
    while executed < steps {
        let (op, arg) = *PROGRAM.get(pc)?;
        pc += 1;
        executed += 1;
        match op {
            0 => break,
            1 => {
                *stack.get_mut(sp)? = arg;
                sp += 1;
            }
            2 => {
                *stack.get_mut(sp)? = input;
                sp += 1;
            }
            3 | 4 | 6 => {
                sp = sp.checked_sub(1)?;
                let below = *stack.get(sp.checked_sub(1)?)?;
                let top = stack[sp];
                stack[sp - 1] = match op {
                    3 => i32c(below + top)?,
                    4 => i32c(below * top)?,
                    _ => i32c(below - top)?,
                };
            }
            _ => {
                let top = *stack.get(sp.checked_sub(1)?)?;
                *stack.get_mut(sp)? = top;
                sp += 1;
            }
        }
    }
    stack.get(sp.checked_sub(1)?).copied()
}

program_test!(stack_vm, "stack_vm", {
    vec![
        Export::new("run", &[("input", I32), ("steps", U32)], I32).product(
            &[
                vec![
                    -5,
                    0,
                    5,
                    1000,
                    50_000_000,
                    2_000_000_000,
                    i128::from(i32::MIN),
                ],
                vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20],
            ],
            |a| run_vm(a[0], a[1]),
        ),
    ]
});

program_test!(enum_basic, "enum_basic", {
    vec![
        Export::new("area_of", &[("kind", U32), ("a", U32), ("b", U32)], U32).product(
            &[
                vec![0, 1, 2, 9],
                vec![0, 3, 70_000, 4_294_967_295],
                vec![0, 4, 70_000],
            ],
            |a| match a[0] {
                0 => u32c(u32c(3 * a[1])? * a[1]),
                1 => u32c(a[1] * a[2]),
                _ => Some(0),
            },
        ),
        Export::new("total", &[("a", U32), ("b", U32)], U32).product(
            &[vec![0, 2, 30_000, 4_294_967_295], vec![0, 3, 100_000]],
            |a| {
                let circle = u32c(u32c(3 * a[0])? * a[0])?;
                let sum = u32c(circle)?;
                let rect = u32c(a[0] * a[1])?;
                u32c(u32c(sum + rect)? + 1000)
            },
        ),
    ]
});

program_test!(recursion_frames, "recursion_frames", {
    // `descend` adds 1 per level, plus the two pads at the bottom: 4 * limit.
    vec![
        Export::new("depth_sum", &[("limit", U32)], U32)
            .product(&[vec![0, 1, 2, 10, 40, 300, 1_000_000]], |a| {
                (a[0] < 1_000_000).then(|| 4 * a[0])
            }),
        Export::new("halving_sum", &[("x", U32)], U32).product(
            &[vec![
                0,
                1,
                100,
                4_294_967_260,
                4_294_967_288,
                4_294_967_289,
                4_294_967_295,
            ]],
            |a| {
                let items: Vec<i128> = (0..8).map(|i| u32c(a[0] + i)).collect::<Option<_>>()?;
                items.iter().try_fold(0, |s, &v| u32c(s + v))
            },
        ),
    ]
});

program_test!(frame_sizes, "frame_sizes", {
    vec![
        Export::new("big_copy", &[("seed", U32)], U32).product(
            &[vec![0, 1, 3, 1000, 4_294_967_232, 4_294_967_233]],
            |a| {
                let filled: Vec<u32> = (0..64)
                    .map(|i| u32c(a[0] + i).map(|v| v as u32))
                    .collect::<Option<_>>()?;
                let checksum = |values: &[u32]| {
                    values
                        .iter()
                        .fold(0u32, |s, &v| s.wrapping_mul(3).wrapping_add(v))
                };
                let mut changed = filled.clone();
                changed[10] = 0;
                Some(i128::from(
                    checksum(&filled).wrapping_sub(checksum(&changed)),
                ))
            },
        ),
        // One more copy of a 256-byte array per level; the stack is 1 MiB, so
        // a million levels run out of it, as they run out of the host stack.
        Export::new("recurse", &[("n", U32)], U32)
            .product(&[vec![0, 1, 2, 10, 100, 400, 1_000_000]], |a| {
                (a[0] < 1_000_000).then(|| 64 + a[0])
            }),
        Export::new("bytes_hash", &[("seed", U8)], U32).product(&[vec![0, 1, 7, 128, 255]], |a| {
            let mut h = 2_166_136_261u32;
            for i in 0..512u32 {
                let byte = (i as u8) ^ (a[0] as u8);
                h = (h ^ u32::from(byte)).wrapping_mul(16_777_619);
            }
            Some(i128::from(h))
        }),
    ]
});

program_test!(phi_aggregates, "phi_aggregates", {
    vec![
        // Two boxes swap each round and the new one is a widened copy.
        Export::new("alternate_boxes", &[("n", U32)], I32).product(&[upto(9)], |a| {
            let widen = |b: (i128, i128)| (b.0 - 1, b.1 + 1);
            let (mut left, mut right) = ((0, 1), (10, 20));
            for _ in 0..a[0] {
                let next = widen(left);
                left = right;
                right = next;
            }
            i32c(i32c(i32c(left.0 * 1000)? + i32c(left.1 * 10)?)? + right.0)
                .and_then(|s| i32c(s - right.1))
        }),
        Export::new("pick_by_parity", &[("n", U32)], I32).product(&[upto(12)], |a| {
            let mut best = (0, 0);
            for k in 0..a[0] {
                let candidate = if k % 2 == 0 { (k, k + 5) } else { (-k, k * 2) };
                if candidate.1 > best.1 {
                    best = candidate;
                }
            }
            i32c(best.0 * 100 + best.1)
        }),
        Export::new("array_phi", &[("n", U32)], I32).product(
            &[vec![0, 1, 2, 3, 10, 20, 30, 40, 44, 45, 46, 100]],
            |a| {
                let (mut cur, mut prev) = ([1i128, 2, 3], [0i128; 3]);
                for _ in 0..a[0] {
                    let next = [cur[1], cur[2], i32c(i32c(cur[0] + cur[1])? + cur[2])?];
                    prev = cur;
                    cur = next;
                }
                i32c(i32c(cur[2] * 100)? + prev[2])
            },
        ),
    ]
});

// ---------------------------------------------------------------------------
// Dynamic allocation.
// ---------------------------------------------------------------------------

program_test!(vec_arena, "vec_arena", {
    vec![
        // 0, 3, 6, ... pushed to a vector that grows by reallocation.
        Export::new("vec_sum", &[("n", U32)], U32)
            .product(&[vec![0, 1, 2, 3, 5, 9, 100, 1000, 100_000]], |a| {
                (0..a[0]).try_fold(0, |s, i| u32c(s + u32c(i * 3)?))
            }),
        // The elements pop in reverse order: sum of (n - k + 1) * k.
        Export::new("vec_pop_all", &[("n", U32)], U32)
            .product(&[vec![0, 1, 2, 5, 17, 1000, 2000, 3000]], |a| {
                (1..=a[0]).try_fold(0, |s, k| u32c(s + u32c((a[0] - k + 1) * k)?))
            }),
        Export::new("zeroed_then_filled", &[("n", U32), ("at", U32)], U32).product(
            &[
                vec![0, 3, 1_000_000, 2_147_483_648],
                vec![0, 2, 7, 8, 4_294_967_295],
            ],
            |a| {
                let mut cells = [0i128; 8];
                *cells.get_mut(usize::try_from(a[1]).ok()?)? = a[0];
                cells.iter().try_fold(0, |s, &v| u32c(u32c(s * 2)? + v))
            },
        ),
        // Blocks that are live together do not overlap.
        Export::new(
            "live_blocks_keep_their_contents",
            &[("a", U32), ("b", U32), ("c", U32)],
            U32,
        )
        .product(
            &[
                vec![0, 1, 7, 400_000_000],
                vec![0, 2, 11],
                vec![0, 3, 123_456_789],
            ],
            |a| {
                let mut cells = std::iter::repeat_n(a[0], 10)
                    .chain(std::iter::repeat_n(a[1], 20))
                    .chain(std::iter::repeat_n(a[2], 30));
                cells.try_fold(0, |s, v| u32c(s + v))
            },
        ),
    ]
});

// ---------------------------------------------------------------------------
// Freed memory is reused.
// ---------------------------------------------------------------------------

program_test!(heap_churn, "heap_churn", {
    vec![
        // Half of the rounds write an odd number; an empty block has no first word
        // to write, so it traps.
        Export::new("churn", &[("rounds", U32), ("n", U32)], U32).product(
            &[vec![0, 1, 2, 7, 1000], vec![0, 1, 4, 100]],
            |a| {
                if a[1] == 0 && a[0] > 0 {
                    None
                } else {
                    Some(a[0] / 2)
                }
            },
        ),
        Export::new("vec_churn", &[("rounds", U32)], U32)
            .product(&[vec![0, 1, 2, 100, 1000]], |a| u32c(a[0] * 20)),
    ]
});

/// The pages of the memory of the instance.
fn memory_pages(running: &mut Running) -> u64 {
    let memory = running
        .instance
        .get_memory(&mut running.store, "memory")
        .expect("the module exports its memory");
    memory.size(&running.store)
}

#[test]
fn memory_does_not_grow_with_the_rounds_of_a_loop_that_allocates_and_drops() {
    let source = fixture_path("heap_churn");
    let temp = tempfile::tempdir().unwrap();
    for level in ["0", "1", "2", "s"] {
        let bytes = build_wasm(&source, level, &temp.path().join(format!("o{level}")));
        for (rounds, words) in [(100_000i128, 100i128), (100_000, 1000)] {
            let churn = Export::new("churn", &[("rounds", U32), ("n", U32)], U32);
            let mut running = Running::new(&bytes);
            running.call(&churn, &[1, words]).unwrap();
            let after_one = memory_pages(&mut running);
            let odd = running.call(&churn, &[rounds, words]).unwrap();
            assert_eq!(odd, rounds / 2, "O{level}");
            // Before the heap reused freed blocks, 100,000 rounds of 400 bytes
            // grew the memory by 38 MiB (600 pages), and of 4,000 bytes by 381 MiB.
            assert!(
                memory_pages(&mut running) <= after_one + 1,
                "O{level}: {} pages after one round, {} after {rounds}",
                after_one,
                memory_pages(&mut running)
            );
        }
        let vec_churn = Export::new("vec_churn", &[("rounds", U32)], U32);
        let mut running = Running::new(&bytes);
        running.call(&vec_churn, &[1]).unwrap();
        let after_one = memory_pages(&mut running);
        assert_eq!(running.call(&vec_churn, &[50_000]).unwrap(), 1_000_000);
        assert!(
            memory_pages(&mut running) <= after_one + 1,
            "O{level}: a vector that grows and is dropped leaks"
        );
    }
}

#[test]
fn a_fallible_allocation_that_cannot_be_met_is_none_and_not_a_trap() {
    let source = fixture_path("heap_churn");
    let temp = tempfile::tempdir().unwrap();
    for level in ["0", "2"] {
        let bytes = build_wasm(&source, level, &temp.path().join(format!("o{level}")));
        let mut running = Running::new(&bytes);
        let try_huge = Export::new("try_huge", &[], U32);
        assert_eq!(running.call(&try_huge, &[]), Ok(0), "O{level}");
        // The failed request left the instance usable.
        let churn = Export::new("churn", &[("rounds", U32), ("n", U32)], U32);
        assert_eq!(running.call(&churn, &[10, 10]), Ok(5), "O{level}");
    }
}
